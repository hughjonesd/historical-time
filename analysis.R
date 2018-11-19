
# == setup ============

library(tibble)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)

# == data cleaning =============

# all matching "XXXX"
years <- readr::read_tsv("googlebooks-years", col_names = c("to", "from", "count", "vols"))
#
years <- complete(years, to, nesting(from), fill = list(count = 0, vols = 0))

# all matching "year XXXX"
years_rob <- readr::read_tsv("googlebooks-years-robust", col_names = c("to", "from", "count", "vols"))
years_rob$to <- readr::parse_number(years_rob$to)
years_rob <- complete(years_rob, to, nesting(from), fill = list(count = 0, vols = 0))

# from hansard-clean.R
years_han <- readRDS("hansard_year_counts.rds")
years_han %<>% 
      as_tibble(rownames = "from") %>% 
      mutate(from = as.numeric(from)) %>% 
      tidyr::gather("to", "count", -from, convert = TRUE) %>% 
      group_by(from) %>% 
      filter(sum(count) > 0)

years <- full_join(years, years_rob, by = c("from", "to"), suffix = c("", "_rob"))
years <- full_join(years, years_han, by = c("from", "to"), suffix = c("", "_han"))

years <- tidyr::gather(years, source, count, matches("count"), matches("vols")) %>% 
      mutate(source = recode(source,
        "count"      = "ngrams",
        "count_han"  = "hansard",
        "count_rob"  = "ngrams_year",
        "vols"       = "ngrams_vols",
        "vols_rob"   = "ngrams_year_vols"
      ))

years %<>% 
      group_by(from, source) %>% 
      mutate(
        count  = if (source == "hansard" && (from < 1803 || from > 2005)) count else 
                       tidyr::replace_na(count, 0),
        diff   = from - to,
        prop   = count/sum(count),
        prop_past = if_else(diff >= 0, count/sum(count[diff >= 0]), NA_real_),
        decade = 10 * floor(from/10)
      ) %>% 
      tidyr::drop_na() %>% 
      select(source, to, from, decade, diff, everything())


year_qs <- years %>% 
      filter(to <= from) %>% 
      arrange(source, from, to) %>% 
      group_by(from, source) %>% 
      do({
        to <- .$to
        count <- .$count
        qs <- try(Hmisc::wtd.quantile(to, weights = count, probs = 0:10/10), silent = TRUE)
        if (inherits(qs, "try-error")) qs <- rep(NA, 11)
        names(qs) <- 0:10 * 10
        as.data.frame(rbind(qs))
      }) %>% 
      tidyr::gather("quantile", "year", -from, -source, convert = TRUE) %>% 
      mutate(
        diff = from - year
      )

# == plots =================

ggplot2::theme_set(theme_minimal())

raster_plot <- function (mysource, y = to, dep_var) {
  dep_var <- enquo(dep_var)
  y <- enquo(y)
  years %>% 
        filter(source == mysource) %>% 
        ggplot(aes(x = from, y = !! y)) + 
          geom_raster(aes(fill = log(!! dep_var))) +
          scale_fill_viridis_c(option = "C") 
}

# example usage with superimposed quantiles
# raster_plot("ngrams", prop_past, y = diff) +
#       scale_y_reverse(limits = c(150, 0)) +
#       xlim(1870, 1930) +
#       geom_smooth(
#         mapping = aes(from, diff, group = quantile, linetype = quantile != 50),
#         data    = year_qs %>% filter(source == "ngrams", quantile %in% c(20, 50, 80)),
#         size    = 0.5,
#         span    = 0.2,
#         se      = FALSE,
#         colour  = "yellow"
#       )

ref_year_plot <- function(mysource, ref_years, y = prop_past) {
  y <- enquo(y)
  years %>% 
    filter(to %in% ref_years, source == mysource) %>% 
    ggplot(aes(x = from, y = !!y, group = to, 
      colour = factor(to))) + 
    geom_line()
}

backwards_plot <- function (mysource, from_years, y = prop_past) {
  y <- enquo(y)
  years %>% 
    filter(from %in% from_years, source == mysource) %>% 
    ggplot(aes(x = to, y = !!y, group = from, 
      colour = factor(from))) + 
    geom_line()
}

quantile_plot <- function(mysource, quantiles = c(20, 50, 80)) {
  year_qs %>% 
        filter(
          from >= 1800,
          source == mysource,
          quantile %in% quantiles
        ) %>% 
        ggplot(aes(
          x = from, 
          y = diff, 
          colour = factor(quantile), 
          group = factor(quantile)
        )) + 
        geom_line(alpha = 0.6) + 
        geom_smooth(method = "loess", span = 0.1, se = FALSE) + 
        scale_color_viridis_d()
}


# == analyses =====================

options(scipen = 10)
summary(lm(I(year - from) ~ from, year_qs, quantile == 50))
summary(lm(I(year - from) ~ from, year_qs, quantile == 20))
