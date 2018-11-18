
# == setup ============

library(tibble)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)

# == data cleaning =============

# all matching "XXXX"
years <- readr::read_tsv("googlebooks-years", col_names = c("to", "from", "matches", "vols"))

# all matching "year XXXX"
years_rob <- readr::read_tsv("googlebooks-years-robust", col_names = c("to", "from", "matches", "vols"))
years_rob$to <- readr::parse_number(years_rob$to)

# from hansard-clean.R
years_han <- readRDS("hansard_year_counts.rds")
years_han %<>% 
      as_tibble(rownames = "from") %>% 
      mutate(from = as.numeric(from)) %>% 
      tidyr::gather("to", "matches", -from, convert = TRUE)
years_han %<>% 
      group_by(from) %>% 
      filter(sum(matches) > 0)

years <- years_han
years <- years_rob

years$diff <- years$from - years$to
years %<>% 
      group_by(from) %>% 
      mutate(
        prop_matches = matches/sum(matches),
#        prop_vols    = vols/sum(vols),
        decade       = 10 * floor(from/10)
      )


year_qs <- years %>% 
      filter(to <= from) %>% 
      arrange(from, to) %>% 
      group_by(from) %>% 
      do({
        to <- .$to
        pm <- .$matches
        qs <- Hmisc::wtd.quantile(to, weights = pm, probs = 0:10/10)
        names(qs) <- 0:10*10
        as.data.frame(rbind(qs))
      }) %>% 
      tidyr::gather("quantile", "year", -from, convert = TRUE)

# == plots =================

# nice:
ggplot(years, aes(x = from, y = to)) + 
      geom_raster(aes(fill=log(matches))) +
      scale_fill_viridis_c(option = "C") +
      theme_minimal() + ylim(1800, 2000)

years %>% 
      filter(to %in% 1913:1915, from >= 1900) %>% 
      ggplot(aes(x = from, y = prop_matches, group = to, 
        colour = factor(to))) + 
      geom_line() + 
      scale_y_log10()

year_qs %>% 
      filter(
        from >= 1800,
        quantile %in% c(10, 20, 50, 80, 90)
      ) %>% 
      ggplot(aes(
        x = from, 
        y = year - from, 
        colour = factor(quantile), 
        group = factor(quantile)
      )) + 
      geom_line(alpha = 0.6) + 
      geom_smooth(method = "loess", span = 0.1, se = FALSE) + 
      scale_color_viridis_d() +
      theme_minimal() +
      scale_x_continuous(breaks = seq(1800, 2000, 20)) 
# not sure if log is appropriate?


years %>% 
      group_by(decade, to) %>% 
      # filter(to <= from) %>% 
      summarize(
        # prop_vols    = mean(prop_vols),
        prop_matches = mean(prop_matches)
      ) %>% 
      filter(decade >= 1900, to >= 1800) %>% 
      ggplot(aes(x = to, y = factor(decade), height = prop_matches)) +
        geom_density_ridges(
          stat = "identity", 
          fill = "black", 
          color = "white",
          rel_min_height = 0.01
        ) + 
        theme_ridges() + xlim(1800, 2000)


# == analyses =====================

options(scipen = 10)
summary(lm(I(year - from) ~ from, year_qs, quantile == 50))
summary(lm(I(year - from) ~ from, year_qs, quantile == 20))
