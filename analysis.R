
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)

years <- readr::read_tsv("googlebooks-years", col_names = c("to", "from", "matches", "vols"))

years_rob <- readr::read_tsv("googlebooks-years-robust", col_names = c("to", "from", "matches", "vols"))
years_rob$to <- readr::parse_number(years_rob$to)

years <- years_rob

years$diff <- years$from - years$to
years %<>% 
      group_by(from) %>% 
      mutate(
        prop_matches = matches/sum(matches),
        prop_vols    = vols/sum(vols),
        decade       = 10 * floor(from/10)
      )

year_qs <- years %>% 
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

year_qs %>% 
      filter(
        from >= 1800,
        to < from,
        quantile %in% c(20, 50, 80)
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
      summarize(
        prop_vols    = mean(prop_vols),
        prop_matches = mean(prop_matches)
      ) %>% 
      filter(decade >= 1900, to >= 1800) %>% 
      ggplot(aes(x = to, y = factor(decade), height = prop_matches)) +
        geom_density_ridges(
          stat = "identity", 
          fill = "black", 
          color = "white",
          rel_min_height = 0.02
        ) + 
        theme_ridges()
