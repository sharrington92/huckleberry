{
  library(tidyverse)
  library(lubridate)
  
  
  theme_set(theme_bw())
}
# https://weather.wsu.edu/

files <- list.files(
  "Data/Addy WA/", pattern = ".csv",
  full.names = T
)

files[[6]] %>% 
  read_csv(., skip = 1) %>% colnames()
  rename_with(~c(
    "date", "day", "air_temp_min", "air_temp_avg", "air_temp_max", "dp_avg", "rh_avg", "soil_temp_2in", "soil_temp_8in_min", "soil_temp_8in_avg",
    "swp_2in", "swp_8in", "precip", "solar_rad", "eto_in", "etr_in"
  ))

data <- lapply(files[8:9], function(x){
  read_csv(x)
}) %>% 
  do.call(rbind, .) %>% 
  distinct() %>% 
  rename_with(~c(
    "date", "day", "air_temp_min", "air_temp_avg", "air_temp_max", "dp_avg", "rh_avg", "soil_temp_2in", "soil_temp_8in_min", "soil_temp_8in_avg",
    "swp_2in", "swp_8in", "precip", "solar_rad", "eto_in", "etr_in"
  )) %>% 
  mutate(
    year = year(date),
    day.year = yday(date),
    max.date = ifelse(date == max(date), 1, 0),
    month = as.factor(month(date)),
    cdd = pmax((air_temp_max + air_temp_min) / 2 - 65, 0)
  )

  


# Cooling Degree Days
data %>% 
  # rowwise() %>% 
  filter(day.year > 125) %>% 
  filter(day.year <= 202) %>% 
  group_by(year) %>% 
  mutate(cdd.agg = cumsum(cdd)) %>% 
  ggplot(aes(x = day.year, y = cdd.agg, color = as.factor(year))) +
  geom_line(size = 1) 


# Max/Min Temp
data %>% 
  filter(day.year <= 250) %>% 
  ggplot(aes(x = day.year, y = air_temp_max, color = as.factor(year))) +
  geom_smooth(se = F) +
  # geom_line() +
  theme_bw()


# Precipitation
{
  # Snowpack
  {
    data %>% 
      mutate(
        year.offset = year(date + days(60))
      ) %>% 
      group_by(year.offset) %>% 
      filter(day.year <= 75 | day.year >= 275) %>% # ~Middle of March
      mutate(precip.cum = cumsum(precip)) %>% 
      ungroup() %>% 
      filter(day.year <= 244) %>% # ~ Bgn of Sept.
      ggplot(aes(x = day.year, y = precip.cum, color = as.factor(year))) +
      # geom_smooth(se = F) +
      geom_line(size = 1.1) +
      geom_point(aes(size = as.factor(max.date))) +
      scale_size_manual(values = c(0,5), guide = "none") +
      ggtitle("Winter Snowpack") +
      theme_bw() +
      theme(
        legend.position = "bottom"
      )
  }
  
  # Spring Precipitation
  {
    data %>% 
      group_by(year) %>% 
      filter(day.year >= 75) %>% # ~Middle of March
      mutate(precip.cum = cumsum(precip)) %>% 
      ungroup() %>% 
      filter(day.year <= 244) %>% # ~ Bgn of Sept.
      ggplot(aes(x = day.year, y = precip.cum, color = as.factor(year))) +
      # geom_smooth(se = F) +
      geom_line(size = 1.1) +
      geom_point(aes(size = as.factor(max.date))) +
      ggtitle("Spring Precipitation") +
      scale_size_manual(values = c(0,5), guide = "none") +
      theme_bw() +
      theme(
        legend.position = "bottom"
      )
  }
  
  # Snowpack + Spring Precip
  {
    data %>% 
      mutate(
        year.offset = year(date + days(60))
      ) %>% 
      filter(year.offset != 2015) %>% 
      group_by(year.offset) %>%
      mutate(precip.cum = cumsum(precip)) %>% 
      ungroup() %>% #select(date, day.year, year, year.offset, precip, precip.cum) %>%  View()
      filter(day.year <= 244) %>%  # ~ Bgn of Sept.
      ggplot(aes(x = day.year, y = precip.cum, color = as.factor(year))) +
      # geom_smooth(se = F) +
      geom_line(size = 1.1) +
      geom_point(aes(size = as.factor(max.date))) +
      scale_size_manual(values = c(0,5), guide = "none") +
      ggtitle("Snowpack + Spring Precipitation") +
      theme_bw() +
      theme(
        legend.position = "bottom"
      )
  }
  
  
}


# Growing Degree Days
{
  gdd_baseline <- 42
  gdd_cutoff <- 86
  a.gdd_harvest <- ifelse(gdd_baseline == 42, 2060, ifelse(gdd_baseline == 50, 1313, NULL))
  
  data.gdd <- data %>% 
    mutate(
      gdd = pmin(pmax((air_temp_max + air_temp_min) / 2 - gdd_baseline, 0), gdd_cutoff - gdd_baseline)
    ) %>% #select(date, gdd, max_air_temp, min_air_temp) %>%  View()
    group_by(year) %>% 
    mutate(
      gdd.agg = cumsum(gdd),
      first.harvest = ifelse(gdd.agg >= a.gdd_harvest, 1, 0),
      first.harvest = ifelse(first.harvest == 1 & lag(first.harvest) == 0, 1, 0),
      gdd.remain = sum(gdd) - cumsum(gdd)
    ) %>% 
    ungroup()
  
  # Accumulated GDD
  {
    (chart.a_gdd <- data.gdd %>% 
       filter(year != 2015) %>% 
       # filter(day.year <= 244) %>% 
      ggplot(aes(x = day.year, y = gdd.agg, color = as.factor(year))) +
      geom_line(size = 1) +
      geom_hline(yintercept = a.gdd_harvest, linetype = "dashed") +
      ggrepel::geom_text_repel(
        aes(label = ifelse(first.harvest == 1, format(as.Date(date), "%m-%d"), "")),
        color = "black", max.overlaps = 100, size = 3
      ) +
      geom_point(aes(size = as.factor(max.date))) +
      geom_vline(xintercept = 239, linetype = "dashed") +
      scale_size_manual(values = c(0,5), guide = "none") +
      ggtitle("Accumulated Growing Degree Days", paste0("Baseline: ", gdd_baseline)) +
      theme_bw() +
      theme(
        legend.position = "bottom"
      ))
  }
  
  # Avg gdd / day
  {
    data.gdd %>% 
      filter(day.year <= 250) %>% 
      ggplot(aes(x = day.year, y = gdd, color = as.factor(year))) +
      geom_smooth(se = F) +
      # geom_line() +
      theme_bw()
  }
  
  # Remaining GDD
  {
    data.gdd %>% 
      ggplot(aes(x = day.year, y = gdd.remain, color = as.factor(year))) +
      geom_line(size = 1) +
      geom_vline(xintercept = yday(today()), linetype = "dashed") +
      geom_vline(xintercept = 244, linetype = "dashed") +
      theme_bw()
  }
  
  
  # Predicted Cultivation Date
  {
    
    model.lm <- data.gdd %>% 
      # ggplot(aes(x = day.year, y = gdd, color = year)) +
      # geom_point()
      lm(
        data = .,
        fabletools::box_cox(gdd, 1) ~ day.year*month
          + I(day.year^2)*month
        # + ifelse(day.year < 50, 1, 0)
        # + ifelse(day.year > 320, 1, 0)
      ); summary(model.lm); par(mfrow = c(2,2)); plot(model.lm)
    # par(mfrow = c(2,2)); plot(model.lm)
    
    
    
    cy.gdd.predict <- tibble(
        date = seq.Date(
          from = floor_date(today(), unit = "years"), 
          to = ceiling_date(today(), unit = "years"), 
          by = "1 day"
        ),
        day.year = yday(date),
        month = as.factor(month(date))
      ) %>% 
      predict(model.lm, newdata = .)
    
    gdd.predict <- tibble(
      date = seq.Date(
        from = floor_date(today(), unit = "years"), 
        to = ceiling_date(today(), unit = "years"), 
        by = "1 day"
      ),
      day.year = yday(date),
      month = as.factor(month(date)),
      year = year(date),
      gdd = cy.gdd.predict
    ) 
    
    agdd.cy <- data.gdd %>% 
      slice_max(order_by = date, n = 1) %>% 
      pull(gdd.agg)
    
    gdd.predict <- gdd.predict %>% 
      filter(date >= today()) %>% 
      bind_rows(
        tibble(
          date = today() - 1,
          gdd = agdd.cy,
        )
      ) %>% 
      arrange(date) %>% 
      mutate(
        gdd.agg = cumsum(gdd)
      ) %>% 
      filter(
        day.year != today() - 1,
        year(date) == year(today())
      )
    
    (predicted.harvest <- gdd.predict %>% 
      filter(gdd.agg >= a.gdd_harvest) %>% 
      slice_min(order_by = date, n = 1) %>% 
      pull(date))
    
    chart.a_gdd +
      geom_line(
        data = gdd.predict,
        linetype = "dotdash", color = "black", size = .75
      ) #+
      # ggrepel::geom_text_repel(
      #   data = gdd.predict %>% 
      #     mutate(first.harvest = ifelse(date == predicted.harvest, 1, 0)),
      #   aes(label = ifelse(first.harvest == 1, format(as.Date(date), "%m-%d"), "")),
      #   color = "black", max.overlaps = 100, size = 3
      # )
  }
}




data %>% 
  rowwise() %>% 
  mutate(
    year = year(date),
    above.90 = ifelse(air_temp_max >= 90, 1, 0),
    day.year = yday(date)
  ) %>% 
  filter(day.year <= 202) %>% 
  group_by(year) %>% 
  summarise(above.90 = sum(above.90)) %>% 
  ggplot(aes(x = year, y = above.90, fill = as.factor(year), label = above.90)) +
  geom_col() +
  geom_label(fill = "white")




