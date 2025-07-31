# Setup
{
  library(tidyverse)
  library(zoo)
  
  
  theme_set(theme_bw())
}
# https://weather.wsu.edu/

# Inputs 
{
  name.lookup <- tibble(
    original = c(
      "1.5 m Min Air Temp", "1.5 m Avg Air Temp", "1.5 m Max Air Temp",
      "9 m Min Air Temp", "9 m Avg Air Temp", "9 m Max Air Temp",
      "1.5 m Avg Dew Point", "1.5 m Min Rel. Hum", "Solar Radiation",
      "Precipiation", "Precipitation", "2 m Avg Wind Speed", "2 m Max Wind Gust",
      "10 m Avg Wind Speed", "10 m Max Wind Gust", "10 m Avg Wind Dir",
      "2 in Avg Soil Temperature", "2 in Avg Soil Water Pot",
      "8 in Avg Soil Temperature", "8 in Avg Soil Water Pot",
      "ETo", "ETr"
    ),
    name = c(
      "air_temp_min", "air_temp_avg", "air_temp_max", 
      "air_temp_min_9m", "air_temp_avg_9m", "air_temp_max_9m", 
      "dp_avg", "rh_avg", "solar.rad", "precip", "precip", 
      "wind.speed", "wind.gust", 
      "wind.speed.10m", "wind.gust.10m", "wind.dir.10m",
      "soil_temp_2in", "soil.water.2in",
      "soil_temp_8in", "soil.water.8in",
      "eto", "etr"
    )
  )
}

# Data Prep
{
  files <- list.files(
    "Data/", pattern = ".csv",
    full.names = T, include.dirs = FALSE
  )
  
  dlist <- list()
  
  for(f in 1:length(files)){
    lines <- files[[f]] %>% 
      read_lines()
    
    # section.starts <- str_which(lines, "Addy|Ephrata")
    section.starts <- str_which(lines, "Date")
    
    dlist.s <- list()
    
    for(s in 1:length(section.starts)){
      station <- lines[section.starts[s]-2] %>% 
        str_extract(., "[:alpha:]*(?=,)")
      
      dlist.s[[s]] <- files[[f]] %>% 
        read_csv(
          skip = section.starts[s]-1, 
          n_max = coalesce(section.starts[s+1] - section.starts[s], 366)
        ) %>% 
        mutate(
          station = station,
          # Date = ymd(Date)
          across(everything(), as.character)
        ) %>% 
        select(-contains("..."))
    }
    
    dlist[[f]] <- dlist.s# %>% do.call(bind_rows, .)
  }
  
  
  data <- dlist %>% 
    do.call(bind_rows, .) %>% 
    relocate(station, .after = Date) %>% 
    mutate(Date = ymd(Date)) %>% 
    filter(!is.na(Date)) %>% 
    pivot_longer(-c(Date, station), names_to = "original") %>% 
    left_join(name.lookup, join_by(original)) %>% 
    select(-original) %>% 
    # group_by(Date, station, name) %>% add_count() %>% filter(n>1)
    pivot_wider(names_from = name, values_from = value) %>% 
    rename(date = Date) %>% 
    mutate(
      across(
        c(
          contains("temp"), contains("dp"), contains("rh"),
          contains("solar"), contains("precip"), contains("speed"),
          contains("gust"), contains("water"), eto, etr
        ),
        as.numeric
      )
    ) %>% 
    mutate(
      year = year(date),
      day.year = yday(date),
      max.date = ifelse(date == max(date), 1, 0),
      current.year = ifelse(year == year(today()), 1, 0),
      month = as.factor(month(date)),
      cdd = pmax((air_temp_max + air_temp_min) / 2 - 65, 0),
      gdd.b40 = pmax((air_temp_max + air_temp_min) / 2 - 40, 0),
      gdd.b40.c85 = pmax(pmin((air_temp_max + air_temp_min) / 2 - 40, 85), 0),
    ) %>% 
    filter(station == "Addy")
  
  
  
  # data <- files %>% 
  #   read_csv(skip = 2) %>% 
  #   distinct() %>% 
  #   select(-contains("...")) %>% 
  #   rename_with(~c(
  #     "date", "air_temp_min", "air_temp_avg", "air_temp_max", 
  #     "air_temp_min_9m", "air_temp_avg_9m", "air_temp_max_9m", 
  #     "dp_avg", "rh_avg", "solar.rad", "precip", 
  #     "wind.speed", "wind.gust", 
  #     "wind.speed.10m", "wind.gust.10m", "wind.dir.10m",
  #     "soil_temp_2in", "soil.water.2in",
  #     "soil_temp_8in", "soil.water.8in",
  #     "eto", "etr"
  #   )) %>% 
  #   mutate(
  #     year = year(date),
  #     day.year = yday(date),
  #     max.date = ifelse(date == max(date), 1, 0),
  #     current.year = ifelse(year == year(today()), 1, 0),
  #     month = as.factor(month(date)),
  #     cdd = pmax((air_temp_max + air_temp_min) / 2 - 65, 0),
  #     gdd.b40 = pmax((air_temp_max + air_temp_min) / 2 - 40, 0),
  #     gdd.b40.c85 = pmax(pmin((air_temp_max + air_temp_min) / 2 - 40, 85), 0),
  #   )
}

  


# Cooling Degree Days
data %>% 
  # rowwise() %>% 
  filter(day.year > 125) %>% 
  filter(day.year <= 202) %>% 
  group_by(year) %>% 
  mutate(cdd.agg = cumsum(cdd)) %>% 
  ggplot(aes(x = day.year, y = cdd.agg)) +
  geom_line(size = 1, aes(color = as.factor(year))) +
  geom_smooth(se = F, color = "black")

# Growing Degree Days
data %>% 
  # rowwise() %>% 
  filter(day.year > 100) %>% 
  filter(day.year <= yday(today())) %>% 
  group_by(year) %>% 
  mutate(gdd.agg = cumsum(gdd.b40)) %>% 
  ggplot(aes(x = day.year, y = gdd.agg)) +
  geom_line(
    aes(color = as.factor(year), linewidth = factor(current.year))
  ) +
  geom_smooth(se = F, color = "black") +
  scale_linewidth_manual(values = c(.5, 1.5)) +
  scale_y_continuous(
    # trans = "log"
  )


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
      ggplot(aes(x = day.year, y = precip.cum)) +
      geom_line(
        aes(color = as.factor(year), linewidth = factor(current.year))
      ) +
      geom_smooth(se = F, color = "black") +
      # geom_point(aes(size = as.factor(max.date))) +
      scale_linewidth_manual(values = c(.5, 1.5)) +
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
      ggplot(aes(x = day.year, y = precip.cum)) +
      geom_line(
        aes(color = as.factor(year), linewidth = factor(current.year))
      ) +
      geom_smooth(se = F, color = "black") +
      # geom_point(aes(size = as.factor(max.date))) +
      scale_linewidth_manual(values = c(.5, 1.5)) +
      scale_size_manual(values = c(0,5), guide = "none") +
      ggtitle("Spring Precipitation") +
      scale_size_manual(values = c(0,5), guide = "none") +
      scale_color_viridis_d(option = "H") +
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
      ggplot(aes(x = day.year, y = precip.cum)) +
      geom_line(
        aes(color = as.factor(year), linewidth = factor(current.year))
      ) +
      geom_smooth(se = F, color = "black") +
      # geom_point(aes(size = as.factor(max.date))) +
      scale_linewidth_manual(values = c(.5, 1.5)) +
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
       filter(day.year <= 244) %>% 
       filter(day.year >= 190) %>%
        ggplot(aes(x = day.year, y = gdd.agg, color = as.factor(year))) +
       # geom_text(aes(label = year, group = year)) +
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
    
    #
    {
      data.gdd %>% 
        ggplot(aes(x = day.year, y = gdd, color = factor(year))) +
        geom_point()
      
      data.gdd %>% 
        ggplot(aes(x = air_temp_avg, y = gdd, color = factor(year))) +
        geom_point()
      
      data.gdd %>% 
        ggplot(aes(x = day.year, y = air_temp_avg, color = factor(year))) +
        geom_point()
    }
    
    # Linear Model
    {
      model.lm <- data.gdd %>% 
        lm(
          data = .,
          fabletools::box_cox(gdd, 1) ~ day.year#*month
          + I(day.year^2)#*month
          + ifelse(day.year < 75, 1, 0)
          + ifelse(day.year > 300, 1, 0)
        ); summary(model.lm); par(mfrow = c(2,2)); plot(model.lm)
      
      
      bc <- 1.55; model.lm <- data.gdd %>% 
        lm(
          data = .,
          fabletools::box_cox(gdd, bc) ~ fabletools::box_cox(air_temp_avg, bc)
            # day.year#*month
          # + I(day.year^2)#*month
          # + ifelse(day.year < 75, 1, 0)
          # + ifelse(day.year > 300, 1, 0)
        ); summary(model.lm); par(mfrow = c(2,2)); plot(model.lm)
      
    }
    
    {
      avg.temp <- data.gdd %>% 
        group_by(day.year) %>% 
        summarize(air_temp_avg = mean(air_temp_avg)) %>% 
        ungroup() %>% 
        mutate(
          air_temp_avg = rollmean(air_temp_avg, k = 7, fill = NA, align = "center"),
          air_temp_avg = zoo::na.approx(air_temp_avg, rule = 2)
        )
    }
    
    
    cy.gdd.predict <- tibble(
        date = seq.Date(
          from = floor_date(today(), unit = "years"), 
          to = ceiling_date(today(), unit = "years"), 
          by = "1 day"
        ),
        day.year = yday(date),
        month = as.factor(month(date))
      ) %>% 
      left_join(y = avg.temp, by = "day.year") %>% 
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

data %>% 
  group_by(year) %>% 
  filter(day.year >= 50, day.year <= 250) %>% 
  summarize(
    mean = mean(air_temp_avg),
    sd = sd(air_temp_avg),
    p75 = quantile(air_temp_avg, 0.75)
  ) %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value, fill = as.factor(year))) +
  geom_col() +
  geom_text(aes(label = round(value,1))) +
  facet_grid(name ~ ., scales = "free") +
  scale_y_continuous(
    # limits = c(50,70)
    # trans = "log"
  )

data %>% 
  filter(day.year >= 50, day.year <= 250)  %>% 
  ggplot(aes(x = air_temp_avg)) +
  geom_histogram(aes(fill = factor(year))) +
  facet_grid(year ~ .)



