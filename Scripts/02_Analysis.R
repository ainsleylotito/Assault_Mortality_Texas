library(tidyverse)
library(broom)
library(tsibble) 
library(fable)
library(feasts) 
  

source("./Scripts/functions.r") 

#Loading the datafiles previously cleaned, making sure they have the right variable types 
female_full <- read.csv("./CleanData/female_full.csv") 
female_full <- female_full %>% 
  mutate(month_year = ymd(month_year),
         month = yearmonth(month_year))

male_full <- read.csv("./CleanData/male_full.csv") 
male_full <- male_full %>% 
  mutate(month_year = ymd(month_year),
         month = yearmonth(month_year))

# Female analysis ----
##  Descriptive ---- 

#whole study period
female_full %>%
  summarise(
    n_months      = n(),
    total_deaths  = sum(Deaths, na.rm = TRUE),
    monthly_mean  = mean(Deaths, na.rm = TRUE),
    monthly_sd    = sd(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) 

#Pre-and-Post bill8 
female_full %>% 
  mutate(bill8 =  if_else(month_year >= "2021-06-01", TRUE, FALSE)) %>% 
  group_by(bill8) %>% 
  summarise(
    n_months      = n(),
    total_deaths  = sum(Deaths, na.rm = TRUE),
    monthly_mean  = mean(Deaths, na.rm = TRUE),
    monthly_sd    = sd(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) 


#correct number of observations 
#turning into ts 


f_ts <- female_full %>%
  as_tsibble(index = month)

#arima  
f_ts <- create_intervention(f_ts, "2021-06-01")

glimpse(f_ts) 

#before we fit arima, checking for gaps in time: 
# quick TRUE/FALSE
has_gaps(f_ts)

# show where gaps occur
scan_gaps(f_ts) 



f_arima <- fit_arima(f_ts) 

## Female model output ----
report(f_arima)  

#tidying up my output: 
tidy(f_arima) %>% 
  mutate( p.value = format(p.value, scientific = FALSE, digits = 4),
          std.error = round(std.error, 2), 
          estimate = round(estimate,2),
          p.value= round(as.numeric(p.value),2))

## diagnostics ---- 
f_arima %>% gg_tsresiduals() 
augment(f_arima) %>% features(.resid, ljung_box)

### excess deaths ---- 

# Forecast the next 43 periods (e.g., months)
# Create counterfactual version for NEW months
f_cf_data <- f_ts %>%
  filter(month >= make_yearmonth(2021, 6)) %>%
  mutate(
    step = 0,
    ramp = 0
  )

# Generate counterfactual predictions
f_cf_preds <- forecast(
  f_arima,
  new_data = f_cf_data
)

# Compare to observed
excess_deaths_f <- f_cf_preds %>%
  as_tibble() %>%
  select(month, counterfactual = .mean) %>%
  left_join(
    f_ts %>%
      as_tibble() %>%
      select(month, observed = Deaths),
    by = "month"
  ) %>%
  mutate(
    excess_deaths = observed - counterfactual
  )

excess_deaths_f %>% 
  arrange(desc(excess_deaths))



#counterfactual forecast
fc_f <- counterfactual_forecast(
  model_fit = f_arima,
  ts_data = f_ts,
  intervention_date = "2021-06-01"
)

plot_counterfactual(
  fc_f,
  f_ts,
  "2021-06-01",
  "Observed vs Counterfactual Female Assault Deaths"
)


## Female Graph ----
#now let's try customizing the graph:  

arima_graph <- autoplot(fc_f, f_ts, level = 95) +
  
  geom_vline(
    xintercept = make_yearmonth(2021, 6),
    linetype = "dashed",
    color = "grey40"
  ) +
  
  scale_fill_manual(
    values = c("#6C63FF"),
    labels = c("95% Confidence Interval"),
    name = NULL
  ) +
  
  scale_x_yearmonth(date_breaks = "6 months",
                    date_labels = "%b %Y") +
  
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35)) +
  
  labs(
    y = "Monthly Female Assault Deaths",
    x = "Time (Monthly)"
  ) +
  
  coord_cartesian(
    xlim = c(make_yearmonth(1999, 1),
             make_yearmonth(2024, 12))
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  
  annotate(
    "text",
    x = make_yearmonth(2021, 6),
    y = 12,
    label = "Bill 8 Ruling (June 2021)",
    vjust = -1,
    angle = 90,
    size = 3
  )
  

ggsave(
  filename = "Female_ARIMA_Graph.png",
  plot = arima_graph,
  path = "./Output",
  width = 12, 
  height = 6
)

### Residual graph -----

# Extracting residuals
f_resid <- augment(f_arima) 


# Calculate 95% residual limits
resid_sd <- sd(f_resid$.resid, na.rm = TRUE) 
resid_sd 

#finding the values for those 
f_resid %>% 
  filter(.resid > 1.96 * resid_sd)   

#values that are above 25 (outside of 95% CI Range)
excess_months <- f_resid %>% 
  filter(.resid > 1.96 * resid_sd) %>% 
  pull(month) 

#finding just those excess deaths 
excess_deaths_f %>% 
  filter(month %in% excess_months) 


#Graphing
resid_graph <- ggplot(f_resid, aes(x = month, y = .resid)) +
  
  # Residual line + points
  geom_line(aes(color = "ARIMA-derived residuals"), linewidth = 0.7) +
  
  geom_point(aes(color = "ARIMA-derived residuals"),
             size = 1.5) +
  
  # red circles for post-interruption outliers upper limit
  geom_point(
    data = f_resid %>%
      filter(
        month >= make_yearmonth(2021, 6),
        .resid > 1.96 * resid_sd
      ),
    shape = 21,
    stroke = 1.2,
    color = "red",
    fill = NA,
    size = 4
  ) +
  
  # Zero reference line
  geom_hline(yintercept = 0,
             color = "blue") +
  
  # 95% confidence interval lines
  geom_hline(
    aes(yintercept = -1.96 * resid_sd,
        linetype = "95% Confidence Interval"),
    color = "black"
  ) +
  
  geom_hline(
    aes(yintercept = 1.96 * resid_sd,
        linetype = "95% Confidence Interval"),
    color = "black"
  ) +
  
  # Bill 8 intervention line
  geom_vline(
    xintercept = make_yearmonth(2021, 6),
    linetype = "dashed",
    color = "red"
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("ARIMA-derived residuals" = "navy")
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c("95% Confidence Interval" = "dashed")
  ) +
  
  scale_x_yearmonth(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  
  labs(
    y = "ARIMA-derived residuals of monthly 
    assault death counts among 15-44-year-old 
    Texan women",
    x = "Time (Monthly)"
  ) +
  
  coord_cartesian(
    xlim = c(make_yearmonth(1999, 1),
             make_yearmonth(2024, 12))
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  
  annotate(
    "text",
    x = make_yearmonth(2021, 6),
    y = max(f_resid$.resid, na.rm = TRUE),
    label = "Bill 8 (June 2021)",
    color = "red",
    vjust = -0.5,
    hjust = 0.5,
    angle = 0,
    size = 3
  )
ggsave(
  filename = "Female_Resid_Graph.png",
  plot = resid_graph,
  path = "./Output",
  width = 12, 
  height = 6
)
 
#making a version of the graph that also highlights the point 
# that are excessive low deaths 
resid_graph2 <- ggplot(f_resid, aes(x = month, y = .resid)) +
  
  # Residual line + points
  geom_line(aes(color = "ARIMA-derived residuals"), linewidth = 0.7) +
  
  geom_point(aes(color = "ARIMA-derived residuals"),
             size = 1.5) +
  
  # red circles for post-interruption outliers upper limit
  geom_point(
    data = f_resid %>%
      filter(
        month >= make_yearmonth(2021, 6),
        .resid > 1.96 * resid_sd | .resid < -1.96 * resid_sd 
      ),
    shape = 21,
    stroke = 1.2,
    color = "red",
    fill = NA,
    size = 4
  ) +
  
  # Zero reference line
  geom_hline(yintercept = 0,
             color = "blue") +
  
  # 95% confidence interval lines
  geom_hline(
    aes(yintercept = -1.96 * resid_sd,
        linetype = "95% Confidence Interval"),
    color = "black"
  ) +
  
  geom_hline(
    aes(yintercept = 1.96 * resid_sd,
        linetype = "95% Confidence Interval"),
    color = "black"
  ) +
  
  # Bill 8 intervention line
  geom_vline(
    xintercept = make_yearmonth(2021, 6),
    linetype = "dashed",
    color = "red"
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("ARIMA-derived residuals" = "navy")
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c("95% Confidence Interval" = "dashed")
  ) +
  
  scale_x_yearmonth(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  
  labs(
    y = "ARIMA-derived residuals of monthly 
    assault death counts among 15-44-year-old 
    Texan women",
    x = "Time (Monthly)"
  ) +
  
  coord_cartesian(
    xlim = c(make_yearmonth(1999, 1),
             make_yearmonth(2024, 12))
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  
  annotate(
    "text",
    x = make_yearmonth(2021, 6),
    y = max(f_resid$.resid, na.rm = TRUE),
    label = "Bill 8 (June 2021)",
    color = "red",
    vjust = -0.5,
    hjust = 0.5,
    angle = 0,
    size = 3
  )
ggsave(
  filename = "Female_Resid_Graph2.png",
  plot = resid_graph2,
  path = "./Output",
  width = 12, 
  height = 6
)


# Male analysis ---- 

male_full <- read.csv("./CleanData/male_full.csv")

##  Descriptive ----  

#whole study period
male_full %>%
  summarise(
    n_months      = n(),
    total_deaths  = sum(Deaths, na.rm = TRUE),
    monthly_mean  = mean(Deaths, na.rm = TRUE),
    monthly_sd    = sd(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) 

#Pre-and-Post bill8 
male_full %>% 
  mutate(bill8 =  if_else(month_year >= "2021-06-01", TRUE, FALSE)) %>% 
  group_by(bill8) %>% 
  summarise(
    n_months      = n(),
    total_deaths  = sum(Deaths, na.rm = TRUE),
    monthly_mean  = mean(Deaths, na.rm = TRUE),
    monthly_sd    = sd(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) 
#turning into ts 

m_ts <- male_full %>%
  mutate(month = yearmonth(month_year)) %>% 
  as_tsibble(index = month)

#arima  
m_ts <- create_intervention(m_ts, "2021-06-01")

glimpse(m_ts) 

#before we fit arima, checking for gaps in time: 
# quick TRUE/FALSE
has_gaps(m_ts)

# show where gaps occur
scan_gaps(m_ts) 



m_arima <- fit_arima(m_ts)  

## Male model output ----

report(m_arima) 
#step of 4.5, s.e. of 6.8. ramp of -1.5, s.e. of 0.5  

## diagnostics ---- 
m_arima %>% gg_tsresiduals() 
augment(m_arima) %>% features(.resid, ljung_box)

#tidying up my output: 
tidy(m_arima) %>% 
  mutate( p.value = format(p.value, scientific = FALSE, digits = 4),
          std.error = round(std.error, 2), 
          estimate = round(estimate,2),
          p.value= round(as.numeric(p.value),2))

### excess deaths ---- 

# Forecast the next 43 periods (e.g., months)
# Create counterfactual version of observed months
cf_data <- m_ts %>%
  mutate(
    step = 0,
    ramp = 0
  )

# Generate counterfactual predictions
cf_preds <- forecast(
  m_arima,
  new_data = cf_data
)

# Compare to observed
excess_deaths_m <- cf_preds %>%
  as_tibble() %>%
  select(month, counterfactual = .mean) %>%
  left_join(
    m_ts %>%
      as_tibble() %>%
      select(month, observed = Deaths),
    by = "month"
  ) %>%
  mutate(
    excess_deaths = observed - counterfactual
  )

excess_deaths_m %>% 
  slice(270:300)

## plot -----

fc_m <- counterfactual_forecast(m_ts, "2021-06-01", 43)

plot_counterfactual(
  fc_m,
  m_ts,
  "2021-06-01",
  "Observed vs Counterfactual Male Assault Deaths"
)

pre_period_m <- male_full %>% 
  filter(month_year < "2021-06-01")

pre_period_m %>% 
  summarize(mean= mean(Deaths, na.rm=TRUE),
            sd= sd(Deaths, na.rm= TRUE)) 


#now let's try customizing the graph:  

autoplot(fc_m, m_ts, level = 95) +
  geom_vline(
    xintercept = make_yearmonth(2021, 6),
    linetype = "dashed",
    color = "grey40"
  ) + 
  scale_x_yearmonth(date_breaks = "6 months", date_labels = "%b %Y") + 
  labs( y = "Monthly Male Assault Deaths", x = "Month" ) +
  coord_cartesian(xlim = c(make_yearmonth(1999, 1), make_yearmonth(2024, 12)))+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text",
           x = make_yearmonth(2021, 6),
           y = 12,
           label = "Bill 8 Ruling (June 2021)",
           vjust = -1,
           angle = 90,
           size = 3) 

