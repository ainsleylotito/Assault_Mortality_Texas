
create_intervention <- function(ts_data, intervention_date){
  
  # Keeping intervention and month_year as Date
  intervention_date <- as.Date(intervention_date)
  ts_data <- ts_data %>%
    mutate(month_year = as.Date(month_year)) 
  
  # Pulling time for intervention date
  intervention_time <- ts_data %>%
    filter(month_year == intervention_date) %>%
    pull(time)
  
  #Fail-safe in case no death data for intervention month
  if(length(intervention_time) == 0){
    stop("intervention_date not found in ts_data$month_year")
  }
  
  #Creating step and ramp variable
  ts_data %>%
    mutate(
      t = row_number(),
      step = if_else(t >= intervention_time, 1, 0),
      ramp = if_else(t >= intervention_time, t - intervention_time + 1, 0)
    )
}

fit_arima <- function(ts_data){
  
  ts_data %>%
    model(
      arima = ARIMA(
        Deaths ~ step + ramp,
        stepwise = FALSE
      )
    )
  
}

counterfactual_forecast <- function(ts_data, intervention_date, horizon){
  
  model_cf <- ts_data %>%
    filter(month < make_yearmonth(year(intervention_date),
                                  month(intervention_date))) %>%
    model(
      arima_null = ARIMA(Deaths, stepwise = FALSE)
    )
  
  forecast(model_cf, h = horizon)
  
}

plot_counterfactual <- function(fc, ts_data, intervention_date, title){
  
  autoplot(fc, ts_data, level = 95) +
    geom_vline(
      xintercept = as.numeric(as_date(make_yearmonth(year(intervention_date),
                                                     month(intervention_date)))),
      linetype = "dashed",
      color = "grey40"
    ) +
    labs(
      title = title,
      y = "Monthly Deaths",
      x = "Month"
    ) +
    theme_minimal()
  
}