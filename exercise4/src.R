
rm(list = ls())
dev.off()

library(tidyverse)
library(gridExtra)
library(stargazer)
library(forecast)

data <- read_csv(
    file = 'https://raw.githubusercontent.com/adamcihlar/IntroductionToForecastingModels/main/data_presentation/10year_state_bond_norway.csv',
    skip = 10
    ) %>%
    rename(y = IRLTLT01NOM156N)

# plot the original time series
data %>% ggplot(mapping = aes(x = observation_date, y = y, group = 1)) +
        geom_line(color = 'darkblue') +
        theme_bw() +
        scale_x_date(
            name = element_blank(), 
            date_minor_breaks = "1 year", 
            limits = c(min(data$observation_date), max(data$observation_date))) +
        scale_y_continuous(limits = c(0, max(data$y)*1.1),
                           name = element_blank())

get_differences <- function(time_series) {
    stationary <- time_series - dplyr::lag(time_series, n = 1)
    return(stationary)
}

data$dy <- get_differences(data$y)

# plot the original time series
data %>% ggplot(mapping = aes(x = observation_date, y = dy, group = 1)) +
        geom_line(color = 'darkblue') +
        theme_bw() +
        scale_x_date(
            name = element_blank(), 
            date_minor_breaks = "1 year", 
            limits = c(min(data$observation_date), max(data$observation_date))) +
        scale_y_continuous(name = element_blank())

train_data <- data %>%
    filter(observation_date < '2016-01-01')

test_data <- data %>%
    filter(observation_date >= '2016-01-01')

# check stationarity using acf
autocor <- acf(train_data$y)


# check stationarity of differences timeseries using acf
autocor <- train_data %>%
    select(dy) %>%
    drop_na() %>%
    acf()

pautocor <- train_data %>%
    select(dy) %>%
    drop_na() %>%
    pacf()
# based on acf and pacf 3 AR models selected - AR {1}, AR {1,3}, AR {1,3,12}

specs <- list(
    orders = list(
        AR1 = c(1, 0, 0), 
        AR1_3 = c(3, 0, 0),
        AR1_3_12 = c(12, 0, 0)
        ),
    fixed = list(
        AR1 = c(NA, NA), 
        AR1_3 = c(NA, 0, NA, NA),
        AR1_3_12 = c(NA, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA)
    )
)

ar_models <- map2(
    specs$orders, specs$fixed,
    ~ arima(train_data$dy, order = .x, fixed = .y)
)

stargazer(ar_models)

# AR {1} is best
ar_model <- ar_models$AR1

plot_df <- tibble(
    observation_date = train_data$observation_date,
    e = ar_model$residuals
    )

# inspect the acf and pacf of residuals for MA part
plot_df %>% drop_na() %>% select(e) %>% acf()
plot_df %>% drop_na() %>% select(e) %>% pacf()


# define ARMA models
specs_arma <- list(
    orders = list(
        AR1 = c(1, 0, 0), 
        AR1_MA8 = c(1, 0, 8),
        AR1_MA15 = c(1, 0, 15),
        AR1_MA8_15 = c(1, 0, 15)
        ),
    fixed = list(
        AR1 = c(NA, NA), 
        AR1_MA8 = c(NA, 0, 0, 0, 0, 0, 0, 0, NA, NA),
        AR1_MA15 = c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA),
        AR1_MA8_15 = c(NA, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, NA, NA)
    )
)

arma_models <- map2(
    specs_arma$orders, specs_arma$fixed,
    ~ arima(train_data$dy, order = .x, fixed = .y)
)

stargazer(arma_models)

dy <- train_data$dy
arma_models <- map2(
    specs_arma$orders, specs_arma$fixed,
    ~ arima(dy, order = .x, fixed = .y)
)


# predictions
predict_arima <- function(model, newdata_ts, nsteps = 1) {
    
    orig_data_name <- str_match(model$series, '(.*)\\$(.*)')
    
    if (sum(is.na(orig_data_name)) != 0) {
        orig_data <- eval(as.name(model$series))
        orig_new_data_combined <- c(orig_data, newdata_ts)
    } else {
        orig_data <- eval(as.name(orig_data_name[2]))[orig_data_name[3]]
        orig_new_data_combined <- unlist(c(orig_data, newdata_ts))
    }
    
    refit <- Arima(orig_new_data_combined, model = model)  
    prediction <- predict(refit, n.ahead = 1, se.fit = FALSE)
    
    return(prediction)
}

predict_arima(model = arma_models$AR1, newdata_ts = test_data$dy[1])



