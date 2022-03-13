
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

# plot the differences
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
autocor <- pacf(train_data$y)

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

#tibble(cor = c(autocor$acf[2:length(autocor$acf)]),
#       x = seq_along(cor)) %>%
#    ggplot(mapping = aes(x = x, y = cor)) +
#    geom_col()

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

fit_metrics_ar <- rbind(
    map_dbl(ar_models, ~ AIC(.)),
    map_dbl(ar_models, ~ BIC(.))
)
rownames(fit_metrics_ar) <- c('AIC', 'BIC')

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

fit_metrics <- rbind(
    map_dbl(arma_models, ~ AIC(.)),
    map_dbl(arma_models, ~ BIC(.))
)
rownames(fit_metrics) <- c('AIC', 'BIC')
stargazer(fit_metrics, summary = FALSE, rownames = TRUE)

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
    prediction <- predict(refit, n.ahead = nsteps, se.fit = FALSE)
    
    return(prediction[nsteps])
}

predict_recursive_arima <- function(model, newdata_full_ts, nsteps = 1) {
    predictions <- map_dbl(
        seq_along(newdata_full_ts),
        ~ predict_arima(model = model, newdata_ts = newdata_full_ts[1:.], nsteps = nsteps)
        )
    
    predictions_full <- c(predict(model, n.ahead = nsteps, se.fit = FALSE), predictions)
    
    return(predictions_full)
}
    
predictions <- map(
    arma_models,
    ~ predict_recursive_arima(model = ., newdata_full_ts = test_data$dy, nsteps = 1)
)
predictions <- map(predictions, ~ .[1:length(test_data$y)])

prediction_errors <- map(
    predictions, ~ . - test_data$dy
)

# prediction metrics
calculate_mae <- function(errors) {
    return(mean(abs(errors)))
}
calculate_rmse <- function(errors) {
    return((mean(errors^2))^(1/2))
}
calculate_mape <- function(y_true, y_pred) {
    return(mean(abs(y_pred - y_true) / y_true))
}

# get back from differences to level values
# MAPE will make much more sense
level_predictions <- map(
    predictions,
    ~ c(train_data$y[nrow(train_data)], test_data$y[1:(nrow(test_data)-1)]) + .
)

prediction_metrics <- rbind(
    unlist(map(prediction_errors, ~ calculate_mae(.))),
    unlist(map(prediction_errors, ~ calculate_rmse(.))),
    unlist(map(level_predictions, ~ calculate_mape(test_data$y, .)))
)
rownames(prediction_metrics) <- c('MAE', 'RMSE', 'MAPE')
stargazer(prediction_metrics, summary = FALSE, rownames = TRUE)


# inspect prediction errors
.get_norm_dist_approx <- function(data) {
    normdist <- data_frame(
        w = seq(
            from = min(data), 
            to = max(data), 
            length.out = length(data)
            ), 
        z = map_dbl(w, ~ dnorm(.,
                               mean = mean(data, na.rm = TRUE),
                               sd = sd(data, na.rm = TRUE)))
        )
}

prediction_errors_norm <- map(prediction_errors, ~ .get_norm_dist_approx(.))
prediction_errors_distributions <- map2(prediction_errors, prediction_errors_norm,
     ~ .x %>%
         as_tibble() %>%
         ggplot(aes(x = value)) +
         geom_histogram(aes(y = ..density..),
                        bins = 61, alpha=0.8, fill='firebrick', colour='black') +
         geom_line(data = .y,
                   aes(x = w, y = z),
                   color = "darkred",
                   size = 1) +
         geom_vline(xintercept = 0, color = 'orange', linetype = 'longdash', size = 1.02) +
         theme_bw() +
         theme(axis.title.x=element_blank()) +
         scale_x_continuous(limits = c(-0.4, 0.4))
)
prediction_errors_distributions <- map2(prediction_errors_distributions, c('AR1', 'AR1 MA{8}', 'AR1 MA{15}', 'AR1 MA{8,15}'),
                                        ~ .x + scale_y_continuous(name = .y))
grid.arrange(grobs = prediction_errors_distributions, ncol = 1)

prediction_errors_normality <- rbind(
    map_dfc(prediction_errors, ~ tseries::jarque.bera.test(.)$statistic),
    map_dfc(prediction_errors, ~ tseries::jarque.bera.test(.)$p.value)
)
rownames(prediction_errors_normality) <- c('Statistic', 'p-value')
stargazer(prediction_errors_normality, summary = FALSE, rownames = TRUE)


# plot out-of-sample predictions
predictions_dfs <- map(level_predictions, ~ tibble(pred = ., Date = test_data$observation_date, y_true = test_data$y))
predictions_plots <- map2(predictions_dfs, c('AR1', 'AR1 MA{8}', 'AR1 MA{15}', 'AR1 MA{8,15}'),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y_true, group = 1)) +
            geom_line(color = 'navyblue', size=1.01) +
            geom_line(mapping = aes(x=Date, y=pred), color = 'skyblue3') +
            theme_bw() +
            scale_x_date(
                name = element_blank(), 
                date_minor_breaks = "1 year", 
                limits = c(as.Date("2016-01-01"), as.Date("2022-01-01"))) +
            scale_y_continuous(name = .y)
)
grid.arrange(grobs = predictions_plots, ncol = 1)

