
dev.off()
rm(list = ls())
setwd('C:/Users/Adam Cihláø/Desktop/materiály/ForecastingModels/assignments/exercise3')

library(tidyverse)
library(readxl)
library(mltools)
library(data.table)
library(gridExtra)
library(stargazer)


data <- read_xls(path = '../data/Sales new cars US.xls', skip = 10) %>%
    mutate(observation_date = as.Date(observation_date),
           m = lubridate::month(observation_date),
           time_index = c(1:nrow(.)),
           time_index_2 = time_index^2,
           y = TOTALNSA,
           y_1 = dplyr::lag(y, n = 1),
           y_2 = dplyr::lag(y, n = 2),
           y_3 = dplyr::lag(y, n = 3),
           y_4 = dplyr::lag(y, n = 4),
           y_12 = dplyr::lag(y, n = 12),
           y_13 = dplyr::lag(y, n = 13),
           )

data$m <- as.factor(data$m)
data <- as_tibble(one_hot(as.data.table(data)))

# plot the original time series
data %>% ggplot(mapping = aes(x = observation_date, y = TOTALNSA, group = 1)) +
        geom_line(color = 'darkblue') +
        theme_bw() +
        scale_x_date(
            name = element_blank(), 
            date_minor_breaks = "1 year", 
            limits = c(as.Date("1976-01-01"), as.Date("2022-01-01"))) +
        scale_y_continuous(limits = c(0, max(data$TOTALNSA)*1.1),
                           name = element_blank())

# split to train and test 
train_data <- data %>%
    filter(observation_date < '2015-01-01') %>%
    drop_na()

test_data <- data %>%
    filter(observation_date >= '2015-01-01')

# formula trend only
trend_formula <- 'TOTALNSA ~ time_index + time_index_2'

# formula seasonality
form <- str_c(unlist(colnames(train_data)[4:13]), colapse = ' + ')
season_formula <- str_c(
    c('TOTALNSA ~ ' , str_c(form, collapse = ''), names(train_data)[14]), 
    collapse = '')

# formula trend + seasonality
trend_season_formula <- str_c(season_formula, 'time_index', 'time_index_2', sep = ' + ')
model_to_determine_AR <- lm(trend_season_formula, data = train_data)

# formula trend + seasonality + cycle (AR)
# inspect the residuals to determine AR process
acf(model_to_determine_AR$residuals)
pacf(model_to_determine_AR$residuals)
# use AR 4 process + 12 (makes sense with the monthly data)
trend_season_cycle_formula <- str_c(trend_season_formula, 'y_1', 'y_2', 'y_3', 'y_4', 'y_12', 'y_13',
                                    sep = ' + ')

formulas <- list(
    `T` = trend_formula,
    `S` = season_formula,
    `T+S` = trend_season_formula,
    `T+S+C` = trend_season_cycle_formula
)

models <- map(formulas, ~ lm(., train_data))
stargazer(models)

# create tibbles with true, fitted and residuals for plotting
true_est_res <- map(models, ~ tibble(
    Date = train_data$observation_date[(length(train_data$observation_date)-length(.$residuals)+1):length(train_data$observation_date)],
    y = .$model$TOTALNSA,
    y_hat = .$fitted.values,
    e = .$residuals)
    )

# plot how the models fit the train data
estimates_plots <- map2(true_est_res, names(true_est_res),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y, group = 1)) +
            geom_line(color = 'navyblue', size = 1) +
            geom_line(mapping = aes(x=Date, y=y_hat), color = 'firebrick') +
            theme_bw() +
            scale_x_date(name = element_blank()) +
            scale_y_continuous(name = .y)
)
grid.arrange(grobs = estimates_plots, ncol = 1)

# inspect models
map(models, ~ summary(.))
fit_metrics <- rbind(
    map_dbl(models, ~ AIC(.)),
    map_dbl(models, ~ BIC(.))
)
rownames(fit_metrics) <- c('AIC', 'BIC')
stargazer(fit_metrics, summary = FALSE, rownames = TRUE)

# create out-of-sample predictions and errors
predictions <- map(models, ~ predict(., test_data))
prediction_errors <- map(predictions, ~ . - test_data$TOTALNSA)

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

prediction_metrics <- rbind(
    unlist(map(prediction_errors, ~ calculate_mae(.))),
    unlist(map(prediction_errors, ~ calculate_rmse(.))),
    unlist(map(predictions, ~ calculate_mape(test_data$TOTALNSA, .)))
)
rownames(prediction_metrics) <- c('MAE', 'RMSE', 'MAPE')
stargazer(prediction_metrics, summary = FALSE, rownames = TRUE)


# inspect the residuals again
par(mfrow = c(2,2))
acfs <- map(models, ~ acf(.$residuals))
pacfs <- map(models, ~ pacf(.$residuals))

# residuals inspection
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


# in-sample residuals plot
models_res_norm <- map(models, ~ .get_norm_dist_approx(.$residuals))
residuals_distributions <- map2(models, models_res_norm,
     ~ .x$residuals %>%
         as_tibble() %>%
         ggplot(aes(x = value)) +
         geom_histogram(aes(y = ..density..),
                        bins = 61, alpha=0.8, fill='firebrick', colour='black') +
         geom_line(data = .y,
                   aes(x = w, y = z),
                   color = "darkred",
                   size = 1) +
         theme_bw() +
         theme(axis.title.y=element_blank()) +
         theme(axis.title.x=element_blank())
)
grid.arrange(grobs = residuals_distributions)
           

# inspect prediction errors
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
         scale_x_continuous(limits = c(-600, 600))
)
prediction_errors_distributions <- map2(prediction_errors_distributions, c('T', 'S', 'T + S', 'T + S + C'),
                                        ~ .x + scale_y_continuous(name = .y))
grid.arrange(grobs = prediction_errors_distributions, ncol = 1)

prediction_errors_normality <- rbind(
    map_dfc(prediction_errors, ~ tseries::jarque.bera.test(.)$statistic),
    map_dfc(prediction_errors, ~ tseries::jarque.bera.test(.)$p.value)
)
rownames(prediction_errors_normality) <- c('Statistic', 'p-value')
stargazer(prediction_errors_normality, summary = FALSE, rownames = TRUE)

# plot out-of-sample predictions
predictions_dfs <- map(predictions, ~ tibble(pred = ., Date = test_data$observation_date, y_true = test_data$y))
predictions_plots <- map2(predictions_dfs, names(predictions_dfs),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y_true, group = 1)) +
            geom_line(color = 'navyblue', size=1.01) +
            geom_line(mapping = aes(x=Date, y=pred), color = 'skyblue3') +
            theme_bw() +
            scale_x_date(
                name = element_blank(), 
                date_minor_breaks = "1 year", 
                limits = c(as.Date("2015-01-01"), as.Date("2022-01-01"))) +
            scale_y_continuous(name = .y)
)
grid.arrange(grobs = predictions_plots, ncol = 1)

pred_df <- data.frame(matrix(unlist(predictions), ncol = length(predictions), byrow = FALSE))
colnames(pred_df) <- names(models)

# serial correlation test of prediction erros
prediction_errors_ext <- map(prediction_errors, ~ tibble(err = .,
                                err_1 = dplyr::lag(., 1),
                                err_2 = dplyr::lag(., 2),
                                err_3 = dplyr::lag(., 3),
                                err_4 = dplyr::lag(., 4)))

autocorrtest_formula <- formula(err ~ err_1 + err_2 + err_3)

autocorrtest_models <- map(prediction_errors_ext, ~ lm(autocorrtest_formula, data = .))
map(autocorrtest_models, ~ summary(.))
stargazer(autocorrtest_models)
