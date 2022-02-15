
setwd('C:/Users/Adam Cihláø/Desktop/materiály/ForecastingModels/assignments/exercise3')

library(tidyverse)
library(readxl)
library(mltools)
library(data.table)
library(gridExtra)

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
           )

data$m <- as.factor(data$m)
data <- as_tibble(one_hot(as.data.table(data)))

# split to train and test 
train_data <- data %>%
    filter(observation_date < '2015-01-01')

test_data <- data %>%
    filter(observation_date >= '2015-01-01')

# formula trend only
trend_formula <- 'TOTALNSA ~ time_index + time_index_2'
trend_m <- lm(formula = TOTALNSA ~ time_index + time_index_2, train_data)
summary(trend_m)

# formula seasonality
form <- str_c(unlist(colnames(train_data)[4:(length(names(train_data))-3)]), colapse = ' + ')
season_formula <- str_c(
    c('TOTALNSA ~ ' , str_c(form, collapse = ''), names(train_data)[length(train_data)-2]), 
    collapse = '')
season_m <- lm(formula = season_formula, data = train_data)
summary(season_m)

# formula trend + seasonality
trend_season_formula <- str_c(season_formula, 'time_index', 'time_index_2', sep = ' + ')
trend_season_m <- lm(formula = trend_season_formula, data = train_data)
summary(trend_season_m)

# formula trend + seasonality + cycle (AR)
trend_season_cycle_formula <- str_c(trend_season_formula, 'y_1', 'y_2', 'y_3', sep = ' + ')

formulas <- list(
    trend = trend_formula,
    season = season_formula,
    trend_season = trend_season_formula,
    trend_season_cycle = trend_season_cycle_formula
)

models <- map(formulas, ~ lm(., train_data))

# create tibbles with true, fitted and residuals for plotting
true_est_res <- map(models, ~ tibble(
    Date = train_data$observation_date[(length(train_data$observation_date)-length(.$residuals)+1):length(train_data$observation_date)],
    y = .$model$TOTALNSA,
    y_hat = .$fitted.values,
    e = .$residuals)
    )

estimates_plots <- map2(true_est_res, names(true_est_res),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y, group = 1)) +
            geom_line(color = 'navyblue', size = 1) +
            geom_line(mapping = aes(x=Date, y=y_hat), color = 'skyblue4') +
            theme_bw() +
            scale_x_date(name = element_blank()) +
            scale_y_continuous(name = .y)
)
grid.arrange(grobs = estimates_plots)


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



# inspect the residuals
map(models, ~ acf(.$residuals))
map(models, ~ pacf(.$residuals))

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
           




# automatically by what is ready in R
time_series <- ts(train_data[,2], start = train_data[1,1], frequency = 12)
components <- decompose(time_series)
plot(components)
components$seasonal
components$trend

components$random %>%
    as_tibble() %>%
    ggplot(mapping = aes(x = `x - seasonal`, y = ..density..)) +
    geom_histogram()