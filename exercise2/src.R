rm(list = ls())

library(tidyverse)
library(readxl)
library(zoo)
library(stargazer)
library(gridExtra)
library(tseries)


data <- read_xlsx('../data/Credit Spreads.xlsx') %>%
    mutate(Date = as.Date(Date))


# plot data
data %>% ggplot(mapping = aes(x=Date, y=baa10y, group = 1)) +
        geom_line(color = 'steelblue') +
        geom_line(mapping = aes(x=Date, y=t10y3m), color = 'firebrick') +
        theme_bw() +
        scale_x_date(
            name = element_blank(), 
            date_minor_breaks = "1 year", 
            limits = c(as.Date("2018-01-01"), as.Date("2022-01-01"))) +
        scale_y_continuous(name = "Return",
                           limits = c(-1, 5))

data %>% ggplot(mapping = aes(x=Date, y=VIX, group = 1)) +
        geom_line(color = 'darkblue') +
        theme_bw() +
        scale_x_date(
            name = element_blank(), 
            date_minor_breaks = "1 year", 
            limits = c(as.Date("2018-01-01"), as.Date("2022-01-01"))) +
        scale_y_continuous(name = "Value")

# add lagged variables
data <- data %>%
    mutate(t10y3m_1 = dplyr::lag(t10y3m, n = 1),
           t10y3m_2 = dplyr::lag(t10y3m, n = 2),
           VIX_1 = dplyr::lag(VIX, n = 1),
           VIX_2 = dplyr::lag(VIX, n = 2))

# train, test split
train_data <- data %>%
    filter(Date < '2021-01-01')

test_data <- data %>%
    filter(Date >= '2021-01-01')

# define and estimate models
models_formulas <- list(M1 = formula(baa10y ~ t10y3m_1 + VIX_1),
               M2 = formula(baa10y ~ VIX_1),
               M3 = formula(baa10y ~ t10y3m_1 + VIX_1 + t10y3m_2 + VIX_2))

models <- map(models_formulas, ~ lm(., train_data))

# basic info about models
stargazer(models$M1, models$M2, models$M3)

# create tibbles with true, fitted and residuals for plotting
true_est_res <- map(models, ~ tibble(
    Date = train_data$Date[(length(train_data$Date)-length(.$residuals)+1):length(train_data$Date)],
    y = .$model$baa10y,
    y_hat = .$fitted.values,
    e = .$residuals)
    )

estimates_plots <- map2(true_est_res, names(true_est_res),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y, group = 1)) +
            geom_line(color = 'navyblue') +
            geom_line(mapping = aes(x=Date, y=y_hat), color = 'skyblue3') +
            theme_bw() +
            scale_x_date(
                name = element_blank(), 
                date_minor_breaks = "1 year", 
                limits = c(as.Date("2018-01-01"), as.Date("2021-01-01"))) +
            scale_y_continuous(name = .y,
                               limits = c(1, 5))
)
grid.arrange(grobs = estimates_plots)
    
residuals_plots <-  map2(true_est_res, names(true_est_res),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=e, group = 1)) +
        geom_line(color = 'firebrick') +
        geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey10') +
        theme_bw() +
        scale_x_date(
            name = element_blank(), 
            date_minor_breaks = "1 year", 
            limits = c(as.Date("2018-01-01"), as.Date("2021-01-01"))) +
        scale_y_continuous(name = .y)
)
grid.arrange(grobs = residuals_plots)


# residuals inspection
res_normality <- rbind(
    map_dfc(models, ~ tseries::jarque.bera.test(.$residuals)$statistic),
    map_dfc(models, ~ tseries::jarque.bera.test(.$residuals)$p.value)
)
rownames(res_normality) <- c('Statistic', 'p-value')
stargazer(res_normality, summary = FALSE)

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

# create out-of-sample predictions
predictions <- map(models, ~ predict(., test_data))
prediction_errors <- map(predictions, ~ . - test_data$baa10y)

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
         theme_bw() +
         theme(axis.title.y=element_blank()) +
         theme(axis.title.x=element_blank())
)
grid.arrange(grobs = prediction_errors_distributions)

prediction_errors_normality <- rbind(
    map_dfc(prediction_errors, ~ tseries::jarque.bera.test(.)$statistic),
    map_dfc(prediction_errors, ~ tseries::jarque.bera.test(.)$p.value)
)
rownames(res_normality) <- c('Statistic', 'p-value')
stargazer(res_normality, summary = FALSE)

pred_df <- data.frame(matrix(unlist(predictions), ncol = length(predictions), byrow = FALSE))
colnames(pred_df) <- names(models)
pred_err_df <- data.frame(matrix(unlist(prediction_errors), ncol = length(prediction_errors), byrow = FALSE))
colnames(pred_err_df) <- str_c(names(models), 'err', sep = '_')

prediction_analysis <- cbind(test_data, pred_df, pred_err_df)


