
rm(list = ls())

library(tidyverse)
library(lubridate)
library(quantmod)
library(vars)
library(aTSA)
library(tsDyn)
library(gridExtra)
library(rugarch)
# library(ARDL)
# library(dLagM)
# library(car)
# library(moments)
# library(tsoutliers)
# library(stargazer)


# constants
.from_date <- '2016-01-01'
.to_date <- '2021-12-31'

.stockmarket <- '^NDX'
.symbols <- c('AAPL','GOOGL', 'MSFT')

.train_test_split_date <- '2021-01-01'


.select_and_filter <- function(title_df, from,  to, only_filter = FALSE) {
    
    if (only_filter) {
        result <- title_df %>%
            as_tibble(rownames = 'date') %>%                        # index to column
            mutate(date = as_date(date),
                   year_month = format(date, format = "%Y-%m")) %>% # convert
            filter(date >= from, date <= to) %>%                    # filter
            mutate(year_month = format(date, format = "%Y-%m-%d")) %>% # substract year-month
            dplyr::select(date, year_month, contains('.Close'))
        
    } else {
        
        # get list of last trading days in months of the particular company
        last_trading_day <- title_df %>%
            as_tibble(rownames = 'date') %>%                        # index to column
            mutate(date = as_date(date)) %>%                        # convert
            dplyr::select(date, contains('.Close')) %>%                    # select cols
            drop_na() %>%                                           # drop rows with missing values
            filter(date >= from, date <= to) %>%                    # filter date
            mutate(year_month = format(date, format = "%Y-%m-%d")) %>% # substract year-month
            group_by(year_month) %>%                                # group by months
            summarise(last_day = max(date))                         # get last days
        
        # inner join last days with values to get last  
        result <- title_df %>%
            as_tibble(rownames = 'date') %>%
            mutate(date = as_date(date)) %>%
            inner_join(last_trading_day, by = c('date' = 'last_day')) %>%
            dplyr::select(date, year_month, contains('.Close'))
    }
    return(result)
}

.get_perc_growth <- function(title_df, r_index_j_m = 'j') {
    
    r_ind <- paste('r', r_index_j_m, sep = '_')
    
    result <- title_df %>%
        mutate(diff_log = unlist(log(.[,3]) - dplyr::lag(log(.[,3]), n = 1))) %>%
        dplyr::select(2:4)
    
    colnames(result) <- c('date', 'val_', r_ind)
    result$date <- as.Date(result$date)
    
    return(result)
}

.set_colnames <- function(df, column_names) {
    colnames(df) <- column_names
    return(df)
}

.lag_tibble <- function(df, n_lags) {
    df <- df[, colnames(df)!='date']
    num_lags <- 10
    train_lagged <- list()
    for (i in 1:num_lags) {
        train_one_lag <- as_tibble(rbind(
            matrix(NA, nrow = i, ncol = ncol(df)),
            as.matrix(df[1:(nrow(df)-i),])
        )) %>% .set_colnames(str_c(colnames(df), i, sep = '_'))
        train_lagged <- append(train_lagged, train_one_lag)
    }
    train_lagged <- as_tibble(reduce(train_lagged, cbind)) %>%
        .set_colnames(names(train_lagged))
    train_full <- cbind(df, train_lagged)
}

# 1) Get data

getSymbols(Symbols = .symbols,
           src = 'yahoo')

titles <- ls(all.names = FALSE)

full_list <- map(titles, ~ eval(as.name(.)))
names(full_list) <- titles

stock_returns <- map(full_list, ~ .select_and_filter(title_df = ., 
                                                     from = .from_date, 
                                                     to = .to_date, 
                                                     only_filter = TRUE)) %>%
    map(., ~ .get_perc_growth(title_df = .,
                              r_index_j_m = '')) %>%
    map(., drop_na)

df <- inner_join(
    stock_returns[1]$AAPL, stock_returns[2]$GOOGL, 
    by = 'date', suffix = c('AAPL', 'GOOGL')
) %>%
    inner_join(stock_returns[3]$MSFT, by='date')
colnames(df)[(ncol(df)-1):ncol(df)] <- c('val_MSFT', 'r_MSFT')

df_r <- df[, str_detect(colnames(df), '^r_')]

train <- df %>% 
    filter(date < .train_test_split_date)
test <- df %>%
    filter(date >= .train_test_split_date)


# 2) Check stationarity
adf_test <- function(timeseries) {
    res <- list(adf.test(unlist(timeseries)));
    return(res)
}

adf_tests <- map(
    c(2:length(colnames(train))),
    ~ adf_test(train[,.])
)
names(adf_tests) <- colnames(train)[2:length(colnames(train))]
adf_tests
# returns are stationary, level values not


# 3) Estimate VAR on returns
train_var <- train[, str_detect(colnames(train), '^r_')]
test_var <- test[, str_detect(colnames(test), '^r_')]

var_sel_aic <- VARselect(train_var, lag.max = 12)$selection[1]
var_sel_bic <- VARselect(train_var, lag.max = 12)$selection[3]

var_aic <- VAR(train_var, lag.max = 12, ic = 'AIC')
var_bic <- VAR(train_var, lag.max = 12, ic = 'SC')


# 4) Analyze models
summary(var_aic)
dev.off()
plot(var_aic)
var_aic$e <- fitted(var_aic) - var_aic$datamat[,1:ncol(fitted(var_aic))]

summary(var_bic)
dev.off()
plot(var_bic)
var_bic$e <- fitted(var_bic) - var_bic$datamat[,1:ncol(fitted(var_bic))]

# Granger causalities
map(colnames(var_aic$y), ~ causality(var_aic, .))
pred_var_aic <- lineVar(train_var, var_sel_aic, model = 'VAR')
map(colnames(var_bic$y), ~ causality(var_bic, .))
pred_var_bic <- lineVar(train_var, var_sel_bic, model = 'VAR')

# Impulse response functions
irf_aic <- irf(var_aic)
plot(irf_aic)
irf_bic <- irf(var_bic)
plot(irf_bic)


# 3) Predictions
predict_var <- function(var_model, new_data, n_ahead=1) {
    n_lags <- var_model$lag
    index_start <- rownames(predict(var_model, new_data[1:n_lags,], n.ahead=1))
    len_new_data <- nrow(new_data)
    column_names <- colnames(new_data)
    predictions <- map(
        1:nrow(new_data),
        ~ predict(var_model, new_data[.:(.+n_lags-1),], n.ahead = n_ahead)
        ) %>%
        set_names(c(index_start:(as.integer(index_start) + len_new_data - 1)))
    predictions <- predictions %>%
        map(function(x) {x <- as.data.frame(x); colnames(x) <- column_names; x}) %>%
        bind_rows() %>% 
        as_tibble()
    rownames(predictions) <- c(index_start:(as.integer(index_start) + len_new_data - 1))
    return(predictions)
}
# because of different lags, we need to create different X (inputs for test sets)
test_aic <- rbind(
    train_var[(nrow(train_var) - pred_var_aic$lag + 1):nrow(train_var),],
    test_var
)
test_bic <- rbind(
    train_var[(nrow(train_var) - pred_var_bic$lag + 1):nrow(train_var),],
    test_var
)
predictions_aic <- drop_na(predict_var(pred_var_aic, test_aic)) %>%
    .[1:(nrow(.)-1),]
predictions_bic <- drop_na(predict_var(pred_var_bic, test_bic)) %>%
    .[1:(nrow(.)-1),]

# get level predictions from returns
pred_levels <- rbind(
    train[nrow(train), str_detect(colnames(train), pattern = '^val_')],
    test[1:(nrow(test)-1), str_detect(colnames(test), pattern = '^val_')]
) * (predictions_aic + 1) %>% 
    .set_colnames(c(str_c('pred', names(full_list), sep='_')))

pred_err_levels <- pred_levels - test[,str_detect(colnames(test), '^val_')]

# errors
pred_err_aic <- predictions_aic - test_var
pred_err_bic <- predictions_bic - test_var

# prediction metrics
calculate_mae <- function(errors) {
    return(mean(abs(errors)))
}
calculate_rmse <- function(errors) {
    return((mean(errors^2))^(1/2))
}
calculate_mape <- function(y_true, y_pred, d=0.00000001) {
    return(mean((abs(y_pred - y_true) / (y_true + d))[,1], na.rm = TRUE))
}
y_true <- test_vecm[,1]
y_pred <- pred_levels[,1]

# returns
as_tibble(rbind(
    map_dbl(1:ncol(pred_err_aic), ~ calculate_mae(unlist(pred_err_aic[,.]))),
    map_dbl(1:ncol(pred_err_bic), ~ calculate_mae(unlist(pred_err_bic[,.]))),
    .name_repair = c('unique')
)) %>% 
    .set_colnames(names(full_list)) %>%
    .[1:(length(.)-1),]


as_tibble(rbind(
    map_dbl(1:ncol(pred_err_aic), ~ calculate_rmse(unlist(pred_err_aic[,.]))),
    map_dbl(1:ncol(pred_err_bic), ~ calculate_rmse(unlist(pred_err_bic[,.]))),
    .name_repair = c('unique')
)) %>% 
    .set_colnames(names(full_list)) %>%
    .[1:(length(.)-1),]

as_tibble(rbind(
    map_dbl(1:ncol(pred_err_aic), ~ calculate_mape(test_var[,.], (pred_err_aic[,.]))),
    map_dbl(1:ncol(pred_err_bic), ~ calculate_mape(test_var[,.], (pred_err_bic[,.]))),
    .name_repair = c('unique')
)) %>% 
    .set_colnames(names(full_list)) %>%
    .[1:(length(.)-1),]

# levels
# returns
as_tibble(rbind(
    map_dbl(1:ncol(pred_err_levels), ~ calculate_mae(unlist(pred_err_levels[,.])))
)) %>% 
    .set_colnames(names(full_list))

as_tibble(rbind(
    map_dbl(1:ncol(pred_err_levels), ~ calculate_rmse(unlist(pred_err_levels[,.])))
)) %>% 
    .set_colnames(names(full_list))

as_tibble(rbind(
    map_dbl(1:ncol(pred_err_levels), ~ calculate_mape(test[,str_detect(colnames(test), '^val_')][,.], (pred_levels[,.])))
)) %>% 
    .set_colnames(names(full_list))



# 6) VECM
train_vecm <- train[,str_detect(colnames(train), '^val')]
test_vecm <- test[, str_detect(colnames(test), '^val')]

predict_vecm <- function(vecm_model, new_data, n_ahead=1) {
    # Prediction from VECM works by transforming VECM to VAR - lags(VECM)+1 == lags(VAR),
    # so we need to add one more observation for the predict function.
    # That is the only difference from predict_var function.
    n_lags <- vecm_model$lag + 1
    index_start <- rownames(predict(vecm_model, new_data[1:n_lags,], n.ahead=1))
    len_new_data <- nrow(new_data)
    column_names <- colnames(new_data)
    predictions <- map(
        1:nrow(new_data),
        ~ predict(vecm_model, new_data[.:(.+n_lags-1),], n.ahead = n_ahead)
    ) %>%
        set_names(c(index_start:(as.integer(index_start) + len_new_data - 1)))
    predictions <- predictions %>%
        map(function(x) {x <- as.data.frame(x); colnames(x) <- column_names; x}) %>%
        bind_rows() %>% 
        as_tibble()
    rownames(predictions) <- c(index_start:(as.integer(index_start) + len_new_data - 1))
    return(predictions)
}

# get cointegration rank using the Johansen procedure
coint_test <- ca.jo(
    train_vecm, 
    ecdet = 'none', # see docs
    type  = 'eigen', # see docs
    K = 2, # lag of VAR - must be at least 2, because lag(VECM) = lag(VAR) - 1
    spec = 'transitory', # see docs
    # season = 52,
    dumvar = NULL)

summary(coint_test)

# estimate the VECM 
vecm_model <- VECM(train_vecm, 
                   lag=1,   # lag = lag(VAR) - 1
                   r=1,     # r = cointegration test - result from ca.jo test
                   estim = 'ML',
                   LRinclude = 'none')
summary(vecm_model)

test_vecm_pred <- rbind(
    train_vecm[(nrow(train_vecm) - vecm_model$lag):nrow(train_vecm),],
    test_vecm
)
predictions_vecm <- drop_na(predict_vecm(vecm_model, new_data = test_vecm_pred)) %>%
    .[1:(nrow(.)-1),]

# plot historical data + forecast data
vecm_true <- rbind(train_vecm, test_vecm)
vecm_pred <- rbind(train_vecm, predictions_vecm) %>%
    .set_colnames(str_c('pred', colnames(.), sep = '_'))
vecm_df <- cbind(date = df$date, vecm_true, vecm_pred)

p1 <- vecm_df %>%
    ggplot(mapping = aes(x=date, y=val_AAPL)) +
    geom_line()+
    geom_line(mapping = aes(x=date, y=pred_val_AAPL), color='red')

p2 <- vecm_df %>%
    ggplot(mapping = aes(x=date, y=val_GOOGL)) +
    geom_line()+
    geom_line(mapping = aes(x=date, y=pred_val_GOOGL), color='red')

p3 <- vecm_df %>%
    ggplot(mapping = aes(x=date, y=val_MSFT)) +
    geom_line()+
    geom_line(mapping = aes(x=date, y=pred_val_MSFT), color='red')
grid.arrange(p1, p2, p3)

# plot forecast data
vecm_true <- rbind(test_vecm)
vecm_pred <- rbind(predictions_vecm) %>%
    .set_colnames(str_c('pred', colnames(.), sep = '_'))
vecm_df <- cbind(date = test$date, vecm_true, vecm_pred)

p1 <- vecm_df %>%
    ggplot(mapping = aes(x=date, y=val_AAPL)) +
    geom_line()+
    geom_line(mapping = aes(x=date, y=pred_val_AAPL), color='red')

p2 <- vecm_df %>%
    ggplot(mapping = aes(x=date, y=val_GOOGL)) +
    geom_line()+
    geom_line(mapping = aes(x=date, y=pred_val_GOOGL), color='red')

p3 <- vecm_df %>%
    ggplot(mapping = aes(x=date, y=val_MSFT)) +
    geom_line()+
    geom_line(mapping = aes(x=date, y=pred_val_MSFT), color='red')
grid.arrange(p1, p2, p3)

pred_err_vecm <- predictions_vecm - test_vecm

# vecm prediction metrics
as_tibble(rbind(
    map_dbl(1:ncol(pred_err_vecm), ~ calculate_mae(unlist(pred_err_vecm[,.])))
)) %>% 
    .set_colnames(names(full_list))

as_tibble(rbind(
    map_dbl(1:ncol(pred_err_vecm), ~ calculate_rmse(unlist(pred_err_vecm[,.])))
)) %>% 
    .set_colnames(names(full_list))

as_tibble(rbind(
    map_dbl(1:ncol(pred_err_vecm), ~ calculate_mape(test_vecm[,.], (predictions_vecm[,.])))
)) %>% 
    .set_colnames(names(full_list))



# 7) GARCH
garch_spec_norm <- ugarchspec(
    mean.model = list(armaOrder=c(0,0)), 
    variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
    distribution.model = 'norm')

garch_spec_std <- ugarchspec(
    mean.model = list(armaOrder=c(0,0)), 
    variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
    distribution.model = 'sstd')

garch_spec_std_arma <- ugarchspec(
    mean.model = list(armaOrder=c(1,1)), 
    variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
    distribution.model = 'sstd')

garch_model_norm <- ugarchfit(data = df$r_AAPL, spec = garch_spec_norm, out.sample = 100)
garch_model_norm

garch_model_std <- ugarchfit(data = df$r_AAPL, spec = garch_spec_std, out.sample = 100)
garch_model_std

garch_model_std_arma <- ugarchfit(data = df$r_AAPL, spec = garch_spec_std_arma, out.sample = 0)
garch_model_std_arma
full_fitted_sigma_arma <- garch_model_std_arma@fit$sigma

garch_model_std_full <- ugarchfit(data = df$r_AAPL, spec = garch_spec_std, out.sample = 0)
full_fitted_sigma <- garch_model_std_full@fit$sigma

# predict - no new data
fitted_sigma <- tibble(val = garch_model_std@fit$sigma, type = 'fit')
predicted_sigma <- tibble(
    val = ugarchforecast(garch_model_std, n.ahead = 100)@forecast$sigmaFor,
    type = 'predict'
    )

sigmas <- cbind(
    rbind(fitted_sigma, predicted_sigma), 
    date = df$date, 
    full_fitted_sigma = full_fitted_sigma,
    full_fitted_sigma_arma = full_fitted_sigma_arma
    )

sigmas %>%
    ggplot(mapping = aes(x=date, y=val, color=type)) +
    geom_line() +
    geom_line(mapping = aes(x=date, y=full_fitted_sigma, color='green')) +
    geom_line(mapping = aes(x=date, y=full_fitted_sigma_arma, color='firebrick'))



# 5) Forecast averaging
# simple mean
# trimmed mean (would need more models)
# simple median (would need more models)
# Least squares weights - regression of the true values on the predictions (can be w/ or w/o intercept)
# MSE weights - each model has weight defined by w_i = (1/MSE_i) / (sum_over_all_models(1/MSE_j))

# least squares
var_pred <- as_tibble(pred_levels) %>%
    .set_colnames(str_c(colnames(vecm_pred), 'var', sep='_'))
vecm_pred <- vecm_pred %>%
    .set_colnames(str_c(colnames(vecm_pred), 'vecm', sep='_'))
test_true <- test_vecm

test_true_pred <- cbind(test_true, var_pred, vecm_pred)

pred_comb_vars <- colnames(test_true_pred)

n_vars <- 3
n_models <- 2

prediction_formulas <- c()
for (v in 1:n_vars) {
    pred_form <- str_c(pred_comb_vars[v],
        paste(
            map_chr(c(1:(n_models)), ~ pred_comb_vars[(. * n_vars) + v]), collapse = ' + '
        ),
        sep = ' ~ '
    )
    prediction_formulas[v] <- pred_form
}

pred_comb_models <- map(
    prediction_formulas,
    ~ lm(., test_true_pred)
)


# MSE
