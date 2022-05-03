rm(list = ls())

library(tidyverse)
library(lubridate)
library(quantmod)
library(vars)
library(aTSA)
library(tsDyn)
# library(ARDL)
# library(dLagM)
# library(car)
# library(gridExtra)
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

train <- df %>% 
    filter(date < .train_test_split_date)
test <- df %>%
    filter(date >= .train_test_split_date)

# create df with lagged values
df_r <- df[, str_detect(colnames(df), '^r_')]
df_full <- .lag_tibble(df_r, n_lags = 10)
df_full['date'] <- df['date']

train_full <- df_full %>% 
    filter(date < .train_test_split_date)
test_full <- df_full %>%
    filter(date >= .train_test_split_date)

# 4) Estimate ARDL model
ardl <- stats::lm(
    formula = 'r_AAPL ~ r_AAPL_1 + r_GOOGL_1 + r_MSFT_1 + r_AAPL_2 + r_GOOGL_2 + r_MSFT_2',
    data = train_full
    )
ardl_pred <- stats::predict(ardl, test_full)
ardl_pred_err <- ardl_pred - test_full$r_AAPL

# get level predictions from returns 
ardl_level_pred <- exp(
    cumsum(ardl_pred) + 
    rep(as.double(log(df[nrow(train_full),'val_AAPL'])), times = length(ardl_pred))
)
ardl_level_pred_err <- ardl_level_pred - test$val_AAPL

# prediction metrics
calculate_mae <- function(errors) {
    return(mean(abs(errors)))
}
calculate_rmse <- function(errors) {
    return((mean(errors^2))^(1/2))
}
calculate_mape <- function(y_true, y_pred, d=0.00000001) {
    return(mean(c(unlist(abs((y_pred - y_true) / (y_true + d)))), na.rm = TRUE))
}

# returns
calculate_mae(ardl_pred_err)
calculate_rmse(ardl_pred_err)
calculate_mape(test_full$r_AAPL, ardl_pred)

# levels
calculate_mae(ardl_level_pred_err)
calculate_rmse(ardl_level_pred_err)
calculate_mape(test$val_AAPL, ardl_level_pred)
