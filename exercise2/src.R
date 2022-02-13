rm(list = ls())

library(tidyverse)
library(rollRegres)
library(quantmod)
library(lubridate)


.from_date <- '2016-01-01'
#.to_date <- '2021-02-28'
.to_date <- '2021-12-31'

.stockmarket <- '^NDX'
.symbols <- c('AAPL','GOOGL')

### Functions

.select_and_filter <- function(title_df, from,  to, only_filter = FALSE) {
    
    if (only_filter) {
        result <- title_df %>%
            as_tibble(rownames = 'date') %>%                        # index to column
            mutate(date = as_date(date),
                   year_month = format(date, format = "%Y-%m")) %>% # convert
            filter(date >= from, date <= to)                        # filter
        
    } else {
        
        # get list of last trading days in months of the particular company
        last_trading_day <- title_df %>%
            as_tibble(rownames = 'date') %>%                        # index to column
            mutate(date = as_date(date)) %>%                        # convert
            select(date, contains('.Close')) %>%                    # select cols
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
            select(date, year_month, contains('.Close'))
    }
    return(result)
}

.get_perc_growth <- function(title_df, r_index_j_m = 'j') {
    
    r_ind <- paste('r_', r_index_j_m, sep = '')
    
    result <- title_df %>%
        mutate(diff_log = unlist(log(.[,3]) - dplyr::lag(log(.[,3]), n = 1))) %>% # proc se z toho udelal list v ramci tibblu netusim
        #select(year_month, contains('diff_log'))
        select(2:4)
    
    colnames(result) <- c('date', 'val', r_ind)
    result$date <- as.Date(result$date)
    
    return(result)
}

getSymbols(Symbols = .symbols,
           src = 'yahoo')

titles <- ls(all.names = FALSE)

full_list <- map(titles, ~ eval(as.name(.)))# prochazi promennou titels, ty ktere se uspesne stahly, 
# se jako stringy sypou do eval(as.name(.)) 
# -> vyhodnoti string jako kus kodu
names(full_list) <- titles  # pojmenovat casti listu

stock_returns <- map(full_list, ~ .select_and_filter(title_df = ., 
                                                     from = .from_date, 
                                                     to = .to_date, 
                                                     only_filter = FALSE)) %>%
    map(., ~ .get_perc_growth(title_df = .,
                              r_index_j_m = 'j')) %>%
    map(., drop_na)

# create tibble with returns
returns <- stock_returns$AAPL %>%
    inner_join(stock_returns$GOOGL, by = 'date') %>%
    select(date, Apple = r_j.x, Google = r_j.y)

# rolling regression with 300 samples window
# Apple explained by Google
rollregression <- roll_regres(Apple ~ Google, returns, width = 300,
                       do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))


# plot 'Google' coefficient variation in time
rollregression$coefs %>%
    as_tibble(.) %>%
    cbind(returns$date) %>%
    as_tibble() %>%
    rename(c(intercept = `(Intercept)`, date = `returns$date`)) %>%
    #mutate(Google = replace_na(Google, 0), intercept = replace_na(intercept, 0))%>%
    drop_na() %>%
    ggplot(mapping = aes(x = date, y = Google, group = 1)) +
    geom_line() +
    scale_x_date(name = element_blank(), date_minor_breaks = "1 year", limits = c(as.Date("2016-01-01"), as.Date("2022-01-01"))) +
    ylim(0, 1) +
    theme_bw()

# plot 1 step prediction error
rollregression$one_step_forecasts %>%
    as_tibble() %>%
    cbind(returns$Apple, returns$date) %>%
    rename(c(predicted = value, y = `returns$Apple`, date = `returns$date`)) %>%
    mutate(err = predicted - y) %>%
    drop_na() %>%
    ggplot(mapping = aes(x = date, y = err, group = 1)) +
    geom_line() +
    scale_x_date(name = element_blank(), date_minor_breaks = "1 year", limits = c(as.Date("2016-01-01"), as.Date("2022-01-01"))) +
    theme_bw()

# my own rolling regression, almost ready to convert to function
# performace of course worse than in rollRegres package, but still ok
window <- 300
steps <- nrow(returns) - 300 + 1
mod <- map(seq(from = 1, by = 1, length.out = steps), ~ lm(Apple ~ Google, returns[(0+.):(window+.),]))

last_fitted_values <- map_dfr(mod, ~ .$fitted.values[length(.$fitted.values)])
betas <- map_dfr(mod, ~ .$coefficients)
