
rm(list = ls())

library(tidyverse)
library(tidyr)
library(lubridate)
library(quantmod)
library(car)
library(gridExtra)
library(moments)
library(tsoutliers)
library(stargazer)


### Variables
#symbols <- read_csv('https://datahub.io/core/nasdaq-listings/r/nasdaq-listed-symbols.csv') # all companies from NASDAQ Composite

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


# 1)
### Get data

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


# 2)
### Plot the time series
stock_returns$AAPL %>% ggplot(mapping = aes(x=date, y=val, group = 1)) +
        geom_line(color = 'steelblue') +
        theme_bw() +
        scale_x_date(name = element_blank(), date_minor_breaks = "1 year", limits = c(as.Date("2016-01-01"), as.Date("2022-01-01"))) + 
        scale_y_continuous(name = "Value")
        #+ ggtitle('Apple Closing Prices') 

stock_returns$GOOGL %>% ggplot(mapping = aes(x=date, y=val, group = 1)) +
        geom_line(color = 'firebrick') +
        theme_bw() +
        scale_x_date(name = element_blank(), date_minor_breaks = "1 year", limits = c(as.Date("2016-01-01"), as.Date("2022-01-01"))) + 
        scale_y_continuous(name = "Value")
        #+ ggtitle('Google Closing Prices') 

### Plot returns
stock_returns$AAPL %>% ggplot(mapping = aes(x=date, y=r_j, group = 1)) +
        geom_line(color = 'steelblue') +
        geom_hline(yintercept = 0, linetype = 'longdash') +
        theme_bw() +
        scale_x_date(name = element_blank(), date_minor_breaks = "1 year", limits = c(as.Date("2016-01-01"), as.Date("2022-01-01"))) + 
        #ggtitle('Apple Returns') +
        scale_y_continuous(name = "Returns")

stock_returns$GOOGL %>% ggplot(mapping = aes(x=date, y=r_j, group = 1)) +
        geom_line(color = 'firebrick') +
        geom_hline(yintercept = 0, linetype = 'longdash') +
        theme_bw() +
        scale_x_date(name = element_blank(), date_minor_breaks = "1 year", limits = c(as.Date("2016-01-01"), as.Date("2022-01-01"))) + 
        #ggtitle('Google Returns') +
        scale_y_continuous(name = "Returns")

### Plot returns distribution
norm_apple <- .get_norm_dist_approx(stock_returns$AAPL$r_j)
stock_returns$AAPL %>%
    ggplot(aes(x = r_j)) +
    geom_histogram(aes(y = ..density..),
                   bins = 61, alpha=0.8, fill='steelblue', colour='black') +
    geom_line(data = norm_apple,
              aes(x = w, y = z),
              color = "darkblue",
              size = 1) +
    theme_bw() +
    #ggtitle('Apple Returns Distribution') +
    theme(axis.title.y=element_blank()) +
    theme(axis.title.x=element_blank())

norm_google <- .get_norm_dist_approx(stock_returns$GOOGL$r_j)
stock_returns$GOOGL %>%
    ggplot(aes(x = r_j)) +
    geom_histogram(aes(y = ..density..),
                   bins = 61, alpha=0.8, fill='firebrick', colour='black') +
    geom_line(data = norm_google,
              aes(x = w, y = z),
              color = "darkred",
              size = 1) +
    theme_bw() +
    #ggtitle('Google Returns Distribution') +
    theme(axis.title.y=element_blank()) +
    theme(axis.title.x=element_blank())

### Summary statistics
returns <- stock_returns$AAPL %>%
    inner_join(stock_returns$GOOGL, by = 'date') %>%
    select(date, Apple = r_j.x, Google = r_j.y)

.get_desc_stat <- function(data, variable, full = TRUE) {
    data <- c(unlist(data[variable]))
    descstat <- list()
    descstat['Mean'] <- mean(data)
    descstat['Std'] <- sd(data)
    descstat['Min'] <- min(data)
    descstat['Max'] <- max(data)
    descstat['Skewness'] <- moments::skewness(data)
    descstat['Excess Kurtosis'] <- moments::kurtosis(data)
    if (full) {
        descstat['Obs'] <- length(data)
        descstat['Median'] <- median(data)
        descstat['Jarque-Bera statistic'] <- tseries::jarque.bera.test(data)$statistic
        descstat['Jarque-Bera p-value'] <- tseries::jarque.bera.test(data)$p.value
        descstat['Autocorrelation (1)'] <-  unlist(acf(data, lag.max = 10, plot = FALSE)[1])[1]
        descstat['Q(1) statistic'] <- Box.test(data, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)$statistic
        descstat['Q(1) p-value'] <- Box.test(data, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)$p.value
        descstat['Q(5) statistic'] <- Box.test(data, lag = 5, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)$statistic
        descstat['Q(5) p-value'] <- Box.test(data, lag = 5, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)$p.value
        descstat['Q(10) statistic'] <- Box.test(data, lag = 10, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)$statistic
        descstat['Q(10) p-value'] <- Box.test(data, lag = 10, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)$p.value
        descstat['1% VaR'] <- quantile(data, probs = c(0.01), na.rm = TRUE)
        descstat['99% VaR'] <- quantile(data, probs = c(0.99), na.rm = TRUE)
        descstat['1% CVaR'] <- mean(data[data<descstat$`1% VaR`])
        descstat['99% CVaR'] <- mean(data[data>descstat$`99% VaR`])
    }
    return(descstat)
}

stat_apple <- .get_desc_stat(returns,'Apple')
stat_google <- .get_desc_stat(returns, 'Google')

stats <- data.frame(
    Statistics = names(stat_apple),
    Apple = unlist(stat_apple),
    Google = unlist(stat_google)
    )
stargazer(stats, summary = FALSE, rownames = FALSE)


yearly_apple <- map(
    seq(from = 2016, to = 2021, by = 1),
    ~ returns[year(returns$date) == .,] %>%
        .get_desc_stat('Apple', FALSE)
    )
y_apple_df <- data.frame(
    Year = seq(from = 2016, to = 2021, by = 1), 
    matrix(unlist(yearly_apple), nrow=length(yearly_apple), byrow=TRUE)
    )
colnames(y_apple_df) <- c('Year', names(yearly_apple[[1]]))
stargazer(y_apple_df, summary = FALSE, rownames = FALSE)

yearly_google <- map(
    seq(from = 2016, to = 2021, by = 1),
    ~ returns[year(returns$date) == .,] %>%
        .get_desc_stat('Google', FALSE)
    )
y_google_df <- data.frame(
    Year = seq(from = 2016, to = 2021, by = 1), 
    matrix(unlist(yearly_google), nrow=length(yearly_google), byrow=TRUE)
    )
colnames(y_google_df) <- c('Year', names(yearly_google[[1]]))
stargazer(y_google_df, summary = FALSE, rownames = FALSE)

.get_cor <- function(data, x, y) {
    return(cor(data[x], data[y]))
}

y_corr <- data.frame(
    Year = as.character(seq(from = 2016, to = 2021, by = 1)),
    Correlation = unlist(
        map(
            seq(from = 2016, to = 2021, by = 1),
            ~ returns[year(returns$date) == .,] %>%
                .get_cor('Apple', 'Google')
        )
    )
)
y_corr_all <- data.frame(Year = 'Overall', Correlation = cor(returns$Apple, returns$Google))
y_corr <- rbind(y_corr_all, y_corr)
stargazer(y_corr, summary = FALSE, rownames = FALSE)

### Scatter plots
stock_cor <- cor(x = returns$Apple, y = returns$Google)
returns %>%
    ggplot(mapping = aes(x = Apple, y = Google)) +
    geom_point(colour = 'firebrick', alpha = 0.5, size = 3) +
    geom_hline(yintercept = 0, linetype = 'longdash', colour = 'gray19') +
    geom_vline(xintercept = 0, linetype = 'longdash', colour = 'gray19') +
    geom_smooth(method='lm', formula = y~x, colour = 'darkorange', linetype = 'longdash', se = FALSE) +
    #labs(title = paste("Correlation = ",signif(stock_cor, 5)))
    geom_text(x = -0.12, y = 0.09, label = paste("Correlation = ",signif(stock_cor, 3)), size=4.5) +
    theme_bw()

### Autocorrelation
acf(returns$Apple, plot = FALSE)
acf(returns$Google)




