
# get ready
rm(list = ls())
dev.off()
setwd("C:/Users/Adam Cihlár/Desktop/materiály/ForecastingModels/assignments/exam/")


# libraries
library(tidyverse)
library(readxl)
library(gridExtra)
library(GGally)
library(plotly)
library(stargazer)
library(aTSA)
library(MASS)



# constants
n_vars <- 10
d_vars <- c('baa10y', 'dtb3', 't10y3m', 'unrate')
r_vars <- c('cpiaucsl', 'dcoilbrenteu', 'indpro', 'm1sl', 'nasdaq100', 'ppiaco')
dep_var <- 'r_dcoilbrenteu'
date_col <- 'date'
train_test_split_date <- '2012-12-01'


# functions
.set_colnames <- function(df, column_names) {
    colnames(df) <- column_names
    return(df)
}

.get_perc_growth <- function(df, columns, prefix='r') {
    orig_cols <- colnames(df)
    for (col in columns) {
        orig_cols <- colnames(df)
        new_name <- paste(prefix, col, sep = '_')
        df <- df %>%
            mutate(dl = unlist(log(.[col]) - dplyr::lag(log(.[col]), n = 1))) %>%
            .set_colnames(c(orig_cols, new_name))
    }
    return(df)
}

.get_differences <- function(df, columns, prefix='d') {
    orig_cols <- colnames(df)
    for (col in columns) {
        orig_cols <- colnames(df)
        new_name <- paste(prefix, col, sep = '_')
        df <- df %>%
            mutate(dl = unlist(.[col] - dplyr::lag(.[col], n = 1))) %>%
            .set_colnames(c(orig_cols, new_name))
    }
    return(df)
}

dr_ <- function(dataframe) {
    return(dataframe[, str_detect(colnames(dataframe), pattern = '^r_') | str_detect(colnames(dataframe), pattern = '^d_')])
}

val_ <- function(dataframe) {
    return(dataframe[, !str_detect(colnames(dataframe), pattern = '^r_') & !str_detect(colnames(dataframe), pattern = '^d_') & colnames(dataframe)!=date_col])
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
    return(normdist)
}

.get_desc_stat <- function(data, variable, full = TRUE) {
    data <- c(unlist(drop_na(data[variable])))
    descstat <- list()
    descstat['Mean'] <- mean(data)
    descstat['Median'] <- median(data)
    descstat['Std'] <- sd(data)
    descstat['Min'] <- min(data)
    descstat['Max'] <- max(data)
    descstat['Skewness'] <- moments::skewness(data)
    descstat['Excess Kurtosis'] <- moments::kurtosis(data)
    descstat['Obs'] <- length(data)
    descstat['Quantile 1%'] <- quantile(data, probs = 0.01)
    descstat['Quantile 99%'] <- quantile(data, probs = 0.99)
    descstat['Jarque-Bera stat.'] <- tseries::jarque.bera.test(data)$statistic
    descstat['Jarque-Bera p-val'] <- tseries::jarque.bera.test(data)$p.value
    descstat['Aug. Dickey-Fuller stat.'] <- adf.test(dataset$r_cpiaucsl, nlag = 1, output = TRUE)$type1[2]
    descstat['Aug. Dickey-Fuller p-val'] <- adf.test(dataset$r_cpiaucsl, nlag = 1, output = TRUE)$type1[3]
    if (full) {
        descstat['Autocorrelation (1)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[1])[1]
        descstat['Autocorrelation (2)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[2])[1]
        descstat['Autocorrelation (3)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[3])[1]
        descstat['Autocorrelation (4)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[4])[1]
        descstat['Autocorrelation (5)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[5])[1]
        descstat['Autocorrelation (6)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[6])[1]
        descstat['Autocorrelation (7)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[7])[1]
        descstat['Autocorrelation (8)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[8])[1]
        descstat['Autocorrelation (9)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[9])[1]
        descstat['Autocorrelation (10)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[10])[1]
        descstat['Autocorrelation (11)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[11])[1]
        descstat['Autocorrelation (12)'] <-  unlist(acf(data, lag.max = 12, plot = FALSE)[12])[1]
    } else {
        descstat['Autocorrelation (1)'] <-  NA
        descstat['Autocorrelation (2)'] <-  NA
        descstat['Autocorrelation (3)'] <-  NA
        descstat['Autocorrelation (4)'] <-  NA
        descstat['Autocorrelation (5)'] <-  NA
        descstat['Autocorrelation (6)'] <-  NA
        descstat['Autocorrelation (7)'] <-  NA
        descstat['Autocorrelation (8)'] <-  NA
        descstat['Autocorrelation (9)'] <-  NA
        descstat['Autocorrelation (10)'] <- NA
        descstat['Autocorrelation (11)'] <- NA
        descstat['Autocorrelation (12)'] <- NA
    }
    return(descstat)
}

.lag_tibble <- function(df, n_lags) {
    df <- df[, colnames(df)!=date_col]
    num_lags <- n_lags
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
    #train_full <- cbind(df, train_lagged)
    return(train_lagged)
}

calculate_mae <- function(errors) {
    return(mean(abs(unlist(errors))))
}
calculate_rmse <- function(errors) {
    return((mean(unlist(errors)^2))^(1/2))
}
calculate_mape <- function(y_true, y_pred) {
    return(mean(abs(unlist(y_pred) - unlist(y_true)) / unlist(y_true)))
}


# load and preprocess data
dataset <- read_excel('data/112839377-Homeexam OKA2014 - 2022 Sjur Westgaard_112853997_1652444012642.xlsx')
dataset[date_col] <- as.Date(dataset$dateid01)
dataset <- dataset[,colnames(dataset)!='dateid01']

dataset <- .get_perc_growth(dataset, r_vars)
dataset <- .get_differences(dataset, d_vars)

crisis <- read_csv('data/JHDUSRGDPBR.csv')
crisis <- crisis %>%
    mutate(crisis_start_end = c(0,diff(JHDUSRGDPBR))) %>%
    dplyr::select(date = DATE, crisis_start_end)





##### 1)

# plotting
# original timeseries
ts_plots <- map(
    c(d_vars, r_vars),
    ~ ggplot(data = dataset, mapping = aes_string(x=date_col, y=.)) + 
        geom_line(color = 'steelblue') +
        theme_bw() +
        scale_x_date(name = element_blank(), date_minor_breaks = "5 year")
        #scale_y_continuous(name = "Value")
    )
grid.arrange(grobs = ts_plots, ncol = 2)

# returns
dr_ts_plots <- map(
    colnames(dr_(dataset)),
    ~ ggplot(data = dataset, mapping = aes_string(x=date_col, y=.)) + 
        geom_line(color = 'steelblue') +
        theme_bw() +
        scale_x_date(name = element_blank(), date_minor_breaks = "5 year")
        #scale_y_continuous(name = "Value")
    )
grid.arrange(grobs = dr_ts_plots, ncol = 2)

# distributions
models_res_norm <- map(
    colnames(dr_(dataset)),
    ~ .get_norm_dist_approx(unlist(drop_na(dataset[.])))
    )

residuals_distributions <- map2(colnames(dr_(dataset)), models_res_norm,
     ~ dataset[.x] %>%
         as_tibble() %>%
         ggplot(aes_string(x = .x)) +
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
residuals_distributions[1]
grid.arrange(grobs = residuals_distributions, ncol = 2)

# scatters
p <- ggpairs(dr_(dataset), columnLabels = c(r_vars, d_vars)) +
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
ggplotly(p)


# desc statistics
desc_stat <- map_dfr(
    colnames(dr_(dataset)),
    ~ .get_desc_stat(dataset, .)
)
stat_names <- colnames(desc_stat)
desc_stat <- desc_stat %>%
    t() %>%
    .set_colnames(c(r_vars, d_vars)) %>%
    as_tibble() %>%
    sapply(as.numeric) %>%
    as_tibble() %>%
    mutate(across(everything(), round, 3))
desc_stat <- as_tibble(cbind(Statistics = stat_names, desc_stat))
stargazer(desc_stat, summary = FALSE, rownames = FALSE)

# desc stat by periods
    crisis_dates <- dataset %>%
    left_join(crisis, by = date_col) %>%
    filter(crisis_start_end==1 | crisis_start_end==-1) %>%
    dplyr::select(date)
crisis_dates <- rbind('1900-01-01', crisis_dates, '2099-01-01')

periods <- map(
    1:(nrow(crisis_dates)-1),
    ~ dataset[dataset$date >= crisis_dates[.,] & dataset$date < crisis_dates[.+1,],]
    )
normal_periods <- list(periods[[1]], periods[[3]], periods[[5]], periods[[7]], periods[[9]])
crisis_periods <- rbind(periods[[2]], periods[[4]], periods[[6]], periods[[8]])

# crisis periods
crisis_stats <- map_dfr(
    colnames(dr_(crisis_periods)),
    ~ .get_desc_stat(crisis_periods, ., full = FALSE)
)
stat_names <- colnames(crisis_stats)
crisis_stats <- crisis_stats %>%
    t() %>%
    .set_colnames(c(r_vars, d_vars)) %>%
    as_tibble() %>%
    sapply(as.numeric) %>%
    as_tibble() %>%
    mutate(across(everything(), round, 3))
crisis_stats <- as_tibble(cbind(Statistics = stat_names, crisis_stats))

# normal periods
normal_stats <- list()
i <- 0
for (period in normal_periods) {
    i <- i + 1
    normal_stats[[i]] <- map_dfr(
        colnames(dr_(normal_periods[[i]])),
        ~ .get_desc_stat(normal_periods[[i]], ., full = TRUE)
    )
    stat_names <- colnames(normal_stats[[i]])
    normal_stats[[i]] <- normal_stats[[i]] %>%
        t() %>%
        .set_colnames(c(r_vars, d_vars)) %>%
        as_tibble() %>%
        sapply(as.numeric) %>%
        as_tibble() %>%
        mutate(across(everything(), round, 3))
    normal_stats[[i]] <- as_tibble(cbind(Statistics = stat_names, normal_stats[[i]]))
}

stats <- append(normal_stats, list(crisis_stats))

baa <- tibble(Statistics = stat_names)
cpi <- tibble(Statistics = stat_names)
dco <- tibble(Statistics = stat_names)
dtb <- tibble(Statistics = stat_names)
ind <- tibble(Statistics = stat_names)
m1s <- tibble(Statistics = stat_names)
nas <- tibble(Statistics = stat_names)
ppi <- tibble(Statistics = stat_names)
t10 <- tibble(Statistics = stat_names)
unr <- tibble(Statistics = stat_names)
for (period in stats) {
    baa <- cbind(baa, period[r_vars[1]])
    cpi <- cbind(cpi, period[r_vars[2]])
    dco <- cbind(dco, period[r_vars[3]])
    dtb <- cbind(dtb, period[r_vars[4]])
    ind <- cbind(ind, period[r_vars[5]])
    m1s <- cbind(m1s, period[r_vars[6]])
    nas <- cbind(nas, period[d_vars[1]])
    ppi <- cbind(ppi, period[d_vars[2]])
    t10 <- cbind(t10, period[d_vars[3]])
    unr <- cbind(unr, period[d_vars[4]])
}

crisis_dates$YM <- format(as.Date(crisis_dates$date), "%Y-%m")
char_dates <- as.character(crisis_dates$YM)
period_dates <- map_chr(
    1:(length(char_dates)/2),
    ~ str_c(char_dates[(.*2)-1], char_dates[.*2], sep = " - ")
)

colnames(baa)[2:7] <- c(period_dates, 'Crises') 
colnames(cpi)[2:7] <- c(period_dates, 'Crises') 
colnames(dco)[2:7] <- c(period_dates, 'Crises') 
colnames(dtb)[2:7] <- c(period_dates, 'Crises') 
colnames(ind)[2:7] <- c(period_dates, 'Crises') 
colnames(m1s)[2:7] <- c(period_dates, 'Crises') 
colnames(nas)[2:7] <- c(period_dates, 'Crises') 
colnames(ppi)[2:7] <- c(period_dates, 'Crises') 
colnames(t10)[2:7] <- c(period_dates, 'Crises') 
colnames(unr)[2:7] <- c(period_dates, 'Crises') 

stargazer(baa, summary = FALSE, rownames = FALSE)
stargazer(cpi, summary = FALSE, rownames = FALSE)
stargazer(dco, summary = FALSE, rownames = FALSE)
stargazer(dtb, summary = FALSE, rownames = FALSE)
stargazer(ind, summary = FALSE, rownames = FALSE)
stargazer(m1s, summary = FALSE, rownames = FALSE)
stargazer(nas, summary = FALSE, rownames = FALSE)
stargazer(ppi, summary = FALSE, rownames = FALSE)
stargazer(t10, summary = FALSE, rownames = FALSE)
stargazer(unr, summary = FALSE, rownames = FALSE)





##### 2)

data_ardl <- cbind(dataset[c(date_col, dep_var)], .lag_tibble(dr_(dataset), n_lags = 2))
train_ardl <- data_ardl %>%
    dplyr::filter(date <= train_test_split_date)
test_ardl <- data_ardl %>%
    dplyr::filter(date > train_test_split_date)

linreg_form <- paste(
    dep_var,
    paste(colnames(data_ardl)[
        !str_detect(colnames(data_ardl), '.*dcoil.*') & !colnames(data_ardl)==date_col
        ], collapse = ' + '),
    sep = ' ~ '
)

linreg_full_model <- lm(linreg_form, data = train_ardl)
linreg_step_model <- step(linreg_full_model, direction = "backward", trace=FALSE) 
summary(linreg_full_model)
summary(linreg_step_model)

ardl_form <- paste(
    dep_var,
    paste(colnames(data_ardl)[
        !colnames(data_ardl)==dep_var & !colnames(data_ardl)==date_col
    ], collapse = ' + '),
    sep = ' ~ '
)
ardl_full_model <- lm(ardl_form, data = train_ardl)
ardl_step_model <- step(ardl_full_model, direction = "backward", trace=FALSE) 
summary(ardl_full_model)
summary(ardl_step_model)

models <- list(
    M1 = linreg_full_model, 
    M2 = linreg_step_model, 
    M3 = ardl_full_model, 
    M4 = ardl_step_model
    )
stargazer(models$M1, models$M2, models$M3, models$M4)

# create tibbles with true, fitted and residuals for plotting
true_est_res <- map(models, ~ tibble(
    Date = train_ardl[(nrow(train_ardl)-length(.$residuals)+1):nrow(train_ardl), date_col],
    y = unlist(.$model[dep_var]),
    y_hat = .$fitted.values,
    e = .$residuals)
    )

estimates_plots <- map2(true_est_res, names(true_est_res),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y, group = 1)) +
            geom_line(color = 'navyblue', size = 1.01) +
            geom_line(mapping = aes(x=Date, y=y_hat), color = 'skyblue4') +
            theme_bw() +
            scale_x_date(
                name = element_blank(), 
                date_minor_breaks = "1 year"
                ) +
            scale_y_continuous(name = .y)
)
grid.arrange(grobs = estimates_plots)
    
residuals_plots <-  map2(true_est_res, names(true_est_res),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=e, group = 1)) +
        geom_line(color = 'firebrick') +
        geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey10') +
        theme_bw() +
        scale_x_date(
            name = element_blank()
            ) +
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

# create out-of-sample predictions and errors
predictions <- map(models, ~ predict(., test_ardl))
prediction_errors <- map(predictions, ~ . - test_ardl[dep_var])

predictions_level <- map(
    predictions,
    ~ val_(dataset)[sum(dataset$date<=train_test_split_date):(nrow(dataset)-1), 'dcoilbrenteu'] * (.+1)
)
prediction_errors_level <- map(
    predictions_level,
    ~ . - dataset[dataset$date > train_test_split_date, 'dcoilbrenteu']
)

prediction_metrics <- rbind(
    unlist(map(prediction_errors_level, ~ calculate_mae(.))),
    unlist(map(prediction_errors_level, ~ calculate_rmse(.))),
    unlist(map(predictions_level, ~ calculate_mape(dataset[dataset$date > train_test_split_date, 'dcoilbrenteu'], .)))
)
rownames(prediction_metrics) <- c('MAE', 'RMSE', 'MAPE')
stargazer(prediction_metrics, summary = FALSE, rownames = TRUE)


# inspect prediction errors
prediction_errors_norm_level <- map(prediction_errors_level, ~ .get_norm_dist_approx(unlist(.)))
prediction_errors_distributions <- map2(prediction_errors_level, prediction_errors_norm_level,
     ~ .x %>%
         as_tibble() %>%
         ggplot(aes_string(x = 'dcoilbrenteu')) +
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
    map_dfc(prediction_errors_level, ~ tseries::jarque.bera.test(unlist(.))$statistic),
    map_dfc(prediction_errors_level, ~ tseries::jarque.bera.test(unlist(.))$p.value)
)
rownames(prediction_errors_normality) <- c('Statistic', 'p-value')
stargazer(prediction_errors_normality, summary = FALSE, rownames = TRUE)

# serial correlation in errors
pred_err_df <- data.frame(matrix(unlist(prediction_errors_level), ncol = length(prediction_errors_level), byrow = FALSE))
colnames(pred_err_df) <- str_c(names(models), 'err', sep = '_')

pred_err_df_ext <- cbind(
    pred_err_df,
    rbind(NA, pred_err_df[1:(nrow(pred_err_df) - 1), ]),
    rbind(NA, NA, pred_err_df[1:(nrow(pred_err_df) - 2), ]),
    rbind(NA, NA, NA, pred_err_df[1:(nrow(pred_err_df) - 3), ])
)

colnames(pred_err_df_ext) <- c(
    names(pred_err_df),
    str_c(names(pred_err_df), '1', sep = '_'),
    str_c(names(pred_err_df), '2', sep = '_'),
    str_c(names(pred_err_df), '3', sep = '_')
)

autocorrtest_formulas <- 
    list(
        M1_err_autocorr = formula(M1_err ~ M1_err_1 + M1_err_2 + M1_err_3),
        M2_err_autocorr = formula(M2_err ~ M2_err_1 + M2_err_2 + M2_err_3),
        M3_err_autocorr = formula(M3_err ~ M3_err_1 + M3_err_2 + M3_err_3),
        M4_err_autocorr = formula(M4_err ~ M4_err_1 + M4_err_2 + M4_err_3)
    )

autocorrtest_models <- map(autocorrtest_formulas, ~ lm(., pred_err_df_ext))
map(autocorrtest_models, summary)
stargazer(autocorrtest_models)

# plot out-of-sample predictions
predictions_dfs <- map(predictions, ~ tibble(pred = ., Date = test_ardl$date, y_true = unlist(test_ardl[dep_var])))
predictions_plots <- map2(predictions_dfs, names(predictions_dfs),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y_true, group = 1)) +
            geom_line(color = 'navyblue', size=1.01) +
            geom_line(mapping = aes(x=Date, y=pred), color = 'skyblue3') +
            theme_bw() +
            scale_x_date(
                name = element_blank(), 
                date_minor_breaks = "1 year"
                ) +
            scale_y_continuous(name = .y)
)
grid.arrange(grobs = predictions_plots)

# plot predictions in levels
predictions_dfs <- map(predictions_level, ~ tibble(pred = unlist(.), Date = test_ardl$date, y_true = unlist(dataset[dataset$date>train_test_split_date,"dcoilbrenteu"])))
predictions_plots <- map2(predictions_dfs, names(predictions_dfs),
    ~ ggplot(data = .x, mapping = aes(x=Date, y=y_true, group = 1)) +
            geom_line(color = 'navyblue', size=1.01) +
            geom_line(mapping = aes(x=Date, y=pred), color = 'skyblue3') +
            theme_bw() +
            scale_x_date(
                name = element_blank(), 
                date_minor_breaks = "1 year"
                ) +
            scale_y_continuous(name = .y)
)
grid.arrange(grobs = predictions_plots)

#forecast combination
test_test_split <- (nrow(test_ardl)-round(nrow(test_ardl)*0.3))

pred_linreg <- predictions_level$M2
pred_ardl <- predictions_level$M4
true <- dataset[dataset$date>train_test_split_date, "dcoilbrenteu"]

pred_comb <- tibble(y_true = unlist(true), y_pred_linreg = unlist(pred_linreg), y_pred_ardl = unlist(pred_ardl))

test_res_ardl <- pred_comb[1:(test_test_split-1),]
test_meta_ardl <- pred_comb[test_test_split:nrow(test_ardl),]

pred_comb_model <- lm(y_true ~ y_pred_linreg + y_pred_ardl, test_res_ardl)
summary(pred_comb_model)

predictions_meta <- predict(pred_comb_model, test_meta_ardl)
prediction_errors_meta <- predictions_meta - test_meta_ardl$y_true

# comparison to forecasts without combining
pred_ardl_meta <- append(
    map(
        predictions_level,
        ~ unlist(.)[test_test_split:nrow(test_ardl)]
    ),
    list(comb = predictions_meta)
)
pred_errors_ardl_meta <- append(
    map(
        prediction_errors_level,
        ~ unlist(.)[test_test_split:nrow(test_ardl)]
    ),
    list(comb = prediction_errors_meta)
)

prediction_metrics <- rbind(
    unlist(map(pred_errors_ardl_meta, ~ calculate_mae(.))),
    unlist(map(pred_errors_ardl_meta, ~ calculate_rmse(.))),
    unlist(map(pred_ardl_meta, ~ calculate_mape(test_meta_ardl[, 'y_true'], .)))
)
rownames(prediction_metrics) <- c('MAE', 'RMSE', 'MAPE')
stargazer(prediction_metrics, summary = FALSE, rownames = TRUE)
