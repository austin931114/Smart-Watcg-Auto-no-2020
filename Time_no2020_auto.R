### Customize desire CPM & Confidence Interval
CPM <- 15  # minimum CPM
pred_interval <- 95


## Set Target Year & Month
year <- year(today())
month <- month(today()) - 1
# the last data of the data
end.date <- as.Date(paste(year, month, "01", sep = "-"))
# one month before the smallest prediction month
end.pred.date <- as.Date(ifelse((month - 3) >= 1,
                                paste(year, month - 3, "01", sep = "-"),
                                paste(year - 2, month - 3 + 12, "01", sep = "-")))
# at least 24 months before the end.pred.date
start.date <- as.Date(ifelse(month == 12,    
                             paste(year - 2, "01", "01", sep = "-"),
                             paste(year - 3, month(end.pred.date) + 1, "01", sep = "-")))  
# one month before the prev 4 for prediction
prev4_month <- ifelse(month - 2 - 5 >= 1,
                      month - 2 - 5,
                      month - 2 - 5 + 12)
prev4_year <- ifelse(month - 2 - 5 >= 1,
                     year - 1,
                     year - 2)
# make aggregate season order for the ts data
if((month - 2) == 1){
  season_order <- c(1:12)
} else{
  season_order <- c((month - 2):12, 1:(month - 3))
}
# make seasonal order ONLY for the previous 4 month (6 months total)
if((prev4_month + 6) > 12){
  prev4_season_order <- c((prev4_month + 1):12, 1:(prev4_month + 6 - 12))
} else{
  prev4_season_order <- c((prev4_month + 1):(prev4_month + 6))
}

###############################################################################
## try to predict the stores with dropping sales volume



### grab the stores that we are interested in
ss_test <- ss %>% 
  filter(year(f.date) != 2020) %>%
  select(store_id_id, f.date:st.date,program) %>%
  filter(f.date <= end.date,
         f.date >= st.date,
         # at least 24 months of data
         st.date <= start.date) %>%
  filter(all(sls(bundle, program))) %>%
  # for store that has 1 or 2 machines at the whole time
  filter(all(machines == 1) | all(machines == 2) | all(machines == 3)) %>% 
  # CPM >= target CPM
  filter(cpm[f.date == end.pred.date] >= CPM)


## for stores that has alternating machine number
# find monthly factor for 2 machines
machine_mon <- ss %>% 
  filter(year(f.date) != 2020) %>%
  filter(all(sls(bundle, program))) %>%
  group_by(machines, month) %>% 
  summarise(cases = mean(cases)) %>% 
  filter(machines > 0 & machines < 5) %>% 
  arrange(machines, month, by_group = TRUE) %>% 
  pivot_wider(names_from = month, values_from = cases) %>% 
  filter(machines == 1 | machines == 2 | machines == 3) %>% 
  select(machines, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

mach2_month_fac <- machine_mon[2, -1] / machine_mon[1, -1]
names(mach2_month_fac) <- c(1:12)
mach2_fac <- mach2_month_fac %>% 
  pivot_longer(c(1:12), names_to = "m_num", values_to = "mach2_fac")
mach2_fac$m_num <- as.numeric(mach2_fac$m_num)

# generate the data
ss_test_AM <- ss %>% 
  filter(year(f.date) != 2020) %>%
  select(store_id_id, f.date:st.date,program) %>% 
  filter(f.date <= end.date,
         f.date >= st.date,
         st.date <= start.date) %>%
  filter(all(sls(bundle, program))) %>%
  # CPM >= target CP,
  filter(cpm[f.date == end.pred.date] >= CPM) %>% 
  # for store that DOES NOT have same machine count at the whole time
  anti_join(ss_test) %>% 
  # filter(!(all(machines == 1) | all(machines == 2) | all(machines == 3) |
  #            all(machines == 4) | all(machines == 5) | all(machines == 7) |
  #            all(machines == 8))) %>%
  mutate(m_num = month(f.date)) %>% 
  inner_join(mach2_fac, by = "m_num") %>% 
  # if there are more than 1 machine, divide by a constant
  # (for more accurate sales number)
  mutate(cases = ifelse(machines == 2,
                        cases / mach2_fac,
                        cases))

## combine both same machine & alternating machine number
ss_test <- bind_rows(ss_test, ss_test_AM)

## find seasonality (additive)
ex <- ss_test %>% 
  group_by(f.date, month) %>% 
  summarise(mean(cases)) %>% 
  filter(f.date >= "2012-01-01") %>% 
  select(3)
ex_ts <- ts(ex[,2], frequency = 12, 
            # data from first day to last day that we are use to predict
            start = c(2012, 1), end = c((year - 1), month))
# use "decompose" to find the seasonality (only for ts object)
# we can also have multiplicative index if needed (attention to how to interpret)
ex_component <- decompose(ex_ts, type = "additive")
season <- ex_component$seasonal[season_order]
prev4_season <- ex_component$seasonal[prev4_season_order] # total 6 months

## model function
model <- function(df, year, month, id, target_year, target_month, interval, season,
                  p4_year, p4_month, p4_season, end.pred) {
  # ts_month <- target_month
  # ts_year <- target_year
  # # deal with cross year months
  # if((ts_month - 3) < 1) {
  #   ts_month <- 12 - abs(ts_month - 3)
  #   ts_year <- ts_year - 1
  # }else {
  #   ts_month <- ts_month - 3
  # }
  ## ts data
  g <- unlist(df)
  # the ts to predict target year, month
  store_ts <- ts(g, frequency = 12,
                 start = c(year, month), end = c(year(end.pred), month(end.pred))) # 2021/2
  # all ts
  store_ts_full <- ts(g, frequency = 12,
                      start = c(year, month), end = c(target_year - 1, target_month)) # 2021/5
  # previous 4 month
  store_ts_p4 <- ts(g, frequency = 12,
                    start = c(year, month), end = c(p4_year, p4_month)) # 2019/11
  
  ## ets
  store_ets <- ets(store_ts, model = "AAA")
  store_ets_forecast <- forecast(store_ets, h = 12, level = interval)
  # no seasonality
  store_ets_NS <- ets(store_ts, model = "AAN")
  store_ets_NS_forecast <- forecast(store_ets_NS, h = 12, level = interval)
  # add back the seasonality
  store_ets_NS_forecast$lower <- store_ets_NS_forecast$lower + season
  store_ets_NS_forecast$upper <- store_ets_NS_forecast$upper + season
  store_ets_NS_forecast$mean <- store_ets_NS_forecast$mean + season
  
  
  ## p4 ets
  store_ets_p4 <- ets(store_ts_p4, model = "AAA")
  store_ets_p4_forecast <- forecast(store_ets_p4, h = 6, level = interval)
  # p4 no seasonality
  store_ets_p4_NS <- ets(store_ts_p4, model = "AAN")
  store_ets_p4_NS_forecast <- forecast(store_ets_p4_NS, h = 6, level = interval)
  # add back the seasonality
  store_ets_p4_NS_forecast$lower <- store_ets_p4_NS_forecast$lower + p4_season
  store_ets_p4_NS_forecast$upper <- store_ets_p4_NS_forecast$upper + p4_season
  store_ets_p4_NS_forecast$mean <- store_ets_p4_NS_forecast$mean + p4_season
  
  
  # ## PLOT
  par(mfrow = c(1,2))
  plot(store_ets_forecast, xlim = c(target_year - 1, 2021.25), # 2020 - 2021/3
       ylim = c(0, 20), main = "**ETS**", sub = id)
  lines(store_ts_full, xlim = c(target_year - 1, 2021.25), ylim = c(0, 20))
  plot(store_ets_NS_forecast, xlim = c(target_year - 1, 2021.5),
       ylim = c(0, 20), main = "**ETS_NS**", sub = id)
  lines(store_ts_full, xlim = c(target_year - 1, 2021.25), ylim = c(0, 20))
  my_plot <- recordPlot()
  
  return(list(store_ts_full = store_ts_full,
              store_ets_NS_forecast = store_ets_NS_forecast$lower[,1],
              store_ets_forecast = store_ets_forecast$lower[,1],
              store_ets_forecast_up = store_ets_forecast$upper[,1],
              store_ets_NS_forecast_up = store_ets_NS_forecast$upper[,1],
              store_ets_p4_forecast_up = store_ets_p4_forecast$upper[,1],
              store_ets_p4_NS_forecast_up = store_ets_p4_NS_forecast$upper[,1],
              predict_plot = my_plot,
              season = season))
}
## indicator function
indic <- function(data) {
  full <- data$store_ts_full
  l <- length(data$store_ts_full)
  g <- data$store_ts_full[(l-2):l]
  p4 <- data$store_ts_full[(l - 6):(l - 1)]
  ## indicator
  NSindic <- 0
  etsindic <- 0
  # find the index where sales is smaller than the forecast
  o <- which((g < data$store_ets_NS_forecast[1:3]) == 1)
  e <- which((g < data$store_ets_forecast[1:3]) == 1)
  
  k <- o[which(g[o] == 0)]
  t <- e[which(g[e] == 0)]
  
  
  
  
  
  # see if the predicted value is greater than the real value
  if(sum(g < data$store_ets_NS_forecast[1:3]) > 0){
    # only true when those index has a sales number 0
    # if (sum(g[o] != 0) == 0) {
    #   NSindic = 1
    # }
    NSindic = sum((data$store_ets_NS_forecast_up[k] - g[k]) /
      (data$store_ets_NS_forecast_up[k] - data$store_ets_NS_forecast[k]))
    
  }
  if(sum(g < data$store_ets_forecast[1:3]) > 0){
    # only true when those index has a number 0
    # if (sum(g[o] != 0) == 0) {
    #   etsindic = 1
    # }
    etsindic = sum((data$store_ets_forecast_up[t] - g[t]) /
      (data$store_ets_forecast_up[t] - data$store_ets_forecast[t]))
  }
  
  ## remove unusual high purchase
  if(length(o) > 0) {
    # only looking for up to previous 4 months
    for(i in 1:4) {
      # if the real sales is greater than the upper bound prediction
      if(p4[5 - i] > data$store_ets_p4_NS_forecast_up[5 - i]) {
        NSindic <- 0
        break
      }
    }
  }
  if(length(o) > 0) {
    # only looking for up to previous 4 months
    for(i in 1:4) {
      # if the real sales is greater than the upper bound prediction
      if(p4[5 - i] > data$store_ets_p4_forecast_up[5 - i]) {
        etsindic <- 0
        break
      }
    }
  }
  NSindic + etsindic
}
## potential problem month
ets_problem <- function(data) {
  full <- data$store_ts_full
  l <- length(data$store_ts_full)
  g <- data$store_ts_full[(l-2):l]
  # find the index where sales is smaller than the forecast
  e <- which((g < data$store_ets_forecast[1:3]) == 1)
  e
  
}
ets_NS_problem <- function(data) {
  full <- data$store_ts_full
  l <- length(data$store_ts_full)
  g <- data$store_ts_full[(l-2):l]
  # find the index where sales is smaller than the forecast
  o <- which((g < data$store_ets_NS_forecast[1:3]) == 1)
  o
}


# Find the minimum year and month
my_store <- ss_test %>% 
  group_by(store_id_id)  %>% 
  mutate(n = n()) %>%
  # at least 12 month
  filter(n >= 12) %>% 
  mutate(min_year = year(min(f.date)),
         min_month = month(min(f.date))) %>% 
  select(store_id_id, cases, min_year, min_month) %>% 
  group_by(store_id_id, min_year, min_month) %>% 
  # nest the cases to perform prediction
  nest()

## remove first non-zero sales
for(i in 1:dim(my_store)[1]) {
  for(g in 1:12)
    if(my_store$data[[i]][g,] != 0) {
      my_store$data[[i]][g,] <- 0
      break
    }
}

test <- my_store

## RUN the functions
test <- test %>% 
  mutate(model = map(data, model, year = min_year, month = min_month,
                     id = store_id_id, target_year = year, target_month = month, 
                     interval = pred_interval, season = season, p4_year = prev4_year, 
                     p4_month = prev4_month, p4_season = prev4_season,
                     end.pred = end.pred.date))


test7 <-  test %>% 
  mutate(indic = map(model, indic)) %>%
  unnest(indic) %>%
  filter(indic > 0) %>% 
  ungroup(min_year, min_month) %>% 
  arrange(desc(indic)) %>%  mutate(n = n_distinct(store_id_id)) %>% psc 
test_2020 <- test7


# test10 %>% anti_join(test_2020, by = "store_id_id")
# 
# test_2020
# 
# g <- test10[test10$store_id_id == 4942,]$model[[1]]$store_ts_full[(l-2):l]
# 
# l <- length(test10[test10$store_id_id == 4942,]$model[[1]]$store_ts_full)
# 
# 
# o <- which((g <  test10[test10$store_id_id == 4942,]$model[[1]]$store_ets_NS_forecast[1:3]) == 1)
# if(sum(g[o] != 0) == 0){
#   print("egg")
# }

test_2020 %>% 
  ungroup(min_month, min_year) %>%
  select(store_id_id, indic) %>% 
  full_join(test7 %>%
              ungroup(min_month, min_year) %>% 
              select(store_id_id, indic), 
            by = "store_id_id") %>% 
  rename(indic_no2020 = indic.x,
         indic_2020 = indic.y) %>%
  # group_by(indic_no2020 >= 2 & indic_2020 >= 2) %>% 
  # arrange(desc(`indic_no2020 >= 2 & indic_2020 >= 2`))
  mutate(indic_no2020 = ifelse(is.na(indic_no2020), 0, indic_no2020),
         indic_2020 = ifelse(is.na(indic_2020), 0, indic_2020),
         both = ifelse(indic_2020 >= 1 & indic_no2020 >= 1, 1, 0),
         no_2020b2 = ifelse(indic_no2020 >= 2, 1, 0),
         with_2020b2 = ifelse(indic_2020 >= 2, 1, 0)) %>%
  arrange(desc(both), desc(no_2020b2), desc(with_2020b2), desc(indic_no2020), desc(indic_2020)) %>% 
  select(1:3) %>% 
  # anti_join(april1 %>% 
  #             filter(Status == "SmartWatch") %>% 
  #             select(store_id_id),
  #           by = "store_id_id") %>% 
  makexl("sw_2021may")


















###################################################################################################
test7[,1] %>% 
  inner_join(test_2020[,1] , by = "store_id_id") %>% 
  anti_join(Book2 %>% rename(store_id_id = store_id)) %>% makexl("SW_May2021")


Book2 <- read_excel("C:/Users/YongNanChang/OneDrive - Freezing Point LLC/Desktop/Book2.xlsx")


test_2020[,1] %>% 
  full_join(test7[,1])

ALL_ets <- test7 %>% 
  mutate(ets_problem = map(model, ets_problem)) %>% 
  unnest(ets_problem) %>%
  ungroup(min_year, min_month) %>% 
  select(store_id_id, ets_problem)

ALL_ets_NS <- test7 %>% 
  mutate(ets_NS_problem = map(model, ets_NS_problem)) %>% 
  unnest(ets_NS_problem) %>%
  ungroup(min_year, min_month) %>% 
  select(store_id_id, ets_NS_problem)

ALL_no_2020_2021.2.3.4 <- ALL_ets %>% 
  full_join(ALL_ets_NS, by = c("store_id_id"))

# make the number to represent the real month 
if((month - 3) >= 1) {
  month_index <- month - 3
  ALL_no_2020_2021.2.3.4$ets_problem <- ALL_no_2020_2021.2.3.4$ets_problem + month_index  
  ALL_no_2020_2021.2.3.4$ets_NS_problem <- ALL_no_2020_2021.2.3.4$ets_NS_problem + month_index
}else if((month - 3) == -1) {
  for(K in 1:length(ALL_no_2020_2021.2.3.4$ets_problem)) {
    if(ALL_no_2020_2021.2.3.4$ets_problem[k] == 3) {
      ALL_no_2020_2021.2.3.4$ets_problem[k] == 2
    }else if(ALL_no_2020_2021.2.3.4$ets_problem[k] == 2) {
      ALL_no_2020_2021.2.3.4$ets_problem[k] == 1
    }else if(ALL_no_2020_2021.2.3.4$ets_problem[k] == 1) {
      ALL_no_2020_2021.2.3.4$ets_problem[k] == 12
    }
    
    if(ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 3) {
      ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 2
    }else if(ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 2) {
      ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 1
    }else if(ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 1) {
      ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 12
    }
  }
}else if ((month - 3) == -2) {
  for(K in 1:length(ALL_no_2020_2021.2.3.4$ets_problem)) {
    if(ALL_no_2020_2021.2.3.4$ets_problem[k] == 3) {
      ALL_no_2020_2021.2.3.4$ets_problem[k] == 1
    }else if(ALL_no_2020_2021.2.3.4$ets_problem[k] == 2) {
      ALL_no_2020_2021.2.3.4$ets_problem[k] == 12
    }else if(ALL_no_2020_2021.2.3.4$ets_problem[k] == 1) {
      ALL_no_2020_2021.2.3.4$ets_problem[k] == 11
    }
    
    if(ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 3) {
      ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 1
    }else if(ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 2) {
      ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 12
    }else if(ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 1) {
      ALL_no_2020_2021.2.3.4$ets_NS_problem[k] == 11
    }
  }
}



makexl(ALL_no_2020_2021.2.3.4)


## example to access specific data
test[test$store_id_id == "394", ]$model[[1]]
