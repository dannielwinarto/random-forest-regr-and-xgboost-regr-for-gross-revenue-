setwd("~/Documents/Project 1/random forest projection")
rm(list = ls())
dev.off()

# notes
# playback_based_cpm = impression_based_cpm * (impressions/monetized_playbacks)

# library(forecast) time series forecasting
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(VIM)
library(zoo)
library(randomForest) # for RF algorithm 
library(caret) # for crossval folding purpose
library(corrplot)


# training set
data_sept = fread("danniel_sept.csv")
data_oct = fread("danniel_oct.csv")
data_nov = fread("danniel_nov.csv")
data_dec = fread("danniel_dec.csv")
data_may_to_august = fread("danniel_may_june_july_august.csv")
colnames(data_may_to_august) = colnames(data_sept) # data from may_to_august doesnt have the column name

# testing set
jan_data = fread("danniel_jan_2017_new.csv")
jan_data$datestamp = ymd(jan_data$datestamp)


############################################
# Data Cleaning
############################################
# working on training set
full_ds = bind_rows(data_may_to_august,data_sept, data_oct, data_nov, data_dec)
full_ds$day = NULL

# converting datestamp col to date format 
full_ds$datestamp = ymd(full_ds$datestamp)
setkeyv(full_ds, c("channel_id", "datestamp"))       
# head(full_ds,20)

# missing_val = aggr(full_ds) # missing value analysis
# adding: col num_days_in_month to show how many days in that month
# adding: col net_sub_gain
# adding: col y_m -> which is year-month combination for looping process
# removing col with that contains a lot of missing values
full_ds[,c("num_days_in_month", "net_sub_gain", "y_m") := 
          list(days_in_month(datestamp), (subscribers_gained + subscribers_lost), paste(year(datestamp), month(datestamp), sep = "-"))]
full_ds$y_m = as.factor(full_ds$y_m)
full_ds[,c("subscribers_gained", "subscribers_lost", "favorites_added", "favorites_removed", "uniques") := NULL]

# finding number of months to be analyzed
total_months = round(time_length(max(full_ds$datestamp) - min(full_ds$datestamp), unit = "month"))

# adding column month_order, earliest month is order 1, ordered it up to n month as the latest
init = min(full_ds$datestamp)
for(i in seq(1:(total_months))){
  full_ds$month_order[ init <= full_ds$datestamp & full_ds$datestamp < (init + months(1))] = i 
  init = init + months(1)
}

full_ds = as.data.table(full_ds)
# table(full_ds$month_order)

# filtering to only for channel with complete observation
# finding the number of days in obs:
days_of_obs = sum(days_in_month(unique(full_ds$month)))
# filtering out channels which has incomplete observation
temp = full_ds%>% group_by(channel_id) %>% summarise(counts = n()) %>% arrange(desc(-counts)) 
temp = temp[ temp$counts == days_of_obs,]
full_ds_freqChannels = as.data.table(filter(full_ds, channel_id %in% temp$channel_id))
# length(unique(full_ds_freqChannels$channel_id)) 35683 channels in total 


#  creating a table: 2 wks performance -> 1 months gross rev
p1 = 14 # period 1 of learning in days
two_wk_dt = data.table()
count = 0
# length(unique(full_ds_freqChannels$channel_id))
for(i in unique(full_ds_freqChannels$month_order)){ # for each month order
  filter_month = full_ds_freqChannels[month_order == i,] # filter dataset by month otder
  temp = rep( # adding 14 days limit variable 
    c(rep("<= n days", times = p1), rep(" > n days", times = (days_in_month(filter_month$datestamp[1])-p1))),  
    times = length(unique(full_ds_freqChannels$channel_id))) 
  filter_month = filter_month[, limit_days := temp] # attaching this variable
  Tot_gross_rev = filter_month[, list(gross_revenue_i = sum(gross_revenue)), by = "channel_id"] # calculate the sum revenue of the whole month 
  filter_month = filter_month[limit_days == "<= n days", # computation of aggregation 14 days by each channel
                              list(views = sum(views), likes = sum(likes), dislikes = sum(dislikes), comments  = sum(comments),
                                   monetized_playbacks = sum(monetized_playbacks), impressions = sum(impressions),
                                   shares = sum(shares), estimated_minutes_watched = sum(estimated_minutes_watched),
                                   average_view_duration = mean(average_view_duration),average_view_percentage = mean(average_view_percentage),
                                   annotation_click_through_rate = sum(annotation_click_through_rate), annotation_close_rate = sum(annotation_close_rate),
                                   annotation_clicks = sum(annotation_clicks), annotation_impressions = sum(annotation_impressions),
                                   net_sub_gain = sum(net_sub_gain), year = mean(year), month = mean(month)),
                              by = "channel_id"]
  filter_month[, gross_revenue := Tot_gross_rev[,gross_revenue_i]] # adding col gross revenue
  if(count == 0){
    two_wk_dt = filter_month
    count = count + 1
  } else{
    two_wk_dt = rbindlist(list(two_wk_dt,filter_month))
  }
}
setkeyv(two_wk_dt, c("channel_id","year", "month"))
two_wk_dt


#  creating a table: 1 month performance -> end of 2 months gross rev
one_mt_dt = data.table()
p2 = 1 # period 2 of learning, in month
# i = 1
for(i in unique(full_ds_freqChannels$month_order)[1:(length(unique(full_ds_freqChannels$month_order))-1)]){ # since this is 2-month pair 
  # analysis, we  should exclude the very last month during our iteration, because the last month will have no pairs
  filter_month = full_ds_freqChannels[month_order == i| month_order == (i + 1),] # filter dataset by month i and month i + 1
  temp = rep( # adding n month limit variable
    c(rep("<= n months", times = (p2*days_in_month(filter_month$datestamp[1]))), 
      rep("> n months", times = days_in_month(filter_month$datestamp[1] + months(1)))),  
    times = length(unique(full_ds_freqChannels$channel_id))) 
  filter_month = filter_month[, limit_days := temp] # attaching this variable
  Tot_gross_rev = filter_month[, list(gross_revenue_i = sum(gross_revenue)), by = "channel_id"] # calculate the sum revenue of the whole month 
  filter_month = filter_month[limit_days == "<= n months", # computation of aggregation 14 days by each channel
                              list(views = sum(views), likes = sum(likes), dislikes = sum(dislikes), comments  = sum(comments),
                                   monetized_playbacks = sum(monetized_playbacks), impressions = sum(impressions),
                                   shares = sum(shares), estimated_minutes_watched = sum(estimated_minutes_watched),
                                   average_view_duration = mean(average_view_duration),average_view_percentage = mean(average_view_percentage),
                                   annotation_click_through_rate = sum(annotation_click_through_rate), annotation_close_rate = sum(annotation_close_rate),
                                   annotation_clicks = sum(annotation_clicks), annotation_impressions = sum(annotation_impressions),
                                   net_sub_gain = sum(net_sub_gain), year = mean(year), month = mean(month)),
                              by = "channel_id"]
  filter_month[, gross_revenue := Tot_gross_rev[,gross_revenue_i]] # adding col gross revenue
  if(count == 0){
    one_mt_dt = filter_month
    count = count + 1
  } else{
    one_mt_dt = rbindlist(list(one_mt_dt,filter_month))
  }
}
setkeyv(one_mt_dt, c("channel_id","year", "month"))
one_mt_dt

#  creating a table: 2 months performance -> end of 4 months gross rev
two_mt_dt = data.table()
p3 = 2 # period 2 of learning, in month

for(i in unique(full_ds_freqChannels$month_order)[1:(length(unique(full_ds_freqChannels$month_order))-4)]){ # since this is 4-month pair 
  # analysis, we  should exclude the  last 3 month during our iteration, because the last 3 months will have no pairs
  filter_month = full_ds_freqChannels[month_order == i| month_order == (i + 1) | month_order == (i + 2) | month_order == (i + 3),] # filter dataset by month i and month i + 1
  length(filter_month$channel_id)
  temp = rep( # adding n month limit variable
    c(rep("<= n months", times = ((p3-1)*days_in_month(filter_month$datestamp[1]) + (p3-1)*days_in_month(filter_month$datestamp[1] + months(1)))), 
      rep("> n months", times = (days_in_month(filter_month$datestamp[1] + months(2))+ days_in_month(filter_month$datestamp[1] + months(3))))),  
    times = length(unique(full_ds_freqChannels$channel_id))) 
  length(temp)
  filter_month = filter_month[, limit_days := temp] # attaching this variable
  Tot_gross_rev = filter_month[, list(gross_revenue_i = sum(gross_revenue)), by = "channel_id"] # calculate the sum revenue of the whole month 
  filter_month = filter_month[limit_days == "<= n months", # computation of aggregation 14 days by each channel
                              list(views = sum(views), likes = sum(likes), dislikes = sum(dislikes), comments  = sum(comments),
                                   monetized_playbacks = sum(monetized_playbacks), impressions = sum(impressions),
                                   shares = sum(shares), estimated_minutes_watched = sum(estimated_minutes_watched),
                                   average_view_duration = mean(average_view_duration),average_view_percentage = mean(average_view_percentage),
                                   annotation_click_through_rate = sum(annotation_click_through_rate), annotation_close_rate = sum(annotation_close_rate),
                                   annotation_clicks = sum(annotation_clicks), annotation_impressions = sum(annotation_impressions),
                                   net_sub_gain = sum(net_sub_gain), year = mean(year), month = min(month)),
                              by = "channel_id"]
  filter_month[, gross_revenue := Tot_gross_rev[,gross_revenue_i]] # adding col gross revenue
  if(count == 0){
    two_mt_dt = filter_month
    count = count + 1
  } else{
    two_mt_dt = rbindlist(list(two_mt_dt,filter_month))
  }
}
setkeyv(two_mt_dt, c("channel_id","year", "month"))


# Final Table
two_wk_dt
one_mt_dt
two_mt_dt
dev.off()
corrplot(cor(two_wk_dt))
two_wk_dt[,LogGrossRev := log(gross_revenue)]

quantile(two_wk_dt$LogGrossRev, probs = seq(0,1,.025))

two_wk_dt = two_wk_dt[LogGrossRev >= quantile(two_wk_dt$LogGrossRev, probs = seq(0,1,.025))[9] &
                        LogGrossRev<= quantile(two_wk_dt$LogGrossRev, probs = seq(0,1,.025))[40]]


# Cleaning final table for training purpose
two_wk_dt_train = two_wk_dt[, c("channel_id", "month", "year", "LogGrossRev") := NULL]
one_mt_dt_train = one_mt_dt[, c("channel_id", "month", "year") := NULL]
two_mt_dt_train = two_mt_dt[, c("channel_id", "month", "year") := NULL]

hist(log(two_wk_dt_train$gross_revenue))
a = quantile(log(two_wk_dt_train$gross_revenue), probs = seq(0,1, .05))

boxplot(log(two_wk_dt_train$gross_revenue))

b = log(two_wk_dt_train$gross_revenue)

rm(data_dec, data_may_to_august, data_sept, data_nov, data_oct,
   full_ds, full_ds_freqChannels, filter_month, Tot_gross_rev, temp); gc()

############################################
# Data Modeling
############################################
# connnection to spark terminal

set.seed(1)
two_wk_fold = createFolds( seq(1:nrow(two_wk_dt_train)), k  = 5)
library(sparklyr)
RMSE = NULL
for (i in seq(1,5)){
  spark_disconnect_all()
  sc = spark_connect(master = "local")
  test = two_wk_dt_train[ two_wk_fold[[i]],]
  train = two_wk_dt_train[-two_wk_fold[[i]],]
  train.sc = copy_to(sc, train)
  test.sc = copy_to(sc, test)
  rf_two_wk_sc = train.sc %>%
    ml_random_forest(gross_revenue~.,
                     num.trees = 100,
                     type = "regression")
  test.prediction = sdf_predict(rf_two_wk_sc, test.sc) %>%
    collect %>% select(gross_revenue, prediction) # collect is to retransform back to dataframe
  RMSE[i] = sqrt(mean((test.prediction$gross_revenue - test.prediction$prediction)^2))
  spark_disconnect_all()
  print (i)
}
RMSE
# with outlier
# [1]  80.77334  42.28721  50.38947  63.47102 184.62678
# with outlier removed
# [1] 1.548842 1.591499 1.520867 1.559713 1.567574


# Without removing outlier
# RMSE 89.2193 , maxbin = 50
# new fold : 
# RMSE = 42.10654
# new fold
# RMSE 50.22131
# RMSE  : 63.76873

# for regression tree, the number of predictors per tree is #totalpredictor/3 -> 15 pred/3 = 5
# for two_wk_dt_train dataset: 

# creating 10-Fold training, for cross validation purpose

# without Spark ~ dont run this function or the rstudio will freeze
start = Sys.time()
set.seed(1)
two_wk_fold = createFolds( seq(1:nrow(two_wk_dt_train)), k  = 5)
rmse = vector()
for (i in seq(1:5)){
  test_temp = two_wk_dt_train[two_wk_fold[[i]]]
  train_temp = two_wk_dt_train[-two_wk_fold[[i]]]
  set.seed(1)
  # rf_two_wk = randomForest(gross_revenue~.,
                           data = train_temp,
                           mtry = round((dim(two_wk_dt_train)[2]-1)/3),
                           ntree = 200,
                           importance = TRUE)
  prediction = predict(rf_two_wk, test_temp)
  rmse[i] = sqrt(mean((prediction - test_temp[,gross_revenue])^2))
}
end = Sys.time()
duration = end - start
duration

# Performing XGBOOST  
library(xgboost)
start = Sys.time()
RMSE_xgb = NULL
for(i in seq(1,5)){
  two_wk_dt_train[two_wk_fold[[i]]]
  test.y = two_wk_dt_train[two_wk_fold[[i]]]$gross_revenue
  test.x = data.matrix( two_wk_dt_train[two_wk_fold[[i]]][, -c("gross_revenue")])
  train.y = two_wk_dt_train[-two_wk_fold[[i]]]$gross_revenue
  train.x = data.matrix( two_wk_dt_train[-two_wk_fold[[i]]][, -c("gross_revenue")])
  dtrain = xgb.DMatrix(data = train.x, label = train.y)
  dtest = xgb.DMatrix(data = test.x, label = test.y)
  watchlist =  list(train=dtrain, test=dtest)
  set.seed(1)
  bst = xgb.train(data = dtrain, 
                  objective = "reg:linear",
                  max.dept = 5, # depth of the tree
                  nthread = 2, # 2 core of CPU used
                  nround  = 20, 
                  eta = 1, 
                  watchlist =  watchlist)
  best_iter = grep(min(bst$evaluation_log$test_rmse),bst$evaluation_log$test_rmse )
  set.seed(1)
  xgb.model = xgboost(data = dtrain, 
                      objective = "reg:linear",
                      max.dept = 5, # depth of the tree
                      nthread = 2, # 2 core of CPU used
                      nround  = best_iter, 
                      eta = 1, 
                      watchlist)
  xgb.predict = predict(xgb.model, dtest)
  RMSE_xgb[i] = sqrt(mean((xgb.predict - test.y)^2))
}
end =  Sys.time()
duration.xgb = end - start
RMSE_xgb
# with outlier
# [1] 119.46510  39.39967  32.82281  42.11602 130.06458
# without outlier
# [1] 1.414146 1.455521 1.387853 1.420347 1.432977



# Random Forest vs XGB

# random forest
# with outlier
# [1]  80.77334  42.28721  50.38947  63.47102 184.62678
# with outlier
# [1] 1.548842 1.591499 1.520867 1.559713 1.567574

# XGB
# with outlier
# [1] 119.46510  39.39967  32.82281  42.11602 130.06458
# without outlier
# [1] 1.414146 1.455521 1.387853 1.420347 1.432977






