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





















s1 = Sys.time()
rf_two_wk  = randomForest(gross_revenue~ . , 
                          data = two_wk_dt_train[1:10000], 
                          mtry = round((dim(two_wk_dt_train)[2]-1)/3), 
                          ntree = 200)
s2 = Sys.time()




class(two_wk_fold)
head(two_wk_fold,10)

s1 = Sys.time()
rf_two_wk  = randomForest(gross_revenue~ . , 
                          data = two_wk_dt_train[1:10000], 
                          mtry = round((dim(two_wk_dt_train)[2]-1)/3), 
                          ntree = 200)
predict()
s2 = Sys.time()
duration_rf_two_wk = s2-s1

hist(two_wk_dt_train$gross_revenue)
boxplot(two_wk_dt_train$gross_revenue)

hist(log(two_wk_dt_train$gross_revenue))
boxplot(log(two_wk_dt_train$gross_revenue))


s1 = Sys.time()
rf_one_mt  = randomForest(gross_revenue~ . , 
                          data = one_mt_dt_train[1:100], 
                          mtry = 5, 
                          ntree = 500)
s2 = Sys.time()
duration_rf_one_mt = s2-s1
a =  two_wk_dt[1:100]
View(a)
s1 = Sys.time()
rf_two_mt  = randomForest(gross_revenue~ . , 
                          data = two_mt_dt_train, 
                          mtry = 5, 
                          ntree = 500)
s2 = Sys.time()
duration_rf_two_mt = s1-s2

# Reorganizing tesing data
channel_interested = 4686
duration = 14 # in days

jan_data[,c("num_days_in_month", "net_sub_gain", "y_m") := 
           list(days_in_month(datestamp), (subscribers_gained + subscribers_lost), paste(year(datestamp), month(datestamp), sep = "-"))]
jan_data[,c("subscribers_gained", "subscribers_lost", "favorites_added", "favorites_removed", "uniques",
            "youtube_id", "name", "title") := NULL]
setkeyv(jan_data,  c("channel_id","year", "month"))

# for 2 wks aggregation
testing = jan_data[channel_id ==channel_interested ]
temp = c(rep("less than or equal", times = duration),rep("more", times = (days_in_month(testing$datestamp[1]) - duration))) 
testing = testing[,limitdays := temp ]
testing_channel = testing[limitdays == "less than or equal", 
                          list(views = sum(views), likes = sum(likes), dislikes = sum(dislikes), comments  = sum(comments),
                               monetized_playbacks = sum(monetized_playbacks), impressions = sum(impressions),
                               shares = sum(shares), estimated_minutes_watched = sum(estimated_minutes_watched),
                               average_view_duration = mean(average_view_duration),average_view_percentage = mean(average_view_percentage),
                               annotation_click_through_rate = sum(annotation_click_through_rate), annotation_close_rate = sum(annotation_close_rate),
                               annotation_clicks = sum(annotation_clicks), annotation_impressions = sum(annotation_impressions),
                               net_sub_gain = sum(net_sub_gain), year = mean(year), month = min(month))]
testing_channel = testing_channel[, c("year", "month") := NULL]


prediction.rf = predict(rf_two_wk, testing_channel)
prediction.xgb = 
y_val = jan_data[channel_id == channel_interested, list(Tot_gross_rev = sum(gross_revenue)) ]
prediction
y_val


gross_rev_df =  jan_data[channel_id ==channel_interested ]
temp = c(rep("less than or equal", times = duration),rep("more", times = (days_in_month(testing$datestamp[1]) - duration))) 
gross_rev_df = gross_rev_df[,limitdays := temp ]
gross_rev_df = gross_rev_df[limitdays =="less than or equal", list(gross_revenue) ]


plt.df = data.frame(x_axis = c(seq(1:14), 30),
                    y_axis = c(cumsum(gross_rev_df)$gross_revenue, prediction))

plot(x = plt.df$x_axis,
     y = plt.df$y_axis,
     type = "b")






# for 1 month aggregation
duration = 1 # in month
testing = jan_data[channel_id ==channel_interested ]
temp = rep("less than or equal", times = (duration*days_in_month(testing$datestamp[1])))
testing = testing[,limitdays := temp ]
testing_channel = testing[limitdays == "less than or equal", 
                          list(views = sum(views), likes = sum(likes), dislikes = sum(dislikes), comments  = sum(comments),
                               monetized_playbacks = sum(monetized_playbacks), impressions = sum(impressions),
                               shares = sum(shares), estimated_minutes_watched = sum(estimated_minutes_watched),
                               average_view_duration = mean(average_view_duration),average_view_percentage = mean(average_view_percentage),
                               annotation_click_through_rate = sum(annotation_click_through_rate), annotation_close_rate = sum(annotation_close_rate),
                               annotation_clicks = sum(annotation_clicks), annotation_impressions = sum(annotation_impressions),
                               net_sub_gain = sum(net_sub_gain), year = mean(year), month = min(month))]
testing_channel = testing_channel[, c("year", "month") := NULL]
predict(rf_one_mt, testing_channel)       











set.seed(1)
sample_id = sample(unique(full_ds_freqChannels$channel_id), size = (.3*(length(unique(full_ds_freqChannels$channel_id)))), replace = F)
length(sample_id)
sample_ds =  as.data.table(full_ds_freqChannels %>% filter(channel_id %in% sample_id))

# getting dataframe for 2 weeks period performance and cum.gross.rev end of respective month
two_wk_dt = data.table()
glimpse(two_wk_dt)


# for loop for each month performance
start = Sys.time()
count = 0
a = 0
for(i in unique(sample_ds$month_order)){ #for each month order
  print(i)
  a = 0
  filter_month =  sample_ds[sample_ds$month_order == i,]   #filter by year
  for(j in unique(filter_month$channel_id)){ # for each channel id
    print(a)
    a = a+1
    filter_chn_mth = filter_month[filter_month$channel_id == j,]  # filter by ID then, check if the no missing observation in each channel in that respective month 
    if(nrow(filter_chn_mth) == days_in_month(filter_chn_mth$datestamp[1])){ # if no missing obs in that month
      two_wk_temp =  data.table( channel_id = j , # j is channel id
                                 y_m = i,
                                 t(colSums(filter_chn_mth[1:14, # aggregating the first 2 weeks
                                                          c("views","likes", "dislikes", "comments", "monetized_playbacks", 
                                                            "impressions", "shares", "estimated_minutes_watched",
                                                            "average_view_duration","average_view_percentage", "annotation_click_through_rate",
                                                            "annotation_close_rate","annotation_clicks", "annotation_impressions", 
                                                            "net_sub_gain")])),
                                 gross_revenue = sum(filter_chn_mth$gross_revenue))
      if(count == 0 ){
        two_wk_dt = two_wk_temp
      }
      two_wk_dt = rbindlist(list(two_wk_dt,two_wk_temp))
      count = 1
    } else{
      next # if the channels data is incomplete, skip to the next one
    }
  }
}
end = Sys.time()
duration = start - end






# for(i in unique(full_ds$y_m)){ #for each year-month factor
#   filter_month = full_ds %>% filter(y_m == i) #filter into based on year-month
#   for(j in unique(filter_month$channel_id)){ # for each channel id
#     filter_chn_mth = filter_month %>% filter( channel_id ==j )# check if the no missing observation in each channel in that respective month 
#     if(nrow(filter_chn_mth) == days_in_month(filter_chn_mth$datestamp[1])){ # if no missing obs in that month
#       two_wk_temp =  data.table( channel_id = j , # j is channel id
#                                  y_m = i,
#                                  t(colSums(filter_chn_mth[1:14, # aggregating the first 2 weeks
#                                                         c("views","likes", "dislikes", "comments", "monetized_playbacks", 
#                                                           "impressions", "shares", "estimated_minutes_watched",
#                                                           "average_view_duration","average_view_percentage", "annotation_click_through_rate",
#                                                           "annotation_close_rate","annotation_clicks", "annotation_impressions", 
#                                                           "net_sub_gain")])),
#                                  gross_revenue = sum(filter_chn_mth$gross_revenue))
#       two_wk_dt = data.table(bind_rows(two_wk_dt, two_wk_temp ))
#       } else{
#       next # if the channels data is incomplete, skip to the next one
#     }
#   }
# }
# end = Sys.time()
# duration = start - end

# Fix it, doesnt work whehn 
# a = full_ds[1:100000,]

a = full_ds[channel_id == 2,]
one_mt_dt = data.table()
for(i in unique(a$month_order)){ # for each month group
  print(i)
  filter_2_months = full_ds[month_order == i | month_order == i+1,]
  for(j in unique(a$channel_id)){
    print(j)
    filter_2_chn_mth = filter_2_months[channel_id == j,]
    if(nrow(filter_2_chn_mth) == (days_in_month(filter_2_chn_mth$datestamp[1]) + days_in_month(filter_2_chn_mth$datestamp[nrow(filter_2_chn_mth)]))){ # if no missing obs in that month
      one_month_temp =  data.table( channel_id = j, # j is channel id
                                    y_m = i,
                                    t(colSums(filter_2_chn_mth[1:14, # aggregating the first 2 weeks
                                                               c("views","likes", "dislikes", "comments", "monetized_playbacks",
                                                                 "impressions", "shares", "estimated_minutes_watched",
                                                                 "average_view_duration","average_view_percentage", "annotation_click_through_rate",
                                                                 "annotation_close_rate","annotation_clicks", "annotation_impressions",
                                                                 "net_sub_gain")])),
                                    gross_revenue = sum(filter_2_chn_mth$gross_revenue))
      one_mt_dt = data.table(bind_rows(one_mt_dt, one_month_temp))
    } else{
      next # if the channels data is incomplete, skip to the next one
    }
  }
}
one_mt_dt






for(i in unique(a$month_order)){ # for each month froup
  print(i)
}


?head(two_wk_dt)
?ifelse
?if







b = as.vector(b)
class(b)
str(b)

glimpse(full_ds)








setkeyv(jan_data,c("channel_id", "datestamp"))

# head(full_ds_freqChannels[order(-full_ds_freqChannels$impression_based_cpm),],10)

# training set:
full_ds_freqChannels = as.data.table(filter(full_ds, channel_id %in% channels_intersect))
# check if the if every channels tracked completely
temp2 = full_ds_freqChannels%>% group_by(channel_id) %>% summarise(counts = n()) %>% arrange(desc(-counts)) # its not!!
# need more filter process to refine the best channels to be analyzed
temp3 = filter(temp2, counts  == days_tracked) # days tracked is 245 days in this case 
temp_table = sort(table(full_ds_freqChannels$channel_id))
length(temp_table != 245) # 39783 channels are tracked inconsistent
temp = filter(full_ds_freqChannels, channel_id == 4660) # turned out that some channels like channel ID :4660 is not being fully monitored in the whole 8 months
temp4 = filter(full_ds_freqChannels, channel_id == 94396) # nov 7 - nov 12 data for this particular channel is not recorded
full_ds_freqChannels = as.data.table(filter(full_ds_freqChannels, channel_id %in% temp3$channel_id))
# dim(full_ds_freqChannels) 8742335 rows remaining, 8742335/245 = 35683 unique channels are monitored consistently
# length(unique(full_ds_freqChannels$channel_id))



# there are 245days/7days_per_week = 35 observed weeks in total
# create variable that keep in track which which weeks that those observation belongs
week_counter = rep((rep(1:(days_tracked/7), each = 7)), times = length(unique(full_ds_freqChannels$channel_id))) 

# also, we need to create the first 10 weeks variable, to keep in track their performance for the first 10 weeks
week10_tracker  = rep(c(rep("<= 10 weeks", each = 10*7), rep("> 10 weeks", each = ((days_tracked/7) - 10)*7)), times = length(unique(full_ds_freqChannels$channel_id)))

full_ds_freqChannels = full_ds_freqChannels[, c("week_counter", "net_sub_gain", "week10_tracker") := list(week_counter, (subscribers_gained + subscribers_lost), week10_tracker)]
# if the average CPM performance for the first 10 weeks are low (<=0.1), we are going to exclude these respective channels 
nonzero_cpm_avg_first10wk = data.frame(full_ds_freqChannels %>% filter(week10_tracker == "<= 10 weeks") %>% group_by(channel_id) %>% 
                                         summarise(cpm_avg = mean(impression_based_cpm))) %>% filter(cpm_avg > .10)
full_ds_freqChannels = as.data.table(filter(full_ds_freqChannels, channel_id %in% nonzero_cpm_avg_first10wk$channel_id))

# training + validation set:
full_ds_freqChannels_complete = rbind(select(full_ds_freqChannels, channel_id,datestamp, impression_based_cpm, views ),select(jan_data, channel_id,datestamp, impression_based_cpm , views) )
setkeyv(full_ds_freqChannels_complete,c("channel_id", "datestamp") )
# write.csv(full_ds_freqChannels_complete, "full_ds_freqChannels_2.csv")

rm(data_may_to_august, data_sept, data_oct, data_nov, data_dec,
   full_ds, temp, temp2, temp3, temp4, temp_table, channels_intersect, jan_data, nonzero_cpm_avg_first10wk, week_counter, week10_tracker); gc()

temp = c("lead1","lead2","lead3")
temp2  = paste("impr_based_cpm_", temp, sep = "")
full_ds_freqChannels[,  (temp2):= shift(.SD, n = 1:3, fill = NA, type = "lead"), .SDcols = grep("impression_based_cpm", colnames(full_ds_freqChannels)), by = channel_id]
rm(temp, temp2); gc()

# unique(full_ds_freqChannels$channel_id)[1:10]
# [1]  2  6  8 15 22 23 31 40 45 52

# getting average CPM during these 8 months
full_ds_8months_impr_based_cpm = full_ds_freqChannels %>% 
  group_by(channel_id )%>% 
  summarise(eight_months_average_cpm= mean(impression_based_cpm), avgDailyViews = mean(views) ) %>%
  arrange(desc(eight_months_average_cpm))
full_ds_8months_impr_based_cpm = as.data.table(full_ds_8months_impr_based_cpm)

# Getting weekly performance of each channel
full_ds_weekly_impr_based_cpm = full_ds_freqChannels %>% 
  group_by(channel_id, week_counter )%>% 
  summarise(weekly_avg_impr_based_cpm = mean(impression_based_cpm))

channel_list  = unique(full_ds_weekly_impr_based_cpm$channel_id)
# [1]  2  6  8 15 22 23 31 40 45 52
# length(channel_list) # 29841 to be analyzed


############################################
# Analysis
############################################

# Finding the growth slope using OLS
OLS_table = data.frame()
temp_date = seq(1:35)
row = 1
for(i in channel_list){
  lm_model_univariate = lm(formula  = (full_ds_weekly_impr_based_cpm[full_ds_weekly_impr_based_cpm$channel_id == i, ][,3]$weekly_avg_impr_based_cpm) ~ temp_date)
  OLS_table[row,1] =  i
  OLS_table[row,2] =  lm_model_univariate$coefficients[1]
  OLS_table[row,3] =  lm_model_univariate$coefficients[2]
  OLS_table[row,4] =  summary(lm_model_univariate)$adj.r.squared
  OLS_table[row,5] =  summary(lm_model_univariate)$coefficients[2,4]
  row = row + 1
}
colnames(OLS_table) = c("channel_id", "intercept", "slope", "adj_Rsq","p_val")
OLS_table_CPM_avg  = inner_join(OLS_table, full_ds_8months_impr_based_cpm, by = "channel_id")
head(OLS_table_CPM_avg[order(-OLS_table_CPM_avg$eight_months_average_cpm),])

# OLS_table

# top 20 for more inspection
# We focus on both growth CPM and average CPM
# GROWTH
top20_growth_simplified = head(filter(OLS_table_CPM_avg[order(-OLS_table_CPM_avg$eight_months_average_cpm),], 
                                      slope > 0, # we're finding channels have positive trend of CPM 
                                      p_val< .05, # we're looking model that statistically significant
                                      adj_Rsq > 0.3, # we're looking at low variability of growth (relativelye non-volatile)
                                      avgDailyViews > 3000) 
                               ,20) # getting the top 20 channel in the market
top20_growth_simplified
top20_growth_complete = as.data.table(filter(full_ds_freqChannels, channel_id %in% top20_growth_simplified$channel_id))
setkeyv(top20_growth_complete, c("channel_id", "datestamp"))

# AVERAGE
top20_avgCPM_simplified = head(filter(OLS_table_CPM_avg[order(-OLS_table_CPM_avg$eight_months_average_cpm),], 
                                      slope > 0) # we're looking at low variability of growth (relativelye non-volatile)
                               ,20) # getting the top 20 channel in the market
top20_avgCPM_complete =  as.data.table(filter(full_ds_freqChannels, channel_id %in% top20_avgCPM_simplified$channel_id))
rm(lm_model_univariate, channel_list, OLS_table, i );gc()

# Growth analysis
count = 1
for(i in top20_growth_simplified$channel_id){
  data_temp = filter(top20_growth_complete, channel_id == i)
  lm_lead_0 = lm(impression_based_cpm ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain,
                 data = data_temp)
  lm_lead_1 = lm(impr_based_cpm_lead1 ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain,
                 data = data_temp)
  lm_lead_2 = lm(impr_based_cpm_lead2 ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain,
                 data = data_temp)
  lm_lead_3 = lm(impr_based_cpm_lead3 ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain,
                 data = data_temp)
  pf_lead_0 = pf(summary(lm_lead_0)$fstatistic[1], summary(lm_lead_0)$fstatistic[2], summary(lm_lead_0)$fstatistic[3], lower.tail = F)
  pf_lead_1 = pf(summary(lm_lead_1)$fstatistic[1], summary(lm_lead_1)$fstatistic[2], summary(lm_lead_1)$fstatistic[3], lower.tail = F)
  pf_lead_2 = pf(summary(lm_lead_2)$fstatistic[1], summary(lm_lead_2)$fstatistic[2], summary(lm_lead_2)$fstatistic[3], lower.tail = F)
  pf_lead_3 = pf(summary(lm_lead_3)$fstatistic[1], summary(lm_lead_0)$fstatistic[2], summary(lm_lead_3)$fstatistic[3], lower.tail = F)
  temp = data.table(pval = c(pf_lead_0, pf_lead_1, pf_lead_2, pf_lead_3),
                    adj.r.sq = c(summary(lm_lead_0)$adj.r.squared,summary(lm_lead_1)$adj.r.squared,summary(lm_lead_2)$adj.r.squared,summary(lm_lead_3)$adj.r.squared))
  best_model_index = temp %>% na.omit() %>% filter(pval < 0.05) %>% summarise(max(adj.r.sq)) %>% grep(temp$adj.r.sq)
  if(best_model_index == 1){
    top20_growth_simplified$lead_effect[count] = "0 days"
    top20_growth_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_0)$coefficient)[summary(lm_lead_0)$coefficient[,4]<.05], collapse = ", ")
    if (length(rownames(summary(lm_lead_0)$coefficient)[summary(lm_lead_0)$coefficient[,4]<.05] ) == 0){
      top20_growth_simplified$significant_variables[count] = "**none of variables are significant**"
    }
  }
  if(best_model_index == 2){
    top20_growth_simplified$lead_effect[count] = "1 days"
    top20_growth_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_1)$coefficient)[summary(lm_lead_1)$coefficient[,4]<.05], collapse = ", ")
    if (length(rownames(summary(lm_lead_1)$coefficient)[summary(lm_lead_1)$coefficient[,4]<.05] ) == 0){
      top20_growth_simplified$significant_variables[count] = "**none of variables are significant**"
    }
  }
  if(best_model_index == 3){
    top20_growth_simplified$lead_effect[count] = "2 days"
    top20_growth_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_2)$coefficient)[summary(lm_lead_2)$coefficient[,4]<.05], collapse = ", ")
    if (length(rownames(summary(lm_lead_2)$coefficient)[summary(lm_lead_2)$coefficient[,4]<.05] ) == 0){
      top20_growth_simplified$significant_variables[count] = "**none of variables are significant**"
    }
  }
  if(best_model_index == 4){
    top20_growth_simplified$lead_effect[count] = "3 days"
    top20_growth_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_3)$coefficient)[summary(lm_lead_3)$coefficient[,4]<.05], collapse = ", ")
    if (length(rownames(summary(lm_lead_3)$coefficient)[summary(lm_lead_3)$coefficient[,4]<.05] ) == 0){
      top20_growth_simplified$significant_variables[count] = "**none of variables are significant**"
    }
  }
  count = count +1
}
top20_growth_simplified # ranked by the Rsq
top20_growth_simplified %>% arrange(lead_effect) %>% select(lead_effect, channel_id, significant_variables)
# paste(top20_growth_simplified$channel_id ,  collapse = ", ")
# end = Sys.time()
# duration  =   end - start 
# duration

count = 1
for(i in top20_avgCPM_simplified$channel_id){
  data_temp = filter(top20_avgCPM_complete, channel_id == i)
  lm_lead_0 = lm(impression_based_cpm ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain, 
                 data = data_temp)
  lm_lead_1 = lm(impr_based_cpm_lead1 ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain, 
                 data = data_temp)
  lm_lead_2 = lm(impr_based_cpm_lead2 ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain, 
                 data = data_temp)
  lm_lead_3 = lm(impr_based_cpm_lead3 ~ views + likes+dislikes + comments + monetized_playbacks +
                   impressions + estimated_minutes_watched + average_view_duration + average_view_percentage +
                   annotation_click_through_rate + annotation_close_rate + uniques + annotation_impressions + net_sub_gain, 
                 data = data_temp)
  pf_lead_0 = pf(summary(lm_lead_0)$fstatistic[1], summary(lm_lead_0)$fstatistic[2], summary(lm_lead_0)$fstatistic[3], lower.tail = F)
  pf_lead_1 = pf(summary(lm_lead_1)$fstatistic[1], summary(lm_lead_1)$fstatistic[2], summary(lm_lead_1)$fstatistic[3], lower.tail = F)
  pf_lead_2 = pf(summary(lm_lead_2)$fstatistic[1], summary(lm_lead_2)$fstatistic[2], summary(lm_lead_2)$fstatistic[3], lower.tail = F)
  pf_lead_3 = pf(summary(lm_lead_3)$fstatistic[1], summary(lm_lead_0)$fstatistic[2], summary(lm_lead_3)$fstatistic[3], lower.tail = F)
  temp = data.table(pval = c(pf_lead_0, pf_lead_1, pf_lead_2, pf_lead_3), 
                    adj.r.sq = c(summary(lm_lead_0)$adj.r.squared,summary(lm_lead_1)$adj.r.squared,summary(lm_lead_2)$adj.r.squared,summary(lm_lead_3)$adj.r.squared))
  best_model_index = temp %>% na.omit() %>% filter(pval < 0.05) %>% summarise(max(adj.r.sq)) %>% grep(temp$adj.r.sq)
  if(length(best_model_index) == 0){
    top20_avgCPM_simplified$lead_effect[count] = "NA - No model"
    top20_avgCPM_simplified$significant_variables[count] = "The growth is too clustered and couldnt be modeled"
  } else{
    if(best_model_index == 1){ 
      top20_avgCPM_simplified$lead_effect[count] = "0 days"
      top20_avgCPM_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_0)$coefficient)[summary(lm_lead_0)$coefficient[,4]<.05], collapse = ", ")
      if (length(rownames(summary(lm_lead_0)$coefficient)[summary(lm_lead_0)$coefficient[,4]<.05] ) == 0){
        top20_avgCPM_simplified$significant_variables[count] = "**none of variables are significant**"
      }
    }
    if(best_model_index == 2){
      top20_avgCPM_simplified$lead_effect[count] = "1 days"
      top20_avgCPM_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_1)$coefficient)[summary(lm_lead_1)$coefficient[,4]<.05], collapse = ", ")
      if (length(rownames(summary(lm_lead_1)$coefficient)[summary(lm_lead_1)$coefficient[,4]<.05] ) == 0){
        top20_avgCPM_simplified$significant_variables[count] = "**none of variables are significant**"
      }
    }
    if(best_model_index == 3){
      top20_avgCPM_simplified$lead_effect[count] = "2 days"
      top20_avgCPM_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_2)$coefficient)[summary(lm_lead_2)$coefficient[,4]<.05], collapse = ", ")
      if (length(rownames(summary(lm_lead_2)$coefficient)[summary(lm_lead_2)$coefficient[,4]<.05] ) == 0){
        top20_avgCPM_simplified$significant_variables[count] = "**none of variables are significant**"
      }
    }
    if(best_model_index == 4){
      top20_avgCPM_simplified$lead_effect[count] = "3 days"
      top20_avgCPM_simplified$significant_variables[count] = paste(rownames(summary(lm_lead_3)$coefficient)[summary(lm_lead_3)$coefficient[,4]<.05], collapse = ", ")
      if (length(rownames(summary(lm_lead_3)$coefficient)[summary(lm_lead_3)$coefficient[,4]<.05] ) == 0){
        top20_avgCPM_simplified$significant_variables[count] = "**none of variables are significant**"
      }
    }
  }
  count = count +1
}

top20_avgCPM_simplified
top20_growth_simplified
# write.csv(top20_growth_simplified, file = "top20_growth_simplified.csv")
# write.csv(top20_avgCPM_simplified, file = "top20_avgCPM_simplified.csv")
full_ds_weekly_impr_based_cpm
# write.csv(full_ds_weekly_impr_based_cpm, file = "full_ds_weekly_impr_based_cpm.csv") # only training 
# write.csv(full_ds_freqChannels, file = "full_ds_freqChannels.csv") # only training
# write.csv(full_ds_freqChannels_complete, file = "full_ds_freqChannels_complete") # this contains training and validation



top20_avgCPM_simplified$channel_id
top20_growth_simplified$channel_id

channel_check = 103423
# weekly performance check for exploratory analysis
channel_check_weekly_performance = full_ds_weekly_impr_based_cpm %>% filter(channel_id == channel_check) 
full_ds_weekly_impr_based_cpm %>% filter(channel_id == channel_check) %>% with(plot(weekly_avg_impr_based_cpm, xlab = "week index", ylab = "weekly average CPM in $")) 
abline(lm(formula  = (full_ds_weekly_impr_based_cpm[full_ds_weekly_impr_based_cpm$channel_id == channel_check, ]$weekly_avg_impr_based_cpm) ~ temp_date))
r2 <- summary(lm(formula  = (full_ds_weekly_impr_based_cpm[full_ds_weekly_impr_based_cpm$channel_id == channel_check, ]$weekly_avg_impr_based_cpm) ~ temp_date))$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 2)))
legend("topleft", legend = mylabel )



# Finding optimal differencing
opt_diff_daily = ndiffs(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm])

# Checking for staitionarity
library(tseries)
adf_result = adf.test(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm],alternative="stationary") 
# Augmented Dickey Fuller test
# Ho = data is Non Stationary
# H1 = data is stationary
# if p-value <0.05, then data is stationary, and vice versa

kpss_result = kpss.test(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm])
# Kwiatkowski–Phillips–Schmidt–Shin (KPSS) tests
# Ho = data is stationary
# H1 = data is Non stationary
# if p-value <0.05, then data is Non Stationary, and vice versa

if(adf_result$p.value > 0.05 || kpss_result$p.value < 0.05){ # if any of the test conclude data is nonstationary
  adf_result_new = adf.test(diff(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm], opt_diff_daily), alternative = "stationary")
  kpss_result_new = kpss.test(diff(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm], opt_diff_daily))
}

if(adf_result_new$p.value > 0.05 || kpss_result_new$p.value < 0.05){
  print("data is too volatile, forecasting may not be accurate")
}

# Forecasting Daily Data
# Model Candidate for Daily data
M1_daily = Arima(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm],
                 order = c(0,opt_diff_daily,0))
M2_daily = Arima(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm],
                 order = c(0,opt_diff_daily,1))
M3_daily = Arima(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm],
                 order = c(1,opt_diff_daily,0))
M4_daily = Arima(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm],
                 order = c(1,opt_diff_daily,1))
# auto arima model with daily data
M5_daily_auto = auto.arima(full_ds_freqChannels[full_ds_freqChannels$channel_id ==channel_check, impression_based_cpm])

best_arima_daily_index = grep( min(M1_daily$aic, M2_daily$aic, M3_daily$aic, M4_daily$aic, M5_daily_auto$aic),
                               c(M1_daily$aic, M2_daily$aic, M3_daily$aic, M4_daily$aic, M5_daily_auto$aic))
best_arima_daily = NULL
if( best_arima_daily_index[1] == 1){ best_arima_daily = M1_daily} # if one of the model have the same AIC, we pick the first one (doesnt matter)
if( best_arima_daily_index[1] == 2){ best_arima_daily = M2_daily}
if( best_arima_daily_index[1] == 3){ best_arima_daily = M3_daily}
if( best_arima_daily_index[1] == 4){ best_arima_daily = M4_daily}
if( best_arima_daily_index[1] == 5){ best_arima_daily = M5_daily_auto}

summary(best_arima_daily)
plot(forecast(best_arima_daily, h = 31), lwd = 2, main = "Forecasting the CPM growth daily\n for the next 30 days", xlab = "days", ylab = "impression based CPM")
lines(fitted(best_arima_daily), col = "red", lwd = 2)
# ma.trailing  = rollmean(top20_growth_complete[top20_growth_complete$channel_id ==channel_check, impression_based_cpm],k  = 4, align = "center")
# length(ma.trailing)
# length(top20_growth_complete[top20_growth_complete$channel_id ==channel_check, impression_based_cpm])
# plot(top20_growth_complete[top20_growth_complete$channel_id ==96363, impression_based_cpm], type = "p")
# lines(rollmean(top20_growth_complete[top20_growth_complete$channel_id ==channel_check, impression_based_cpm], k  = 7, align = "right"),
#       x = c(7:245), type = "l", lwd = 3, col = "green")
best_arima_daily_RMSE=sqrt(best_arima_daily$sigma2)
lines(fitted(forecast(best_arima_daily,h=31))-rep(2*best_arima_daily_RMSE,length(fitted(forecast(best_arima_daily,h=31)))),col=3)
lines(fitted(forecast(best_arima_daily,h=31))+rep(2*best_arima_daily_RMSE,length(fitted(forecast(best_arima_daily,h=31)))),col=3)

# Forecasting Weekly Data
# Finding optimal differencing
opt_diff_weekly = ndiffs(channel_check_weekly_performance$weekly_avg_impr_based_cpm)
# Model Candidate for Weekly data
M1_weekly = Arima(channel_check_weekly_performance$weekly_avg_impr_based_cpm,
                  order = c(0,opt_diff_weekly,0))
M2_weekly = Arima(channel_check_weekly_performance$weekly_avg_impr_based_cpm,
                  order = c(0,opt_diff_weekly,1))
M3_weekly = Arima(channel_check_weekly_performance$weekly_avg_impr_based_cpm,
                  order = c(1,opt_diff_weekly,0))
M4_weekly = Arima(channel_check_weekly_performance$weekly_avg_impr_based_cpm,
                  order = c(1,opt_diff_weekly,1))
# auto arima model with weekly data
M5_weekly_auto = auto.arima(channel_check_weekly_performance$weekly_avg_impr_based_cpm)

best_arima_weekly_index = grep( min(M1_weekly$aic, M2_weekly$aic, M3_weekly$aic, M4_weekly$aic, M5_weekly_auto$aic),
                                c(M1_weekly$aic, M2_weekly$aic, M3_weekly$aic, M4_weekly$aic, M5_weekly_auto$aic))
best_arima_weekly = NULL
if( best_arima_weekly_index[1] == 1){ best_arima_weekly = M1_weekly}
if( best_arima_weekly_index[1] == 2){ best_arima_weekly = M2_weekly}
if( best_arima_weekly_index[1] == 3){ best_arima_weekly = M3_weekly}
if( best_arima_weekly_index[1] == 4){ best_arima_weekly = M4_weekly}
if( best_arima_weekly_index[1] == 5){ best_arima_weekly = M5_weekly_auto}


# arima model with weekly data
summary(best_arima_weekly)
plot(forecast(best_arima_weekly, h = 4), main = "Forecasting the CPM growth week\n for the next 4 weeks", xlab = "week", ylab = "impression based CPM")
lines(fitted(best_arima_weekly), lwd = 2, col = "red")
best_arima_weekly_RMSE = sqrt(best_arima_weekly$sigma2)
lines(fitted(forecast(best_arima_weekly,h=4))-rep(2*best_arima_weekly_RMSE,length(fitted(forecast(best_arima_weekly,h=4)))),col=3)
lines(fitted(forecast(best_arima_weekly,h=4))+rep(2*best_arima_weekly_RMSE,length(fitted(forecast(best_arima_weekly,h=4)))),col=3)










#### END
# 
# jan = fread("danniel_jan_2017.csv")
# data_jan = select(jan,channel_id,impression_based_cpm,youtube_id,name,title, year, month, day)
# head(data_jan, 25)
# unique(data_jan$channel_id)
# 
# 
# 
# 
# 
# 
# 
# 
# channel_focus = 2
# temp = seq(1:35)
# tsdisplay(full_ds_weekly_impr_based_cpm[full_ds_weekly_impr_based_cpm$channel_id == channel_focus, ][,3])
# lm_model = lm(formula  = (full_ds_weekly_impr_based_cpm[full_ds_weekly_impr_based_cpm$channel_id == channel_focus, ][,3]$weekly_avg_impr_based_cpm) ~ temp)
# summary(lm_model)
# 
# lm_model$coefficients[2]
# lm_model$residuals
# summary(lm_model)$adj.r.squared
# z = summary(lm_model)
# z$coefficients[2,4]
# str(z)
# str(lm_model)
# 
# plot(resid(lm_model))
# 
# plot(x = temp, y = log(full_ds_weekly_impr_based_cpm[full_ds_weekly_impr_based_cpm$channel_id == channel_focus, ][,3]$weekly_avg_impr_based_cpm) ))
# abline(lm_model)
# 
# 
# 
# a = data.table(col1 = c(10,11,12), col2 = c(5,6,7))
# b = c(10,12)
# 
# filter(a, col1 %in% b)
# 
# a$col1 %in% b
# 
# channel_id_2 = filter(full_ds, channel_id ==2)
# par(mfrow = c(4,1))
# par(mfrow = c(1,1))
# 
# plot(channel_id_2$impression_based_cpm*30, type = "l")
# lines(channel_id_2$views , col = "blue", type = "l")
# lines(channel_id_2$playback_based_cpm  + 30, col = "red", type = "l")
# lines(channel_id_2$monetized_playbacks , col = "green", type = "l")
# 
# 
# a = channel_id_2 %>% group_by()
# 
# 
# tbl_df(channel_id_2)
# 
# 
# tail(channel_id_2)
# 
# adf.test(channel_id_2$impression_based_cpm)
# auto.arima(channel_id_2$impression_based_cpm)
# plot(channel_id_2$impression_based_cpm, type = "l")
# lines(fitted(auto.arima(channel_id_2$impression_based_cpm)), col = "red")
# 
# plot(forecast(auto.arima(channel_id_2$impression_based_cpm)))
# 
# channel_id_2.ts = ts(channel_id_2$impression_based_cpm, frequency = 30, start = c(2016,9,1))
# channel_id_2.ts
# fit = tbats(channel_id_2.ts)
# 
# length(channel_id_2.ts)
# 
# plot(channel_id_2.ts)
# tsdisplay(channel_id_2.ts)
# tsdisplay(diff(channel_id_2.ts,7))
# fit = tslm(channel_id_2.ts ~ trend )
# summary(fit)
# plot(forecast(fit))
# 
# 
# pacf(channel_id_2$impression_based_cpm, lag.max = 60)
# 
# glimpse(data_sept)
# tbl_df(data_sept)
# head(data_sept)
# 
# data_sept_ordered = data_sept[order(channel_id),]
# head(data_sept_ordered,32)
# head(data_sept)
# sum(data_sept_ordered$channel_id ==2) # represent 30 days in sept
# length(unique(data_sept_ordered$channel_id)) # 42097 channel ids are observed in sept
# length(unique(data_oct$channel_id)) # 42769 channel ids are observed in sept
# # 42769 - 42097 =  672
# length(intersect(unique(data_sept$channel_id), unique(data_oct$channel_id))) # 41671 channel ids are observed in both months, so not all channels observed are being tracked consistently 
# 
# # doesnt work
# # length(intersect(unique(data_sept$channel_id), unique(data_oct$channel_id)), unique(data_nov$channel_id), unique(data_dec$channel_id)))
# 
# length(Reduce(intersect, list(unique(data_sept$channel_id), unique(data_oct$channel_id), unique(data_nov$channel_id), unique(data_dec$channel_id))))
# # only 41051 channel id are consistently observed in these 4 months
# 
# colnames(data_sept)
# # [1] "id"                           "channel_id"                    "views"                         "likes"                        
# # [5] "dislikes"                      "comments"                      "monetized_playbacks"           "impressions"                  
# # [9] "gross_revenue"                 "subscribers_gained"            "subscribers_lost"              "earnings"                     
# # [13] "year"                          "month"                         "day"                           "datestamp"                    
# # [17] "shares"                        "favorites_added"               "favorites_removed"             "estimated_minutes_watched"    
# # [21] "average_view_duration"         "average_view_percentage"       "annotation_click_through_rate" "annotation_close_rate"        
# # [25] "uniques"                       "playback_based_cpm"            "impression_based_cpm" ads         "annotation_clicks"            
# # [29] "annotation_impressions"        "red_partner_revenue"          
# 
# 
# # Reduce(intersect, list(c(1,2,3), c(3,4,5), c),c(3,7,7))
# # 
# # a <- c(1,3,5,7,9)
# # b <- c(3,6,8,9,10)
# # c <- c(2,3,4,5,7,9)
# 
# # intersect(intersect(a,b),c)
# # Reduce(intersect, list(a,b,c))
# 
# 
# table(data_sept_ordered$channel_id)
# table(data_oct$channel_id)
# 
# # filter for channel id == 2
# data_sept_channel_id_2  =  filter(data_sept_ordered, channel_id ==2)
# # removing column with all NAs
# 
# 
# data_sept_channel_id_2.ts.views  = ?msts(data_sept_channel_id_2$views, frequency = 7, start = )
# data_sept_channel_id_2.ts.views
# 
# tsdisplay(data_sept_channel_id_2.ts.views)
# tsdiag(data_sept_channel_id_2.ts.views)
# 
# class(data_sept_channel_id_2.ts.views)
# ?tbats()
# 
# ?logLik()
# 
# sum(data_oct$channel_id == 2)
# attach(mtcars)
# zz = data_sept
# zz[order(views),]
# 
# mtcars$mpg
# 
# sort(data_sept$channel_id)
# 
# sum(data_dec$channel_id == 2)
# 
# data_sept[channel_id == 2]
# 
# data
# 
# 
# a = c(data_sept$channel_id,data_oct$channel_id,data_nov$channel_id,data_dec$channel_id)
# length(a)
# length(unique(a))
# 
# sort(a)
# 
# z = data.frame(a)
# data_sept[,1, with = FALSE]
# data_sept[1:4, with  = FALSE]
# 
# transform(data_sept)
# 
# sum(duplicated(data_sept$id))
# sum(is.na())
# 
# library(nycflights13)
# 
# flights
# library(data.table)
# sum(is.na(flights))
# ans <- flights[, sum(month), by = "origin" ]
# 
# library(TSA)

