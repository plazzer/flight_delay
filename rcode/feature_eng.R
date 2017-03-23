# Michael Plazzer, Ignite Date Solutions. March 2017
# This self contained R code demonstrates feature engineering, using the well known airline delay data set. It is written
#   in base-r so anybody can run it! (except for the modelling bit)
# R version 3.3.2 
set.seed(201703) 
require("h2o")
#download.file("http://stat-computing.org/dataexpo/2009/1987.csv.bz2","./1987.csv.bz2") 
df_airline<-read.csv("./1987.csv.bz2")#,stringsAsFactors = FALSE) #re-evaluate stingsAsfactors later, since I create factors below
#
# I'll first make a function to call h2o gradient boosting machine; you can run this code whenever you want to run the model
fn_model<-function(df_airline)
{
  library(h2o) #h2o version 3.10.4.1
  h2o.init()
  hex_train<-as.h2o(df_airline[,!names(df_airline) %in% c("Year","Date")])#,"med_day_dep","cnt_day_dep","cnt_day_dep_diff")]) #This is an issue for the Mayan cyclical calendar
  gbm_model<-h2o.gbm(y="ArrDelay",training_frame = hex_train)
  return(gbm_model)
}

# Let's drop some columns to make this more clear (also some of them leak the results e.g. 'ArrTime')
df_airline<-df_airline[,c(15,1,2,3,4,6,8,9,13,17,18,19)] #usually I would use names, but you can get away with it at the start
# remove rows with NULLs
df_airline<-df_airline[ complete.cases(df_airline), ]
# We can now run the model, and see what the accuracy is before feature engineering
fn_model(df_airline) #MSE:  562.3008 RMSE:  23.71288 MAE:  12.811

# straight conversion the following to characters, a month is not a number!
df_airline$DayofMonth<-as.factor(df_airline$DayofMonth)
fn_model(df_airline) # MSE:  532.2477 RMSE:  23.07049 MAE:  12.59049
df_airline$DayOfWeek<-as.factor(df_airline$DayOfWeek)
fn_model(df_airline) RMSE:  # MSE:  533.4763  RMSE:  23.09711 MAE:  12.60946
df_airline$Month<-as.factor(df_airline$Month)
fn_model(df_airline) # MSE:  533.3054 RMSE:  23.09341 MAE:  12.60518

# let's floor these to the hour, there isn't much difference between 13:45 and 13:47, 
# and 1:10 (0110) isn't twice as much as 0:55 (0055). In terms of time.
df_airline$CRSDepTime<-as.factor(round(df_airline$CRSDepTime,-2))
df_airline$CRSArrTime<-as.factor(round(df_airline$CRSArrTime,-2))
fn_model(df_airline) #MSE:  530.878  RMSE:  23.04079  MAE:  12.56472

# let's describe minutes late/month with a quantity, although this will be largely covered by the 'Month' categorical variable
agg_late_monthly_ori <- aggregate(df_airline$ArrDelay,by=list(df_airline$Month,df_airline$Origin),FUN=sum)
names(agg_late_monthly_ori)<-c("Month","Origin","min_late_ori_month")
df_airline<-merge(agg_late_monthly_ori,df_airline);rm(agg_late_monthly_ori)
fn_model(df_airline)
#MSE:  532.1113 RMSE:  23.06754 MAE:  12.58387

# difference on median count dep per airport per day; presumably an airport knows how many flights are departing in advance?
df_airline$Date<-as.Date(paste(df_airline$Year,df_airline$Month,df_airline$DayofMonth,sep="/"))
agg_count_dep_date<-aggregate(df_airline$Distance,by=list(df_airline$Origin,df_airline$Date),FUN=length)
agg_med_dep_date<-aggregate(agg_count_dep_date$x,by=list(agg_count_dep_date$Group.1),FUN=median)
names(agg_count_dep_date)<-c("Origin","Date","cnt_day_dep")
names(agg_med_dep_date)<-c("Origin","med_day_dep")
df_airline<-merge(agg_count_dep_date,df_airline);rm(agg_count_dep_date)
fn_model(df_airline) # MSE:  526.7915  RMSE:  22.95194  MAE:  12.53809
df_airline<-merge(agg_med_dep_date,df_airline);rm(agg_med_dep_date)
fn_model(df_airline) # MSE:  526.7915 RMSE:  22.95194  MAE:  12.53809 # no change
df_airline$cnt_day_dep_diff<-df_airline$med_day_dep-df_airline$cnt_day_dep
fn_model(df_airline) # MSE:  520.2243 RMSE:  22.80843 MAE:  12.48434

# difference on median count arrivals per airport per day; presumably an airport knows how many flights are arriving in advance?
agg_count_arr_date<-aggregate(df_airline$Distance,by=list(df_airline$Dest,df_airline$Date),FUN=length)
agg_med_arr_date<-aggregate(agg_count_arr_date$x,by=list(agg_count_arr_date$Group.1),FUN=median)
names(agg_count_arr_date)<-c("Dest","Date","cnt_day_arr")
names(agg_med_arr_date)<-c("Dest","med_day_arr")
df_airline<-merge(agg_count_arr_date,df_airline);rm(agg_count_arr_date)
fn_model(df_airline) # MSE:  517.1347 RMSE:  22.7406 MAE:  12.46563
df_airline<-merge(agg_med_arr_date,df_airline);rm(agg_med_arr_date)
fn_model(df_airline) # MSE:  517.1347 RMSE:  22.7406 MAE:  12.46563 # no change
df_airline$cnt_day_arr_diff<-df_airline$med_day_arr-df_airline$cnt_day_arr
fn_model(df_airline) # MSE:  505.3978 RMSE:  22.48105 MAE:  12.35899

# days until holiday feature - originally from Hortonworks
holidays<-c(as.Date("1987-10-12"),as.Date("1987-11-11"),as.Date("1987-11-26"),as.Date("1987-12-25"))
fn_holiday<-function(df_airline_Date,holidays)
{
  closest<-as.numeric(min(abs(df_airline_Date-holidays)))
  return(closest)
}
holiday_key<-data.frame(Date=unique(df_airline$Date),days2holiday=sapply(unique(df_airline$Date),fn_holiday,holidays)) #maybe quicker to apply for each unqiue date, then join back
holiday_key
df_airline<-merge(df_airline,holiday_key);rm(holiday_key)
                               
fn_model(df_airline)
# MSE:  506.2752  RMSE:  22.50056 MAE:  12.36566

h2o.shutdown(prompt = FALSE)
