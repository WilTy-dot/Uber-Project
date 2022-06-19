#SECTION A

## Prediction without S&P500 daily return

### Data rearrangement

####Packages loading and data import/merging

if(!require(pacman))install.packages("pacman")
p_load(geosphere,ggplot2,dplyr,readr,geosphere,distm, data.table,quantmod,esquisse,corrplot,tibble,lubridate,stargazer,vis_dat,readr,visdat)
uber_test<-fread('uber_test.csv')
uber_train_apr14<-fread('uber-raw-data-apr14.csv')
uber_train_may14<-fread('uber-raw-data-may14.csv')
uber_train_jun14<-fread('uber-raw-data-jun14.csv')
uber_train_jul14<-fread('uber-raw-data-jul14.csv')
uber_train_comb<-rbind(uber_train_apr14,uber_train_may14,uber_train_jun14,uber_train_jul14)

#### Filtering the merged training set (raw data)to account for pick up with 1km of NYSE

uber_train_comb$distance_to_NYSE <- distm(x=cbind(uber_train_comb$Lon,uber_train_comb$Lat), y=c(-74.011322,40.706913), fun = distHaversine)
uber_train_comb_int<-uber_train_comb[distance_to_NYSE<=1000]

#### Creating a vector containing all relevant timepoints with 15 minutes gap
uber_train_comb_int$`Date/Time`<-mdy_hms(uber_train_comb_int$`Date/Time`)#, format ='%m/%d/%Y%H:%M:%S')
time_points<-seq(ymd_hms("2014-04-1 00:00:00"), ymd_hms("2014-07-31 24:00:00"), by = "15 min")

#### Creating 15min-interval objects 
time_intervals<-int_diff(time_points)

#### Matching the raw data time points to interval objects

for (dat in uber_train_comb_int$`Date/Time`){
    if (as_datetime(dat)%within%time_intervals){
    uber_train_comb_int$intervals<-
      floor_date(uber_train_comb_int$`Date/Time`,unit = "15min") }
  }

#### filtering the raw data to match the period [ 17:00 to 00:00] and computing the number of rides per interval object

uber_train_comb_fin<-uber_train_comb_int[hour(uber_train_comb_int$`Date/Time`)>=17,#%in% seq(17,23,by=1),
                    .(number_of_rides_per_interval=.N),
                    by=.(intervals)]


### Exploratory analysis

#### Adding new features namely ,wday, federal holiday, weekend, timespan, Mon,Tue,Wed,Thu,Fri, Sat, Sun and hour_numeric
##### federal holiday, weekend, Mon,Tue,Wed,Thu,Fri, Sat, Sun are boolean features (0,1) and hour_numeric represents the 
##### time in minutes from 17:00.

uber_train_comb_fin$wDay<-lubridate::wday(uber_train_comb_fin$intervals,label = TRUE, abbr = FALSE)
holidays<- ymd(c("2014-05-26", "2014-07-04"))
uber_train_comb_fin$federal_holiday<-ifelse(as_date(uber_train_comb_fin$intervals)%in%holidays,1,0)
uber_train_comb_fin$weekend<-ifelse(uber_train_comb_fin$wDay%in%c("Saturday", "Sunday"),1,0)
uber_train_comb_fin$timespan<-format(uber_train_comb_fin$intervals,format="%H:%M:%S")
uber_train_comb_fin$Mon<-ifelse(uber_train_comb_fin$wDay=="Monday",1,0)
uber_train_comb_fin$Tue<-ifelse(uber_train_comb_fin$wDay=="Tuesday",1,0)
uber_train_comb_fin$Wed<-ifelse(uber_train_comb_fin$wDay=="Wednesday",1,0)
uber_train_comb_fin$Thu<-ifelse(uber_train_comb_fin$wDay=="Thursday",1,0)
uber_train_comb_fin$Fri<-ifelse(uber_train_comb_fin$wDay=="Friday",1,0)
uber_train_comb_fin$Sat<-ifelse(uber_train_comb_fin$wDay=="Saturday",1,0)
uber_train_comb_fin$Sun<-ifelse(uber_train_comb_fin$wDay=="Sunday",1,0)
uber_train_comb_fin$hour_numeric<-(-17+hour(ymd_hms(uber_train_comb_fin$intervals))+
                                  minute(ymd_hms(uber_train_comb_fin$intervals))/60)



#### Visualization


##### Distribution of pick up demand per wday
ggplot(uber_train_comb_fin) +
 aes(x = wDay, y = number_of_rides_per_interval) +
 geom_boxplot(fill = "#112446") +
 theme_minimal()



##### Distribution of ride demand per timespan

ggplot(uber_train_comb_fin) +
 aes(x = timespan, y = number_of_rides_per_interval) +
 geom_boxplot(fill = "#114615") +
 labs(title = "Distribution of ride demand per timespan") +
 guides(x = guide_axis(angle = 90))+
 theme_bw()

##### Distribution of ride demand per weekday

ggplot(uber_train_comb_fin) +
 aes(x = timespan, y = number_of_rides_per_interval) +
 geom_boxplot(fill = "#46337E") +
 labs(title = "Distribution of ride demand per weekday") +
 guides(x = guide_axis(angle = 90))+
 theme(axis.title.x = element_text(size = 4, hjust = 0.5))+
 theme_minimal() +
 facet_wrap(vars(wDay), nrow = 3)



#### Correlations

cor(uber_train_comb_fin$number_of_rides_per_interval,uber_train_comb_fin$weekend)
cor(uber_train_comb_fin$number_of_rides_per_interval,uber_train_comb_fin$federal_holiday)
cor(uber_train_comb_fin[,-c("wDay","federal_holiday","weekend","timespan","intervals")])
corrplot(cor(uber_train_comb_fin[,-c("wDay","federal_holiday","weekend","timespan","intervals")]), type = "upper")  
cor(uber_train_comb_fin$number_of_rides_per_interval,uber_train_comb_fin$hour_numeric)

### Model estimation

#### Sampling split

set.seed(100)
sample <- sample.int(n = nrow(uber_train_comb_fin), size = floor(0.7*nrow(uber_train_comb_fin)), replace = F)
uber_training <- uber_train_comb_fin[sample, ]
uber_val <- uber_train_comb_fin[-sample, ]


#### linear models

##### Model 1: The number_of_rides trend in the evening

mod1<-lm(number_of_rides_per_interval~hour_numeric, data=uber_training)
summary(mod1)

##### Model 2: The number_of_rides trend in the evening + each day impact

mod2<-lm(number_of_rides_per_interval~hour_numeric+Mon+Tue+Wed+Thu+Fri+Sat,
         data=uber_training)
summary(mod2)

##### Model 3: The number_of_rides trend in the evening + each day impact + interaction between
#####          each day and evening time

mod3<-lm(number_of_rides_per_interval~hour_numeric*(Mon+Tue+Wed+Thu+Fri+Sat),
         data=uber_training)
summary(mod3)

##### Model 4: The number_of_rides trend in the evening + weekend/weekday impact + interaction between
#####          weekend/weekday and evening time

mod4<-lm(number_of_rides_per_interval~hour_numeric+weekend + hour_numeric*weekend,
         data=uber_training)
summary(mod4)


stargazer(mod1,mod2,mod3,mod4,type="text")

#### Accuracy checks

##### Model 1: MAPE(Mean Absolute Percentage Error)
rides_Pred1 <- predict(mod1, uber_val)
actuals_preds1 <- data.frame(cbind(actuals=uber_val$number_of_rides_per_interval, predicteds=rides_Pred1))
correlation_accuracy1 <- cor(actuals_preds1)
mape1  <- mean(abs((actuals_preds1$predicteds - actuals_preds1$actuals))/actuals_preds1$actuals)
mape1

##### Model 2: MAPE(Mean Absolute Percentage Error)
rides_Pred2 <- predict(mod2, uber_val)
actuals_preds2 <- data.frame(cbind(actuals=uber_val$number_of_rides_per_interval, predicteds=rides_Pred2))
correlation_accuracy2 <- cor(actuals_preds2)
mape2  <- mean(abs((actuals_preds2$predicteds - actuals_preds2$actuals))/actuals_preds2$actuals)
mape2

##### Model 3: MAPE(Mean Absolute Percentage Error)
rides_Pred3 <- predict(mod3, uber_val)
actuals_preds3 <- data.frame(cbind(actuals=uber_val$number_of_rides_per_interval, predicteds=rides_Pred3))
correlation_accuracy3 <- cor(actuals_preds3)
mape3  <- mean(abs((actuals_preds3$predicteds - actuals_preds3$actuals))/actuals_preds3$actuals)
mape3

##### Model 4: MAPE(Mean Absolute Percentage Error)
rides_Pred4 <- predict(mod4, uber_val)
actuals_preds4 <- data.frame(cbind(actuals=uber_val$number_of_rides_per_interval, predicteds=rides_Pred4))
correlation_accuracy4 <- cor(actuals_preds4)
mape4 <- mean(abs((actuals_preds4$predicteds - actuals_preds4$actuals))/actuals_preds4$actuals)
mape4

## Conclusion: Model 1 displays the best results in term of accuracy, therefore it will used to predict the number of rides


#SECTION B

##Prediction with S&P500 daily return

### Importing S&P500 daily data from 2014-04-01 to 2014-07-31

getSymbols(c('^GSPC'),
           from='2014-04-01',
           to='2014-07-31')

### Extracting the daily returns and transforming the table

uber_train_comb_fin$Date<-date(uber_train_comb_fin$intervals)

GSPC_DRet<-
  periodReturn(GSPC,'daily')

RetDF<-data.frame(GSPC_DRet)

RetDF<-tibble::rownames_to_column(RetDF, 'Date')
RetDF<-as.data.table(RetDF)
RetDF$Date<-as.Date(RetDF$Date)

#### Merging train_transformed set & S&P500 daily returns from 2014-04-01 to 2014-07-31
uber_filtered_SP500<-merge.data.table(uber_train_comb_fin,RetDF, by='Date', all.x = TRUE)

#### Visualizing the merged data and treating NAs 
vis_dat(uber_filtered_SP500)
uber_filtered_SP500$daily.returns[is.na(uber_filtered_SP500$daily.returns)] <- mean(uber_filtered_SP500$daily.returns,na.rm = TRUE)


###Exploratory analysis

##### Impact of S&P500 on uber pick up demand

ggplot(uber_filtered_SP500) +
 aes(x = intervals, y = daily.returns, size = number_of_rides_per_interval) +
 geom_line(colour = "#2E200E") +
  labs(title = "Impact of S&P500 on uber pick up demand")+
 theme_minimal()

#### Correlations

cor(uber_filtered_SP500$number_of_rides_per_interval,uber_filtered_SP500$daily.returns)
cor(uber_filtered_SP500[,-c("wDay","federal_holiday","timespan","intervals","Date")])
corrplot(cor(uber_filtered_SP500[,-c("wDay","federal_holiday","timespan","intervals","Date")]), type = "upper")  
cor(uber_train_comb_fin$number_of_rides_per_interval,uber_train_comb_fin$hour_numeric)

### Model estimation

#### Sampling split

set.seed(100)
sample_f <- sample.int(n = nrow(uber_filtered_SP500), size = floor(0.7*nrow(uber_filtered_SP500)), replace = F)
uber_SP500_training <- uber_filtered_SP500[sample_f, ]
uber_SP500_val <- uber_filtered_SP500[-sample_f, ]


#### linear models

##### Model 1: The number_of_rides trend with SP500 impact

mod1_SP500<-lm(number_of_rides_per_interval~daily.returns, data=uber_SP500_training)
summary(mod1_SP500)

##### Model 2: The number_of_rides trend in the evening with SP 500 impact 

mod2_SP500<-lm(number_of_rides_per_interval~daily.returns+hour_numeric, data=uber_SP500_training)
summary(mod2_SP500)

stargazer(mod1_SP500,mod2_SP500,type="text")


#### Accuracy checks

##### Model 2: MAPE(Mean Absolute Percentage Error)
SP500_rides_Pred <- predict(mod2_SP500, uber_SP500_val)
SP500_actuals_preds <- data.frame(cbind(actuals=uber_SP500_val$number_of_rides_per_interval, predicteds=SP500_rides_Pred))
SP500_correlation_accuracy <- cor(SP500_actuals_preds)
SP500_mape  <- mean(abs((SP500_actuals_preds$predicteds - SP500_actuals_preds$actuals))/SP500_actuals_preds$actuals)
SP500_mape

####Conclusion: SP helps improve the initial model

## SECTION C

#Test prediction and saving file

## Importing uber_test file

uber_test<-read.csv("C:/Users/USER/OneDrive/Documents/MMA/Programme/Courses/Data science for business I/Data science I/Assignments/Final Assignment/Data sets/uber_test.csv")

## Adding hour_numeric features to the test set

uber_test$Time_Interval <- mdy_hm(uber_test$Time_Interval)

uber_test$hour_numeric<-(-17+hour(uber_test$Time_Interval)+
                           minute(uber_test$Time_Interval)/60)

### Importing S&P500 daily data from 2014-09-01 to 2014-09-30

getSymbols(c('^GSPC'),
           from='2014-09-01',
           to='2014-09-30')

### Extracting the daily returns and transforming the table

GSPC_DRet1<-
  periodReturn(GSPC,'daily')

RetDF1<-data.frame(GSPC_DRet1)

RetDF1<-tibble::rownames_to_column(RetDF1, 'Date')
RetDF1<-as.data.table(RetDF1)
RetDF1$Date<-as.Date(RetDF1$Date)


## Adding Date, Merging the test set with SP500 daily returns and impute calculatated values to NA's 

uber_test$Date<-date(uber_test$Time_Interval)
uber_SP500_test<-merge.data.table(uber_test,RetDF1, by='Date', all.x = TRUE)
uber_SP500_test$daily.returns[is.na(uber_SP500_test$daily.returns)] <- mean(uber_SP500_test$daily.returns,na.rm = TRUE)
vis_dat(uber_SP500_test)


## Predictions on Test set

prediction.test <- predict(mod1, uber_test)
prediction.test500 <- predict(mod2_SP500,uber_SP500_test)
uber_test$pick_num_withoutSP <- prediction.test
uber_test$pick_num_withSP <- prediction.test500

final_submission <- uber_test %>% select(Time_Interval,pick_num_withoutSP,pick_num_withSP)

write_csv(final_submission,"Uber_submission.csv")













