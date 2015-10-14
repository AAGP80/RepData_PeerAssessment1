# pre anailisi per assessment
require(lubridate)
require(dplyr)
require(ggplot2)

## Carica i dati e trasforma testo in data
activity<-read.csv("activity.csv",stringsAsFactors = FALSE)
activity$date<-ymd(activity$date)

## What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the   
## dataset. Calculate the total number of steps taken per day (DONE)
## If you do not understand the difference between a histogram and a barplot,
## research the difference between them. 
## Make a histogram of the total number of steps taken each day (DONE)
## Calculate and report the mean and median of the total number of steps taken 
## per day (DONE)

activity_by_date<-group_by(activity,date)
tot_steps_by_date<-summarise(activity_by_date,number=sum(steps,na.rm=TRUE))

p <- ggplot(tot_steps_by_date, aes(x=number))+
        geom_histogram()
print(p)

pp<-mean(tot_steps_by_date$number)
ppp<-median(tot_steps_by_date$number)

## What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
## and the average number of steps taken, averaged across all days (y-axis)
## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?

activity_by_int<-group_by(activity,interval)
avg_steps_by_int<-summarise(activity_by_int,avg=mean(steps,na.rm=TRUE))

q <- ggplot(avg_steps_by_int, aes(interval,avg))+
        geom_line()
print(q)

qq<-as.numeric(avg_steps_by_int[which.max(avg_steps_by_int$avg),1])

## Imputing missing values
## Note that there are a number of days/intervals where there are missing
## values (coded as NA).
## The presence of missing days may introduce bias into some calculations
## or summaries of the
## data. Calculate and report the total number of missing values in the dataset
## (i.e. the total number of rows with NAs) (DONE)
## Devise a strategy for filling in all of the missing values in the dataset.
## (DONE)
## Create a new dataset that is equal to the original dataset but with the 
## missing data filled in. (DONE)
## Make a histogram of the total number of steps taken each day and Calculate
## and report the mean and median total number of steps taken per day. Do these
## values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total
## daily number of steps? (DONE)

tot_NA_by_date<-summarise(activity_by_date,count=sum(is.na(steps)))
## elenco giorni mancanti
rr<-filter(tot_NA_by_date,count!=0)
## deciso di imputare valore medio per intervallo a quell'intervallo

#avg_steps_by_int<-mutate(avg_steps_by_int,typical=avg)
activity_imp<-activity

for (i in 1:nrow(activity_imp)) {
         if (is.na(activity_imp$steps[i])) {
                 activity_imp$steps[i] <- avg_steps_by_int[
                         which(activity_imp$interval[i] == 
                                       avg_steps_by_int$interval), ]$avg
        }
 }

activity_imp_by_date<-group_by(activity_imp,date)
tot_steps_imp_by_date<-summarise(activity_imp_by_date,
                                 number=sum(steps,na.rm=TRUE))

measured<-as.data.frame(tot_steps_by_date$number)
measured$origin <- 'measured'
names(measured)<-c("number","origin")

imputed<-as.data.frame(tot_steps_imp_by_date$number)
imputed$origin <- 'imputed'
names(imputed)<-c("number","origin")

combined<-rbind(measured, imputed)

s <- ggplot(combined, aes(x=number, fill=origin))+
        geom_histogram(position="dodge")
print(s)

ss<-mean(tot_steps_imp_by_date$number)
sss<-median(tot_steps_imp_by_date$number)


## Are there differences in activity patterns between weekdays and weekends?
## For this part the weekdays() function may be of some help here. Use the 
## dataset
## with the filled-in missing values for this part.
## Create a new factor variable in the dataset with two levels – 
## “weekday” and “weekend” 
## indicating whether a given date is a weekday or weekend day. (DONE)
## Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval
## (x-axis) and the average number of steps taken, averaged across all weekday
## days or weekend (DONE)
## days (y-axis). See the README file in the GitHub repository to see an 
## example of what this 
## plot should look like using simulated data.

activity_imp<-mutate(activity_imp,
                     weekx=ifelse(weekdays(date,abbreviate=TRUE)=="Lun" |
                                    weekdays(date,abbreviate=TRUE)=="Mar" |
                                    weekdays(date,abbreviate=TRUE)=="Mer" |
                                    weekdays(date,abbreviate=TRUE)=="Gio" |
                                    weekdays(date,abbreviate=TRUE)=="Ven",
                                        "weekday","weekend")) %>%
        mutate(weekx=as.factor(weekx))
activity_imp_by_weekx_by_int<-group_by(activity_imp,weekx,interval)

avg_steps_by_weekx_by_int<-summarise(activity_imp_by_weekx_by_int,
                                     avg=mean(steps,na.rm=TRUE))

t <- ggplot(avg_steps_by_weekx_by_int, aes(interval,avg))+
        geom_line()+
        facet_wrap(~weekx,ncol=1)
print(t)

