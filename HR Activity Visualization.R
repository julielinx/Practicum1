library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(xlsx)
library(corrplot)

#Import HR Activity data set
hr_activity <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/HR_Activity.csv")

#Convert HR Activity data set into a Dataframe
hr_activity_df <- data.frame(hr_activity)

#Convert DATE variable into date-type
hr_activity_df$DATE <- as.Date(hr_activity_df$DATE, "%m/%d/%Y")

#Conduct summary statistics on Heart Rate Value for Entire Month of January-2017
summary(hr_activity_df)
### Mean: 77.84  Max: 166   Min: 49


###

#Group HR VALUE by DATE with average HR
av_hr_by_day <- hr_activity_df %>%
  group_by(DATE)%>%
  summarise(av_hr = mean(HEART.RATE.VALUE))

#View new table
(av_hr_by_day)

#Conduct summary statistics on average Heart Rate Value for each day of January 2017
summary(av_hr_by_day)

###

#Read-in dataset that includes Days of the Week (DOW)
av_hr_by_dow <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/av_hr_by_dow.csv")

#Convert DATE into date-type
av_hr_by_dow$DATE <- as.Date(av_hr_by_dow$DATE, "%m/%d/%Y")   

#Convert DOW into factor variable
av_hr_by_dow$DOW <- factor(av_hr_by_dow$DOW, levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday"),ordered=TRUE)
   

# Group ave HR VALUE by DOW
av_hr_by_dow2 <- av_hr_by_dow %>%
  group_by(DOW)%>%
  summarise(av_hr_per_dow = mean(av_hr))

#View new table
View(av_hr_by_dow2)
### Friday has the highest average HR at 80.33


###

#Read-in dataset that includes Steps & Days of the Week (DOW)
av_st_by_dow <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/POTS/av_st_by_dow.csv")

#Convert DATE into date-type
av_st_by_dow$DATE <- as.Date(av_st_by_dow$DATE, "%m/%d/%Y")   

#Convert DOW into factor variable
av_st_by_dow$DOW <- factor(av_st_by_dow$DOW, levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday"),ordered=TRUE)


# Group ave HR VALUE by DOW
av_st_by_dow2 <- av_st_by_dow %>%
  group_by(DOW)%>%
  summarise(av_st_per_dow = mean(av_st))

#View new table
View(av_st_by_dow2)
### Friday has the highest average Steps taken at 5592.75 steps


###

#Read-in dataset that includes minutes asleep & Days of the Week (DOW)
av_as_by_dow <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/POTS/av_as_by_dow.csv")

#Convert DATE into date-type
av_as_by_dow$date <- as.Date(av_as_by_dow$date, "%m/%d/%Y")   

#Convert DOW into factor variable
av_as_by_dow$DOW <- factor(av_as_by_dow$DOW, levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday"),ordered=TRUE)


# Group ave HR VALUE by DOW
av_as_by_dow2 <- av_as_by_dow %>%
  group_by(DOW)%>%
  summarise(av_as_per_dow = mean(min_asleep))

#View new table
View(av_as_by_dow2)
### Friday has the highest average minutes asleep at 456 minutes



### Visuals ###

#Create line graph of HR VALUE vs Time 
ggplot(data = hr_activity_df, aes(x = TIME, y = HEART.RATE.VALUE, group = 1)) + geom_line() + 
  labs(x="TIME (00:00:00 - 23:58:50)", y="HEART RATE (49 - 166)", title="Heart Rate Values", subtitle= "Jan.1-31",
       caption= "Based on data collected from J. Fisher's wearable activity tracker")

#Create line graph of average HR vs DATE
ggplot(data= av_hr_by_day, aes(x=DATE, y=av_hr, group=1)) + geom_line() + 
  labs(x="DATE (01/01/17 - 01/31/17)", y="Average HR (71.34 - 87.64)", title="Average Heart Rate values per Day (Jan. '17)", subtitle= "Jan.1-31",
       caption= "Based on data collected from J. Fisher's wearable activity tracker")

#Create line graph of Average Minutes Asleep vs DATE
ggplot(av_min_as_by_day, aes(x=date, y=av_min_asleep, group=1)) + geom_line() + 
  labs(x="DATE (01/01/17 - 01/31/17)", y="Total Min Asleep (344 - 860)", title="Total Minutes Asleep per Day (Jan. '17)", subtitle= "Jan.1-31",
       caption= "Based on data collected from J. Fisher's wearable activity tracker")
summary(av_min_as_by_day)


#Create BoxPlot of Ave HR vs Day of Week
ggplot(av_hr_by_dow, aes(DOW, av_hr )) + geom_boxplot() + labs(x= "Day of the Week (Mon - Fri)", y="Average HR",
        title= "Average HR per Day of the Week")

#Create BoxPlot of Ave Steps vs Day of Week
ggplot(av_st_by_dow, aes(DOW, av_st )) + geom_boxplot() + labs(x= "Day of the Week (Mon - Fri)", y="Average Steps",
                                                               title= "Average Steps per Day of the Week")

#Create BoxPlot of Ave Minutes Asleep vs Day of Week
ggplot(av_as_by_dow, aes(DOW, min_asleep )) + geom_boxplot() + labs(x= "Day of the Week (Mon - Fri)", y="Average Min Asleep",
                                                               title= "Average Minutes Asleep per Day of the Week")





###

#Import Step Activity Data   ***This dataframe/ grouping is used to 'clean' data, to be added to addit. data.
st_activity <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/POTS/Activity201701.csv")

#Convert Step Activity data set into a Dataframe
st_activity_df <- data.frame(st_activity)

#Convert DATE variable into date-type
st_activity_df$DATE <- as.Date(st_activity_df$DATE, "%m/%d/%Y")

# Group Steps by Date 
av_st_by_day <- st_activity_df %>%
  group_by(DATE)%>%
  summarise(av_st = sum(STEPS))

#View Table
view(av_st_by_day)


###

#Create Step_Sleep_HR table
st_sl_hr_table <- read.csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/POTS/step_sleep_hr_data.csv")

#Convert Step_Sleep data set into a Dataframe
st_sl_hr_df <- data.frame(st_sl_hr_table)

#Convert DATE variable into date-type
st_sl_hr_df$date <- as.Date(st_sl_hr_df$date, "%m/%d/%Y")

#View summary statistics
summary(st_sl_hr_df)

# Group Sleep = by Day
av_sl_day <- st_sl_hr_df %>%
  group_by(date)%>%
  summarise(Tol_Sleep = sum(min_asleep))

#Run summary statistics    
summary(av_sl_day)




###
#Import Min Asleep  Data   ***This dataframe/ grouping is used to 'clean' data, to be added to addit. data.
min_asleep_ave <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/POTS/step_sleep_hr_data(min_asleep_null_mean).csv")

#Convert Min Asleep data set into a Dataframe
min_asleep_ave <- data.frame(min_asleep_ave)

#Convert DATE variable into date-type
min_asleep_ave$date <- as.Date(min_asleep_ave$date, "%m/%d/%Y")

# Group Average Minutes Asleep by Date 
av_min_as_by_day <- min_asleep_ave %>%
  group_by(date)%>%
  summarise(av_min_asleep = sum(min_asleep))

#View Table
View(av_min_as_by_day)





###TEST###

# Examination of correlations
cor(st_sl_hr_df[2:4])
pairs(st_sl_hr_df[2:4], pch=16)
corrplot(cor(st_sl_hr_df[2:4]), method="ellipse")

### Steps+HR show strong positive relationship
### Min_Asleep+HR show weak negative relationship



#####TEST CODE####
# FIT FULL MODEL
act.full <- lm(hr ~ min_asleep, data=st_sl_hr_df)
summary(act.full)

# Residual plot
act.full.df <- fortify(act.full)
ggplot(act.full.df, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Fitted Values", y="Residuals")

# Check for normality
qqPlot (act.full$residuals, pch=16)
shapiro.test(act.full$residuals)
  