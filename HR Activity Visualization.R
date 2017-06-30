library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(xlsx)

#Import HR Activity data set
hr_activity <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/HR_Activity.csv")

#Convert HR Activity data set into a Dataframe
hr_activity_df <- data.frame(hr_activity)

#Convert DATE variable into date-type
hr_activity_df$DATE <- as.Date(hr_activity_df$DATE, "%m/%d/%Y")

#Conduct summary statistics on Heart Rate Value for Entire Month of January-2017
summary(hr_activity_df)
### Mean: 77.84  Max: 166   Min: 49



# Group HR VALUE by DATE
av_hr_by_day <- hr_activity_df %>%
  group_by(DATE)%>%
  summarise(av_hr = mean(HEART.RATE.VALUE))

#View new table
(av_hr_by_day)

#Conduct summary statistics on average Heart Rate Value for each day of January 2017
summary(av_hr_by_day)

#Read-in dataset that includes Days of the Week (DOW)
av_hr_by_dow <- read_csv("C:/Users/steal/Data Science/Statistics/MSDS 5043/Data Sets/av_hr_by_dow.csv")

#Convert DATE into date-type
av_hr_by_dow$DATE <- as.Date(av_hr_by_dow$DATE, "%m/%d/%Y")   

#Convert DOW into factor variable
av_hr_by_dow$DOW <- factor(av_hr_by_dow$DOW, levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday"),ordered=TRUE)
   
summary(av_hr_by_dow)

# Group ave HR VALUE by DOW
av_hr_by_dow2 <- av_hr_by_dow %>%
  group_by(DOW)%>%
  summarise(av_hr_per_dow = mean(av_hr))

#View new table
(av_hr_by_dow2)
### Friday has the highest average HR at 80.33



#Visuals
#Create line graph of HR VALUE vs Time 
ggplot(data = hr_activity_df, aes(x = TIME, y = HEART.RATE.VALUE, group = 1)) + geom_line() + 
  labs(x="TIME (00:00:00 - 23:58:50)", y="HEART RATE (49 - 166)", title="Heart Rate Values", subtitle= "Jan.1-31",
       caption= "Based on data collected from J. Fisher's wearable activity tracker")

#Create line graph of average HR vs DATE
ggplot(data= av_hr_by_day, aes(x=DATE, y=av_hr, group=1)) + geom_line() + 
  labs(x="DATE (01/01/17 - 01/31/17)", y="Average HR (71.34 - 87.64)", title="Average Heart Rate values per Day (Jan. '17)", subtitle= "Jan.1-31",
       caption= "Based on data collected from J. Fisher's wearable activity tracker")

#Create BoxPlot of Ave HR vs DATE
ggplot(av_hr_by_dow, aes(DOW, av_hr )) + geom_boxplot() + labs(x= "Day of the Week (Mon - Fri)", y="Average HR",
        title= "Average HR per Day of the Week")


