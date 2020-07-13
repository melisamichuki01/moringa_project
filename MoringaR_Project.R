# Behaviour analysis
# Importing necesary packages
library("data.table")
library(tidyverse)
library(lubridate)


# Loading dataset 

addf <- fread('http://bit.ly/IPAdvertisingData')

# Preview of the head of our dataset
head(addf)

# Preview of the tail of the dataset
tail(addf)

# Summary of the advertising dataset
summary(addf)

# Summary of the advertising dataset
names(addf)

# Summary of the classes of the advertising dataset
#
class(addf)
# Check for duplicates
duplicated_addf <- addf[duplicated(addf),]
duplicated_addf

# Check for unique values
unique_addf <- unique(addf)
unique_addf

# From the previous run code,we found no duplicated values in our dataset

# Check for null values
null_addf <- is.null(addf)
null_addf

na_addf <- na.omit(dt)
na_addf
# We can also see that there are no null values in our dataset
class(addf)
class(addf$Timestamp)

# The timestamp column is in character dtype
# We convert it to datatime format for easier analyse
addf_date<-  as.Date(addf$Timestamp)
addf_date

# Check for outliers

# Violin Plots
library(vioplot)
x1 <- addf$`Daily Time Spent on Site`[addf$`Clicked on Ad`==0]
x2 <- addf$`Daily Time Spent on Site`[addf$`Clicked on Ad`==1]
vioplot(x1, x2,names=c("Clicked on Ad", "Did not click on add"),
        col="blue")
title("Violin Plots of daily time spent on site")


# Box Plots
x3 <- addf$Age[addf$`Clicked on Ad`==0]
x4 <- addf$Age[addf$`Clicked on Ad`==1]
boxplot(x3, x4,names=c("Clicked on Ad", "Did not click on add"),
        col="gold")
title("Box Plots of Age range ")

# Listing out outliers
boxplot.stats(x3)$out

# Univariate analysis
# Mininimum age that accessed the ad
minAge <- min(addf$Age)
minAge

# Maximum age
maxAge <- max(addf$Age)
maxAge

# The youngest person that accessed the ad was 19yrs with the oldest being 61yrs

# Mean of Daily Internet Usage
mean_addf <- mean(addf$`Daily Internet Usage`)
mean_addf

# Mean of time spent on the site
mean_addf1<- mean(addf$`Daily Time Spent on Site`)
mean_addf1

# The mean amount of min of daily internet usage is 180 while the mean of the daily time spent on the site is 65 min

# Quartiles of Internet usage
addf_InternetUsage_quantile <- quantile(addf$`Daily Internet Usage`)
addf_InternetUsage_quantile

# Quartiles of Time spent on a site
addf_TimeSpent_quantile <- quantile(addf$`Daily Time Spent on Site`)
addf_TimeSpent_quantile

# Standard of Time spent on a site
addf_TimeSpent_sd <- sd(addf$`Daily Time Spent on Site`)
addf_TimeSpent_sd

# Standard deviation of Internet usage
addf_InternetUsage_sd <- sd(addf$`Daily Internet Usage`)
addf_InternetUsage_sd

# Histogram 
hist(addf$Age,col="purple")

# Most people involved in this study were between the age of 30-40 yrs


timespent_hist= hist(addf$`Daily Time Spent on Site`,
         main = "Daily time Spent on Site",
         xlab = "Daily time Spent on Site",
         col = "pink"
)

library(DataExplorer)
plot_density(addf)
plot_histogram(addf)
# Frequency distribution of gender
gender <- addf$Male
gender_freq <- table(gender)
barplot(gender_freq,
        col = "green")
title("Frequency distribution of gender")

# More females were involved in this study than males

# Barplots

# Comparison by gender on number of clicks

clicks = table(addf$Male,addf$`Clicked on Ad`)
barplot(clicks,main="No of clicks on Ad per gender,0=Female,1=male",xlab="Ad clicks",col=c("white","cyan"),beside=TRUE)
legends = rownames(clicks)

# Comparison by age on number of clicks

clicks1 = table(addf$Age,addf$`Clicked on Ad`)
barplot(clicks1,main="No of clicks on Ad by age,)=Not clicked Ad,1=Clicked Ad",xlab="Ad clicks",col=c("white","purple"),beside = TRUE)
legends = rownames(clicks1)

# Bivariate analysis
## Corelation
### correlation is a normalized measurement of how the two are linearly related

timespent_c <- addf$`Daily Time Spent on Site`
internetusage_c <- addf$`Daily Internet Usage`

cor(timespent_c,internetusage_c)

pairs(data=addf,
      ~`Daily Time Spent on Site`+ `Daily Internet Usage` + Age)

library(ggcorrplot)
corr = round(cor(select_if(addf, is.numeric)), 4)
ggcorrplot(corr, hc.order = T, ggtheme = ggplot2::theme_grey,
           colors = c("cyan", "peachpuff4", "pink"), lab = T)
## Covariance
### Covariance is a number that reflects the degree to which two variable vary together

cov(timespent_c,internetusage_c)

# We have a high covariance of 360 which indicates a strong relationship between the variables

## Scatterplots
plot(timespent_c,internetusage_c, xlab="Daily time spent on site", ylab="Daily Internet Usage")

