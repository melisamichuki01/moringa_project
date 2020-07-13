# Business Understanding

Cryptography is the art of writing or solving code.
A Kenyan entrepreneur has created an online cryptography course and would want to advertise it on her blog. She currently targets audiences originating from various countries. In the past, she ran ads to advertise a related course on the same blog and collected data in the process. 
She would now like to employ your services as a Data Science Consultant to help her identify which individuals are most likely to click on her ads. 

# Business objectives
The main objective of this study is to enable our client to identify which individuals are most likely to click on her ads. 


# Assessing the situation at hand
## Resources inventory
### Datasets 

Dataset url<-http://bit.ly/IPAdvertisingData

Software (Github,R)

# Assumptions 
The data is up to date and relevant.
# Constraints
There are no constraints currently


# Importing necesary packages
```{r}
library("data.table")
library(tidyverse)
library(lubridate)
```

# Loading dataset 
```{r}
addf <- fread('http://bit.ly/IPAdvertisingData')
```

# Preview of the head of our dataset

```{r}
head(addf)
```
# Preview of the tail of the dataset
```{r}
tail(addf)
```
# Summary of the advertising dataset
```{r summary}
summary(addf)
```
# Summary of the advertising dataset information
```{r info}
names(addf)
```
# Summary of the classes of the advertising dataset
```{r}
class(addf)
```
# Check for duplicates
```{r duplicates}
duplicated_addf <- addf[duplicated(addf),]
duplicated_addf
```
# Check for unique values
```{r unique}
unique_addf <- unique(addf)
unique_addf
```

From the previous run code,we found no duplicated values in our dataset

# Check for null values
```{r}
null_addf <- is.null(addf)
null_addf
```

We can also see that there are no null values in our dataset
# Check for outliers

# Violin Plots
```{r}
library(vioplot)
x1 <- addf$`Daily Time Spent on Site`[addf$`Clicked on Ad`==0]
x2 <- addf$`Daily Time Spent on Site`[addf$`Clicked on Ad`==1]
vioplot(x1, x2,names=c("Clicked on Ad", "Did not click on add"),
        col="blue")
title("Violin Plots of daily time spent on site")
```

# Box Plots
```{r}
x3 <- addf$Age[addf$`Clicked on Ad`==0]
x4 <- addf$Age[addf$`Clicked on Ad`==1]
boxplot(x3, x4,names=c("Clicked on Ad", "Did not click on add"),
        col="gold")
title("Box Plots of Age range ")
```
# Listing out outliers

```{r}
boxplot.stats(x3)$out
```
# Univariate analysis
# Mininimum age that accessed the ad

```{r}
minAge <- min(addf$Age)
minAge
```
# Maximum age
```{r}
maxAge <- max(addf$Age)
maxAge
```
The youngest person that accessed the ad was 19yrs with the oldest being 61yrs

# Mean of Daily Internet Usage
```{r}
mean_addf <- mean(addf$`Daily Internet Usage`)
mean_addf
```
# Mean of time spent on the site
```{r}
mean_addf1<- mean(addf$`Daily Time Spent on Site`)
mean_addf1
```
The mean amount of min of daily internet usage is 180 while the mean of the daily time spent on the site is 65 min

# Quartiles of Internet usage

```{r}
addf_InternetUsage_quantile <- quantile(addf$`Daily Internet Usage`)
addf_InternetUsage_quantile
```

# Quartiles of Time spent on a site
```{r}
addf_TimeSpent_quantile <- quantile(addf$`Daily Time Spent on Site`)
addf_TimeSpent_quantile
```

# Standard of Time spent on a site
```{r}
addf_TimeSpent_sd <- sd(addf$`Daily Time Spent on Site`)
addf_TimeSpent_sd
```

# Standard deviation of Internet usage
```{r}
addf_InternetUsage_sd <- sd(addf$`Daily Internet Usage`)
addf_InternetUsage_sd
```

# Histogram

```{r}
hist(addf$Age,col="purple")
```
Most people involved in this study were between the age of 30-40 yrs

```{r}
timespent_hist= hist(addf$`Daily Time Spent on Site`,
         main = "Daily time Spent on Site",
         xlab = "Daily time Spent on Site",
         col = "pink"
)
```

```{r}
library(DataExplorer)
plot_density(addf)
```

```{r}
plot_histogram(addf)
```
# Frequency distribution of gender

```{r}
gender <- addf$Male
gender_freq <- table(gender)
barplot(gender_freq,
        col = "green")
title("Frequency distribution of gender")
```
More females were involved in this study than males

# Barplots

# Comparison by gender on number of clicks

```{r}
clicks = table(addf$Male,addf$`Clicked on Ad`)
barplot(clicks,main="No of clicks on Ad per gender,0=Female,1=male",xlab="Ad clicks",col=c("white","cyan"),beside=TRUE)
legends = rownames(clicks)
```
Females tend to click on ads more than men

# Comparison by age on number of clicks
```{r}
clicks1 = table(addf$Age,addf$`Clicked on Ad`)
barplot(clicks1,main="No of clicks on Ad by age,)=Not clicked Ad,1=Clicked Ad",xlab="Ad clicks",col=c("white","purple"),beside = TRUE)
legends = rownames(clicks1)
```
The group that clicked on the ads more frequently were between the age of 36 to 47 yrs 

# Bivariate analysis
## Corelation
Correlation is a normalized measurement of how the two are linearly related
```{r}
timespent_c <- addf$`Daily Time Spent on Site`
internetusage_c <- addf$`Daily Internet Usage`

cor(timespent_c,internetusage_c)
```
```{r}
pairs(data=addf,
      ~`Daily Time Spent on Site`+ `Daily Internet Usage` + Age)
```{r}
library(ggcorrplot)
corr = round(cor(select_if(addf, is.numeric)), 4)
ggcorrplot(corr, hc.order = T, ggtheme = ggplot2::theme_grey,
           colors = c("cyan", "blue", "maroon"), lab = T)
```         
## Covariance
Covariance is a number that reflects the degree to which two variable vary together

```{r}
cov(timespent_c,internetusage_c)
```

We have a high covariance of 360 which indicates a strong relationship between the variables

## Scatterplots

```{r}
plot(timespent_c,internetusage_c, xlab="Daily time spent on site", ylab="Daily Internet Usage")
```
