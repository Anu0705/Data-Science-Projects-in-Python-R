# **Hypothesis Testing of Credit Card Fraud Detection**
  

 individual is a fraud or not.

## *Step 1 - Loading required Libraries*

```{r Declaring Packages,warning=FALSE,message=FALSE}
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tinytex)
library(readxl)
```

## *Step 2 - Reading the Data*


data <- read.csv("E://Classes Fall 2021//STAT//project//credit_card_dataset//application_data.csv")
data1 <- as_tibble(data)


## *Step 3 - Understanding the Data*

summary(data1)

  
## *Step 4 - Data Cleaning*


col_list <- c('SK_ID_CURR',
              'TARGET',
              'NAME_CONTRACT_TYPE',
              'CODE_GENDER',
              'FLAG_OWN_CAR',
              'FLAG_OWN_REALTY',
              'CNT_CHILDREN',
              'AMT_INCOME_TOTAL',
              'AMT_CREDIT',
              'AMT_ANNUITY',
              'NAME_TYPE_SUITE',
              'NAME_INCOME_TYPE',
              'NAME_EDUCATION_TYPE',
              'NAME_FAMILY_STATUS',
              'NAME_HOUSING_TYPE',
              'REGION_POPULATION_RELATIVE',
              'DAYS_BIRTH',
              'DAYS_EMPLOYED',
              'DAYS_REGISTRATION',
              'DAYS_ID_PUBLISH',
              'OWN_CAR_AGE',
              'FLAG_MOBIL',
              'FLAG_EMP_PHONE',
              'FLAG_WORK_PHONE',
              'FLAG_CONT_MOBILE',
              'FLAG_PHONE',
              'FLAG_EMAIL',
              'OCCUPATION_TYPE',
              'CNT_FAM_MEMBERS',
              'REGION_RATING_CLIENT',
              'REGION_RATING_CLIENT_W_CITY',
              'WEEKDAY_APPR_PROCESS_START',
              'HOUR_APPR_PROCESS_START',
              'REG_REGION_NOT_LIVE_REGION',
              'REG_REGION_NOT_WORK_REGION',
              'LIVE_REGION_NOT_WORK_REGION',
              'REG_CITY_NOT_LIVE_CITY',
              'REG_CITY_NOT_WORK_CITY',
              'LIVE_CITY_NOT_WORK_CITY',
              'ORGANIZATION_TYPE')

data_cleaned <- data1 %>% select(col_list)
data_cleaned <- data_cleaned %>% replace(is.na(.), 0)
data_cleaned <- data_cleaned %>% 
  filter(CODE_GENDER != "XNA")


## *Step 5 - Exploratory Data Analysis *

### Summary statistics of Data


str(data_cleaned)
summary(data_cleaned)


defaulters <- filter(data_cleaned,TARGET==1)
defaulters

percentage_defaulters <- nrow(defaulters)/nrow(data_cleaned)
percentage_defaulters*100

data_cleaned %>%
  group_by(CODE_GENDER) %>%
  summarise(fraction_defaulters = sum(TARGET)/n()*100)

data_cleaned['age_in_years'] <- floor(abs(data_cleaned$DAYS_BIRTH/365))



## *Step 6 - Let us filter the dataset for defaulters or TARGET = 1*

defaulters <- filter(data_cleaned,TARGET==1)
defaulters

defaulters['age_in_years'] <- floor(abs(defaulters$DAYS_BIRTH/365))


## *Step 7 - Visualizations to understand the defaulters dataset*


ggplot(data = defaulters, aes(x=factor(TARGET))) +
  geom_bar(stat = "count", position = position_dodge()) +
  facet_grid(NAME_INCOME_TYPE ~ CODE_GENDER) +
  ggtitle("Total Number of Defaulters by their Income Type classified on Gender") +
  xlab("Defaulters") + ylab("Total Number of Defaulters count")



p <- ggplot(data=defaulters, aes(x=NAME_FAMILY_STATUS, fill=NAME_FAMILY_STATUS)) 
p + geom_bar(stat = "count") +
  ggtitle("Familial status of Defaulters - Working Professionals ") +
  xlab("Family Status") + ylab("Number of Defaulters Count ") + labs(fill = "Family Status") +
  coord_flip()



defaulters[defaulters==""]<-NA
p <- ggplot(defaulters, aes(x=factor(1), fill=OCCUPATION_TYPE))
p + geom_bar() + coord_polar(theta="y") +
  ggtitle("Occupation status of Defaulters - Working Professionals ") +
  xlab(" ") + ylab(" ") + labs(fill = "Occupation Type")

bwt_box <- ggplot(defaulters, aes(x=factor(TARGET),
                                  y=age_in_years,color=factor(CODE_GENDER)))
bwt_box + geom_boxplot() +
  ggtitle(" Boxplots to compare the mean ages of Defaulters") +
  xlab("Defaulters") + ylab("Age in Years") 

scatter_plot <- ggplot(data=defaulters, aes(x=AMT_CREDIT, y=AMT_ANNUITY, color= factor(CODE_GENDER))) 
scatter_plot + geom_point() + 
  ggtitle("Scatter  plot  of  Credit Amount vs Loan Annuity ")+
  xlab("Credit Amount ") + ylab("Annuity Amount ")

## *Step 8 -  Hypothesis Testing*


### Sampling process for Hypothesis Testing


sample1 <-defaulters[sample(nrow(defaulters), 10), ]
sample2 <-defaulters[sample(nrow(defaulters), 25), ]
sample3 <-defaulters[sample(nrow(defaulters), 45), ]


### Let us check the distribution of Annual Income for the different samples


sample_1_plot <- ggplot(sample1, aes(x=AMT_INCOME_TOTAL)) + 
  geom_density() +
  ggtitle("Sampling Distribution of Total Annual Income (n=10) ")+
  xlab("Credit Amount ") + ylab("Annuity Amount ")
sample_1_plot

sample_2_plot <- ggplot(sample2, aes(x=AMT_INCOME_TOTAL)) + 
  geom_density() +
  ggtitle("Sampling Distribution of Total Annual Income (n=25) ")+
  xlab("Credit Amount ") + ylab("Annuity Amount ")
sample_2_plot

sample_3_plot <- ggplot(sample3, aes(x=AMT_INCOME_TOTAL)) + 
  geom_density() +
  ggtitle("Sampling Distribution of Total Annual Income (n=45)")+
  xlab("Credit Amount ") + ylab("Annuity Amount ")
sample_3_plot


## *Step 9 -  Formulating Null and Alternative Hypothesis*


## *Step 10 -  Test Statistic , calculating Z_observed*

n <- nrow(sample2)

x_bar <- c()

for (i in 1:n){
  x_bar <- sum(sample2$AMT_INCOME_TOTAL)
}

x_bar <- x_bar/n

pop_sd <- sd(defaulters$AMT_INCOME_TOTAL)
pop_mean <- 165611

Zobs <- (x_bar-165611)/(746677/sqrt(45))
Zobs


## *Step 11 -  Calculating p-value of Z_observed assuming alpha = 0.5 at 95% Confidence Level*

p_value <- pnorm(abs(Zobs))
p_value


## *Step 12 -  Statistical Conclusion*



## *Step 13 - English Conclusion*

