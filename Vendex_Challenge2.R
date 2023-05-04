library(data.table)
library(readxl)
library(tidyverse)
library(janitor)
library(lubridate)
library(tidymodels)
library(GGally)
library(corrplot)
library(ROCR)

files <- list.files(path = '~/Desktop/R_Course/Assignments', pattern = '.csv', full.names = TRUE)

machines <- read.csv("final_data/machine_data.csv")
failures <- read.csv("final_data/machine_failures.csv")
products <- read.csv("final_data/product_data.csv")
txns <- read.csv("final_data/transactional_data.csv")

#1
txns <- failures %>%
      select(timestamp, failure) %>%
      right_join(txns, by='timestamp') %>%
      mutate(failure = replace_na(failure, 0)) 

#2
txns <- txns %>%
  arrange(machine, timestamp) %>%
  group_by(machine) %>%
  mutate(last_vend = shift(timestamp, n = 1, fill = NA))

#3
txns <- txns %>% 
      mutate(deltahours= as.numeric(difftime(timestamp, last_vend, units = "hours")))
  
#4
machine_daily_average<- txns %>% 
                        group_by(machine, date) %>%
                        summarise(sales = n()) %>%
                        aggregate(sales ~ machine, FUN = mean) %>%
                        rename(daily_sales_machine = sales)

txns <- txns %>%
        left_join(machine_daily_average, by='machine')
  

#5
txns <- txns %>%
        mutate(delta = deltahours /(24/daily_sales_machine) )


#6
#Dropping rows where delta is NA
txns <- txns[complete.cases(txns$delta), ]   

##Before dropping NAs, no. of rows = 1840500
##After dropping NAs, no. of rows = 1838005
##Removed rows = 2495 (0.13% data)


#Checking for Outliers in delta column
hist(txns$delta,
     main='Histogram of delta before outlier removal')

#Removing outliers from the income_average column in the machines dataframe
quantiles = quantile(txns$delta, na.rm=TRUE)
iqr = quantiles[4]-quantiles[2]

txns <- txns %>% 
  filter(delta <= quantiles[4]+(1.5*iqr)) %>%
  filter(delta >= quantiles[2]-(1.5*iqr)) 


hist(txns$delta,
     main='Histogram of delta after outlier removal')

summary(txns$delta)

##Before removing outliers, no. of rows = 1838005
##After removing outliers, no. of rows = 1640953
##Removed rows = 197052 (10.7% data)


#Building Logistic Regression model
set.seed(2022)
split <- initial_split(txns, strata = machine, prop = 0.7)
train <- training(split)
test <- testing(split)

m <- glm(formula = failure ~ delta, data = train, family = 'binomial')
summary(m)

intercept = m$coefficients[1]
coefficient = m$coefficients[2]

##a

##b
###Plotting the sigmoid function
summary(txns$delta)
lr <- function(x) 1/(1+exp(-(intercept + coefficient*x)))
curve(lr, from = 0, to = 25)


##c
### i) Finding the threshold deltas
lr_med <- function(x) (1/(1+exp(-(intercept + coefficient*x))))-0.6
lr_high <- function(x) (1/(1+exp(-(intercept + coefficient*x))))-0.8

med_threshold <- uniroot(lr_med, c(0,25))$root
high_threshold <- uniroot(lr_high, c(0,25))$root

### ii) Average triggers per day for each threshold
avg_daily_med_alarms = nrow(txns %>% filter(delta > med_threshold)) / uniqueN(txns$date)
avg_daily_high_alarms = nrow(txns %>% filter(delta > high_threshold)) / uniqueN(txns$date)

### iii) % of false alarms
total_med_alarms = nrow(txns %>% filter(delta > med_threshold))
total_med_false_alarms = nrow(txns %>% filter((delta > med_threshold) & (failure==0)))
percent_med_false_alarms = total_med_false_alarms/total_med_alarms

total_high_alarms = nrow(txns %>% filter(delta > high_threshold))
total_high_false_alarms = nrow(txns %>% filter((delta > high_threshold) & (failure==0)))
percent_high_false_alarms = total_high_false_alarms/total_high_alarms


##d
current_profit = nrow(txns)*1.7

### IF WE ONLY IMPLEMENT MEDIUM RISK ALARMS
med_alarm <- txns %>% filter(delta > med_threshold)
med_alarm$threshold_hours <- med_threshold*(24/med_alarm$daily_sales_machine)
med_alarm$delta_fixed <- (med_alarm$threshold_hours+1.5)*(med_alarm$daily_sales_machine/24)
med_alarm$won_sales <- (med_alarm$delta - med_alarm$delta_fixed)*med_alarm$failure

added_revenue_med = sum(med_alarm$won_sales)*1.7*(12/3)
added_cost_med = uniqueN(txns$machine)*10*2.2
added_profit_med = added_revenue_med-added_cost_med

profit_uptick_med = (added_profit_med/current_profit)*100

### IF WE ONLY IMPLEMENT HIGH RISK ALARMS
high_alarm <- txns %>% filter(delta > high_threshold)
high_alarm$threshold_hours <- high_threshold*(24/high_alarm$daily_sales_machine)
high_alarm$delta_fixed <- (high_alarm$threshold_hours+1.5)*(high_alarm$daily_sales_machine/24)
high_alarm$won_sales <- (high_alarm$delta - high_alarm$delta_fixed)*high_alarm$failure

added_revenue_high = sum(high_alarm$won_sales)*1.7*(12/3)
added_cost_high = uniqueN(txns$machine)*10*2.2
added_profit_high = added_revenue_high-added_cost_high

profit_uptick_high = (added_profit_high/current_profit)*100