product_data <- read.csv("final_data/product_data.csv")
machine_data <- read.csv("final_data/machine_data.csv")
transactional_data <- read.csv("final_data/transactional_data.csv")

#1
#1a There are 2495 items
library(dplyr) 
str(machine_data) 

#1b 38.4% are small
percentage <- machine_data %>% group_by(small_machine) %>% summarise( percent = 100 * n() / nrow( machine_data ) )


#1c 
# distribution:
#location_type  percent
#<chr>            <dbl>
#  1 others         27.7   
#2 petrol station 13.7   
#3 transport      58.5   
#4 NA              0.0401
location_percentage <- machine_data %>% group_by(location_type) %>% summarise( percent = 100 * n() / nrow( machine_data ) )


#1d There are 63 products. Carbonates and energy drinks havs the highest
num_of_product <-str(product_data) 
category_of_product<-product_data %>% group_by(category) %>% summarise( percent = 100 * n() / nrow( product_data ) )  

#1e
#Highest:Milk based- 3.42; Lowest: Sugar candy- 2.3
category_price <-product_data %>% group_by(category) %>% summarise( mean_value = mean(price)) %>% arrange(mean_value) 
#Snack-2.42; Drink-3.18
snack_and_drink_price<-product_data %>% group_by(type_drink_snack) %>% summarise( mean_value2 = mean(price)) %>% arrange(mean_value2) 

#1f 
  #Result: 0- 14666; 1- 7058
  #Reasons:
    #1.big machines has more variety in product 
    #2.the capacity of small machine is less than big machine which attributes to the less total sales

transactional_data$date <- as.Date(transactional_data$date, format = "%Y-%m-%d")
small_big <- transactional_data %>% 
  filter(format(date, "%m") == "03" & format(date, "%Y") == "2017") %>% 
  left_join(machine_data, by = "machine") %>% 
  select(machine, small_machine,timestamp,date,column,product_name) %>% 
  group_by(small_machine) %>%
  summarise(mean1 = length(timestamp)/length(unique(date)))


#2
#2a 
  #general trend of drinks: (increase and steady) from the graph we can see generally there is a steady rise in the lower bond; for the upper bond it increases steadily from JAn to late Feb but then followed by a dramatic increase. Then the upper bond fluctuates in a specific range ever since
  #general trend of snacks: (fluctuating without a clear trend) there is a rise in the upper bond from Jan to Feb but since Feb it fluctuates and reaches the peaks in late Feb. For lower bond it also fluctuates during the time and reveals a slight decline since late Feb to April.
  #the trends are not the same, the demand for the snacks are pretty much the same in each season while in winter the demand for drinks drops

#2b 
  #specific of drinks: for the upper bond it increases steadily from JAn to late Feb but then followed by a dramatic increase. Then the upper bond fluctuates in a specific range ever since
  #specific for snacks:there is a rise in the upper bond from Jan to Feb but since Feb it fluctuates and reaches the peaks in late Feb. For lower bond it also fluctuates during the time and reveals a slight decline since late Feb to April.
  #each period of hitting the lower bond and higher bond corresponds (due to the inventory), and they reach the peak at the same time which might be due to the ocassional event occuring in the period


#3 outliers
  #3a
library(ggplot2)
summary(machine_data$income_average)
    #boxplot
boxplot(machine_data$income_average,
        ylab = "income average"
)
    #getting the outlier
boxplot.stats(machine_data$income_average)$out
length(boxplot.stats(machine_data$income_average)$out)

    #getting the row number of the outlier
out <- boxplot.stats(machine_data$income_average)$out
out_ind <- which(machine_data$income_average %in% c(out))
out_ind

    #percentage in all cases of the outlier
percent<-length(out)/length(machine_data$income_average)
    #how to treat these outliers: from the boxplot I would removed all values outside of the boxplot
machine_data <- machine_data[-out_ind,]




#3b
  #There are 3 options we have: 1. Deletion: Removing rows or columns with missing values. 2.Imputation: Filling in the missing values with a substitute value such as the mean, median or mode of the data. 3.Interpolation: Using statistical methods to estimate missing values based on the values of other observations in the data.  
  #Our choice:imputation -getting the daily average per item per machine
  #Because from the plot (below) we can see that the missing values in income_average correspond to the values which are within the non-NA's box in average daily items, so it would be the best to not delete those NA columns, as the value in average daily items are quite representative and useful
  #So I choose to do imputation with mean strategy


daily_average_item_machine<-transactional_data %>% 

  left_join(machine_data, by = "machine") %>% 
  select(machine, small_machine,timestamp,date,column,product_name) %>% 
  group_by(machine) %>%
  summarise(mean_machine = length(timestamp)/length(unique(date)))

    #create a new dataframe to see the relation between NA and average daily items
newdf <- daily_average_item_machine %>% 
  left_join(machine_data, by="machine") %>%
  select(machine, mean_machine, income_average)
    #relation
range_na <-newdf %>% group_by(is.na(income_average)) %>% summarise(range_meanmachine = range(mean_machine, na.rm = TRUE))
mean_na <-newdf %>% group_by(is.na(income_average)) %>% summarise(mean_meanmachine = mean(mean_machine, na.rm = TRUE))
ggplot(newdf, aes(x = factor(is.na(income_average)), y = mean_machine)) + geom_boxplot()
    #from the plot we can see that the missing values in income_average correspond to the values which are within the non-NA's box in average daily items, so it would be the best to not delete those NA columns, as the value in average daily items are quite representative and useful
    #so i choose to do imputation with mean strategy

mean_income_average <- mean(machine_data$income_average, na.rm = TRUE)
machine_data$income_average <- ifelse(is.na(machine_data$income_average), mean_income_average, machine_data$income_average)


#4. 1.0


#5
library(data.table)
library(tidyverse)
library(tidymodels)
library(GGally)
library(corrplot)

    #get daily item into machine_data
machine_data <-machine_data %>% left_join(daily_average_item_machine,by="machine")

    #data processing
machine_data <- machine_data %>% 
  mutate(train_AvgDailyPassengers_new = ifelse(is.na(train_AvgDailyPassengers),0,1))  #for train_AvgDailyPassengers
    #fill na in total_number_of_routes_600 with mean strategy
mean_600 <- mean(machine_data$total_number_of_routes_600, na.rm = TRUE)
machine_data$total_number_of_routes_600 <- ifelse(is.na(machine_data$total_number_of_routes_600), mean_600, machine_data$total_number_of_routes_600)

    #begin to build model
set.seed(2022)
machine_lm_model <- lm(formula = mean_machine ~ num_vendex_nearby_300 + small_machine + num_hotels_45 + total_number_of_routes_600 + income_average + train_AvgDailyPassengers_new, data = machine_data)
summary(machine_lm_model)

#5a 
  #not all variables show statistical significance. The one that doesn't- num_vendex_nearby_300, total_number_of_routes_600, income_average; how do i know: Pr(>|t|)>significanxe level
  
#5b 
  #yes it shows significance in  0.0001level

    #getting log_transport
machine_data2 <- machine_data %>% 
  mutate(log_transport = log10(total_number_of_routes_600))
    #building model
machine_lm_model2 <- lm(formula = mean_machine ~ num_vendex_nearby_300 + small_machine + num_hotels_45 + log_transport + income_average + train_AvgDailyPassengers_new, data = machine_data2)
summary(machine_lm_model2)
    #so it shows significance in  0.0001level

    #remove income_average as it doesn't show enough significance and build final model
final_model <-lm(formula = mean_machine ~ num_vendex_nearby_300 + small_machine + num_hotels_45 + log_transport + train_AvgDailyPassengers_new, data = machine_data2)


#5c 
  #It would be [-0.01756, -3.94444] = [-1.981e+00 -2.086e-01*-9.500,-1.981e+00 +2.086e-01*-9.500] 
  #meaning there might be 0 to 3.9 daily items less that small machine sells
summary(final_model)



#5d
  #the interval:[-0.000386, -0.186844] =[-0.09352 -(0.02581 *-3.624),-0.09352 +(0.02581 *-3.624))]
  #meaning if there is one more machine nearby, the sales on current will drop by from 0 to 0.187 daily

#5e
    #prediction
machine_lm_predictions <- machine_data2 %>% 
  bind_cols(predict(final_model, newdata = machine_data2)) %>% 
  rename(predictions = ...15) %>%
  arrange(desc(predictions))

    #getting top and bottom 20
top_20_index <- top_20_index <- ceiling(0.2 * nrow(machine_lm_predictions))
bottom_20_index <- floor(0.8 * nrow(machine_lm_predictions))

top_20_real_sales <- machine_lm_predictions$mean_machine[1:top_20_index]
bottom_20_real_sales <- machine_lm_predictions$mean_machine[bottom_20_index:nrow(machine_lm_predictions)]
ratio <- mean(top_20_real_sales) / mean(bottom_20_real_sales,na.rm = TRUE)

#5f
  #I will choose the first one location i

    #location i
new_data1 <-data.frame(
  num_vendex_nearby_300 = 0, 
  small_machine = 0,
  num_hotels_45 = 2,
  log_transport = log10(20) ,
  train_AvgDailyPassengers_new = 0
)
    #location ii
new_data2 <-data.frame(
  num_vendex_nearby_300 = 3, 
  small_machine = 0,
  num_hotels_45 = 0,
  log_transport = log10(10) ,
  train_AvgDailyPassengers_new = 1
)



predict(final_model, newdata = new_data1)
predict(final_model, newdata = new_data2)


  #first one (i) is 8.745 and second one (ii) is 7.35, I will choose the first one (i)
  # to sum up, I will choose the location i to set up the machine
