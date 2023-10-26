# Load the dataset
data <- read.csv("sales_data.csv")


# Convert month_name to factor
data$month_name <- factor(data$month_name, levels=c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")) 
is.factor(data$month_name)

#1. Data Exploration
# Load ggplot2 and dplyr library
library(ggplot2)
library(dplyr)

#plot a histogram for temperature
ggplot(data, aes(x = temperature)) +
  geom_histogram(binwidth = 1.5, fill = "blue", color = "green") +
  labs(
    title = "Temperature Distribution for the data",
    x = "Average temperature in degrees celcius",
    y = "Frequency"
  ) +
  theme_minimal()

#  histogram plot for humidity
ggplot(data, aes(x = humidity)) +
  geom_histogram(binwidth = 5, fill = "navy", color = "black") +
  labs(
    title = "Humidity Distribution",
    x = "Average Humidity as %",
    y = "Frequency"
  ) +
  theme_minimal()




# Calculate the average ice cream sales per month
monthly_sales <- data %>%
  group_by(month_name) %>%        #group by the month name
  summarize(average_icecream_sales = mean(icecream_sales))

# make a bar plot for monthly ice cream sales
ggplot(monthly_sales, aes(x = month_name, y = average_icecream_sales)) +
  geom_bar(stat = "identity", fill = "maroon") +
  labs(
    title = "Mean Ice Cream Sales/Month",
    x = "Month of the Year",
    y = "Mean ice cream sales"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 2))



#Calculate the average hot drink sales per month
monthly_sales <- data %>%
  group_by(month_name) %>%        #group by the month name
  summarize(average_hotdrink_sales = mean(hotdrink_sales))

# make a bar plot for monthly ice cream sales
ggplot(monthly_sales, aes(x = month_name, y = average_hotdrink_sales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Mean Hot Drink Sales/Month",
    x = "Month of the Year",
    y = "Mean hot sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 2))



#2.1 Estimate the Proportion of Days with Fewer than 200 Ice Cream Sales

# Calculating the proportion of days with fewer than 200 ice cream sales
prop_less_than_200_icecream <- mean(data$icecream_sales < 200)

# Calculate the 95% confidence interval for the proportion
conf_interval <- prop.test(sum(data$icecream_sales < 200), nrow(data), conf.level = 0.95)

# Displaying the results from the codes
prop_less_than_200_icecream
conf_interval


#2.2 Estimate the Proportion of Days with Fewer than 200 Total Sales
# Calculate total sales
data_T<- data %>% mutate(total_sales = icecream_sales + hotdrink_sales)

# Calculate the proportion of days with fewer than 200 total sales
total_sales <- data_T %>% filter(total_sales < 200)
prop_total_sales <- nrow(total_sales) / nrow(data_T)

# Calculate 95% confidence interval
ci_total_sales <- prop.test(nrow(total_sales), nrow(data_T), conf.level = 0.95)

# Print results
prop_total_sales
ci_total_sales

#2.3 Estimate the Odds Ratios for January and August

#  group the data for January and August using subset function
jan_data <- subset(data, month_name == "Jan")
aug_data <- subset(data, month_name == "Aug")

# Calculate the odds ratio for January
odds_jan <- sum(jan_data$icecream_sales > 0) / sum(jan_data$hotdrink_sales > 0)

# Calculate the odds ratio for August
odds_aug <- sum(aug_data$icecream_sales > 0) / sum(aug_data$hotdrink_sales > 0)

# Calculate 95% confidence intervals for both odds ratios
CI_jan <- exp(c(log(odds_jan) - 1.96 * sqrt(1 / sum(jan_data$icecream_sales > 0) + 1 / sum(jan_data$hotdrink_sales > 0)),
                               log(odds_jan) + 1.96 * sqrt(1 / sum(jan_data$icecream_sales > 0) + 1 / sum(jan_data$hotdrink_sales > 0))))

CI_aug <- exp(c(log(odds_aug) - 1.96 * sqrt(1 / sum(aug_data$icecream_sales > 0) + 1 / sum(aug_data$hotdrink_sales > 0)),
                              log(odds_aug) + 1.96 * sqrt(1 / sum(aug_data$icecream_sales > 0) + 1 / sum(aug_data$hotdrink_sales > 0))))

#Print the results
odds_jan
CI_jan
odds_aug
CI_aug


#2.4 Test for Significant Difference in Odds Ratios

# Create a binary variable for ice cream purchase
data <- data %>% mutate(icecream_purchase = ifelse(icecream_sales > 0, 1, 0))

# Fit logistic regression model
model <- glm(icecream_purchase ~ month_name, family = "binomial", data = data)

# Perform ANOVA test
anova_test <- anova(model, test="Chisq")

# Check the p-value for the month_name effect
p_value <- anova_test$`Pr(>Chi)`[2]
cat("P-value for the difference in odds ratios between months: ", round(p_value, 6), "\n")



#3.1

# Filtering the data on weekdays (Mon-Fri) and weekends
weekday_data <- data %>% filter(weekend == 0)
weekend_data <- data %>% filter(weekend == 1)

#mean number of ice cream sales on weekdays and weekends
meansales_weekday <- mean(weekday_data$icecream_sales)
meansales_weekend <- mean(weekend_data$icecream_sales)



# conduct a t-test to compare the means of ice cream sales
t_test <- t.test(weekday_data$icecream_sales, weekend_data$icecream_sales)


# Display the results
cat("Mean Ice Cream Sales on Weekdays:", meansales_weekday, "\n")
cat("Mean Ice Cream Sales on Weekends:", meansales_weekend, "\n")
cat("T-Test P-Value:", t_test$p.value, "\n")


#3.2. 

# Sample means
mean_1 <- mean(data$weekend==0)    #mean week days
mean_2 <- mean(data$weekend==1)    #mean weekends


# Pooled standard deviation
n_1 <- 52       #total week day
n_2 <- 51       #total weekend days
sd_pooled <- sqrt(((n_1 - 1) * sd(data$weekend==0)^2 + (n_2 - 1) * sd(data$weekend==1)^2) / (n_1 + n_2 - 2))

# Calculate effect size
d <- (mean_1 - mean_2) / sd_pooled

# Compute power
power <- pwr.t.test(d = d, n = n_1, sig.level = 0.05, type = "two.sample", alternative = "two.sided")

# Print power
print(power$power)

n<-length(data)

#3.3 Determine required effect size for 90% power use the pwr.t.test function to solve for effect size. 
required_effect_size <- pwr.t.test(power = 0.9, n = n, sig.level = 0.05, type = "two.sample")$d
cat("Required effect size for 90% power:", required_effect_size, "\n")


#3.4. Determine required sample size for 90% power with observed effect size
d<-0.4538371  #effective size
power<-0.90

# Calculate required sample size for 90% power and given effect size
required_n <- pwr.t.test(d = d, power = 0.9, sig.level = 0.05)$n
cat("Required sample size for 90% power: ", required_n, "\n")



#4.1 Building the Multilinear Regression Model
#create a regression model using the lm()  in R.

model <- lm(icecream_sales ~ temperature + humidity + windspeed + weekend + bank_holiday + school_holidays, data = data)
summary(model)


#4.2 Estimating Sales under Specific Conditions
#use the predict() function to estimate the number of ice cream sales under specific conditions. You'll need to create new data frames for each set of conditions and then use these for prediction.       

# Creating new data frames for predictions given
new_data_1 <- data.frame(temperature = 18, humidity = 6, windspeed = 10, weekend = 0, bank_holiday = 0, school_holidays = 0, month_name = "May")
new_data_2 <- data.frame(temperature = 28, humidity = 35, windspeed = 5, weekend = 1, bank_holiday = 0, school_holidays = 1, month_name = "April")
new_data_3 <- data.frame(temperature = 12, humidity = 90, windspeed = 35, weekend = 0, bank_holiday = 0, school_holidays = 0, month_name = "September")
new_data_4 <- data.frame(temperature = -2, humidity = 75, windspeed = 15, weekend = 1, bank_holiday = 0, school_holidays = 0, month_name = "January")

# Making predictions
pred_1 <- predict(model, newdata = new_data_1, interval = "confidence")
pred_2 <- predict(model, newdata = new_data_2, interval = "confidence")
pred_3 <- predict(model, newdata = new_data_3, interval = "confidence")
pred_4 <- predict(model, newdata = new_data_4, interval = "confidence")

# Printing predictions
cat("Week day in May: ", pred_1, "\n")
cat("School holiday on weekend in April: ", pred_2, "\n")
cat("Week day in September: ", pred_3, "\n")
cat("January weekend (not a holiday): ", pred_4, "\n")
