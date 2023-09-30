#Task 1
#Preliminary Data Analysis And EDA:
# Load packages
library(ggplot2)
library(tidyverse)
library(lubridate)
library(data.table)
library(DescTools)
library(cluster)
library(GGally)
library(DBI)
library(RMySQL)
library(XML)
library(RCurl)
library(rvest)
library(magrittr)

# Load data
customerDF <- fread('customer_shopping_data.csv')

head(customerDF)

dim(customerDF)

# Extract by sampling
customerDF <- customerDF %>% mutate(invoice_date = as.Date(invoice_date,'%d/%m/%Y'))
customerDF <- customerDF %>% mutate(total_sale = price*quantity)
customerDF <- customerDF %>% mutate(age_group = ifelse(age < 20, "10's",
                                                       ifelse(age < 30, "20's",
                                                              ifelse(age < 40, "30's",
                                                                     ifelse(age < 50, "40's",
                                                                            ifelse(age < 60, "50's", "60's")))))) %>% 
  dplyr::select(shopping_mall, invoice_date, invoice_no, customer_id, gender, age, age_group, category, quantity, price, total_sale,
                payment_method)

summary(customerDF)

summary(customerDF[, c("age", "quantity", "price"), on = c("column1", "column2")])

sapply(customerDF[, c("shopping_mall", "gender", "category", "payment_method", "age_group")], function(x) length(unique(x)))

table(customerDF$shopping_mall)
table(customerDF$gender)
table(customerDF$category)
table(customerDF$payment_method)
table(customerDF$age_group)

price_range <- range(customerDF$price, na.rm = TRUE)
cat("Price range: ", price_range[1], " - ", price_range[2], "\n")

price_sd <- sd(customerDF$price, na.rm = TRUE)
cat("Price standard deviation: ", price_sd, "\n")

price_var <- var(customerDF$price, na.rm = TRUE)
cat("Price variance: ", price_var, "\n")

price_iqr <- IQR(customerDF$price, na.rm = TRUE)
cat("Price interquartile range: ", price_iqr, "\n")

ggplot(customerDF, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Price") +
  xlab("Price") +
  ylab("Frequency")

#Develop box plots, histograms and frequency tables/charts using the plotting features of R:
customerDF %>% ggplot(data = customerDF, mapping = aes(x = factor(quantity), y = total_sale*0.052)) +
  geom_boxplot() +
  xlab("Quantity") +
  ylab("Total Sales in USD") +
  ggtitle("Box Plot for Quantites and Total Sales ($)")

customerDF %>% ggplot(data = customerDF, mapping = aes(x = factor(payment_method), y = quantity)) +
  geom_boxplot() +
  xlab("Payment Method") +
  ylab("Quantity") +
  ggtitle("Box Plot for Quantites and Payment Method")

#Develop histograms:
customerDF %>% ggplot(data = customerDF, mapping = aes(x = shopping_mall)) +
  geom_histogram(binwidth = 5, fill = 'skyblue', col = 'black', stat = "count") + facet_wrap(~ category) + 
  xlab("Shopping Mall") +
  ylab("Frequency") +
  ggtitle("Histogram for Shopping Mall by Category") +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        axis.title = element_text(face = 'italic'),
        axis.text = element_text(face = 'bold', size = 5.5, angle = 35))

customerDF %>% ggplot(data = customerDF, mapping = aes(x = category)) +
  geom_histogram(binwidth = 5, fill = 'brown', col = 'black', stat = "count") + facet_wrap(~ gender) + 
  xlab("Category") +
  ylab("Frequency") +
  ggtitle("Histogram for Category by Gender") +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        axis.title = element_text(face = 'italic'),
        axis.text = element_text(face = 'bold', size = 7, angle = 20))

customerDF %>% ggplot(data = customerDF, mapping = aes(x = age_group)) +
  geom_histogram(binwidth = 5, fill = 'darkblue', col = 'black', stat = "count") + facet_wrap(~ category) + 
  xlab("Age Group") +
  ylab("Frequency") +
  ggtitle("Histogram for Age Group by Category") +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        axis.title = element_text(face = 'italic'),
        axis.text = element_text(face = 'bold', size = 7, angle = 0))
frequency_table1 <- customerDF %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(frequency_table1, aes(x = "", y = percentage, fill = gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Pie Chart for Gender") + labs(fill= "Gender") +
  theme_void()

frequency_table2 <- customerDF %>%
  count(age_group) %>%
  mutate(percentage = n / sum(n) * 100) 

ggplot(frequency_table2, aes(x = "", y = percentage, fill = age_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Pie Chart for Age Group") + labs(fill= "Age Group") +
  theme_void()

frequency_table3 <- customerDF %>%
  count(category) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(frequency_table3, aes(x = "", y = percentage, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Pie Chart for Category") + labs(fill= "Category") +  theme_void()

frequency_table4 <- customerDF %>%
  count(shopping_mall) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(frequency_table4, aes(x = "", y = percentage, fill = shopping_mall)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Pie Chart for Shopping Mall") + labs(fill= "Shopping Mall") +  theme_void()

#Choosing sub-groups and plotting some charts with faceting
gender_vector <- customerDF %>% group_by(gender, shopping_mall) %>% summarise(customer = n(), sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% arrange(sales_USD %>% desc()) 
gender_vector %>% 
  ggplot(aes(reorder(shopping_mall, sales_USD), sales_USD)) +
  geom_col(position = 'dodge', fill = 'darkblue', col = 'black', alpha = .7) +
  coord_flip() + 
  facet_wrap(~ gender) +
  labs(title = 'Consumption by gender by shopping mall',
       subtitle = '1 = 1,000$') +
  ylab('Sales') +
  xlab('Shopping Mall') +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        axis.title.y = element_text(face = 'italic'),
        axis.title.x = element_text(face = 'italic'),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        strip.text.x = element_text(face = 'bold' , color = 'white', size = 7),
        strip.background.x = element_rect(fill = 'darkred'))

age_vector <- customerDF %>% group_by(age_group,category) %>% summarise(customer = n(), sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% arrange(sales_USD %>% desc()) 
age_vector %>% 
  ggplot(aes(reorder(category, sales_USD), sales_USD)) +
  geom_col(position = 'dodge', fill = 'yellow', col = 'black', alpha = .7) +
  coord_flip() + 
  facet_wrap(~ age_group) +
  labs(title = 'Consumption by age group by category',
       subtitle = '1 = 1,000$') +
  ylab('Sales') +
  xlab('Category') +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        axis.title.y = element_text(face = 'italic'),
        axis.title.x = element_text(face = 'italic'),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        strip.text.x = element_text(face = 'bold' , color = 'white', size = 7),
        strip.background.x = element_rect(fill = 'darkred'))

category_vector <- customerDF %>% group_by(category,payment_method) %>% summarise(customer = n(), sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% arrange(sales_USD %>% desc()) 
category_vector %>% 
  ggplot(aes(reorder(payment_method, sales_USD), sales_USD)) +
  geom_col(position = 'dodge', fill = 'purple', col = 'black', alpha = .7) +
  coord_flip() + 
  facet_wrap(~ category) +
  labs(title = 'Consumption by category by payment method',
       subtitle = '1 = 1,000$') +
  ylab('Sales') +
  xlab('Payment Method') +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        axis.title.y = element_text(face = 'italic'),
        axis.title.x = element_text(face = 'italic'),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        strip.text.x = element_text(face = 'bold' , color = 'white', size = 7),
        strip.background.x = element_rect(fill = 'darkred'))

mall_vector <- customerDF %>% group_by(shopping_mall) %>% summarise(customer = n(), sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% arrange(sales_USD %>% desc())
mall_vector %>% 
  ggplot() + 
  geom_col(aes(reorder(shopping_mall, sales_USD), sales_USD), 
           fill = 'darkgreen', col = 'black', alpha = .6,width = .7) +
  coord_flip() + 
  labs(title = 'Shopping Mall Ranking', subtitle = '1 = 1,000$') + ylab('Total Sales in USD') +
  xlab('') +
  theme(plot.title = element_text(size = 10, face = 'bold', hjust = .5),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold', size = 7),
        axis.title.x = element_text(face = 'italic', size = 7),
        axis.text.x = element_text(face = 'bold'))
payment_vector <- customerDF %>% group_by(payment_method, age_group) %>% summarise(customer = n(),sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% arrange(sales_USD %>% desc())
payment_vector %>% 
  ggplot(aes(reorder(age_group, sales_USD), sales_USD)) +
  geom_col(position = 'dodge', fill = 'orange', col = 'black', alpha = .7) +
  coord_flip() + 
  facet_wrap(~ payment_method) +
  labs(title = 'Consumption by payment method by age group ',
       subtitle = '1 = 1,000$') +
  ylab('Sales') +
  xlab('Payment Method') +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        axis.title.y = element_text(face = 'italic'),
        axis.title.x = element_text(face = 'italic'),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        strip.text.x = element_text(face = 'bold' , color = 'white', size = 7),
        strip.background.x = element_rect(fill = 'darkred'))
customerDF %>% group_by(shopping_mall, age_group, category) %>% 
  summarise(sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% 
  filter(category %in% c('Clothing','Shoes','Technology','Cosmetics')) %>%
  ggplot(aes(reorder(category, -sales_USD), sales_USD, fill = age_group)) +
  geom_col(position = 'dodge', col = 'black', alpha = .7) +
  facet_wrap(~ shopping_mall) + 
  labs(title = 'Shopping malls preferred by customers by age group filtering top 4 categories',
       subtitle = '1 = 1,000$', fill = "Age Group") +
  xlab('Category') + ylab('Total Sales in USD') + theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        strip.background.x = element_rect(fill = 'darkgreen'),
        strip.text.x = element_text(face = 'bold', color = 'white', size = 7),
        axis.title = element_text(face = 'italic'),
        axis.text = element_text(face = 'bold', size = 7, angle = 20)) +
  scale_fill_brewer(palette = 'Set1')

customerDF %>% mutate(date2 = invoice_date %>% str_sub(1,7) %>% ym()) %>% 
  group_by(date2, shopping_mall) %>% 
  summarise(sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>%
  filter(date2 <= '2023-02-01') %>% 
  ggplot(aes(date2, sales_USD, col = shopping_mall)) +
  geom_line(linewidth = .5, alpha = .7) +
  theme_bw() +
  facet_wrap(~ shopping_mall) +
  labs(title = 'Time series plot of sales',
       subtitle = '1 = 1,000$') +
  xlab('') + ylab('Total Sales in USD') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        strip.background = element_rect(fill = '#fd8d3c'),
        strip.text = element_text(face = 'bold',colour = 'white', size = 7),
        axis.title = element_text(face = 'italic'),
        axis.text = element_text(face = 'bold', size = 7, angle = 25),
        legend.position = 'none') +
  scale_x_date(date_breaks = '5 month', date_labels = '%Y-%m')

#Display the correlations between various combinations of variables
gender_vector %>% 
  ggplot() + geom_point(aes(customer, sales_USD, color=gender)) + facet_wrap(~gender) + labs(title = "Gender Spending Scatter Plot") +  xlab('Customer') + ylab('Total Sales in USD') + guides(color = guide_legend(title = "Gender"))

customerDF %>% group_by(category, age_group, quantity,payment_method) %>% summarise(sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% 
  ggplot() + geom_point(aes(quantity, sales_USD, color=payment_method)) + facet_wrap(. ~category) + labs(title = "Purchasing Behavior by Category Scatter Plot",subtitle = '1 = 1,000$') + xlab('Quantity') + ylab('Total Sales in USD') + guides(color = guide_legend(title = "Payment Method")) + theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        strip.background.x = element_rect(fill = 'darkorange'),
        strip.text.x = element_text(face = 'bold', color = 'white', size = 7),
        axis.title = element_text(face = 'italic'),
        axis.text.y = element_text(face = 'bold', size = 7, angle = 20)) +
  scale_fill_brewer(palette = 'Set1')

customerDF %>% group_by(shopping_mall, age_group, category) %>% 
  summarise(sales_USD = sum(total_sale*0.052/1000), .groups = "drop") %>% 
  filter(category %in% c('Clothing','Shoes','Technology','Cosmetics')) %>%
  ggplot(aes(reorder(category, -sales_USD), sales_USD, color = age_group)) +
  geom_point() +
  facet_wrap(~ shopping_mall) + 
  labs(title = 'Shopping malls preferred by customers by age group filtering top 4 categories',
       subtitle = '1 = 1,000$') +
  xlab('Category') + ylab('Total Sales in USD') + guides(color = guide_legend(title = "Age Group")) +   theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = .5),
        strip.background.x = element_rect(fill = 'darkblue'),
        strip.text.x = element_text(face = 'bold', color = 'white', size = 7),
        axis.title = element_text(face = 'italic'),
        axis.text = element_text(face = 'bold', size = 7, angle = 20)) +
  scale_fill_brewer(palette = 'Set1')
#Regression:
#Hypothesis 1: Age and Gender Impact Shopping Behavior
shopping_model1 <- lm(quantity ~ age + gender, data = customerDF)
summary(shopping_model1)
ggplot(data = customerDF, aes(x = age, y = quantity, color = gender)) +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line
  labs(x = "Age", y = "Quantity", title = "Regression of Quantity on Age by Gender") + guides(color = guide_legend(title = "Gender"))

#Hypothesis 2: Payment Method Affects Purchase Amount
shopping_model2 <- lm(price ~ payment_method, data = customerDF)
summary(shopping_model2)
ggplot(data = customerDF, aes(x = payment_method, y = price)) +
  geom_boxplot() +
  labs(x = "Payment Method", y = "Price", title = "Boxplot of Price by Payment Method")
#Hypothesis 3: Shopping Mall Location Influences Purchase Frequency
shopping_model3 <- lm(quantity ~ shopping_mall, data = customerDF)
summary(shopping_model3)
ggplot() +
  geom_col(data = customerDF, aes(x = shopping_mall, y = quantity), fill = 'darkgreen') +
  labs(x = "Shopping Mall", y = "Quantity", title = "Bar chart of Quantity by Shopping Malls") +
  theme(axis.text = element_text(face = 'bold', size = 7, angle = 20))

#Hypothesis 4: Product Category Affects Purchase Amount
shopping_model4 <- lm(price ~ category, data = customerDF)
summary(shopping_model4)
ggplot(customerDF, aes(x = category, y = price)) +
  geom_boxplot() +
  labs(x = "Product Category", y = "Price") +
  ggtitle("Boxplot of Price by Product Category")

head(customerDF)

