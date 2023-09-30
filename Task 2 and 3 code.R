#Task 2.1
# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")

# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)

# Extract theta hat (model coefficients) for each model
theta_hat_model1 <- coef(model1)
theta_hat_model2 <- coef(model2)
theta_hat_model3 <- coef(model3)
theta_hat_model4 <- coef(model4)
theta_hat_model5 <- coef(model5)

# Create a data frame to display the coefficients
coefficients_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Theta1 = c(theta_hat_model1[1], theta_hat_model2[1], theta_hat_model3[1], theta_hat_model4[1], theta_hat_model5[1]),
  Theta2 = c(theta_hat_model1[2], theta_hat_model2[2], theta_hat_model3[2], theta_hat_model4[2], theta_hat_model5[2]),
  Theta3 = c(theta_hat_model1[3], theta_hat_model2[3], theta_hat_model3[3], theta_hat_model4[3], theta_hat_model5[3]),
  Theta4 = c(theta_hat_model1[4], theta_hat_model2[4], theta_hat_model3[4], theta_hat_model4[4], theta_hat_model5[4]),
  Theta5 = c(theta_hat_model1[5], theta_hat_model2[5], theta_hat_model3[5], theta_hat_model4[5], theta_hat_model5[5]),
  ThetaBase = c(theta_hat_model1[6], theta_hat_model2[6], theta_hat_model3[6], theta_hat_model4[6], theta_hat_model5[6])
)

# Print the coefficients data frame
print(coefficients_df)

#Task 2.2
# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")
# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)

# Calculate RSS for each model
rss_model1 <- sum(model1$residuals^2)
rss_model2 <- sum(model2$residuals^2)
rss_model3 <- sum(model3$residuals^2)
rss_model4 <- sum(model4$residuals^2)
rss_model5 <- sum(model5$residuals^2)

# Create a data frame to display the RSS values
rss_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  RSS = c(rss_model1, rss_model2, rss_model3, rss_model4, rss_model5)
)

# Print the RSS data frame
print(rss_df)

#Task 2.3
#Task 2.3 -log-likelihood-age
# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")

# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)


# Function to compute log-likelihood
log_likelihood <- function(model) {
  residuals <- residuals(model)
  n <- length(residuals)
  variance <- var(residuals)
  log_lik <- -0.5 * n * log(2 * pi) - 0.5 * n * log(variance) - 0.5 * sum((residuals^2) / variance)
  return(log_lik)
}

# Compute log-likelihood for each model
log_lik_model1 <- log_likelihood(model1)
log_lik_model2 <- log_likelihood(model2)
log_lik_model3 <- log_likelihood(model3)
log_lik_model4 <- log_likelihood(model4)
log_lik_model5 <- log_likelihood(model5)

# Create a data frame to display the log-likelihood values
log_lik_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  LogLikelihood = c(log_lik_model1, log_lik_model2, log_lik_model3, log_lik_model4, log_lik_model5)
)

# Print the log-likelihood data frame
print(log_lik_df)

#Task 2.4 --BIC- AIC--
# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")

# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)


# Function to compute AIC
compute_aic <- function(model) {
  n <- length(model$residuals)
  k <- length(model$coefficients)
  rss <- sum(model$residuals^2)
  aic <- n * log(rss / n) + 2 * k
  return(aic)
}

# Function to compute BIC
compute_bic <- function(model) {
  n <- length(model$residuals)
  k <- length(model$coefficients)
  rss <- sum(model$residuals^2)
  bic <- n * log(rss / n) + k * log(n)
  return(bic)
}

# Compute AIC and BIC for each model
aic_model1 <- compute_aic(model1)
aic_model2 <- compute_aic(model2)
aic_model3 <- compute_aic(model3)
aic_model4 <- compute_aic(model4)
aic_model5 <- compute_aic(model5)

bic_model1 <- compute_bic(model1)
bic_model2 <- compute_bic(model2)
bic_model3 <- compute_bic(model3)
bic_model4 <- compute_bic(model4)
bic_model5 <- compute_bic(model5)

# Create a data frame to display the AIC and BIC values
aic_bic_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  AIC = c(aic_model1, aic_model2, aic_model3, aic_model4, aic_model5),
  BIC = c(bic_model1, bic_model2, bic_model3, bic_model4, bic_model5)
)

# Print the AIC and BIC data frame
print(aic_bic_df)

#Task 2.5 QQ plots
# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")

# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)
# Function to compute log-likelihood
compute_log_likelihood <- function(model) {
  residuals <- residuals(model)
  n <- length(residuals)
  variance <- var(residuals)
  log_lik <- -0.5 * n * log(2 * pi) - 0.5 * n * log(variance) - 0.5 * sum((residuals^2) / variance)
  return(log_lik)
}

# Function to compute AIC
compute_aic <- function(model) {
  n <- length(model$residuals)
  k <- length(model$coefficients)
  rss <- sum(model$residuals^2)
  aic <- n * log(rss / n) + 2 * k
  return(aic)
}

# Function to compute BIC
compute_bic <- function(model) {
  n <- length(model$residuals)
  k <- length(model$coefficients)
  rss <- sum(model$residuals^2)
  bic <- n * log(rss / n) + k * log(n)
  return(bic)
}

# Function to create and display Q-Q plots
create_qq_plot <- function(model, model_name) {
  residuals <- residuals(model)
  qqnorm(residuals, main = paste("Q-Q Plot for", model_name, "Residuals"))
  qqline(residuals)
}

# Compute RSS, log-likelihood, AIC, BIC for each model
rss_model1 <- sum(model1$residuals^2)
rss_model2 <- sum(model2$residuals^2)
rss_model3 <- sum(model3$residuals^2)
rss_model4 <- sum(model4$residuals^2)
rss_model5 <- sum(model5$residuals^2)

loglik_model1 <- compute_log_likelihood(model1)
loglik_model2 <- compute_log_likelihood(model2)
loglik_model3 <- compute_log_likelihood(model3)
loglik_model4 <- compute_log_likelihood(model4)
loglik_model5 <- compute_log_likelihood(model5)

aic_model1 <- compute_aic(model1)
aic_model2 <- compute_aic(model2)
aic_model3 <- compute_aic(model3)
aic_model4 <- compute_aic(model4)
aic_model5 <- compute_aic(model5)

bic_model1 <- compute_bic(model1)
bic_model2 <- compute_bic(model2)
bic_model3 <- compute_bic(model3)
bic_model4 <- compute_bic(model4)
bic_model5 <- compute_bic(model5)

# Set smaller margins for the plots
par(mar = c(4, 4, 2, 1))

# Create and display Q-Q plots
create_qq_plot(model1, "Model 1")
create_qq_plot(model2, "Model 2")
create_qq_plot(model3, "Model 3")
create_qq_plot(model4, "Model 4")
create_qq_plot(model5, "Model 5")

#Task 2.6--Analysing the Model
# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")

# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)
# Function to calculate QQ plot statistics
calculate_qq_stats <- function(model) {
  residuals <- residuals(model)
  qq_stats <- c(mean(residuals), sd(residuals), e1071::skewness(residuals), e1071::kurtosis(residuals))
  return(qq_stats)
}

# Calculate QQ plot statistics for each model
qq_stats_model1 <- calculate_qq_stats(model1)
qq_stats_model2 <- calculate_qq_stats(model2)
qq_stats_model3 <- calculate_qq_stats(model3)
qq_stats_model4 <- calculate_qq_stats(model4)
qq_stats_model5 <- calculate_qq_stats(model5)

# Create a data frame to display QQ plot statistics
qq_stats <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Mean = c(qq_stats_model1[1], qq_stats_model2[1], qq_stats_model3[1], qq_stats_model4[1], qq_stats_model5[1]),
  SD = c(qq_stats_model1[2], qq_stats_model2[2], qq_stats_model3[2], qq_stats_model4[2], qq_stats_model5[2]),
  Skewness = c(qq_stats_model1[3], qq_stats_model2[3], qq_stats_model3[3], qq_stats_model4[3], qq_stats_model5[3]),
  Kurtosis = c(qq_stats_model1[4], qq_stats_model2[4], qq_stats_model3[4], qq_stats_model4[4], qq_stats_model5[4])
)

# Display QQ plot statistics
print(qq_stats)


# Display QQ plot statistics
print(qq_stats)

#Task 2.7 train test split -Model evaluation:
# Load necessary libraries
library(caret)
library(dplyr)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
split_ratio <- 0.7
index <- createDataPartition(data$quantity, p = split_ratio, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Fit Model 4 using the training dataset
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data = train_data)

# Summarize the model
summary(model4)

# Predict on the testing dataset
test_predictions <- predict(model4, newdata = test_data)

# Evaluate the model's performance (e.g., RMSE)
rmse <- sqrt(mean((test_predictions - test_data$quantity)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

#Task 2.8 -- 95% confidence Interval Plot
# Load necessary libraries
library(caret)
library(dplyr)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
split_ratio <- 0.7
index <- createDataPartition(data$quantity, p = split_ratio, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Fit Model 4 using the training dataset
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data = train_data)

# Summarize the model
summary(model4)

# Predict on the testing dataset
test_predictions <- predict(model4, newdata = test_data)

# Evaluate the model's performance (e.g., RMSE)
rmse <- sqrt(mean((test_predictions - test_data$quantity)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))



# Calculate 95% confidence intervals for predictions
confidence_intervals <- predict(model4, newdata = test_data, interval = "confidence")

# Create a data frame with predictions and confidence intervals
predictions_df <- data.frame(
  price = test_data$price,
  quantity = test_data$quantity,
  Predicted = test_predictions,
  Lower_CI = confidence_intervals[, "lwr"],
  Upper_CI = confidence_intervals[, "upr"]
)

# Plot the data points, model predictions, and confidence intervals
library(ggplot2)

ggplot(predictions_df, aes(x = price, y = quantity)) +
  geom_point() +  # Data points
  geom_line(aes(y = Predicted), color = "blue") +  # Model predictions
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1, color = "red") +  # Confidence intervals
  labs(x = "Price", y = "Quantity") +
  theme_minimal() +
  ggtitle("Model 4 Predictions with 95% Confidence Intervals ") +
  theme(plot.title = element_text(hjust = 0.5))

#Task 3 --parameters setup

# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")

# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)
# Function to compute log-likelihood
compute_log_likelihood <- function(model) {
  residuals <- residuals(model)
  n <- length(residuals)
  variance <- var(residuals)
  log_lik <- -0.5 * n * log(2 * pi) - 0.5 * n * log(variance) - 0.5 * sum((residuals^2) / variance)
  return(log_lik)
}

# Function to compute AIC
compute_aic <- function(model) {
  n <- length(model$residuals)
  k <- length(model$coefficients)
  rss <- sum(model$residuals^2)
  aic <- n * log(rss / n) + 2 * k
  return(aic)
}

# Function to compute BIC
compute_bic <- function(model) {
  n <- length(model$residuals)
  k <- length(model$coefficients)
  rss <- sum(model$residuals^2)
  bic <- n * log(rss / n) + k * log(n)
  return(bic)
}

# Function to create and display Q-Q plots
create_qq_plot <- function(model, model_name) {
  residuals <- residuals(model)
  qqnorm(residuals, main = paste("Q-Q Plot for", model_name, "Residuals"))
  qqline(residuals)
}

# Compute RSS, log-likelihood, AIC, BIC for each model
rss_model1 <- sum(model1$residuals^2)
rss_model2 <- sum(model2$residuals^2)
rss_model3 <- sum(model3$residuals^2)
rss_model4 <- sum(model4$residuals^2)
rss_model5 <- sum(model5$residuals^2)

loglik_model1 <- compute_log_likelihood(model1)
loglik_model2 <- compute_log_likelihood(model2)
loglik_model3 <- compute_log_likelihood(model3)
loglik_model4 <- compute_log_likelihood(model4)
loglik_model5 <- compute_log_likelihood(model5)

aic_model1 <- compute_aic(model1)
aic_model2 <- compute_aic(model2)
aic_model3 <- compute_aic(model3)
aic_model4 <- compute_aic(model4)
aic_model5 <- compute_aic(model5)

bic_model1 <- compute_bic(model1)
bic_model2 <- compute_bic(model2)
bic_model3 <- compute_bic(model3)
bic_model4 <- compute_bic(model4)
bic_model5 <- compute_bic(model5)

# Set smaller margins for the plots
par(mar = c(4, 4, 2, 1))

# Create and display Q-Q plots
create_qq_plot(model1, "Model 1")
create_qq_plot(model2, "Model 2")
create_qq_plot(model3, "Model 3")
create_qq_plot(model4, "Model 4")
create_qq_plot(model5, "Model 5")
#Task 3.1 --Posteririor Distribution histogram:
# Load the required libraries
library(dplyr)

# Load the CSV file
data <- read.csv("customer_shopping_data.csv")

# Define the model (Model 4)
model <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)

# Function to compute the model's RSS
compute_rss <- function(model) {
  return(sum(model$residuals^2))
}

# Compute RSS for Model 4
rss_model4 <- compute_rss(model)

# Define the tolerance level (you may need to adjust this)
epsilon <- 1000  # Adjust as needed

# Number of samples to generate
num_samples <- 1000  # Adjust as needed

# Initialize empty vectors to store parameter samples
param1_samples <- numeric(0)
param2_samples <- numeric(0)

# Perform Rejection ABC
for (i in 1:num_samples) {
  # Generate random parameter values within reasonable ranges
  param1 <- runif(1, min = -1000, max = 1000)  # Adjust min/max as needed
  param2 <- runif(1, min = -1000, max = 1000)  # Adjust min/max as needed
  
  # Set the other parameters to the fixed values
  fixed_params <- coef(model)[-c(2, 3)]  # Remove the two parameters to be estimated
  params <- c(param1, fixed_params[1], param2, fixed_params[2])
  
  # Create a new model with the generated parameters
  new_model <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data, start = params)
  
  # Compute the RSS for the new model
  new_rss <- compute_rss(new_model)
  
  # Check if the RSS is within the tolerance level
  if (abs(new_rss - rss_model4) <= epsilon) {
    # If within tolerance, store the parameter values
    param1_samples <- c(param1_samples, param1)
    param2_samples <- c(param2_samples, param2)
  }
}

# Visualize the posterior distributions (e.g., using histograms)
hist(param1_samples, main = "Posterior Distribution of Parameter 1")
hist(param2_samples, main = "Posterior Distribution of Parameter 2")

#Task 3.2 ---Marginal and Joint Distribution Code:
# Load the required libraries
library(ggplot2)

# Load the CSV file (replace 'your_data.csv' with your actual data file)
data <- read.csv("customer_shopping_data.csv")

# Define the models
model1 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^24) + I(price^14) + 1, data)
model2 <- lm(quantity ~ I(price^4) + I(price^13) + I(price^34) + 1, data)
model3 <- lm(quantity ~ I(price^33) + I(price^34) + 1, data)
model4 <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data)
model5 <- lm(quantity ~ I(price^4) + I(price^12) + I(price^13) + I(price^34) + 1, data)

# Compute RSS values for each model
RSS_model1 <- sum(model1$residuals^2)
RSS_model2 <- sum(model2$residuals^2)
RSS_model3 <- sum(model3$residuals^2)
RSS_model4 <- sum(model4$residuals^2)
RSS_model5 <- sum(model5$residuals^2)

# Combine RSS values into a vector
RSS_values <- c(RSS_model1, RSS_model2, RSS_model3, RSS_model4, RSS_model5)

# Define the number of samples
num_samples <- 1000

# Initialize empty vectors to store parameter samples
param1_samples <- numeric(0)
param2_samples <- numeric(0)

# Define the prior range for parameters (adjust as needed)
param_min <- -1000
param_max <- 1000

# Define the tolerance level (adjust as needed)
epsilon <- 1000

# Loop over each model
for (rss_model in RSS_values) {
  for (i in 1:num_samples) {
    # Generate random parameter values within prior range
    param1 <- runif(1, min = param_min, max = param_max)
    param2 <- runif(1, min = param_min, max = param_max)
    
    # Assuming Model 4 as an example, modify for other models
    # Create a new model with the generated parameters
    # Use your model formula here
    new_model <- lm(quantity ~ I(price^2) + I(price^13) + I(price^34) + 1, data, start = c(param1, param2))
    
    # Compute the RSS for the new model
    new_RSS <- sum(new_model$residuals^2)
    
    # Check if the RSS is within tolerance
    if (abs(new_RSS - rss_model) <= epsilon) {
      param1_samples <- c(param1_samples, param1)
      param2_samples <- c(param2_samples, param2)
    }
  }
}

# Plot the joint and marginal posterior distributions
# Joint posterior distribution
joint_plot <- ggplot(data.frame(param1 = param1_samples, param2 = param2_samples), aes(x = param1, y = param2)) +
  geom_point() +
  labs(title = "Joint Posterior Distribution", x = "Parameter 1", y = "Parameter 2")

# Marginal posterior distribution for parameter 1
marginal_param1_plot <- ggplot(data.frame(param1 = param1_samples), aes(x = param1)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.7) +
  labs(title = "Marginal Posterior Distribution (Parameter 1)", x = "Parameter 1", y = "Frequency")

# Marginal posterior distribution for parameter 2
marginal_param2_plot <- ggplot(data.frame(param2 = param2_samples), aes(x = param2)) +
  geom_histogram(binwidth = 10, fill = "red", alpha = 0.7) +
  labs(title = "Marginal Posterior Distribution (Parameter 2)", x = "Parameter 2", y = "Frequency")

# Display the plots
print(joint_plot)
print(marginal_param1_plot)
print(marginal_param2_plot)

