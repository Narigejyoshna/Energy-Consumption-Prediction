library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(Metrics)

file_path <- "C:/Users/narig/OneDrive/Documents/household_power_consumption.txt"
data <- read_delim(file_path, delim = ";", na = "?")

data <- data %>%
  mutate(Datetime = dmy_hms(paste(Date, Time))) %>%
  drop_na(Global_active_power)

num_cols <- names(data)[3:9]
data[num_cols] <- lapply(data[num_cols], as.numeric)

set.seed(42)
data_sample <- data %>% sample_n(1000)

data_sample <- data_sample %>%
  mutate(
    Year  = year(Datetime),
    Month = month(Datetime),
    Day   = day(Datetime),
    Hour  = hour(Datetime)
  ) %>%
  select(Global_active_power, Global_reactive_power, Voltage,
         Year, Month, Day, Hour)

data_sample <- na.omit(data_sample)

set.seed(42)
train_index <- createDataPartition(data_sample$Global_active_power, p = 0.8, list = FALSE)
train_data <- data_sample[train_index, ]
test_data  <- data_sample[-train_index, ]

rf_model <- randomForest(
  Global_active_power ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

pred <- predict(rf_model, newdata = test_data)

mse_val  <- mse(test_data$Global_active_power, pred)
mae_val  <- mae(test_data$Global_active_power, pred)
rmse_val <- rmse(test_data$Global_active_power, pred)

cat("✅ Model Evaluation:\n")
cat("Mean Squared Error (MSE):", mse_val, "\n")
cat("Mean Absolute Error (MAE):", mae_val, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_val, "\n")

plot_data <- tibble(
  Index = 1:min(100, nrow(test_data)),
  Actual = test_data$Global_active_power[1:min(100, nrow(test_data))],
  Predicted = pred[1:min(100, nrow(test_data))]
)

ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1) +
  labs(
    title = "Actual vs Predicted Energy Consumption (Sample of 100)",
    x = "Sample Index",
    y = "Global Active Power (kilowatts)"
  ) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())
