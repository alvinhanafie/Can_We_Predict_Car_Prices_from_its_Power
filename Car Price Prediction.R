# Prediction of Car Price based on Car Horsepower using Linear Regression

# Import Library
library(ggplot2) # for plotting
library(caTools) # for train-test data split

# Import Data
df <- read.csv("/Users/alvinkurniawan/Downloads/Prakerja Data Science/mobil_mesin_harga.csv")

# Change column name of KekuatanMesin and Harga
names(df)[c(1, 2)] <- c("Car_Horsepower", "Car_Price")

# Checking for missing values and descriptive statistics
summary(df)

# No missing value found
# Mean of Car_Horsepower is higher than its median
# Data distribution for Car_Horsepower is positively-skewed

# Car Price vs. Car Horsepower Scatterplot Visualization
ggplot(df, aes(x = Car_Horsepower, y = Car_Price)) +
  geom_point() +
  labs(title = "Car Price vs. Car Horsepower", 
       x = "Car_Horsepower", y = "Car_Price")

# Since the feature is only one, we do not scale the data for model interpretability.

# Below are codes if the data is scaled
# df$Car_Horsepower <- scale(df$Car_Horsepower)

# Set seed or random state for reproducibility
set.seed(123)

# Train-test data split before create a model for model testing and evaluation
df_split = sample.split(df$Car_Price, SplitRatio = 0.7)

# Train set and test set by ratio 0.7
trainset = subset(df,df_split==TRUE)
testset = subset(df,df_split==FALSE)

# Train the model using linear regression
linear_regression <- lm(Car_Price ~ Car_Horsepower, data = trainset)

# Show summary for model training results
summary(linear_regression)

# Car_Horsepower is strong predictor for Car_Price, highlighted by *** in the summary. 

# The coefficient for the model is Car_Price = -33.01 + 1.57*Car_Horsepower

# Based on Linear Regression model: 
# Every 1 increment from Car_Horsepower will increase Car_Price by 1.57

# Predict car price using test set for testing
Car_Price_Prediction = predict(linear_regression, newdata=testset)

# Create df_merge to merge between actual car price and predicted car price
df_merge = cbind(testset, Car_Price_Prediction)

# View df_merge to compare between actual car price and predicted car price
View(df_merge)

# Model Evaluation using Mean Absolute Error (MAE) and Root Mean Square Error (RMSE)

# Mean Absolute Error
mae <- mean(abs(df_merge$Car_Price_Prediction - df_merge$Car_Price))
print(paste("Mean Absolute Error (MAE):", mae))

# MAE = 33.21
# This means that the model have average deviation from the true value by 33.21.
# Since the range of car price is 402.82, the MAE value from model is relatively low.
# This information indicated that the model already performed well at car price prediction.

# Root Mean Square Error
rmse <- sqrt(mean((df_merge$Car_Price_Prediction - df_merge$Car_Price) ** 2))
print(paste("Root Mean Square Error (RMSE):", rmse))

# RMSE = 53.97
# This means that the model have average deviation from the true value by 53.97.
# Since the range of car price is 402.82, the RMSE value from model is relatively low.
# This information indicated that the model already performed well at car price prediction.

# Since the difference of MAE and RMSE is not high, the model is robust with outliers. 
# Both MAE and RMSE are in relatively low, which means the errors from model is not large. 

# Thank You!