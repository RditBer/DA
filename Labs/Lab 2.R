library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("C:/Users/ardit/Downloads/Lab 2-selected/NY-House-Dataset.csv")
dataset <- NY_House_Dataset

## log10 transformations applied to PRICE and PROPERTYSQFT to improve linearity and reduce skewness

## print initial dataset size
cat("Initial number of observations:", nrow(dataset), "\n")

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
cat("After price filter:", nrow(dataset), "\n")

## additional data cleaning - remove outliers/problematic values
dataset <- dataset[dataset$PROPERTYSQFT > 0,]  # remove zero or negative sqft
dataset <- dataset[!is.na(dataset$BEDS),]      # remove missing beds
dataset <- dataset[!is.na(dataset$BATH),]      # remove missing bath
cat("After removing NA and sqft <= 0:", nrow(dataset), "\n")

## fit initial model to identify outliers
temp_model <- lm(log10(PRICE)~log10(PROPERTYSQFT) + BEDS + BATH, data = dataset)

## remove extreme outliers based on residuals (keep residuals within ±2.5 SD)
dataset$residuals <- resid(temp_model)
dataset <- dataset[abs(dataset$residuals) < 2.5 * sd(dataset$residuals), ]
dataset$residuals <- NULL  # remove temporary column
cat("After removing extreme outliers:", nrow(dataset), "\n\n")

## ==================== MODEL 1: log(PRICE) ~ log(PROPERTYSQFT) ====================

## fit linear model
model1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
cat("MODEL 1: log(PRICE) ~ log(PROPERTYSQFT)\n")
summary(model1)

## scatter plot with best fit line - most significant variable: log10(PROPERTYSQFT) (smallest p-value)
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm", col="red") +
  labs(title = "Model 1: log(Price) vs log(PropertySqFt)",
       x = "log10(Property Square Feet)",
       y = "log10(Price)") +
  theme_minimal()

## residual plot
ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0) +
  labs(title = "Model 1: Residual Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

## ==================== MODEL 2: log(PRICE) ~ BEDS ====================

## fit linear model
model2 <- lm(log10(PRICE)~ BEDS, data = dataset)

## print model output
cat("\nMODEL 2: log(PRICE) ~ BEDS\n")
summary(model2)

## scatter plot of most significant variable vs Price with best fit line
## most significant variable: BEDS (smallest p-value)
ggplot(dataset, aes(x = BEDS, y = log10(PRICE))) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.4) +
  stat_smooth(method = "lm", col="red") +
  labs(title = "Model 2: log(Price) vs Beds",
       x = "Number of Bedrooms",
       y = "log10(Price)") +
  theme_minimal()

## residual plot
ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0) +
  labs(title = "Model 2: Residual Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

## ==================== MODEL 3: log(PRICE) ~ log(PROPERTYSQFT) + BEDS + BATH ====================

## fit linear model
model3 <- lm(log10(PRICE)~log10(PROPERTYSQFT) + BEDS + BATH, data = dataset)

## print model output
cat("\nMODEL 3: log(PRICE) ~ log(PROPERTYSQFT) + BEDS + BATH\n")
summary(model3)

## scatter plot of most significant variable vs Price with best fit line
## most significant variable: BATH (smallest p-value)
ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.4) +
  stat_smooth(method = "lm", col="red") +
  labs(title = "Model 3: log(Price) vs Bath",
       x = "Number of Bathrooms",
       y = "log10(Price)") +
  theme_minimal()

## residual plot
ggplot(model3, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0) +
  labs(title = "Model 3: Residual Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

## ==================== MODEL COMPARISON ====================

cat("\n========== MODEL COMPARISON ==========\n")
cat("Model 1 - Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")
cat("Model 2 - Adjusted R-squared:", summary(model2)$adj.r.squared, "\n")
cat("Model 3 - Adjusted R-squared:", summary(model3)$adj.r.squared, "\n")

cat("Model 1 - Residual Standard Error:", summary(model1)$sigma, "\n")
cat("Model 2 - Residual Standard Error:", summary(model2)$sigma, "\n")
cat("Model 3 - Residual Standard Error:", summary(model3)$sigma, "\n")

cat("Model 1 - AIC:", AIC(model1), "\n")
cat("Model 2 - AIC:", AIC(model2), "\n")
cat("Model 3 - AIC:", AIC(model3), "\n")

## Model Comparison Summary:
## Model 3 is the best, it has the highest adjusted R-squared (0.486) and lowest 
## residual standard error (0.294). Now when I include Beds and Bath the model
## has a better fit than Models 1 & 2. All predictors in Model 3 are 
## statistically significant (p < 0.001), indicating that property square
## footage, number of beds and baths are all meaningful to predicting prices.

### THE END ###
