###Prepara datos para regresión logistica
##Convertir en Factores, Manejar falta de datos y Combinar predictores
library(readr)
donors <- read_csv("R/donors.csv")
########################
# Convert the wealth rating to a factor
########################
donors$wealth_levels <- factor(donors$wealth_rating, 
                               levels = c(0, 1, 2, 3), 
                labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() to change reference category to Medium
donors$wealth_levels <- relevel(donors$wealth_levels, 
                                ref = "Medium")

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_levels, 
            data = donors, family = "binomial"))
#########################
# Handling missing data
#########################
# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age), 
                       round(mean(donors$age, na.rm = TRUE), 2),
                       donors$age )

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)
#############################
## Building a more sophisticated model
#############################
# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ recency * frequency + money,
                 data = donors, family = "binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, data = donors, type = "response")

# Plot the ROC curve for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = "red")
auc(ROC)
