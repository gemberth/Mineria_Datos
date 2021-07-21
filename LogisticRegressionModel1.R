
library(readr)

donors <- read_csv("R/donors.csv")
# Examine the dataset to identify potential independent variables
View(donors)

# Explore the dependent variable
table(donors$donated)

#Build the donation model
#if * on formula then combined varibles
donation_model <- glm( donated ~ bad_address + 
                            interest_religion+
                            interest_veterans,
                               data = donors , 
                         family = "binomial" )

# Summarize the model results
#summary(donation_model)
################################
# Estimate the donation probability
donors$donation_prob <- predict(donation_model,donors, type = "response")

# Find the donation probability of the average prospect
mean(donors$donation_prob)

# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donated == donors$donation_pred)
##################################################################
###### ROC curves ########

#Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)
###############


