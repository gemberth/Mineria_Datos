###knn para clasificar un conjunto test
# Load the 'class' package
library(class); library(readr)

#obtener datos
signsfile <- read_csv("R/senales.csv")

#Separar training y test sets.
signs <- subset(signsfile,signsfile$sample == "train")
View(signs)
table(signs$sign_type)   # Count the number of signs of each type
#aggregate(r10 ~ sign_type, data = signs, mean)   # Check r10's average red level by sign type
test_signs <- subset(signsfile,signsfile$sample == "test")
View(test_signs)
table(test_signs$sign_type)

# Create a vector of labels
class_senal <- signs$sign_type   #signs[,3]

#Clasificar test dataset
k_1 = knn(train = signs[,c(-1,-2,-3)], 
          test  = test_signs[,c(-1,-2,-3)],
          cl = class_senal)
table(k_1)
# Create a confusion matrix of the predicted values vs actual values
table(k_1, test_signs$sign_type) 

# Compute the accuracy & error rate
mean(k_1 == test_signs$sign_type)*100
mean(k_1 != test_signs$sign_type)*100