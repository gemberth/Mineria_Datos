###Clasificar con otros 'k' values 
library(class); library(readr)

#Obtener datos
signsfile <- read_csv("R/senales.csv")

#Separar datos training y test
signs <- subset(signsfile,signsfile$sample == "train")
test_signs <- subset(signsfile,signsfile$sample == "test")

# Create a vector of labels
class_senal <- signs$sign_type

# Compute the accuracy of the model with k = 1
k_1 <- knn(train = signs[c(-1,-2,-3)], 
           test = test_signs[c(-1,-2,-3)], 
           cl = class_senal )

mean(k_1 == test_signs$sign_type)

# Compute the accuracy of the model with k = 7
k_7 <- knn(train = signs[c(-1,-2,-3)], 
           test = test_signs[c(-1,-2,-3)], 
           cl = class_senal, k=7)

mean(k_7 == test_signs$sign_type)

# Compute the accuracy of the model with k = 15
k_15 <- knn(train = signs[c(-1,-2,-3)], 
            test = test_signs[c(-1,-2,-3)], 
            cl = class_senal, k=15)

mean(k_15 == test_signs$sign_type)

####################
# Use the "prob" parameter to get the proportion of votes 
# for the winning class

k_7 <- knn(train = signs[c(-1,-2,-3)], 
           test = test_signs[c(-1,-2,-3)],
           cl = class_senal, k=7, 
           prob=TRUE)

k_7

# Get the "prob" attribute from the predicted classes
(sign_prob <- attr(k_7, "prob"))

# Examine the first several predictions
head(k_7, n=5)
# Examine the proportion of votes for the winning class
head(sign_prob, n=5)
