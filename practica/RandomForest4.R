#Load the randomForest package
#Para Supervised & Unsupervised problems
library(randomForest); library(readr)

car <- read_csv("R/car.csv")
##########
#####Create the training and test dataset
#70% 1728 = 1209.6
sample_rows <- sample(1728, 1210)
### Creating random training dataset
cars_train <- car[sample_rows, ] 
### Creating random test dataset
cars_test <- car[-sample_rows, ]
##########
car_model = randomForest(as.factor(accept) ~ . , 
                data = cars_train,
                ntree = 50,		#number of trees in the forest
                mtry = sqrt(6))		#number of predictors (p) per tree
#car_model
# Compute the accuracy of the random forest
cars_test$pred <- predict(car_model, cars_test, type = "class")
mean(cars_test$pred == cars_test$accept)

###
importance(car_model)
importance(car_model, type=1)
plot(car_model) 
