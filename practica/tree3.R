######
#Data Set from: https://archive.ics.uci.edu/ml/datasets/Car+Evaluation
# Load the rpart package
#existen otros paquetes: C50, party...
library(readr); library(rpart)
car <- read_csv("R/car.csv")

#####Create the training and test dataset
## Determine the number of rows for training
nrow(car) * 0.70 	#70% 1728 = 1209.6
# Create a random sample of row IDs
sample_rows <- sample(1728, 1210)

### Creating random training dataset
cars_train <- car[sample_rows, ] 
### Creating random test dataset
cars_test <- car[-sample_rows, ]
#############
### Build a lending model predicting loan outcome 
car_model <- rpart(accept ~ ., data = cars_train, 
                   method = "class", 
                   control = rpart.control(cp = 0))
#other parameters: rpart.control(cp = 0, maxdepth = 6, 
#                  minsplit = 500)
###################
##Make predictions on the test dataset; no es necesario el -7
cars_test$pred = predict(car_model, cars_test[,-7], type = "class")
###################
#### Examine the confusion matrix
tab <- table(cars_test$pred, cars_test$accept,
             dnn = c("Prediction", " Reference") )
# Compute the accuracy on the test dataset
mean(cars_test$pred == cars_test$accept)
###
library(caret)
confusionMatrix(tab)
#######plot model
plot(car_model, uniform=TRUE)
text(car_model, use.n=TRUE, all=TRUE, cex=.5)
############################################################
############################################################
#By using post-pruning
#Examine the complexity plot:
#cost-complexity parameter (CP) of the tree, 
#the MIN cross validation error (x-val Relative Error)
#tree size (number of nodes)
car_model$cptable
#min(car_model$cptable[,"xerror"])
#car_model$cptable[which.min(car_model$cptable[,"xerror"]),"CP"]
plotcp(car_model)	
# Prune the tree
car_model_pruned <- prune(car_model, cp = 0.007)
# Compute the accuracy of the pruned tree
cars_test$pred2 <- predict(car_model_pruned, 
                           cars_test, type = "class")
############################################################
###Graficos varios
############################################################
### Load the rpart.plot package
library(rpart.plot)
# Plot the model with default settings
rpart.plot(car_model)
rpart.plot(car_model_pruned)
## Plot the model with customized settings
#rpart.plot(car_model_pruned, type = 4, 
#           box.palette = list("Oranges","Reds", "Greens","Blues"),
#           fallen.leaves = TRUE)
