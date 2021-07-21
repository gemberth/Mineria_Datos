#####################Trabajando con el dataset Iris.
##load the package class
library(class)

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris_norm)

##Generate a random number that is 70% of the total number of rows in dataset.
ran <- sample(1:nrow(iris_norm), 0.7 * nrow(iris_norm)) 

##extract training set
iris_train <- iris_norm[ran,] 
##extract testing set
iris_test <- iris_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
iris_target_category <- iris[ran,5]
##extract 5th column if test dataset to measure the accuracy
iris_test_category <- iris[-ran,5]

##run knn function
k_13 <- knn(iris_train,iris_test,cl=iris_target_category,k=13)

##create confusion matrix
(tab <- table(k_13,iris_test_category) )
mean(k_13 == iris_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.
#accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
#accuracy(tab)
