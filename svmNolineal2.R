#Modelo no lineal 2 de SVM
library(e1071)

set.seed(1011)
x = matrix(rnorm(800),400,2)
ind = (x[,1]^2 + x[,2]^2 ) > 1
y = rep(c(1,-1), c(200,200))
y[ind] = 1;
y[ind == FALSE] = -1

plot(x, col = y +3, pch = 19)

#SVM 

train = data.frame(x,y=as.factor(y))

svmfit=svm(y~.,data=train,kernel="radial",cost =1, scale="F")

print(svmfit)
plot(svmfit, train)


