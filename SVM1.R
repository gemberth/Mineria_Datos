##SVM No lineal
#Cancer.R
library(e1071)
datacancer=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep = ",")

datospred = datacancer[,3:32]
diagnosis = as.factor(datacancer[,2]) # (label: M = malignant, B = benign)

plot(datospred[,c(5,3)],col=c("blue","red"), pch = 19)

#SVM model, kernel = radial o lineal
svmfit=svm(diagnosis ~.,
           data = conj,#datospred[,c(3,5)],
           kernel="radial",cost =1, scale=FALSE)
#svmfit
out = predict(svmfit, datospred[,c(3,5)])
100*sum(out==diagnosis)/length(diagnosis)

### plot SVM
conj = data.frame(datospred[,c(3,5)], diagnosis)
plot(svmfit, conj)
###

#test & training sets
train = datospred[1:500,]
y=diagnosis[1:500]

#SVM model
svmfit1=svm(y~.,data=train,kernel="radial",cost =1, scale=FALSE)

test = datospred[501:569,]
out = predict(svmfit1, test)

100*sum(out==diagnosis[501:569])/69
