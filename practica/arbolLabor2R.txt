##Use Labor Relations dataset
##from UCI ML Repository (https://archive.ics.uci.edu/ml/datasets/Labor+Relations)
labor <- read.csv("R/labor.txt", header=T, sep= ',' )
View(labor)

##Train the algorithm (classifier) on the train data,
library(rpart)
arvore <- rpart(negoc ~., labor)
#arvore

##Representação gráfica de uma árvore de decisão
plot(arvore)
text(arvore)
#plot.rpart()
plot(arvore,uniform=T,branch=0)
text(arvore,digits=2,cex=0.72,
  font=15, pretty=50,fancy=T,fwidth=2.4, fheight=1,4)

####Generating predictions
##Use the algorithm (classifier) to classify cases using the test data:
previsoes.modelo <- predict(arvore, labor, type="class")


####Generate confusion matrix:
m.conf <- table(labor$negoc, previsoes.modelo)
m.conf
###Calculating Error
##Calculate the total of correct classifications on the diagonal 
##of the confusion matrix
#accu.rate
mean(labor$negoc == previsoes.modelo)
#error.rate
1 - mean(labor$negoc == previsoes.modelo)
