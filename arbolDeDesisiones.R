
#install.packages("rattle")

#cargamos librerias
library(tidyverse)

#carga la libreria de titanit 
library(titanic)
data("titanic_train")
head(titanic_train)

# otras librerias
library(rpart)
library(rpart.plot)
library(rattle)


# modelamos el arbol de desisiones 

arbol <- rpart(
  formula = Survived ~ Sex +Age,
  data = titanic_train,
  method = "class"
)

# grafico del arbol
fancyRpartPlot(arbol)

# procedimiento del arbol 
pred_arbol <-predict(arbol, type = "class")
titanic_pred <- cbind(titanic_train,pred_arbol)
# pasajero masculino de 4 años de edad 
predict(object = arbol,
        newdata = data.frame(Age=4,
                             Sex='male'),
        type = 'class')

