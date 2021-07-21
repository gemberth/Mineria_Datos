#Neural networks con estandarización
#library(nnet)
library(neuralnet)
set.seed(65)
#cargar datos
data(Boston, package = "MASS")

#selección de Números de filas
nrow(Boston)
muestra70 <- sample(nrow(Boston), 0.70 * nrow(Boston)) 
#######
Boston_mtrx = scale(Boston)
Boston_std = as.data.frame(scale(Boston))
train <- Boston_std[muestra70, ]
test <- Boston_std[-muestra70, ]  ####Boston[-muestra70, ]
##Modelo red neuronal
modelo_nn <- neuralnet(medv ~.,        #formula: class ~ V1 + V2 + ...
                       data = train,    #training set
                       hidden = c(7,5), #neuronas de capa oculta
                       threshold = 0.05, #stop iter, si error <= 0.05
                       algorithm = "rprop+",  #algoritmo
                       #act.fct = "logistic", #suaviza resultado
                       #linear.output = F)    #False por act.fct
                       )   
plot(modelo_nn)
## Predecir la probabilidad del conjunto test usando el modelo
#predict(modelo_nn, test)
pred = compute(modelo_nn,test)
pred$net.result

#retornar a los valores antes de estandarizar
#DMwR
library(DMwR)
valor_pred = unscale(pred$net.result,Boston_mtrx)
valor_pred

##SUMA DE ERROR CUADRATICO
test_sin_str = Boston[-muestra70, ]
mae.nn <- mean( abs(valor_pred - test_sin_str[, "medv"]) )
mse.nn <- mean( (valor_pred - test_sin_str[, "medv"])^2 )

library(ggplot2)
qplot(x=test_sin_str[, "medv"], y=valor_pred, method="lm",
      geom=c("point","smooth"),
      main=paste("Prediccion vs Real. Suma de Error Cuadratico = ", round(mse.nn,2)) )


#ifelse(pred$net.result>0.5, 1, 0)

#plot(test_sin_str[, "medv"], valor_pred, main = "Neural Net Predictions", ylab = "Observed
#Values")
#abline(0, 1, lty = 2, col = "red")
