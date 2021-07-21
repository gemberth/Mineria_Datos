#ANN sin estandarizar
library(nnet)
set.seed(65)
data(Boston, package = "MASS")
muestra70 <- sample(nrow(Boston), 0.70 * nrow(Boston))
train <- Boston[ muestra70, ]
test <-  Boston[-muestra70, ]
modelo_nn <- nnet(medv ~ ., train, size = 7, decay = 0.001, maxit = 1000,linout = T)
prevs <- predict(modelo_nn, test)
mae.nn <- mean(abs(prevs - test[, "medv"]))
mse.nn <- mean((prevs - test[, "medv"])^2)
plot(test[, "medv"], prevs, main = "Neural Net Predictions", ylab = "Observed
Values")
abline(0, 1, lty = 2, col = "red")

#NeuralNetTools package
library(NeuralNetTools)
plotnet(modelo_nn)


