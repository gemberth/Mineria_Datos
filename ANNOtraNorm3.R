#ANN con otro tipo de normalización
# -----------------------------------------------------
library(neuralnet); library(ggplot2)
data(Boston, package = "MASS")

set.seed(65)
datos = Boston 
n        <- nrow(datos)
muestra  <- sample(n, n * .70)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]


# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
#datos_nrm <- as.data.frame(scale(datos))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]

# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(medv ~.,
                       data          = train_nrm,
                       hidden        = c(7,5), # ver Notas para detalle 
                       threshold     = 0.05,   # ver Notas para detalle
                       algorithm     = "rprop+" 
)


# PREDICCION
# -----------------------------------------------------
pr.nn   <- compute(modelo.nn,test_nrm)

# se transoforma el valor escalar al valor nominal original
medv.predict <- pr.nn$net.result*(max(datos$medv)-min(datos$medv))+min(datos$medv)
medv.real    <- (test_nrm$medv)*(max(datos$medv)-min(datos$medv))+min(datos$medv)



# SUMA DE ERROR CUADRATICO
# -----------------------------------------------------
(se.nn <- sum((medv.real - medv.predict)^2)/nrow(test_nrm))


#GRAFICOS
# -----------------------------------------------------
# Errores
qplot(x=medv.real, y=medv.predict, geom=c("point","smooth"), method="lm", 
      main=paste("Real Vs Prediccion. Summa de Error Cuadratico=", round(se.nn,2)))
# Red
plot(modelo.nn)