#Algoritmo Bayesiano
#Computar la probabilidad del lugar donde estará la persona,
#dado un predictor: "dia de semana" a las 9am.

# Load the naivebayes package
library(naivebayes) ; library(readr)

#cargar datos de archivo
datos_localizacion = read_csv("R/locations.csv")
View(datos_localizacion)

#El data frame: where9am, contendrá la localización de una persona 
#a las 9am, todos los días. Será el conjunto Traning.
where9am = subset(datos_localizacion, hour == 9)
View(where9am)
#############################################
#### Computar Probabilidades #####
#############################################
# Compute P(office) a las 9 am
subset(where9am, location == "office")
p_A <-( nrow(subset(where9am, location == "office")) 
       / nrow(where9am) )

# Compute P(weekday) a las 9am
p_B <- ( nrow(subset(where9am, daytype == "weekday")) 
       / nrow(where9am) )

# Compute the observed P(office and weekday)
p_AB <- ( nrow(subset(where9am, location == "office" & daytype == "weekday"))
        / nrow(where9am) )

# Compute  P(office | weekday)  
( p_A_dado_B <- p_AB / p_B )

###################################################
##Modelo de localización simple de Naive Bayes, con una var. predictoria.

# Build the location prediction model
locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict Thursday's 9am location = P( lugar | weekday)
jueves9am = data.frame(daytype = "weekday")
predict(locmodel, jueves9am,type = "prob")

# Predict Saturdays's 9am location = P( lugar | weekend)
saturday9am = data.frame(daytype ="weekend")
predict(locmodel, saturday9am,type = "prob")
        