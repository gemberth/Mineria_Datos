##Aplicar algoritmo bayesiano a dataset: golf.csv
##El archivo en su conjunto será para training.
#Clasificar Test Example, desconocido

# Load the naivebayes package
library(naivebayes)

#cargar datos training de archivo
golf_file = read.csv("R/golf.csv", sep=';')
View(golf_file)

##Cálculo de probabilidades 
p_yes <- nrow( subset(golf_file, Play == "yes")) / 14
p_rain <- nrow(subset(golf_file, Outlook == "rain")) / 14
p_YesRain <- nrow(subset(golf_file, Play == "yes" & Outlook == "rain")) / 14
#Compute P(yes | rain)
(p_yes_given_rain <- p_YesRain / p_rain)

###Build the location prediction model
playmodel1 <- naive_bayes(Play ~ Outlook +
                                    Wind ,
                        data = golf_file )
                         
#Predecir Test Example, desconocido
test_clima1 = data.frame(Outlook="sunny",
                        Wind = "true")

predict(playmodel1, test_clima1)

########MODELO COMPLETO############################
playmodel <- naive_bayes(Play ~ Outlook +
                            Temperature +
                               Humidity +
                                   Wind ,
                        data = golf_file,
                        laplace = 1 )

#Predecir Test Example, desconocido
#Predict game state & Obtain the predicted probabilities
test_clima = data.frame(Outlook="sunny",
                        Temperature = 66, 
                        Humidity = 90, 
                        Wind = "true")

predict(playmodel, test_clima, type = "prob")

