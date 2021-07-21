#############
#A more sophisticated location model
#############
library(naivebayes) ; library(readr)

#cargar datos de archivo, será todo para training.
datos_localizacion = read_csv("R/locations.csv")

# Build a NB model of location
locmodel <- naive_bayes(location ~ daytype + hourtype, 
                        data = datos_localizacion)

# Predict person's location on a weekday afternoon
weekday_afternoon = data.frame(daytype = "weekday",
                           hourtype = "afternoon")

predict(locmodel, weekday_afternoon)

# Predict person's location on a weekday evening
weekday_evening = data.frame(daytype = "weekday", 
		            hourtype = "evening")
predict(locmodel, weekday_evening)
#########################################
#Preparing for unforeseen circumstances, prob = 0

# Observe the predicted probabilities for 
#a weekend morning
weekend_morning = data.frame(daytype = "weekend", 
		           hourtype = "morning")

predict(locmodel, weekend_morning, type = "prob")
#observar prob. ceros

### Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype + hourtype, 
                         data = datos_localizacion, 
			 laplace = 1)
 
#Observe the new predicted probabilities for
#a weekend morning
predict(locmodel2, weekend_morning, type = "prob")
