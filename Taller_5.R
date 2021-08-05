
library(arules)


#Extracion de datos heder para emologar los titulos

datos <- read.csv("F:/OneDrive - ULEAM/6to-Semestre/Mineria de datos/codigos_repo/datos_t5.csv", sep=";",header=TRUE)

View(datos)
#como puede elevarse mayor que 1.0 por que cumple el requerimiento en la base de datos
rules <- apriori(datos,parameter = list(supp = 0.1, conf = 0.60)) 
#no salen las rules por que la base de datos no cumple
summary(rules)
#inspeccionar las reglas
inspect(rules)
#la variable con la regla del lift q  ue sea mayor del 3
rules.sub<-subset(rules,subset=lift>3)
inspect(rules.sub)
