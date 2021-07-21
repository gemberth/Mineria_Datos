#Usar knn, para clasificar una sola señal desconocida:
#r1,g1,b1,r2,g2,b2,r3,g3,b3,r4,g4,b4,r5,g5,b5,r6,g6,b6,r7,g7,b7,r8,g8,b8,r9,g9,b9,r10,g10,b10,r11,g11,b11,r12,g12,b12,r13,g13,b13,r14,g14,b14,r15,g15,b15,r16,g16,b16
#204,227,220,196,59,51,202,67,59,204,227,220,236,250,234,242,252,235,205,148,131,190,50,43,179,70,57,242,229,212,190,50,43,193,51,44,170,197,196,190,50,43,190,47,41,165,195,196

# Load packages
library(class); library(readr)

# Datos para entrenamiento
signsfile <- read_csv("R/senales.csv")
View(signsfile)
#Separar training y test sets; test no se aplica aquí.
signs <- subset(signsfile,signsfile$sample == "train")
View(signs)
#test_signs <- subset(signsfile,signsfile$sample == "test")

# Create a vector of labels
class_senal <- signs$sign_type   #signs[,3]
View(class_senal)
#######################
# Unknow case
senal_prox = data.frame(204,227,220,196,59,51,202,67,59,204,227,220,
                        236,250,234,242,252,235,205,148,131,190,50,43,
                        179,70,57,242,229,212,190,50,43,193,51,44,170,
                        197,196,190,50,43,190,47,41,165,195,196)

# Clasificar la señal desconocida, k=1
signs_pred = knn(train = signs[,c(-1,-2,-3)], 
                 test = senal_prox, 
                 cl = class_senal)

signs_pred
