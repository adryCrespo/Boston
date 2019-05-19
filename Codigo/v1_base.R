##################################
#Version 01
# Fecha_ 19/05/2019
#Objetivos: importacion de datos y exploracion inicial
#Resultados
####################################


library(data.table)#Importacion datos
library(DataExplorer)#Exploracion datos automatico

#Importar datos ----
data1    <- as.data.frame(fread('Datos\\Hdma.csv')) #datos training


##Variables ----
listconti <- c("Time", "Fromlow", "Slope", "Rw")
listclass <- c("Amphiso", "Subst", "Pool", "Water", "Cobble")
vardep <- c("Gunnel")
apply(is.na(data1),2,sum)

#Exploracion de datos ----
create_report(data1, y = "deny")# crea informe en fotos
table(data1$deny)
# no  yes 
# 2096  285 
#Desbalance 7.33

