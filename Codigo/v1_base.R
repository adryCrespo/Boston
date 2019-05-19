setwd("D:\\Master_UCM\\Machine Learning\\HDMA")
library(data.table)#Importacion datos
library(DataExplorer)#Exploracion datos automatico

#Importar datos ----
data1    <- as.data.frame(fread('Hdma.csv')) #datos training
create_report(data1, y = "deny")# crea informe en fotos
sum(is.na(data1$deny))
apply(is.na(data1),2,sum)
