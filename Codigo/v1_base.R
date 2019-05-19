setwd("D:\\Master_UCM\\Machine Learning\\HDMA")
library(data.table)#Importacion datos
library(DataExplorer)#Exploracion datos automatico

#Importar datos ----
data1    <- as.data.frame(fread('Datos\\Hdma.csv')) #datos training
create_report(data1, y = "deny")# crea informe en fotos


apply(is.na(data1),2,sum)
