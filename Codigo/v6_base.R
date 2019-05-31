##################################
#Version 06
# Fecha_ 127/05/2019
#Objetivos: seleccion de variables con datos balanceados

#Resultados
####################################

#librerias
library(pROC)
library(caret)
library(randomForest)
library(ranger)
library(ROSE)

source('Funciones\\funcion steprepetido binaria2.R')#logistica binaria AIC y BIC
# library()

#Importacion de datos
load(file="Datos\\datos.Rdata")
data1$deny <- ifelse(data1$deny == 'Yes','1','0')
data1$deny <- factor(data1$deny)
data1$V1 <- NULL
levels(data1$deny)
df<-data1

#Variables
dput(names(data1))
listconti <- c("dir", "hir", "lvr", "ccs", "mcs", "uria")
listclass <- c("pbcr", "dmi", "self", "single", "comdominiom", "black")
vardep <- c("deny")

#Tecnicas para balancear el dataset
f<- formula(paste0(vardep,"~."))
set.seed(1234)
dfb <- ROSE(f,data = df)$data

#Seleccion de variables ----

#Aproximacion lineal ----
#Algoritmo regresion logistica
set.seed(1234)

#AIC con remuestreo
modeloStepAIC <- steprepetidobinaria2(data = df,
                                     vardep = vardep,listconti = listconti,sinicio = 12345,
                                     sfinal = 12355,porcen = 0.8,criterio="AIC")

tabla_AIC <- modeloStepAIC[[1]]
dput(modeloStepAIC [[2]][[1]])
#c("ccs", "dir", "lvr", "mcs", "uria")
#c("ccs", "dir", "lvr", "mcs", "uria","hir")


modeloStepBIC <- steprepetidobinaria2(data = df,
                                     vardep = vardep,listconti = listconti,sinicio = 12345,
                                     sfinal = 12355,porcen = 0.8,criterio = "BIC")
tabla_BIC <- modeloStepBIC[[1]]
dput(modeloStepBIC  [[2]][[1]])
#c("ccs", "lvr", "dir", "mcs", "uria")


#AIC todas las observaciones
full <- glm(deny~.,data = df,family = binomial(link = "logit"))
null <- glm(deny~1,data = df,family = binomial(link = "logit"))
modeloStepAIC_full <- stepAIC(null, scope = list(lower = null, upper = full), direction = "both")
summary(modeloStepAIC_full)
formula(modeloStepAIC_full)
dput(names(modeloStepAIC_full$coefficients))

# c( "dmi", "ccs", "dir", "pbcr", "black", "lvr", 
#   "self", "single", "uria", "mcs")

## #### Valiacion cruzada de modelos con regresion logistica

m01l <- c("ccs", "dir", "lvr", "mcs", "uria") #AIC y BIC con remuestreo
m02l <- c("ccs", "dir", "lvr", "mcs", "uria","hir") #AIC con remuestreo
m03l <- c( "dmi", "ccs", "dir", "pbcr", "black", "lvr","self", "single", "uria", "mcs") #AIC entero
#m04 <- c("1")
modelos <- lapply(list(m01l,m02l,m03l)
                 , function(x)formula(paste0(vardep,"~",paste0(x,collapse = "+"))))
total <- c()

for(i in seq_along(modelos)){
  print(modelos[[i]])
  aux<-GlmDesbalance(df=df,ecuacion=modelos[[i]],grupos = 4,repeticiones = 5,vardep = vardep) 
  if(i<10){
    aux$modelo <- paste0("Modelo0",i)
  }
  else{aux$modelo <- paste0("Modelo",i)}
  total <- rbind(total,data.frame(aux) )
}
boxplot(Accuracy~modelo,data=total,main="Accuracy", cex.axis=1,las=2)                 
boxplot(Sensitivity~modelo,data=total,main="Sensivity", cex.axis=1,las=2) 
boxplot(auc~modelo,data=total,main="ROC", cex.axis=1,las=2) 





#Aproximacion no lineal ----
#Algoritmo Random Forest
set.seed(1234)
rfbis<-randomForest(factor(deny)~.,data=dfb,
                    mtry=3,ntree=1000,sampsize=300,nodesize=10,replace=TRUE, importance=TRUE)



tabla <- as.data.frame(importance(rfbis))
tabla <- tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla <- as.data.frame(tabla)

dput(row.names(tabla))


barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla),las=2, main="Decrease in accuray"
        ,cex.main=1.25, cex.lab=2.5, cex.axis=1.75, cex.names = 1.2)


##cv de modelos tentativos con rf
#lista de modelos


m01 <- c("dir", "dmi", "hir", "ccs", "pbcr", "lvr", "black", "uria", 
  "mcs", "self", "single", "comdominiom")
m02 <- c("dir", "dmi", "hir")
m03 <- c("dir", "dmi", "hir","ccs")
m04 <- c("dir", "dmi", "hir","pbcr" )
m05 <- c("dir", "dmi", "hir",  "lvr")      
m06 <- c("dir", "dmi", "hir","black")
m07 <- c("dir", "dmi", "hir","uria" )
m08 <- c("dir", "dmi", "hir","mcs")
m09 <- c("dir", "dmi", "hir","self")   
m10 <- c("dir", "dmi", "hir","single" )
m11 <- c("dir", "dmi", "hir","comdominiom")
        
modelos<- lapply(list(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11)
                 , function(x)formula(paste0(vardep,"~",paste0(x,collapse = "+"))))

#modelos <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)
total <- c()
for(i in  seq_along(modelos)){
aux <- RangerDesbalance(df = df,ecuacion = modelos[[i]],grupos = 4,repeticiones = 5,vardep = 'deny',
                 mtry = 3,num.trees = 500,min.node.size = 10)
if(i<10){
aux$modelo <- paste0("Modelo0",i)
}
else{aux$modelo <- paste0("Modelo",i)}
total <- rbind(total,data.frame(aux) )
}

boxplot(Accuracy~modelo,data=total,main="Accuracy", cex.axis=1,las=2)                 
boxplot(Sensitivity~modelo,data=total,main="Sensivity", cex.axis=1,las=2) 
boxplot(auc~modelo,data=total,main="ROC", cex.axis=1,las=2) 


## Siguientes modelos

m11 <- c("dir", "dmi", "hir","ccs")
m12 <- c("dir", "dmi", "hir","ccs","pbcr")
m13 <- c("dir", "dmi", "hir","ccs","lvr")
m14 <- c("dir", "dmi", "hir","ccs","black")
m15 <- c("dir", "dmi", "hir","pbcr","lvr" )
m16 <- c("dir", "dmi", "hir","pbcr","black" )
m17 <- c("dir", "dmi", "hir",  "lvr","black")      
modelos<- lapply(list(m11,m12,m13,m14,m15,m16,m17)
                 , function(x)formula(paste0(vardep,"~",paste0(x,collapse = "+"))))


total1 <- c()
for(i in  seq_along(modelos)){
  aux <- RangerDesbalance(df = df,ecuacion = modelos[[i]],grupos = 4,repeticiones = 5,vardep = 'deny',
                          mtry = 3,num.trees = 500,min.node.size = 10)
  if(i<10){
    aux$modelo <- paste0("Modelo0",i+10)
  }
  else{aux$modelo <- paste0("Modelo",10+i)}
  total1 <- rbind(total1,data.frame(aux) )
}

boxplot(Accuracy~modelo,data=total1,main="Accuracy", cex.axis=1,las=2)                 
boxplot(Sensitivity~modelo,data=total1,main="Sensivity", cex.axis=1,las=2) 
boxplot(auc~modelo,data=total1,main="ROC", cex.axis=1,las=2) 

## Siguientes modelos 

m18 <- c("dir", "dmi", "hir","ccs")
m19 <- c("dir", "dmi", "hir","ccs","pbcr")
m20 <- c("dir", "dmi", "hir","ccs","lvr")
m21 <- c("dir", "dmi", "hir","ccs","black")
m22 <- c("dir", "dmi", "hir","ccs","pbcr","lvr")
m23 <- c("dir", "dmi", "hir","ccs","pbcr","black")
m24 <- c("dir", "dmi", "hir","ccs","lvr","black")
m25 <- c("dir", "dmi", "hir","ccs","pbcr","lvr","black")


modelos<- lapply(list(m18,m19,m20,m21,m22,m23,m24,m25)
                 , function(x)formula(paste0(vardep,"~",paste0(x,collapse = "+"))))


total2 <- c()
for(i in  seq_along(modelos)){
  aux <- RangerDesbalance(df = df,ecuacion = modelos[[i]],grupos = 4,repeticiones = 5,vardep = 'deny',
                          mtry = 3,num.trees = 500,min.node.size = 10)
  if(i<10){
    aux$modelo <- paste0("Modelo0",i+17)
  }
  else{aux$modelo <- paste0("Modelo",17+i)}
  total2 <- rbind(total2,data.frame(aux) )
}
boxplot(Accuracy~modelo,data=total2,main="Accuracy", cex.axis=1,las=2)                 
boxplot(Sensitivity~modelo,data=total2,main="Sensivity", cex.axis=1,las=2) 
boxplot(auc~modelo,data=total2,main="ROC", cex.axis=1,las=2) 


#Comparacion modelos  ----
# 30 repeticiones, se usa random forest con random forest
m01l <- c("ccs", "dir", "lvr", "mcs", "uria")
f1 <- formula(paste0(vardep,"~",paste0(m01l,collapse = "+")))
medias1 <- RangerDesbalance(df = df,ecuacion = f1,grupos = 4,repeticiones = 30,vardep = 'deny',
                            mtry = 3,num.trees = 500,min.node.size = 10)
medias1$modelo <- "Lineal"

m23 <- c("dir", "dmi", "hir","ccs","pbcr","black")
f2 <- formula(paste0(vardep,"~",paste0(m23,collapse = "+")))
medias2 <- RangerDesbalance(df = df,ecuacion = f2,grupos = 4,repeticiones = 30,vardep = 'deny',
                 mtry = 3,num.trees = 500,min.node.size = 10)
medias2$modelo <- "No Lineal 1"

m25 <- c("dir", "dmi", "hir","ccs","pbcr","lvr","black")
f3 <- formula(paste0(vardep,"~",paste0(m25,collapse = "+")))
medias3 <- RangerDesbalance(df = df,ecuacion = f3,grupos = 4,repeticiones = 30,vardep = 'deny',
                 mtry = 3,num.trees = 500,min.node.size = 10)
medias3$modelo <- "No Lineal 2"

total <- rbind(medias1,medias2,medias3)
boxplot(Accuracy~modelo,data=total,main="Accuracy", cex.axis=1,las=2)                 
boxplot(Sensitivity~modelo,data=total,main="Sensivity", cex.axis=1,las=2) 
boxplot(auc~modelo,data=total,main="ROC", cex.axis=1,las=2) 

aggregate(Sensitivity~modelo,data=total,mean)
# Elegimos el Modelo no lineal 2 tiene mejor Sensivity y ROC

save(df,file="Datos\\datos_originales_limpios.Rdata")
