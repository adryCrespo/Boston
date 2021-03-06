##################################
#Version 02
# Fecha_ 22/05/2019
#Objetivos: prediccion por categorias
#Resultados
####################################

library(data.table)#Importacion datos
library(DataExplorer)#Exploracion datos automatico
library(MASS) #stepwise
library(pROC)
library(caret)
library(randomForest)
library(ModelMetrics)
library(ranger)
source('Funciones\\funcion steprepetido binaria.R')#logistica binaria AIC y BIC
source('Funciones\\cruzadas avnnet y log binaria.R')# cv con avnet y logistica
source('Funciones\\cruzada rf binaria.R')#cv con rf

load("Datos\\datos.Rdata")

sensEspCorte<-function(modelo,dd,nombreVar,ptoCorte,evento){
  probs <-predict(modelo,newdata=dd,type="response")
  cm<-caret::confusionMatrix(data=factor(ifelse(probs>ptoCorte,1,0)),
                      reference=dd[,nombreVar],positive=evento)
  c(cm$overall[1],cm$byClass[1:4])
}


#Particion Training/Test
set.seed(123456)
data1$V1 <- NULL
trainIndex <- createDataPartition(data1$deny, p=0.8, list=FALSE)
data_train <- data1[trainIndex,]#quito las variables transformadas por el momento
data_test <- data1[-trainIndex,]

data_train2 <- data_train
data_test2 <- data_test
#Modelos

#modelo lineal ----
data_train2 <- data_train
data_test2 <- data_test

m<-c( "dmi", "ccs", "dir", "pbcr", "black", "lvr", 
      "self", "single", "uria", "mcs")

data_train2$deny<-ifelse(data_train2$deny=='Yes','1','0')
data_train2$deny <- factor(data_train2$deny)
data_test2$deny<-ifelse(data_test2$deny=='Yes','1','0')
data_test2$deny <- factor(data_test2$deny)


f1 <- paste0("factor(deny)~",paste0(m,collapse='+'))
modelo1 <- glm(f1,data=data_train2,family=binomial)

matrix_lineal <- sensEspCorte(modelo1,data_test2,"deny",0.5,'1')

pred1_bc <- predict(modelo1, data_test2, type = "response")
cc <- caret::confusionMatrix(factor(ifelse(pred1_bc>0.5,'1','0')),data_test2$deny )
levels(data_test2$deny)
cc$byClass[,1]
levels(pred1_bc)

shape(data_test2$deny) 
#modelo no lineal 1
m15 <- c("dmi","dir","lvr","pbcr")
f2 <- paste0("factor(deny)~",paste0(m15,collapse='+'))
f2 <- formula(f2)

rf1 <- ranger(f2,data=data_train2,
       mtry=3,num.trees = 500,min.node.size=10, replace = TRUE,probability = TRUE)

probs <- (predict(rf1,data_test2)$predictions)[,2]
cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                       reference=data_test2[,'deny'],positive='1')
modelo_rf1 <- c(cm$overall[1],cm$byClass[1:4])
caret::confusionMatrix(


#modelo no lineal 2
m19 <- c("dmi","dir","lvr","pbcr","hir","single")
f3 <- paste0("factor(deny)~",paste0(m19,collapse='+'))
f3 <- formula(f3)

rf2 <- ranger(f3,data=data_train2,
              mtry=3,num.trees = 500,min.node.size=10, replace = TRUE,probability = TRUE)

probs <- (predict(rf2,data_test2)$predictions)[,2]
cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                           reference=data_test2[,'deny'],positive='1')
modelo_rf2 <-c(cm$overall[1],cm$byClass[1:4])
cm$byClass

#modelo no lineal 3
m22 <- c("dmi","dir","lvr","pbcr","hir","black","single")
f4 <- paste0("factor(deny)~",paste0(m22,collapse='+'))
f4 <- formula(f4)

rf3 <- ranger(f4,data=data_train2,
              mtry=3,num.trees = 500,min.node.size=10, replace = TRUE,probability = TRUE)

probs <- (predict(rf3,data_test2)$predictions)[,2]
cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                           reference=data_test2[,'deny'],positive='1')
modelo_rf3 <-c(cm$overall[1],cm$byClass[1:4])
cm$table


#Matrices de confusion ----

resumen <- data.frame(matrix_lineal,modelo_rf1,modelo_rf2,modelo_rf3)
save(resumen,file="Datos\\resumen.Rdata" )


### Desbalance en categorias target ----

library(ROSE)


data.rose <- ROSE(deny~.,data=data1, seed = 1234)$data
 table(data.rose$deny)

 # data.rose$deny <- ifelse(data.rose$deny == 'Yes','1','0')
 # data.rose$deny <- factor(data.rose$deny, levels = c('1','0') )
 # levels(data.rose$deny)  
 levels(data1$deny)
 #Particion Training/Test
 set.seed(123456)
 #data1$V1 <- NULL
 trainIndex2 <- createDataPartition(data.rose$deny, p=0.8, list=FALSE)
 data_train3 <- data.rose[trainIndex2,]#quito las variables transformadas por el momento
 data_test3 <- data.rose[-trainIndex2,]
 
 data_train3$deny <- ifelse(data_train3$deny=='Yes','1','0')
 data_train3$deny <- factor(data_train3$deny)
 
 data_test3$deny <- ifelse(data_test3$deny=='Yes','1','0')
 data_test3$deny <- factor(data_test3$deny)
 
 
 table(data_train3$deny)
 levels(data_train3$deny)
 
 #modelo no lineal 1
 f2
 rf1b <- ranger(f2,data=data_train3,
               mtry=3,num.trees = 500,min.node.size=10, replace = TRUE,probability = TRUE)
 
 probs <- (predict(rf1b,data_test3)$predictions)[,2]
 cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                            reference=data_test3[,'deny'],positive='1')
 cm$byClass[,1]
 modelo_rf1b <- c(cm$overall[1],cm$byClass[1:4])
 
 #### Todas las variables
 rff <- ranger(deny~.,data=data_train3,
               mtry=3,num.trees = 500,min.node.size=10, replace = TRUE,probability = TRUE)
 
 probs <- (predict(rff,data_test3)$predictions)[,2]
 cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                            reference=data_test3[,'deny'],positive='1')
 modelo_rf1qqq <- c(cm$overall[1],cm$byClass[1:4])
 
 
 probs <- (predict(rff,data_test2)$predictions)[,2]
 cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                            reference=data_test2[,'deny'],positive='1')
 modelo_rf1qqq <- c(cm$overall[1],cm$byClass[1:4])
 
 #modelo no lineal 2
 f2
 rf2b <- ranger(f3,data=data_train3,
                mtry=3,num.trees = 500,min.node.size=10, replace = TRUE,probability = TRUE)
 
 probs <- (predict(rf2b,data_test3)$predictions)[,2]
 cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                            reference=data_test3[,'deny'],positive='1')
 modelo_rf2b <- c(cm$overall[1],cm$byClass[1:4])
 
 
 
 
 probs <- (predict(rf2b,data_test2)$predictions)[,2]
 cm<-caret::confusionMatrix(data=factor(ifelse(probs>0.5,'1','0')),
                            reference=data_test2[,'deny'],positive='1')
 modelo_rf2b <- c(cm$overall[1],cm$byClass[1:4])
 