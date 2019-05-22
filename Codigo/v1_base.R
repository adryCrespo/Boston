##################################
#Version 01
# Fecha_ 19/05/2019
#Objetivos: importacion de datos , exploracion inicial,seleccion de variables
#Resultados
####################################


library(data.table)#Importacion datos
library(DataExplorer)#Exploracion datos automatico
library(MASS) #stepwise
library(pROC)
library(caret)
library(randomForest)

source('Funciones\\funcion steprepetido binaria.R')#logistica binaria AIC y BIC
source('Funciones\\cruzadas avnnet y log binaria.R')# cv con avnet y logistica
source('Funciones\\cruzada rf binaria.R')#cv con rf
#Importar datos ----
data1    <- as.data.frame(fread('Datos\\Hdma.csv')) #datos training

##Variables ----
dput(names(data1))
listconti <- c("dir", "hir", "lvr", "ccs", "mcs", "uria")
listclass <- c("pbcr", "dmi", "self", "single", "comdominiom", "black")
vardep <- c("deny")
### Contabilizar los missings

 apply(is.na(data1),2,sum)
# V1         dir         hir 
# 0           0           0 
# lvr         ccs         mcs 
# 0           0           0 
# pbcr         dmi        self 
# 1           0           1 
# single        uria comdominiom 
# 0           0           0 
# black        deny 
# 0           0 
 
data1 <- na.omit(data1)

#Exploracion de datos ----
create_report(data1, y = "deny")# crea informe en fotos
table(data1$deny)
# no  yes 
# 2096  285 
#Desbalance 7.33

data1[,c(listclass[-5]) ] <- apply(data1[,c(listclass[-5]) ],2,function(x) ifelse(x == "yes","1","0"))
data1[,c(listclass,vardep) ] <- lapply(data1[,c(listclass,vardep) ],function(x) factor(x))
data1$deny <- ifelse(data1$deny == 'yes','Yes','no')
#variables cate: 1 y 0, excepto objetivo que es deny yes or no 
# todas factoriales

save(data1,file="Datos\\datos.Rdata")

#SELECCION DE VARIABLES ----

#1 Aproximacion lineal
#AIC todas las observaciones
full <- glm(deny~.,data = data1[,-1],family = binomial(link = "logit"))
null <- glm(deny~1,data = data1[,-1],family = binomial(link = "logit"))
modeloStepAIC_full <- stepAIC(null, scope = list(lower = null, upper = full), direction = "both")
summary(modeloStepAIC_full)
formula(modeloStepAIC_full)
#deny ~ dmi + ccs + dir + pbcr + black + lvr + self + single + uria + mcs
c( "dmi", "ccs", "dir", "pbcr", "black", "lvr", 
  "self", "single", "uria", "mcs")

#AIC con remuestreo
modeloStepAIC <- steprepetidobinaria(data = data1[,-1],
                           vardep = vardep,listconti = listconti,sinicio = 12345,
                           sfinal = 12355,porcen = 0.8,criterio="AIC")

tabla <- modeloStepAIC[[1]]
dput(modeloStepAIC [[2]][[1]])
      #"deny~ccs+dir+lvr+mcs+uria"
      #c("ccs", "dir", "lvr", "mcs", "uria")
#BIC con remuestreo
modeloStepBIC <- steprepetidobinaria(data = data1[,-1],
                           vardep = vardep,listconti = listconti,sinicio = 12345,
                           sfinal = 12355,porcen = 0.8,criterio = "BIC")
tabla <- modeloStepBIC[[1]]
dput(modeloStepBIC  [[2]][[1]])
    #"deny~ccs+dir+lvr"
    #c("ccs", "dir", "lvr")

#### Valiacion cruzada de modelos con regresion logistica
medias1<-cruzadalogistica(data=data1,
                          vardep=vardep,listconti=c("ccs", "dir", "lvr", "mcs", "uria"),
                          listclass=c(""), grupos=4,sinicio=1234,repe=5)
medias1$modelo="Log_AIC_remuestreo"


medias2<-cruzadalogistica(data=data1,
                          vardep=vardep,listconti=c("ccs", "dir", "lvr"),
                          listclass=c(""), grupos=4,sinicio=1234,repe=5)
medias2$modelo="Log_BIC_remuestreo"


medias3 <- cruzadalogistica(data = data1,
                          vardep = vardep,listconti=c("ccs", "dir", "lvr", "uria", "mcs"),
                          listclass = c("dmi", "pbcr", "black", "self", "single"), grupos=4,sinicio=1234,repe=5)
medias3$modelo="Log_AIC_total"

union1<-rbind(medias1,medias2,medias3)
par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",cex.axis=0.75)
boxplot(data=union1,auc~modelo,main="AUC",cex.axis=0.75,las=1)

aggregate(auc~modelo,data=union1,mean)
# modelo       tasa
# 1 Log_AIC_remuestreo 0.11487395
# 2      Log_AIC_total 0.09655462
# 3 Log_BIC_remuestreo 0.11445378
aggregate(auc~modelo,data=union1,sd)
#modelo         tasa
# 1 Log_AIC_remuestreo 0.0015888799
# 2      Log_AIC_total 0.0007516195
# 3 Log_BIC_remuestreo 0.0008712790
aggregate(tasa~modelo,data=union1,mean)
#modelo       auc
# 1 Log_AIC_remuestreo 0.7656991
# 2      Log_AIC_total 0.8226280
# 3 Log_BIC_remuestreo 0.7628130
aggregate(tasa~modelo,data=union1,sd)
#modelo         auc
# 1 Log_AIC_remuestreo 0.002128531
# 2      Log_AIC_total 0.001845248
# 3 Log_BIC_remuestreo 0.002292732





#2 Aproximacion no lineal ----



#Gradient Boosting

library(caret)
set.seed(12345)
gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(5,10,20),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2))
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE)
gbm<- train(factor(deny)~.,data=data1[,-1],
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)
gbm
plot(gbm)


gbmgrid<-expand.grid(shrinkage=c(0.03),
                     n.minobsinnode=c(20),
                     n.trees=c(5000),
                     interaction.depth=c(2))
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE)
gbm<- train(factor(deny)~.,data=data1[,-1],
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)
gbm
plot(gbm)


summary(gbm)
tabla99 <- summary(gbm)
par(cex = 1.5,las = 2)
barplot(tabla99$rel.inf,names.arg=row.names(tabla),main = "Gradient Boosting",las=2
        ,cex.main = 1.25, cex.lab=2.5, cex.axis = 1.75, cex.names = 1.2)

#Random Forest
rfbis<-randomForest(factor(deny)~.,data=data1[,-1],
                    mtry=2,ntree=1000,sampsize=300,nodesize=10,replace=TRUE, importance=TRUE)



tabla <- as.data.frame(importance(rfbis))
tabla <- tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla <- as.data.frame(tabla)

barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla),las=2, main="Decrease in accuray"
        ,cex.main=1.25, cex.lab=2.5, cex.axis=1.75, cex.names = 1.2)


##cv de modelos tentativos con rf
#lista de modelos
#c("dmi","dir","lvr","css","pbcr","hir")
m1 <- c("dmi","dir","lvr","css","pbcr","hir","self")
m2 <- c("dmi","dir","lvr","css","pbcr","hir","single")
m3 <- c("dmi","dir","lvr","css","pbcr","hir","urina")
m4 <- c("dmi","dir","lvr","css","pbcr","hir","dominiom")

m5 <- c("dmi","dir","lvr","css","pbcr","hir","mcs")
m6 <- c("dmi","dir","lvr","css","pbcr","hir","black")
m7 <- c("dmi","dir","lvr","css","pbcr","hir")
m8 <- c("dmi","dir","lvr","css","pbcr")
m9 <- c("dmi","dir","lvr","css","hir")
m10 <- c("dmi","dir","lvr","pbcr","hir")

m11 <- c("dmi","dir","lvr","css")
m12 <- c("dmi","dir","lvr","pbcr")
m13 <- c("dmi","dir","lvr","hir")
m14 <- c("dmi","dir","lvr")

modelos <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)
total <- c()

for(i in seq_along(modelos) ){
 
aux <- cruzadarfbin(data = data1[,-1],
                   vardep=vardep,listconti=dput(modelos[[i]][modelos[[i]] %in% listconti]),
                   listclass=dput(modelos[[i]][modelos[[i]] %in% listclass]),
                   grupos=4,sinicio=1234,repe=5,
                   nodesize=10,replace=TRUE,ntree=600,mtry=3)

aux$modelo <- paste0("Modelo",i)
total <- rbind(total,data.frame(aux) )

}

boxplot(auc~modelo,data=total,main="ROC", cex.axis=1,las=2) #el 3 es peor, los otros parecidos
boxplot(tasa~modelo,data=total,main="tasa de fallos",cex.axis=1,las=2)
aggregate(auc~modelo, data = total, mean) 
aggregate(auc~modelo, data = total, sd) 
aggregate(tasa~modelo, data = total, mean) 
aggregate(tasa~modelo, data = total, sd) 


m15 <- c("dmi","dir","lvr","pbcr")
m16 <- c("dmi","dir","lvr","pbcr","hir")
m17 <- c("dmi","dir","lvr","pbcr","single")
m18 <- c("dmi","dir","lvr","pbcr","black")
m19 <- c("dmi","dir","lvr","pbcr","hir","single")
m20 <- c("dmi","dir","lvr","pbcr","hir","black")
m21 <- c("dmi","dir","lvr","pbcr","single","black")
m22 <- c("dmi","dir","lvr","pbcr","hir","black","single")

modelos1 <- list(m15,m16,m17,m18,m19,m20,m21,m22)
total1 <- c()
for(i in seq_along(modelos1) ) {
  print(i)
  aux <- cruzadarfbin(data = data1[,-1],
                      vardep=vardep,listconti=dput(modelos1[[i]][modelos1[[i]] %in% listconti]),
                      listclass=dput(modelos1[[i]][modelos1[[i]] %in% listclass]),
                      grupos=4,sinicio=1234,repe=5,
                      nodesize=10,replace=TRUE,ntree=600,mtry=3)
  
  print("aaa")
  aux$modelo <- paste0("Modelo",(i+14) )
  total1 <- rbind(total1,data.frame(aux) )
  
}

boxplot(auc~modelo,data=total1,main="ROC", cex.axis=1,las=2) #el 3 es peor, los otros parecidos
boxplot(tasa~modelo,data=total1,main="tasa de fallos",cex.axis=1,las=2)
aggregate(auc~modelo, data = total1, mean) 
aggregate(auc~modelo, data = total1, sd) 
aggregate(tasa~modelo, data = total1, mean) 
aggregate(tasa~modelo, data = total1, sd) 



### RESUMEN ----
m<-c( "dmi", "ccs", "dir", "pbcr", "black", "lvr", 
   "self", "single", "uria", "mcs")
m15 <- c("dmi","dir","lvr","pbcr")
m19 <- c("dmi","dir","lvr","pbcr","hir","single")
m22 <- c("dmi","dir","lvr","pbcr","hir","black","single")
