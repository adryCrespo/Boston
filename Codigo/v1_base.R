##################################
#Version 01
# Fecha_ 19/05/2019
#Objetivos: importacion de datos y exploracion inicial
#Resultados
####################################


library(data.table)#Importacion datos
library(DataExplorer)#Exploracion datos automatico
library(MASS) #stepwise
library(pROC)
library(caret)

source('Funciones\\funcion steprepetido binaria.R')
source('Funciones\\cruzadas avnnet y log binaria.R')

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


#SELECCION DE VARIABLES ----

#1 Aproximacion lineal
#AIC todas las observaciones
full <- glm(deny~.,data = data1[,-1],family = binomial(link = "logit"))
null <- glm(deny~1,data = data1[,-1],family = binomial(link = "logit"))
modeloStepAIC_full <- stepAIC(null, scope = list(lower = null, upper = full), direction = "both")
summary(modeloStepAIC_full)
formula(modeloStepAIC_full)
#deny ~ dmi + ccs + dir + pbcr + black + lvr + self + single + uria + mcs
c( "dmi1", "ccs", "dir", "pbcr", "black", "lvr", 
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
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")

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


#2 Aproximacion no lineal


