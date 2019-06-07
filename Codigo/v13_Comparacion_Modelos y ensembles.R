##################################
#Version 13
# Fecha_ 05/06/2019
#Objetivos: comparar modelos y ensembles

####################################

#Librerias
library(caret)
library(ROSE)
library(dplyr)
library(dummies)
#importacion funciones
source(file="Funciones\\funcionesDesbalance.R")
source(file="Funciones\\cruzada gbm binaria2.R")
source(file="Funciones\\cruzada SVM binaria lineal2.R")
source(file = "Funciones\\cruzada SVM binaria polinomial2.R")
source(file = "Funciones\\cruzada SVM binaria RBF2.R")
source(file = "Funciones\\cruzada rf binaria2.R")
source(file = "Funciones\\cruzadas avnnet y log binaria2.R")


#Datos
load(file="Datos\\datos_originales_limpios.Rdata")

#Variables
vardep <- c("deny")
vars <- c("dir", "dmi", "hir","ccs","pbcr","lvr","black")
f <- formula(paste0(vardep,"~",paste0(vars,collapse = "+")))
listconti <- dput(names(Filter(is.numeric,df[,vars])))
listclass <- dput(names(Filter(is.factor,df[,vars])))

# #Cambio nombre varaible dependiente por caret
df$deny <- ifelse(df$deny==1,'Yes','No')
df$deny <- factor(df$deny)

semilla <- 1234
set.seed(semilla)
repeticiones <- 20

##Modelo Red ----
    # size 5 decay 0.1
  # medidas1 <- RedesDesbalance(df=df,listconti,listclass,grupos = 4, repeticiones = repeticiones,vardep = vardep,
  #               size_net = 5,decay_net= 0.1,numIter = 150,semilla = semilla)

medidas1 <- cruzadaavnnetbin2(data=df,vardep=vardep,
  listconti=listconti,listclass=listclass,grupos=4,sinicio=semilla,repe=repeticiones,
  size=c(5),decay=c(0.1),repeticiones=5,itera= 150,trace=TRUE)


  medidas1bis <- medidas1[[1]] 
  medidas1bis$modelo <- "Redes"
  medidas1bis <- medidas1bis[,c("Accuracy","auc","sensibilidad","modelo")]
  colnames(medidas1bis) <- c("Accuracy","auc","Sensitivity","modelo")  
  
  predi1 <- as.data.frame(medidas1[2])
  predi1$net <- predi1$Yes
  
##Modelo Rf ----
    # num.trees=800,min.node.size=10,mtry=2
  # medidas2 <- RangerDesbalance(df=df,ecuacion=f,grupos=4,repeticiones=repeticiones,vardep=vardep,
  #                            mtry=2,num.trees = 800,min.node.size=10,semilla=semilla)
  # 
  medidas2 <- cruzadarfbin2(data=df,vardep=vardep,listconti=listconti,listclass=listclass,
    grupos=4,sinicio=semilla,repe=repeticiones,nodesize=10,
    mtry=2,ntree=800,replace=TRUE)
  
  medidas2bis <- medidas2[[1]] 
  medidas2bis$modelo <- "Rf"
  medidas2bis <- medidas2bis[,c("Accuracy","auc","sensibilidad","modelo")]
  colnames(medidas2bis) <- c("Accuracy","auc","Sensitivity","modelo")  
  
  predi2 <- as.data.frame(medidas2[2])
  predi2$Rf <- predi2$Yes
  
  
##Modelo regresion logistica ----
  # medidas3 <-   GlmDesbalance(df=df,ecuacion=f,grupos = 4,repeticiones = repeticiones,vardep = vardep) 
  # 
  # medidas3$modelo <- "Logistica"
  # medidas3 <- medidas3[,c("Accuracy","auc","Sensitivity","modelo")]
  # 
  medidas3 <-  cruzadalogistica2(data=df,vardep=vardep,
                                listconti=listconti,listclass=listclass,grupos=4,sinicio=semilla,repe=repeticiones)
  medidas3bis <- medidas3[[1]] 
  medidas3bis$modelo <- "Logistica"
  medidas3bis <- medidas3bis[,c("Accuracy","auc","sensibilidad","modelo")]
  colnames(medidas3bis) <- c("Accuracy","auc","Sensitivity","modelo")  
  
  predi3 <- as.data.frame(medidas3[2])
  predi3$Log <- predi3$Yes
  
##Modelo bagging ----
 # num.trees=800,min.node.size=10
  
  # medidas4 <- RangerDesbalance(df=df,ecuacion=f,grupos=4,repeticiones=repeticiones,vardep=vardep,
  #                 mtry=length(vars),num.trees = 800,min.node.size=10,semilla=semilla)
  # 
  # medidas4$modelo <- "Bagging"
  # medidas4 <- medidas4[,c("Accuracy","auc","Sensitivity","modelo")]

  medidas4 <- cruzadarfbin2(data=df,vardep=vardep,listconti=listconti,listclass=listclass,
                            grupos=4,sinicio=semilla,repe=repeticiones,nodesize=10,
                            mtry=length(vars),ntree=800,replace=TRUE)
  
  medidas4bis <- medidas4[[1]] 
  medidas4bis$modelo <- "Bagging"
  medidas4bis <- medidas4bis[,c("Accuracy","auc","sensibilidad","modelo")]
  colnames(medidas4bis) <- c("Accuracy","auc","Sensitivity","modelo")  
  
  predi4 <- as.data.frame(medidas2[2])
  predi4$Bagging <- predi4$Yes
  

##Modelo GBM ----  
  # shrinkage= 0.05
  # n.minobsinnode= 5
  # n.trees= 500
medidas5 <-  cruzadagbmbin2(data=df,vardep=vardep,listconti=listconti,listclass=listclass,
                 grupos=4,sinicio=semilla,repe=repeticiones,
                 n.minobsinnode=5,shrinkage=0.05,
                 n.trees=500,interaction.depth=2)

medidas5bis <- medidas5[[1]]  
medidas5bis$modelo <- "GBM"
medidas5bis <- medidas5bis[,c("Accuracy","auc","sensibilidad","modelo")]
colnames(medidas5bis) <- c("Accuracy","auc","Sensitivity","modelo")  
  
predi5 <- as.data.frame(medidas5[2])
predi5$GBM <- predi5$Yes



#Modelo SVM simple ----
  #C 0.05

medidas6 <- cruzadaSVMbin2(data = df,vardep = vardep,
                 listconti = listconti,listclass = listclass,
                 grupos = 4,sinicio = semilla,repe = repeticiones,
                 C=0.05,replace = TRUE)

medidas6bis <- medidas6[[1]]
medidas6bis$modelo <- "SVM_lineal"
medidas6bis <- medidas6bis[,c("Accuracy","auc","sensibilidad","modelo")] 
colnames(medidas6bis) <- c("Accuracy","auc","Sensitivity","modelo")

predi6 <- as.data.frame(medidas6[2])
predi6$SVM_lineal <- predi6$Yes

##Modelo SVM polinomial ----
# C 0.05
# degree 2
# scale 0.5

medidas7 <- cruzadaSVMbinPoly2(data=df,vardep=vardep,
                   listconti=listconti,listclass=listclass,
                   grupos=4,sinicio=semilla,repe=repeticiones,
                   C=0.05,degree=2,scale=0.5)

medidas7bis <- medidas7[[1]]
medidas7bis$modelo <- "SVM_polinomial"
medidas7bis <- medidas7bis[,c("Accuracy","auc","sensibilidad","modelo")] 
colnames(medidas7bis) <- c("Accuracy","auc","Sensitivity","modelo")

predi7 <- as.data.frame(medidas7[2])
predi7$SVM_Poli <- predi7$Yes


##Modelo SVM RBF---- 
# C 0.2
# sigma 0.5

medidas8 <- cruzadaSVMbinRBF2(data=df,vardep=vardep,
                  listconti=listconti,listclass=listclass,
                  grupos=4,sinicio=semilla,repe=repeticiones,
                  C=0.2,sigma=0.5)

medidas8bis <- medidas8[[1]]
medidas8bis$modelo <- "SVM_RBF"
medidas8bis <- medidas8bis[,c("Accuracy","auc","sensibilidad","modelo")] 
colnames(medidas8bis) <- c("Accuracy","auc","Sensitivity","modelo")

predi8 <- as.data.frame(medidas8[2])
predi8$SVM_RBF <- predi8$Yes

##Junto todos los modelos ----
total <-  rbind(medidas1bis,medidas2bis,medidas3bis,medidas4bis,medidas5bis,
                medidas6bis,medidas7bis,medidas8bis)
save(total, file = "Datos\\Comparacion.Rdata")

boxplot(Accuracy~modelo,data=total,las=2,main="Comparacion Accuracy", ylab="Accuracy")
boxplot(Sensitivity~modelo,data=total,las=2,main="Comparacion Sensitivity", ylab="Sensitivity")
boxplot(auc~modelo,data=total,las=2,main="Comparacion Auc", ylab="Auc")


## Construccion ensembles ----
# 1: net    unipredi$net
# 2: Rf     unipredi$Rf
# 3: Log    unipredi$Log  
# 4: Bagging  unipredi$Bagging
# 5: GBM      unipredi$GBM
# 6: SVM_lineal   unipredi$SVM_lineal 
# 7: SVM_Poli     unipredi$SVM_Poli 
# 8: SVM_RBF      unipredi$SVM_RBF
unipredi <- cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)
lista <- c("unipredi$net","unipredi$Rf","unipredi$Log" ,"unipredi$Bagging",
           "unipredi$GBM","unipredi$SVM_lineal", "unipredi$SVM_Poli","unipredi$SVM_RBF" )
listas <- c("net","Rf","Log" ,"Bagging",
           "GBM","SVM_lineal", "SVM_Poli","SVM_RBF" )

save(unipredi,file="Datos\\unipredi.Rdata")

abc<-corrplot(cor(unipredi[,listas]))
library(corrplot)

unipredi$predi9  <- (unipredi$net + unipredi$Rf)/2
unipredi$predi10  <- (unipredi$net + unipredi$Log)/2
unipredi$predi11  <- (unipredi$net + unipredi$GBM)/2
unipredi$predi12  <- (unipredi$net + unipredi$SVM_lineal)/2
unipredi$predi13  <- (unipredi$net + unipredi$SVM_Poli )/2
unipredi$predi14  <- (unipredi$net + unipredi$SVM_RBF)/2

unipredi$predi15  <- (unipredi$Rf + unipredi$Log)/2
unipredi$predi16  <- (unipredi$Rf + unipredi$Bagging)/2
unipredi$predi17  <- (unipredi$Rf + unipredi$GBM)/2
unipredi$predi18  <- (unipredi$Rf + unipredi$SVM_lineal)/2
unipredi$predi19  <- (unipredi$Rf + unipredi$SVM_Poli )/2
unipredi$predi20  <- (unipredi$Rf + unipredi$SVM_RBF)/2
unipredi$predi21  <- (unipredi$Log + unipredi$Bagging)/2
unipredi$predi22  <- (unipredi$Log + unipredi$GBM)/2
unipredi$predi23  <- (unipredi$Log + unipredi$SVM_lineal)/2
unipredi$predi24  <- (unipredi$Log + unipredi$SVM_Poli )/2
unipredi$predi25  <- (unipredi$Log + unipredi$SVM_RBF)/2
unipredi$predi26  <- (unipredi$Bagging + unipredi$GBM)/2
unipredi$predi27  <- (unipredi$Bagging + unipredi$SVM_lineal)/2
unipredi$predi28  <- (unipredi$Bagging + unipredi$SVM_Poli)/2
unipredi$predi29  <- (unipredi$Bagging + unipredi$SVM_RBF)/2
unipredi$predi30  <- (unipredi$SVM_lineal + unipredi$SVM_Poli)/2 
unipredi$predi31  <- (unipredi$SVM_lineal + unipredi$SVM_RBF)/2
unipredi$predi32  <- (unipredi$SVM_Poli + unipredi$SVM_RBF)/2
 
unipredi$predi33   <- (unipredi$net + unipredi$Rf+unipredi$Log)/3
unipredi$predi34   <- (unipredi$net + unipredi$Rf+unipredi$GBM)/3
unipredi$predi35   <- (unipredi$net + unipredi$Rf+unipredi$SVM_lineal)/3
unipredi$predi36   <- (unipredi$net + unipredi$Rf+unipredi$SVM_Poli)/3
unipredi$predi37   <- (unipredi$net + unipredi$Rf+unipredi$SVM_RBF)/3
unipredi$predi38   <- (unipredi$net + unipredi$Log+unipredi$GBM)/3
unipredi$predi39   <- (unipredi$net + unipredi$Log+unipredi$SVM_lineal)/3
unipredi$predi40   <- (unipredi$net + unipredi$Log+unipredi$SVM_Poli)/3
unipredi$predi41   <- (unipredi$net + unipredi$Log+unipredi$SVM_RBF)/3
unipredi$predi42   <- (unipredi$net + unipredi$SVM_lineal+unipredi$SVM_Poli)/3
unipredi$predi43   <- (unipredi$net + unipredi$SVM_lineal+unipredi$SVM_RBF)/3
unipredi$predi44   <- (unipredi$net + unipredi$SVM_Poli+unipredi$SVM_RBF)/3

unipredi$predi45   <- (unipredi$Rf+unipredi$Log+unipredi$GBM)/3
unipredi$predi46   <- (unipredi$Rf+unipredi$Log+unipredi$SVM_lineal)/3
unipredi$predi47   <- (unipredi$Rf+unipredi$Log+unipredi$SVM_Poli)/3
unipredi$predi48   <- (unipredi$Rf+unipredi$Log+unipredi$SVM_RBF)/3

unipredi$predi49   <- (unipredi$Log+unipredi$GBM+unipredi$SVM_lineal)/3
unipredi$predi50   <- (unipredi$Log+unipredi$GBM+unipredi$SVM_Poli)/3
unipredi$predi51   <- (unipredi$Log+unipredi$GBM+unipredi$SVM_RBF)/3

unipredi$predi52   <- (unipredi$GBM+unipredi$SVM_lineal+unipredi$SVM_Poli)/3
unipredi$predi53   <- (unipredi$GBM+unipredi$SVM_lineal+unipredi$SVM_RBF)/3
unipredi$predi54   <- (unipredi$SVM_lineal+unipredi$SVM_Poli+unipredi$SVM_RBF)/3



unipredi$predi55   <- (unipredi$net + unipredi$Rf + unipredi$Log + unipredi$GBM)/4
unipredi$predi56   <- (unipredi$net + unipredi$Rf + unipredi$Log +unipredi$SVM_lineal)/4
unipredi$predi57   <- (unipredi$net + unipredi$Rf + unipredi$Log +unipredi$SVM_Poli)/4
unipredi$predi58   <- (unipredi$net + unipredi$Rf + unipredi$Log +unipredi$SVM_RBF)/4

unipredi$predi59   <- ( unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_lineal)/4
unipredi$predi60   <- ( unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_Poli)/4
unipredi$predi61   <- ( unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_RBF)/4

unipredi$predi62   <- ( unipredi$Log + unipredi$GBM+unipredi$SVM_lineal+unipredi$SVM_Poli)/4
unipredi$predi63   <- ( unipredi$Log + unipredi$GBM+unipredi$SVM_lineal+unipredi$SVM_RBF)/4
unipredi$predi64   <- ( unipredi$GBM+unipredi$SVM_lineal+unipredi$SVM_Poli+unipredi$SVM_RBF)/4


unipredi$predi65   <- (unipredi$net + unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_lineal)/5
unipredi$predi66   <- (unipredi$net + unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_Poli)/5
unipredi$predi67   <- (unipredi$net + unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_RBF)/5

unipredi$predi68   <- ( unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_lineal+unipredi$SVM_Poli)/5
unipredi$predi69   <- ( unipredi$Rf + unipredi$Log + unipredi$GBM+unipredi$SVM_lineal+unipredi$SVM_RBF)/5

# unipredi <- cbind(predi1,predi3)
# unipredi <- unipredi[, !duplicated(colnames(unipredi))]
# unipredi$11 <- unipredi$SVM_lineal+ unipredi$Rf

# unipredi$
# 
# unipredi$predi9<-(unipredi$reg+unipredi$avnnet)/2
# unipredi$predi10<-(unipredi$reg+unipredi$rf)/2
# unipredi$predi11<-(unipredi$reg+unipredi$gbm)/2
# unipredi$predi12<-(unipredi$reg+unipredi$xgbm)/2
# unipredi$predi13<-(unipredi$reg+unipredi$svmLinear)/2
# unipredi$predi14<-(unipredi$reg+unipredi$svmPoly)/2
# unipredi$predi15<-(unipredi$reg+unipredi$svmRadial)/2
# unipredi$predi16<-(unipredi$avnnet+unipredi$rf)/2
# unipredi$predi17<-(unipredi$avnnet+unipredi$gbm)/2
# unipredi$predi18<-(unipredi$avnnet+unipredi$xgbm)/2
# unipredi$predi19<-(unipredi$avnnet+unipredi$svmLinear)/
#   unipredi$predi20<-(unipredi$avnnet+unipredi$svmPoly)/2
# unipredi$predi21<-(unipredi$avnnet+unipredi$svmRadial)/
#   unipredi$predi22<-(unipredi$rf+unipredi$gbm)/2
# unipredi$predi23<-(unipredi$rf+unipredi$xgbm)/2
# unipredi$predi24<-(unipredi$rf+unipredi$svmLinear)/2
# unipredi$predi25<-(unipredi$rf+unipredi$svmPoly)/2
# unipredi$predi26<-(unipredi$rf+unipredi$svmRadial)/2
# unipredi$predi27<-(unipredi$gbm+unipredi$xgbm)/2
# unipredi$predi28<-(unipredi$gbm+unipredi$svmLinear)/2
# unipredi$predi29<-(unipredi$gbm+unipredi$svmPoly)/2
# unipredi$predi30<-(unipredi$gbm+unipredi$svmRadial)/2

listado <- c("net","Rf","Log" ,"Bagging",
            "GBM","SVM_lineal", "SVM_Poli","SVM_RBF",
            "predi9", 
            "predi10", "predi11", "predi12", "predi13", "predi14", "predi15", 
            "predi16", "predi17", "predi18", "predi19", "predi20", "predi21", 
            "predi22", "predi23", "predi24", "predi25", "predi26", "predi27", 
            "predi28", "predi29", "predi30", "predi31", "predi32", "predi33", 
            "predi34", "predi35", "predi36", "predi37", "predi38", "predi39", 
            "predi40", "predi41", "predi42", "predi43", "predi44", "predi45", 
            "predi46", "predi47", "predi48", "predi49", "predi50", "predi51", 
            "predi52", "predi53", "predi54", "predi55", "predi56", "predi57", 
            "predi58", "predi59", "predi60", "predi61", "predi62", "predi63", 
            "predi64", "predi65", "predi66", "predi67", "predi68", "predi69")

# repeticiones1<-nlevels(factor(unipredi$Rep))

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)


medias0_auc<-data.frame(c())
for (prediccion in listado){
  for (repe in 1:repeticiones)
  {
   
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-(paso[,prediccion])
    obs<-paso[,c("obs")]
    t<-auc(obs,pre)
    t<-as.data.frame(t)
    t$modelo<-prediccion
    medias0_auc<-rbind(medias0_auc,t)
  }
}

colnames(medias0_auc)<-c("auc","modelo")
boxplot(auc~modelo,data=medias0_auc, las=2,main="k")

# PRESENTACION TABLA MEDIAS de auc

tablamedias_auc<-medias0_auc %>%
  group_by(modelo) %>%
  summarize(auc=mean(auc))     

tablamedias_auc<-tablamedias_auc[order(tablamedias_auc$auc,decreasing = FALSE),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0_auc$modelo <- with(medias0_auc,
                       reorder(modelo,auc, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0_auc,auc~modelo,
        col="pink",las=2,main="Comparacion modelos auc",
        ylab="AUC", ylim=c(0.78,0.84))



# Cambio a Yes, No, todas las predicciones

for (prediccion in listado){
  unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
}

# Defino funcion tasafallos

tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}
library(pROC)
auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

sensibilidad <- function(x,y) {
  confu<-confusionMatrix(x,y)
  sens <- confu$byClass[[1]]
  return(sens)
}
# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)


medias0<-data.frame(c())
for (prediccion in listado){
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    obs<-paso[,c("obs")]
    Accuracy=tasafallos(pre,obs)
    #aucROC=auc(pre,obs)
    sens=sensibilidad(pre,obs)
    t<-as.data.frame(Accuracy)
    #tt<-as.data.frame(aucROC)
    #ttt<-as.data.frame(sens)
    t$modelo<-prediccion
    medias0<-rbind(medias0,t)
  }
}

# Finalmente boxplot (solo he calculado tasa fallos)

par(cex.axis=0.5,las=2)
boxplot(data=medias0,Accuracy~modelo)


# PRESENTACION TABLA MEDIAS

tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(Accuracy=mean(Accuracy))     

tablamedias<-tablamedias[order(tablamedias$Accuracy,decreasing = TRUE),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,Accuracy, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,Accuracy~modelo,col="pink",
        las=2,main="Comparacion modelos accuracy",
        ylab="Accuracy",ylim=c(0.86,0.94))

############


medias0_sens<-data.frame(c())
for (prediccion in listado){
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    obs<-paso[,c("obs")]
    Accuracy=tasafallos(pre,obs)
    #aucROC=auc(pre,obs)
    sens=sensibilidad(pre,obs)
    #t<-as.data.frame(Accuracy)
    #t<-as.data.frame(aucROC)
    t<-as.data.frame(sens)
    t$modelo<-prediccion
    medias0_sens<-rbind(medias0_sens,t)
  }
}

# Finalmente boxplot (solo he calculado tasa fallos)

par(cex.axis=0.5,las=2)
boxplot(data=medias0_sens,sens~modelo)


# PRESENTACION TABLA MEDIAS

tablamedias_sens<-medias0_sens %>%
  group_by(modelo) %>%
  summarize(Sensivity=mean(sens))     

tablamedias_sens<-tablamedias_sens[order(tablamedias_sens$Sensivity,decreasing = TRUE),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0_sens$modelo <- with(medias0_sens,
                       reorder(modelo,sens, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0_sens,sens~modelo,col="pink",las=2,main="Comparacion modelos sensivity", ylab="Sensivity"
        ,ylim=c(0.9,1))

save(tablamedias,file = "Datos\\tablamedias.Rdata")
save(tablamedias_auc,file = "Datos\\tablamedias_auc.Rdata")
save(tablamedias_sens,file = "Datos\\tablamedias_sens.Rdata")
