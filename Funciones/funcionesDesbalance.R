#variables

# #Ejemplos
# m22 <- c("dmi","dir","lvr","pbcr","hir","black","single")
# f4 <- paste0("factor(deny)~",paste0(m22,collapse='+'))
# f4 <- formula(f4)
# x <- RangerDesbalance(df=df,ecuacion=f4,grupos=5,repeticiones=2,vardep='deny',
#                  mtry=3,num.trees = 500,min.node.size=10)
# vardep='deny'
# grupos=5
# mtry=3 
# num.trees = 500
# min.node.size=10
# x <- RangerDesbalance(df=df,ecuacion=f4,grupos=5,repeticiones=2,vardep='deny',
#                       mtry=3,num.trees = 500,min.node.size=10)


RangerDesbalance <- function(df,ecuacion,grupos=grupos,repeticiones=repeticiones,vardep=vardep,
                              mtry=3,num.trees = 500,min.node.size=10,semilla=111){
    
   library(ranger)
   library(ROSE)
  
  
  auc <- function(x,y) {
    curvaroc <- roc(response=x,predictor=y)
    auc <- curvaroc$auc
    return(auc)
  }
   #lista de resultados 
    resultados <-data.frame()
    total <- c() #vector de apoyo a cada una de las repeticiones
    #medias <- c() #listas con repeticiones
    
    for(j in 1:repeticiones){
      set.seed(semilla + j)
      #shuffle samples
      
      df <- df[sample(nrow(df)),]
      flds <- caret::createFolds(df[,vardep], k = grupos, list = TRUE, returnTrain = TRUE)
      
      
     for (i in 1:grupos) {
      #Particiones
       
      #Training partition
      f<- formula(paste0(vardep,"~."))
      dfTraining <- ROSE(f,data = df[flds[[i]] ,])$data
       
      #dfTraining <- df[flds[[i]],] #Training partition
      dfTest <- df[-flds[[i]],]# Test partition
      
      #modelo ranger
      modelo <- ranger(formula=ecuacion,data=dfTraining ,
                     mtry=mtry,num.trees = num.trees,min.node.size=min.node.size, replace = TRUE,probability = TRUE)
      
      
      #Probabilidades
      probs <- (predict(modelo,dfTest)$predictions)[,2]
      cm <- caret::confusionMatrix(data = factor(ifelse(probs>0.5,'1','0')),
                                    reference = dfTest[,vardep],positive='1')
      modelo_r <- c(cm$overall[1],cm$byClass[1:4],auc=auc(dfTest[,vardep],probs) )
      
      
      
      total <- rbind(total,data.frame(t(modelo_r)) )
      # resultados <- rbind(resultados,data.frame(lapply(total,mean),repeticion='rep1') )
     }
      
     resultados <- rbind(resultados,data.frame(lapply(total,mean),repeticion=paste0('rep',j) ) )
    
    }
     return(resultados)
  }
 
###• Regresion logistica + Datos desbalanceados

GlmDesbalance <- function(df,ecuacion,grupos=grupos,repeticiones = repeticiones,vardep = vardep){
  

  library(ROSE)
  
  
  auc <- function(x,y) {
    curvaroc <- roc(response=x,predictor=y)
    auc <- curvaroc$auc
    return(auc)
  }
  #lista de resultados 
  resultados <-data.frame()
  total <- c() #vector de apoyo a cada una de las repeticiones
  #medias <- c() #listas con repeticiones
  
  for(j in 1:repeticiones){
    set.seed(111 + j)
    #shuffle samples
    df <- df[sample(nrow(df)),]
    flds <- caret::createFolds(df[,vardep], k = grupos, list = TRUE, returnTrain = TRUE)
    
    
    for (i in 1:grupos) {
      #Particiones
      
      #Training partition
      f<- formula(paste0(vardep,"~."))
      dfTraining <- ROSE(f,data = df[flds[[i]] ,])$data
      
      #dfTraining <- df[flds[[i]],] #Training partition
      dfTest <- df[-flds[[i]],]# Test partition
      
      #modelo logistica
      modelo <- glm(formula=ecuacion,data=dfTraining ,family = binomial(link = "logit"))
      
       
      #Probabilidades
      probs <- (predict(modelo,dfTest,type="response"))
      cm <- caret::confusionMatrix(data = factor(ifelse(probs>0.5,'1','0')),
                                   reference = dfTest[,vardep],positive='1')
      modelo_r <- c(cm$overall[1],cm$byClass[1:4],auc=auc(dfTest[,vardep],probs) )
      
      
      
      total <- rbind(total,data.frame(t(modelo_r)) )
      # resultados <- rbind(resultados,data.frame(lapply(total,mean),repeticion='rep1') )
    }
    
    resultados <- rbind(resultados,data.frame(lapply(total,mean),repeticion=paste0('rep',j) ) )
    
  }
  return(resultados)}
      
### Redes + Datos desbalanceados      
RedesDesbalance <- function(df,listconti,listclass,grupos=grupos, repeticiones = repeticiones,vardep = vardep,
                            size_net,decay_net,numIter=150,semilla=111){
  #   
  #listconti variables continuas
  #listclass variables categoricas
  #size_net=3 #nodos
  #decay_net=0.1 #learning rate
  #repeticiones , numero de repeticiones cv
  #grupos <- 2 ,grupos cv
  #numIter <- 150 #iteracciones  
  #semilla , semilla inicial
  # Ejemplo
  # RedesDesbalance(df,listconti,listclass,grupos=grupos, repeticiones = repeticiones,vardep = vardep,
  #                 size_net,decay_net,numIter=150,semilla=111)
  
  
  library(dummies)
  library(ROSE)
  library(pROC)
  library(caret)
  
  resultados <- c()
  
  if  (listclass!=c(""))
  {
    databis<-df[,c(vardep,listconti,listclass)]
    databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis<-data[,c(vardep,listconti)]
  }
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  databis[,vardep]<-as.factor(databis[,vardep])
  
  
  
  
  
  total <- c() #vector de apoyo a cada una de las repeticiones
  
  
  for(j in 1:repeticiones){
    set.seed(semilla + j)
    #shuffle samples
    df2 <- databis[sample(nrow(databis)),]
    flds <- caret::createFolds(df2[,vardep], k = grupos, list = TRUE, returnTrain = TRUE)
    
    
    for (i in 1:grupos) {
      #Particiones
      
      #Training partition
      f<- formula(paste0(vardep,"~."))
      dfTraining <- ROSE(f,data = df[flds[[i]] ,])$data
      
      means <- apply(dfTraining[,listconti],2,mean)
      sds <- sapply(dfTraining[,listconti],sd)
      datacon <- scale(dfTraining[,listconti], center = means, scale = sds)
      numerocont <- which(colnames(dfTraining)%in%listconti)
      dfTraining <- cbind(datacon,dfTraining[,-numerocont,drop=FALSE ])
      dfTraining[,vardep] <- as.factor(dfTraining[,vardep])
      
      
      #dfTraining <- df[flds[[i]],] #Training partition
      dfTest <- df[-flds[[i]],]# Test partition
      
      #modelo redes
      modelo <- avNNet(formula=f,data=dfTraining ,
                       size=size_net,
                       decay=decay_net,bag=F,maxit=numIter,trace=FALSE,linout=FALSE
      )
      
      
      modelo$pred
      #Probabilidades
      predict(modelo,dfTest)
      probs <- (predict(modelo,dfTest,type="prob"))[,2]
      cm <- caret::confusionMatrix(data = factor(ifelse(probs>0.5,'1','0')),
                                   reference = dfTest[,vardep],positive='1')
      modelo_r <- c(cm$overall[1],cm$byClass[1:4],auc=auc(dfTest[,vardep],probs) )
      
      
      
      total <- rbind(total,data.frame(t(modelo_r)) )
    }
    
    parametros <- function(dfTraining,size_net){
      nodos_input <- length(names(dfTraining)) - 1
      
      return(size_net*(nodos_input + 1) + size_net + 1)
    }
    resultados <- rbind(resultados,data.frame(lapply(total,mean),repeticion=paste0('rep',j),parametros = parametros(dfTraining,size_net) ) )
    
  }
  return(resultados)
}

RedesDesbalance_pred <- function(df,listconti,listclass,grupos=grupos, repeticiones = repeticiones,vardep = vardep,
                            size_net,decay_net,numIter=150,semilla=111){
  #   
  #listconti variables continuas
  #listclass variables categoricas
  #size_net=3 #nodos
  #decay_net=0.1 #learning rate
  #repeticiones , numero de repeticiones cv
  #grupos <- 2 ,grupos cv
  #numIter <- 150 #iteracciones  
  #semilla , semilla inicial
  # Ejemplo
  # RedesDesbalance(df,listconti,listclass,grupos=grupos, repeticiones = repeticiones,vardep = vardep,
  #                 size_net,decay_net,numIter=150,semilla=111)
  
  
  library(dummies)
  library(ROSE)
  library(pROC)
  library(caret)
  
  resultados <- c()
  
  if  (listclass!=c(""))
  {
    databis<-df[,c(vardep,listconti,listclass)]
    databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis<-data[,c(vardep,listconti)]
  }
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  databis[,vardep]<-as.factor(databis[,vardep])
  
  
  
  
  
  total <- c() #vector de apoyo a cada una de las repeticiones
  
  
  for(j in 1:repeticiones){
    set.seed(semilla + j)
    #shuffle samples
    df2 <- databis[sample(nrow(databis)),]
    flds <- caret::createFolds(df2[,vardep], k = grupos, list = TRUE, returnTrain = TRUE)
    
    
    for (i in 1:grupos) {
      #Particiones
      
      #Training partition
      f<- formula(paste0(vardep,"~."))
      dfTraining <- ROSE(f,data = df[flds[[i]] ,])$data
      
      means <- apply(dfTraining[,listconti],2,mean)
      sds <- sapply(dfTraining[,listconti],sd)
      datacon <- scale(dfTraining[,listconti], center = means, scale = sds)
      numerocont <- which(colnames(dfTraining)%in%listconti)
      dfTraining <- cbind(datacon,dfTraining[,-numerocont,drop=FALSE ])
      dfTraining[,vardep] <- as.factor(dfTraining[,vardep])
      
      
      #dfTraining <- df[flds[[i]],] #Training partition
      dfTest <- df[-flds[[i]],]# Test partition
      
      #modelo redes
      modelo <- avNNet(formula=f,data=dfTraining ,
                       size=size_net,
                       decay=decay_net,bag=F,maxit=numIter,trace=FALSE,linout=FALSE
      )
      
      
      modelo$pred
      #Probabilidades
      predict(modelo,dfTest)
      probs <- (predict(modelo,dfTest,type="prob"))[,2]
      total <- rbind(total,data.frame(probs) )
     
      
    }
  resultados <- rbind(resultados,data.frame(lapply(total,mean) ) )
  
  }
  return(resultados)
}


#En sucion ----      
      
# #lista de resultados
# resultados <-list()
# 
# 
# #shuffle samples
# df2<-df[sample(nrow(df)),]
# flds <- caret::createFolds(df2$deny, k = 10, list = TRUE, returnTrain = TRUE)
# df111<-df2[flds$Fold02,]
# length(flds) #10
# 
# 
# dfTraining <- df2[flds[[1]],] #Training partition
# dfTest <- df2[-flds[[1]],]# Test partition
# 
# m22 <- c("dmi","dir","lvr","pbcr","hir","black","single")
# f4 <- paste0("factor(deny)~",paste0(m22,collapse='+'))
# f4 <- formula(f4)
# 
# 
# library(ranger)
# rf31 <- ranger(f4,data=dfTraining ,
#               mtry=3,num.trees = 500,min.node.size=10, replace = TRUE,probability = TRUE)
# 
# probs1 <- (predict(rf31,dfTest)$predictions)[,2]
# cm1 <- caret::confusionMatrix(data=factor(ifelse(probs1>0.5,'1','0')),
#                            reference=dfTest[,'deny'],positive='1')
# modelo_rf31 <- c(cm1$overall[1],cm1$byClass[1:4])
# modelo_rf31 <- c(modelo_rf31,auc=auc(dfTest$deny,probs1))
# 
# mm<-modelo_rf31
# # auc(dfTest$deny,factor(ifelse(probs1>0.5,'1','0')))
# # 
# # levels(dfTest$deny)
# # levels(factor(ifelse(probs1>0.5,'1','0')))
# 
# auc(dfTest$deny,probs1)
# 
# auc<-function(x,y) {
#   curvaroc<-roc(response=x,predictor=y)
#   auc<-curvaroc$auc
#   return(auc)
# }
# 
# i <- 1
# #transformacion de listas
# modelo_rf32 <- modelo_rf31
# total <- c()
# total <- rbind(total,data.frame(modelo_rf31) )
# total <- rbind(total,data.frame(modelo_rf32) )
# total$modelo <- NULL
# 
# 
# total1 <- data.frame(lapply(total,mean))
# total$modelo<- "m2"
# u <- c()
# u<-  rbind(u,total)
# 
# #agregar
# 
# cm$table
# 
# #dat[flds$train,] gets you the training set, dat[ flds[[2]], ] 
# 
# trainIndex <- createDataPartition(data1$deny, p=0.8, list=FALSE)
# data_train <- data1[trainIndex,]#quito las variables transformadas por el momento
# data_test <- data1[-trainIndex,]