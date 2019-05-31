
listconti <- var_cont
listclass <- var_cat
size_net=3 #nodos
decay_net=0.1 #learning rate
resultados <- c()
repeticiones <- 2
grupos <- 2
numIter <- 150 #iteracciones

summary(dfTraining)
hist(dfTraining$dir)
hist(databis$dir)

qq <- RedesDesbalance(df,listconti,listclass,grupos=grupos, repeticiones = repeticiones,vardep = vardep,
                size_net,decay_net,numIter=150)


RedesDesbalance <- function(df,listconti,listclass,grupos=grupos, repeticiones = repeticiones,vardep = vardep,
                            size_net,decay_net,numIter=150){
#   
#listconti variables continuas
#listclass variables categoricas
#size_net=3 #nodos
#decay_net=0.1 #learning rate
#repeticiones , numero de repeticiones cv
#grupos <- 2 ,grupos cv
#numIter <- 150 #iteracciones  
  
  # Ejemplo
  # RedesDesbalance(df,listconti,listclass,grupos=grupos, repeticiones = repeticiones,vardep = vardep,
  #                 size_net,decay_net,numIter=150)
  
  
library(dummies)
  

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
  set.seed(111 + j)
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
                     decay=decay_net,bag=F,maxit=numIter
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
