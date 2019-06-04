##################################
#Version 08
# Fecha_ 31/05/2019
#Objetivos: tunear random forest

#Resultados:num.trees=800,min.node.size=10,mtry=2
####################################
#Datos
load(file="Datos\\datos_originales_limpios.Rdata")

#Variables
vardep <- c("deny")
vars <- c("dir", "dmi", "hir","ccs","pbcr","lvr","black")
f <- formula(paste0(vardep,"~",paste0(vars,collapse = "+")))


#Tuneado numero de arboles
total <- c()
nnetgrid <- expand.grid(arboles=c(50,100,200,300,500,800,1000))

for(i in seq(1,dim(nnetgrid)[1]) ){
aux <- RangerDesbalance(df,ecuacion=f,grupos=4,repeticiones=5,vardep=vardep,
  mtry=3,num.trees = nnetgrid[i,1],min.node.size=10,semilla=111)

aux$repeticion <- NULL
aux1 <- data.frame(lapply(aux,  mean))
total <- rbind(total,aux1)
}
total1 <- cbind(nnetgrid,total)
print(total1[,-c(5,6,7)])
plot(total1$arboles,total1$auc,xlab='numero de arboles', ylab='Auc' )
plot(total1$arboles,total1$Accuracy,xlab='numero de arboles', ylab='Accuracy')
plot(total1$arboles,total1$Sensitivity,xlab='numero de arboles', ylab='Sensitivity')
#800


#Tuenado mtry
total <- c()
nnetgrid <- expand.grid(m=c(2,3,4))

for(i in seq(1,dim(nnetgrid)[1]) ){
  aux <- RangerDesbalance(df,ecuacion=f,grupos=4,repeticiones=5,vardep=vardep,
                          mtry=nnetgrid[i,1],num.trees = 800,min.node.size=10,semilla=111)
  
  aux$repeticion <- NULL
  aux1 <- data.frame(lapply(aux,  mean))
  total <- rbind(total,aux1)
}
total1 <- cbind(nnetgrid,total)
print(total1[,-c(5,6,7)])
plot(total1$m,total1$auc,xlab='mtry', ylab='Auc' )
plot(total1$m,total1$Accuracy,xlab='mtry', ylab='Accuracy')
plot(total1$m,total1$Sensitivity,xlab='mtry', ylab='Sensitivity')
#2

#Tuneado min.node.size
total <- c()
nnetgrid <- expand.grid(m=c(1,2,3,5,8,10))

for(i in seq(1,dim(nnetgrid)[1]) ){
  aux <- RangerDesbalance(df,ecuacion=f,grupos=4,repeticiones=5,vardep=vardep,
                          mtry=2,num.trees = 800,min.node.size=nnetgrid[i,1],semilla=111)
  
  aux$repeticion <- NULL
  aux1 <- data.frame(lapply(aux,  mean))
  total <- rbind(total,aux1)
}
total1 <- cbind(nnetgrid,total)
print(total1[,-c(5,6,7)])
plot(total1$m,total1$auc,xlab='minimo tamaño nodo', ylab='Auc' )
plot(total1$m,total1$Accuracy,xlab='minimo tamaño nodo', ylab='Accuracy')
plot(total1$m,total1$Sensitivity,xlab='minimo tamaño nodo', ylab='Sensitivity')
#10