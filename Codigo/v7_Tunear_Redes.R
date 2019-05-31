##################################
#Version 07
# Fecha_ 30/05/2019
#Objetivos: tunear redes

#Resultados:Elegimos size 5 decay 0.1 parametros 71
####################################

#Librerias

#Datos
load(file="Datos\\datos_originales_limpios.Rdata")

#Variables
vardep <- c("deny")
vars <- c("dir", "dmi", "hir","ccs","pbcr","lvr","black")

listconti <- dput(names(Filter(is.numeric,df[,vars])))
listclass <- dput(names(Filter(is.factor,df[,vars])))

#Tuneo de parametros
total <- c()
nnetgrid <- expand.grid(size=c(5,10,15,20),decay=c(0.1,0.01,0.001))
for(i in seq(1,dim(nnetgrid)[1]) ){
  aux <- RedesDesbalance(df,listconti,listclass,grupos = 2, repeticiones = 2,vardep = vardep,
                                size_net = nnetgrid[i,1],decay_net=nnetgrid[i,2],numIter = 150)
aux$repeticion <- NULL
aux1 <- data.frame(lapply(aux,  mean))
total <- rbind(total,aux1)
}
                
total1 <- cbind(nnetgrid,total)
print(total1[,-c(5,6,7)])


aggregate(Accuracy~size,total1,mean)
aggregate(Sensitivity~size,total1,mean)
aggregate(auc~size,total1,mean)


aggregate(Accuracy~decay,total1,mean)
aggregate(Sensitivity~decay,total1,mean)
aggregate(auc~decay,total1,mean)

x<- total1[total1$size<=10,]

total <- c()
nnetgrid <- expand.grid(size=c(6,7,8,9),decay=c(0.1,0.01,0.001))
for(i in seq(1,dim(nnetgrid)[1]) ){
  aux <- RedesDesbalance(df,listconti,listclass,grupos = 2, repeticiones = 2,vardep = vardep,
                         size_net = nnetgrid[i,1],decay_net=nnetgrid[i,2],numIter = 150)
  aux$repeticion <- NULL
  aux1 <- data.frame(lapply(aux,  mean))
  total <- rbind(total,aux1)
}

total2 <- cbind(nnetgrid,total)
print(total2[,-c(5,6,7)])

total12 <- rbind(x,total2)
aggregate(Accuracy~size,total12,mean)
aggregate(Sensitivity~size,total12,mean)
aggregate(auc~size,total12,mean)


aggregate(Accuracy~decay,total12,mean)
aggregate(Sensitivity~decay,total12,mean)
aggregate(auc~decay,total12,mean)


save(total12,file="Datos\\tuneadoRedes.Rdata")
write.csv(print(total12[,-c(5,6,7)]),file="Datos\\tuneadoRedes.csv")

#Elegimos size 5 decay 0.1 parametros 71