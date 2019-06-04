##################################
#Version 11
# Fecha_ 03/06/2019
#Objetivos: tunear GBM

#Resultados:
# shrinkage= 0.05
# n.minobsinnode= 5
# n.trees= 500
####################################

#Librerias
library(caret)
library(ROSE)
library(dummies)
#Datos
load(file="Datos\\datos_originales_limpios.Rdata")

#Variables
vardep <- c("deny")
vars <- c("dir", "dmi", "hir","ccs","pbcr","lvr","black")
f <- formula(paste0(vardep,"~",paste0(vars,collapse = "+")))
listconti <- dput(names(Filter(is.numeric,df[,vars])))
listclass <- dput(names(Filter(is.factor,df[,vars])))

#Cambio nombre varaible dependiente por caret
df$deny<-make.names(df$deny)
df$deny <- factor(df$deny)

#Definir el grid
total <- c()
nnetgrid <- expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
                        n.minobsinnode=c(5,10,20),
                        n.trees=c(100,500,1000,5000)
                        )


for(i in seq(1,dim(nnetgrid)[1]) ){

aux <- cruzadagbmbin2(data=df,vardep=vardep,listconti=listconti,listclass=listclass,
                     grupos=4,sinicio=1234,repe=5,
                     n.minobsinnode=nnetgrid[i,2],shrinkage=nnetgrid[i,1],
                     n.trees=nnetgrid[i,3],interaction.depth=2)
aux$Rep <- NULL

#aux1 <- data.frame(lapply(aux,  mean))
total <- rbind(total,apply(aux,2,mean))
}
total <- cbind(nnetgrid,total)

plot(aggregate(Accuracy~shrinkage,data=total,mean), xlab='shrinkage', ylab='Accuracy')
plot(aggregate(sensibilidad~shrinkage,data=total,mean), xlab ='shrinkage' ,ylab = 'Sensivity')
plot(aggregate(auc~shrinkage,data=total,mean),xlab='shrinkage', ylab ='Auc')


#0.05

plot(aggregate(Accuracy~n.minobsinnode,data=total,mean),xlab='minimo tamaño nodo', ylab='Accuracy')
     plot(aggregate(sensibilidad~n.minobsinnode,data=total,mean),xlab='minimo tamaño nodo',ylab = 'Sensivity')
          plot(aggregate(auc~n.minobsinnode,data=total,mean),xlab='minimo tamaño nodo', ylab ='Auc')
#5


#Tuneado n.trees          
          
          total1 <- c()
          nnetgrid <- expand.grid(shrinkage=c(0.05),
                                  n.minobsinnode=c(5),
                                  n.trees=c(100,500,1000,5000,10000)
          )
          
          
          for(i in seq(1,dim(nnetgrid)[1]) ){
            
            aux <- cruzadagbmbin2(data=df,vardep=vardep,listconti=listconti,listclass=listclass,
                                  grupos=4,sinicio=1234,repe=5,
                                  n.minobsinnode=nnetgrid[i,2],shrinkage=nnetgrid[i,1],
                                  n.trees=nnetgrid[i,3],interaction.depth=2)
            aux$Rep <- NULL
            
            #aux1 <- data.frame(lapply(aux,  mean))
            total1 <- rbind(total1,apply(aux,2,mean))
          }
          total1 <- cbind(nnetgrid,total1) 
          
          plot(aggregate(Accuracy~n.trees,data=total1,mean), xlab='numero arboles', ylab='Accuracy')
          plot(aggregate(sensibilidad~n.trees,data=total1,mean), xlab ='numero arboles' ,ylab = 'Sensivity')
          plot(aggregate(auc~n.trees,data=total1,mean),xlab='numero arboles', ylab ='Auc')
          