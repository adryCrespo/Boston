##################################
#Version 12
# Fecha_ 04/06/2019
#Objetivos: tunear GBM
#Resultados lineal:
#C 0.05

#Resultados polinomial:
# C 0.05
# degree 2
# scale 0.5

#Resultados RBF:
# C 0.2
# sigma 0.5

####################################


#Librerias
library(caret)
library(ROSE)
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



### Tuneo SVM lineal ----
source(file="Funciones\\cruzada SVM binaria lineal2.R")
# parametros
 grid <- expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))
 
total <- c()
for(i in seq(1,dim(grid)[1]) ){

  aux <- cruzadaSVMbin2(data=df,vardep=vardep,
    listconti=listconti,listclass=listclass,
    grupos=4,sinicio=1234,repe=5,
    C=grid[i,1],replace=TRUE)
  
  aux$Rep <- NULL 
  total <- rbind(total,apply(aux,2,mean))
  
}

total <- cbind(grid,total)
plot(total$Accuracy~total$C, xlab='C',ylab='accuracy')
plot(total$auc~total$C, xlab='C',ylab='Auc')
plot(total$sensibilidad~total$C, xlab='C',ylab='sensibilidad')


### Tuneo SVM polinomial ----
 source(file = "Funciones\\cruzada SVM binaria polinomial2.R")
SVMgrid <- expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
                      degree = c(2,3),scale = c(0.1,0.5,1,2,5))

total <- c()
for(i in seq(1,dim(SVMgrid)[1]) ){
  
  
  aux <- cruzadaSVMbinPoly2(data=df,vardep=vardep,
    listconti=listconti,listclass=listclass,
    grupos=4,sinicio=1234,repe=5,
    C=SVMgrid[i,1],degree=SVMgrid[i,2],scale=SVMgrid[i,3])
  
  aux$Rep <- NULL 
  total <- rbind(total,apply(aux,2,mean))
  
}

total <- cbind(SVMgrid,total)

plot(total$Accuracy~total$C, xlab='C',ylab='accuracy')
plot(total$auc~total$C, xlab='C',ylab='Auc')
plot(total$sensibilidad~total$C, xlab='C',ylab='sensibilidad')

plot(total$Accuracy~total$degree, xlab='degree',ylab='accuracy')
plot(total$auc~total$degree, xlab='degree',ylab='Auc')
plot(total$sensibilidad~total$degree, xlab='degree',ylab='sensibilidad')

plot(total$Accuracy~total$scale, xlab='scale',ylab='accuracy')
plot(total$auc~total$scale, xlab='scale',ylab='Auc')
plot(total$sensibilidad~total$scale, xlab='scale',ylab='sensibilidad')

cond1 <- total$degree==2
cond2 <- total$Accuracy > 0.90
cond3 <- total$sensibilidad > 0.95

totalxx<- total[cond1&cond3,]

plot(totalxx$C,totalxx$scale,xlab='C',ylab='Scale')

save(total,file = "SVM_polinomial.Rdata")
# C 0.05
# degree 2
# scale 0.5
# Accuracy 0.9026891
# auc 0.8067725
# sensibilidad 0.972124
### Tuneo SVM  RBF ----
source(file = "Funciones\\cruzada SVM binaria RBF2.R")

SVMgrid <- expand.grid(C = c(0.01,0.05,0.1,0.2,0.5),
               sigma = c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30))
total <- c()
for(i in seq(1,dim(SVMgrid)[1]) ){

    aux <- cruzadaSVMbinRBF2(data=df,vardep=vardep,
         listconti=listconti,listclass=listclass,
         grupos=4,sinicio=1234,repe=5,C=SVMgrid[i,1],sigma=SVMgrid[i,2])
        
    aux$Rep <- NULL 
    total <- rbind(total,apply(aux,2,mean))
    
    }    

total <- cbind(SVMgrid,total)
plot(total$Accuracy~total$C, xlab='C',ylab='accuracy')
plot(total$auc~total$C, xlab='C',ylab='Auc')
plot(total$sensibilidad~total$C, xlab='C',ylab='sensibilidad')

cond1 <- total$Accuracy>0.85
cond2 <- total$auc>0.75
cond3 <- total$sensibilidad>0.85
t<-total[cond1&cond3&cond2,]

plot(t$C,t$sigma,xlab='C',ylab='sigma')


save(total,file = "Datos\\SVM_RBF.Rdata")
# C 0.20
# Sigma 0.50
# Accuracy 0.8898319
# Auc 0.8027136
# Sensibilidad 0.9502625
