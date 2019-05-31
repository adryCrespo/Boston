##################################
#Version 02
# Fecha_ 27/05/2019
#Objetivos: seleccion de variables con arbol
          #¿Cambia con ROSE las variables importantes?
#Resultados
#maxsurrogate significa que no hace imputaciones, solo pone las variables del arbol
#las importancia de las variables cambia un monton con ROSE
####################################

library(rpart)
library(ROSE)
library(rpart.plot)
# Seleccion variables datos con arbol
#maxsurrogate=0

arbol2 <- rpart(deny ~ ., data = df,minbucket =30,
                method = "class",cp=0, maxsurrogate=0)
par(cex=0.7)
arbol2$variable.importance
barplot(arbol2$variable.importance)
rpart.plot(arbol2,extra=105)#

# Seleccion variables datos con arbol
#no surrogate

arbol <- rpart(deny ~ ., data = df,minbucket =30,
                method = "class",cp=0)
par(cex=0.7)
arbol$variable.importance
barplot(arbol$variable.importance)
rpart.plot(arbol,extra=105)#


arbol33 <- rpart(deny ~ ., data = df,minbucket =30,
               method = "class",cp=0, maxsurrogate=1)
par(cex=0.7)
arbol33$variable.importance
barplot(arbol33$variable.importance)
rpart.plot(arbol33,extra=105)#

# Seleccion variables datos con arbol
#no surrogate

f<- formula(paste0(vardep,"~."))
arbol22 <- rpart(deny ~ ., data = ROSE(f,data =df)$data,minbucket =30,
                method = "class",cp=0, maxsurrogate=0)
par(cex=0.7)
arbol22$variable.importance
barplot(arbol22$variable.importance)
rpart.plot(arbol22,extra=105)#


# Seleccion variables datos con arbol
#
f <- formula(paste0(vardep,"~."))
arbol22 <- rpart(deny ~ ., data = ROSE(f,data =df)$data,minbucket =30,
                 method = "class",cp=0)
par(cex=0.7)
arbol22$variable.importance
barplot(arbol22$variable.importance)
rpart.plot(arbol22,extra=105)#