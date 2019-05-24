

#shuffle samples
df2<-df[sample(nrow(df)),]
flds <- caret::createFolds(df2$deny, k = 10, list = TRUE, returnTrain = TRUE)


df111<-df2[flds$Fold02,]

dim(flds)

caret::train

#dat[flds$train,] gets you the training set, dat[ flds[[2]], ] 

trainIndex <- createDataPartition(data1$deny, p=0.8, list=FALSE)
data_train <- data1[trainIndex,]#quito las variables transformadas por el momento
data_test <- data1[-trainIndex,]