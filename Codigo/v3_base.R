library(ROSE)

dfff<- data1
dfff$deny <- ifelse(dfff$deny=='Yes','1','0')
dfff$deny <- factor(dfff$deny)
x<-c("deny","dmi","dir","lvr","pbcr","hir","single")
z<-ROSE.eval(factor(deny)~.,
             data=data1, glm, 
          method.assess="LKOCV",
          K=5,control.learner=list(family=binomial), seed=1)

z
dfff[,x]
f3

z1<-ROSE.eval(factor(deny)~.,
              data=data_train2, learner=ranger, 
             method.assess="LKOCV",trace = TRUE,
             K=400)
             
is.data.frame(df)
z1<-ROSE.eval(deny ~ dmi + dir + lvr + pbcr + hir + single,
              data=dfff, ranger, 
              method.assess="LKOCV",
              K=5)
#              control.learner=list( mtry=3,num.trees = 500,min.node.size=10,
 #                                   replace = TRUE,probability = TRUE), seed=1)
