### test sur les gens qui partent c'est ? dire les moins de 38 ans


summary(fichier)
base_nove=fichier %>%
  filter(AGE=="21-30 age"  | AGE=="31-38 age")



outMCA=MCAshiny(base_nove)
summary(base_nove)

## validation vs apprentissage

set.seed(123)
trainingRowIndex=sample(1:nrow(base_nove),0.7*nrow(base_nove))
training=base_nove[trainingRowIndex,]  ## base apprentissage
validation=base_nove[-trainingRowIndex,] ## base validation
table(base_nove$Y_6)
table(validation$Y_6)
table(training$Y_6)
## modele logit
logit=glm(Y_6~.
          , data=training,family=binomial(link="logit"))
logit
summary(logit)

# les varaibles les plus significaif

best_logit = step(logit)
summary(best_logit)
formula(best_logit)
logit
best_logit
summary(best_logit)

## prevision
pred.logit=predict(best_logit, newdata=validation, type="response")
pred.logit

analyse_resultats_glm = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_glm)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                "taux_success_total","taux_1_predits","taux_0_predits")
analyse_resultats_glm


for (i in seq(0.01,0.4 , by=0.01)){
  result_logit=ifelse(pred.logit>i,"1","0")
  
  xtab=table(result_logit,validation$Y_6)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total= (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_glm = rbind(analyse_resultats_glm,resultats_i )
  
}

analyse_resultats_glm = analyse_resultats_glm[-1,]
analyse_resultats_glm

result_logit=ifelse(pred.logit>0.15,"1","0")
pred.logit
result_logit

xtab=table(result_logit,validation$Y_6)
confusionMatrix(xtab)



# aire sous la courbe ROC
install.packages("pROC")
library(pROC)
?roc.curve
roc.curve(validation$Y_6, predicted = pred.logit)#####################AUC=0.84
auc(validation$Y_6, pred.logit, quiet=TRUE)#####################AUC=0.84


### random forest

## random forest
install.packages("randomForest")
library(randomForest)
RFM=randomForest(Y_6~.,data=training)
RFM

print(RFM)
summary(RFM)
## prediction
pred.random=predict(RFM,newdata=validation)
## matrice de confusion
xtab3=table(pred.random,validation$Y_6)
confusionMatrix(xtab3)

roc.curve(validation$Y_6, pred.random)


## XGBOOST
library(Matrix)
library(xgboost)
library(dplyr)
library(magrittr)

set.seed(123)
# creAte matrix


trainingM=model.matrix(Y_6 ~ .,data=training)

validationM=model.matrix(Y_6 ~ .,data=validation)
length(validationM)

train_matrix=xgb.DMatrix(data=as.matrix(trainingM),label=as.numeric(training$Y_6)-1)
valid_matrix=xgb.DMatrix(data=as.matrix(trainingM),label=as.numeric(training$Y_6)-1)

params=list(set.seed=123,
            eval_metric="auc",
            objective="binary:logistic")

gdbt <- xgb.train(params=params, data=train_matrix,
                  nrounds=20, num_parallel_tree =1000, nthread = 1, verbose=1)
pred.gbm <- predict(gdbt, newdata=validationM)

###
analyse_resultats_xgboost = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_xgboost)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                    "taux_sucess_total","taux_1_predits","taux_0_predits")
analyse_resultats_xgboost


for (i in seq(0.01,0.4 , by=0.01)){
  result_gbm=ifelse(pred.gbm>i,"1","0")
  
  xtab=table(result_gbm,validation$Y_6)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total = (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_xgboost = rbind(analyse_resultats_xgboost,resultats_i )
  
}

analyse_resultats_xgboost = analyse_resultats_xgboost[-1,]
analyse_resultats_xgboost

result_gbm=ifelse(pred.gbm>0.15,"1","0")

## matrice de confusion
xtab=table(result_gbm,validation$Y_6)
confusionMatrix(xtab)
roc.curve(validation$Y_6, pred.gbm)### AUC=0.5

## multiple roc curve

x=roc.curve(validation$Y_6, pred.random,col="blue")
y=roc.curve(validation$Y_6, predicted = pred.logit,col="red",add=TRUE)
Z=roc.curve(validation$Y_6, pred.gbm,col="green",add=TRUE)
##legend(lengend=c("RANDOM FOREST","regression logistique","XGBOOST"),col=c("blue","red","green"))



legend("topleft", c("RANDOM FOREST","regression logistique","XGBOOST"), 
       col=1:3, lty=1:2, lwd=2)

### ACM NOUVEAU CLUSTER
summary(base_nove)
Y6_cluster_age=base_nove[,-8]## j'ai enlevé l'age car je fais l'acm sur les moins de 38 ans, ils ont le meme age
outMCA_y_6_cluster=MCAshiny(Y6_cluster_age)
summary(Y6_cluster_age)


