############### programme ACM DES DONNEES POUR LE MACHINE LEARNING####
install.packages("funModeling")
install.packages("corrplot")
install.packages("visreg")
install.packages("caret")


getwd()
library(FactoMineR)
library(Factoshiny)
library(funModeling)
library(questionr)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(visreg)
library(caret)
library(ROSE)
library(dplyr)

fichier=base_etude

summary(fichier)

str(fichier)
any(is.na(fichier)) ## pour voir s'il des valeurs manquantes
View(fichier)


## fr?quence des variables

freq(fichier)

## on voit que baisse classification na pas au moins 5% de valeurs , je le supprime de la base 

fichier=fichier[,-6]

summary(fichier)

## barplot


## corr√©lation
install.packages("GoodmanKruskal")
library(GoodmanKruskal)

varSet1 <- c("AGE", "Y_6","ANC_grp","ANC_EMPLOI","ABS_AUTRES","CADRE","SEXE","FILIERE",
             "PROMO","NOMI","DIPLOME","ABS_MA","FORM","tr_REMU","Augm")
CarFrame1 <- subset(fichier, select = varSet1)
GKmatrix1 <- GKtauDataframe(CarFrame1)
plot(GKmatrix1)



## ( base des gens qui sont sortis ‡ 6 mois)
fichier=fichier%>%
  select(SEXE,FILIERE,CADRE,PROMO,NOMI,MOB_GEO,Y_6,AGE,
                ANC_grp,ANC_EMPLOI,ABS_AUTRES,ABS_MATER,FORM,ABS_MA,tr_REMU,Augm,DIPLOME)

##(base des gens qui sont sortis ‡ 9 mois)
base_Y9=base_etude%>%
  select(SEXE,FILIERE,CADRE,PROMO,NOMI,MOB_GEO,Y_9,AGE,
         ANC_grp,ANC_EMPLOI,ABS_AUTRES,ABS_MATER,FORM,ABS_MA,tr_REMU,Augm,DIPLOME)

##( les gens qui sont sortis ‡ 12 mois)
base_Y12=base_etude%>%
  select(SEXE,FILIERE,CADRE,PROMO,NOMI,MOB_GEO,Y_12,AGE,
         ANC_grp,ANC_EMPLOI,ABS_AUTRES,ABS_MATER,FORM,ABS_MA,tr_REMU,Augm,DIPLOME)




############################ Etude avec Y_6  ########################################


## etude ACM (Analyse Composante Multiple) avec factoshiny

summary(fichier)

outMCA=MCAshiny(fichier)### nous renvoi directement sur l'application factoShiny



#Investigate(res,file="MCA.Rmd",document = "word_document")

#################################### Prevision #######################################

## validation vs apprentissage

set.seed(123)
trainingRowIndex=sample(1:nrow(fichier),0.7*nrow(fichier))
training=fichier[trainingRowIndex,]  ## base apprentissage
validation=fichier[-trainingRowIndex,] ## base validation
table(fichier$Y_6)
table(validation$Y_6)
table(training$Y_6)

summary(validation)
summary(training)
## ModeleS
################################################## 1 regression lOGISTIQUE

## modele logit
logit=glm(Y_6~.
    , data=training,family=binomial(link="logit"))
logit
summary(logit)

# selection du meilleur mod√®le avec le crit√®re d'AIC

best_logit = step(logit)
summary(best_logit)
formula(best_logit)
logit
best_logit
B=margins(logit)
plot(B)

exp(coef(logit))
## prevision
pred.logit=predict(best_logit, newdata=validation, type="response")
pred.logit


## d√©coupage du cut

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

## mod?le probit

probit=glm(Y_6~., data=training,family=binomial(link="probit"))

summary(probit)
##
best_probit = step(probit)
summary(best_probit)
formula(best_probit)
## prediction
pred.probit=predict(best_probit, newdata=validation, type="response")
pred.probit


analyse_resultats_probit = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_probit)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                "taux_sucess_total","taux_1_predits","taux_0_predits")
analyse_resultats_probit


for (i in seq(0.01,0.4 , by=0.01)){
  result_probit=ifelse(pred.probit>i,"1","0")
  
  xtab=table(result_probit,validation$Y_6)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total = (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_probit = rbind(analyse_resultats_probit,resultats_i )
  
}

analyse_resultats_probit = analyse_resultats_probit[-1,]
analyse_resultats_probit

result_probit=ifelse(pred.probit>0.08,"1","0")
pred.probit
result_probit



# aire sous la courbe ROC
roc.curve(validation$Y_6, predicted = pred.probit)#####################AUC=0.84
auc(validation$Y_6, pred.probit, quiet=TRUE)#####################AUC=0.84


## #########################################foret al?atoire 
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





######################################XGBOOST
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

######################







x1=roc.curve(validation12$Y_12, predicted = pred.logit12,col="blue")
y=roc.curve(validation$Y_6, predicted = pred.logit,col="red",add=TRUE)
z1=roc.curve(validation9$Y_9, predicted = pred.logit9,add=TRUE)
legend("topleft", c("logit_Y_6","logit_Y_9","logit_Y_12"), 
       col=1:3, lty=1:2, lwd=5)


## multiple roc curve

x=roc.curve(validation$Y_6, pred.random,col="blue")
y=roc.curve(validation$Y_6, predicted = pred.logit,col="red",add=TRUE)
Z=roc.curve(validation$Y_6, pred.gbm,col="green",add=TRUE)
##legend(lengend=c("RANDOM FOREST","regression logistique","XGBOOST"),col=c("blue","red","green"))



legend("topleft", c("RANDOM FOREST","regression logistique","XGBOOST"), 
       col=1:3, lty=1:2, lwd=2)

