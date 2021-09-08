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


## corrélation
install.packages("GoodmanKruskal")
library(GoodmanKruskal)

varSet1 <- c("AGE", "Y_6","ANC_grp","ANC_EMPLOI","ABS_AUTRES","CADRE","SEXE","FILIERE",
             "PROMO","NOMI","DIPLOME","ABS_MA","FORM","tr_REMU","Augm")
CarFrame1 <- subset(fichier, select = varSet1)
GKmatrix1 <- GKtauDataframe(CarFrame1)
plot(GKmatrix1)



## ( base des gens qui sont sortis � 6 mois)
fichier=fichier%>%
  select(SEXE,FILIERE,CADRE,PROMO,NOMI,MOB_GEO,Y_6,AGE,
                ANC_grp,ANC_EMPLOI,ABS_AUTRES,ABS_MATER,FORM,ABS_MA,tr_REMU,Augm,DIPLOME)

##(base des gens qui sont sortis � 9 mois)
base_Y9=base_etude%>%
  select(SEXE,FILIERE,CADRE,PROMO,NOMI,MOB_GEO,Y_9,AGE,
         ANC_grp,ANC_EMPLOI,ABS_AUTRES,ABS_MATER,FORM,ABS_MA,tr_REMU,Augm,DIPLOME)

##( les gens qui sont sortis � 12 mois)
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

# selection du meilleur modèle avec le critère d'AIC

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


## découpage du cut

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







########################################################arbre de d?cision 
## CLASSIFIER
install.packages("rpart.plot")
library(rpart)
library(caret)
library(rpart.plot)
library(e1071)

  cart <- rpart(Y_6 ~ ., data = training,method="class",minsplit=25,cp=0.009)
parametres=rpart.control(minsplit=0,cp=1)
#
## 8 et 0.009 
## pour savoir quelle parametre est le meilleur

tune.model = tune.rpart(Y_6~ ., data = training, minsplit = c(8,20,25,27,34), cp = c( 0.009,2,
 0.1,0.05,0.0044), maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = c(10, 15), surrogatestyle = 0, maxdepth = c(25, 30))


summary(tune.model)
printcp(cart)
##
class(cart)
print(cart)
summary(cart)
prp(cart,extra=1)
plot(cart$y)

## pr?vision
pred.rpart=predict(cart,newdata=validation,type="class",probability=TRUE)
plotcp(model)
printcp(cart)

length(pred.rpart)
# matrice de confusion 
t=table(validation$Y_6,pred.rpart)
confusionMatrix(t)

## Aire sous la courbe de Roc
install.packages("ROSE")
library("ROSE")
roc.curve(validation$Y_6, pred.rpart)############################################## AUC=0.52

##" KNN
install.packages("class")
library(class)
NROW(training)## number of observation in a base training
knn.43=knn(train=training,test=validation,cl=training$Y_6,k=43)

ctrl=trainControl(method="cv",number=10)
KNN=train(Y_6 ~ .,data=training,method='knn',trControl=ctrl)


pred.knn=predict(KNN,newdata=validation)

#####################################################################boosting
install.packages("mlr")
library(mlr)
## 1) Define the task
## Specify the type of analysis (e.g. classification) and provide data and response variable
train.task = makeClassifTask(data = training, target = "Y_6")
test.task  = makeClassifTask(data = validation, target = "Y_6")

## 2) Define the learner
## Choose a specific algorithm (e.g. linear discriminant analysis)
lrn <- makeLearner("classif.xgboost", predict.type ="prob")
lrn <- makeLearner("classif.xgboost", predict.type = "response", par.vals = list(objective = "binary:logistic",eval_metric = "error",nrounds = 200))
lrn <- makeLearner("classif.randomForest", predict.type ="prob")
print(lrn)

## 3) Fit the model
## Train the learner on the task using a random subset of the data as training set
model = train(lrn, train.task)
#afficher le mod???le
print(getLearnerModel(model))

## 4) Make predictions
## Predict values of the response variable for new observations by the trained model
## using the other part of the data as test set
pred = predict(model, task = test.task)

## 5) Evaluate the learner
## Calculate the mean misclassification error and accuracy
performance(pred, measures = list(auc, mmce, acc))


library(caret)
model <- train(Cible ~ ., method = "rf", data = train)
model <- train(Cible ~ ., method = "xgbTree", data = train)


### bagging
library(ipred)
set.seed(123)
bag=bagging(Y_6 ~ ., data=training,nbagg=500,coob=TRUE, control= rpart.control(maxdepth=1,cp=0))
## prediction
pred.bg <- predict(bag,  type="prob",validation)
head(pred.bg)
auc(validation$Y_6, pred.bg[,2], quiet=TRUE)
## boosting
install.packages("ada")
library(ada)
set.seed(235)
boost <- ada(Y_6 ~ ., data=training, type="discrete", loss="exponential", control = rpart.control(maxdepth=1,cp=-1,minsplit=0), iter = 5000, nu = 0.01,
             test.y=validation[,"Y_6"], test.x=validation[,c(1:6,8:16)])
boost
## classification tree
install.packages("tree")
library(tree)
set.seed(123)
TREE=tree(Y_6 ~ ., data=training)
pred.tree=predict(TREE,validation,type="class")
table(pred.tree,validation$Y_6)
##cross validation tree
cv=cv.tree(TREE,FUN=prune.misclass)
plot(cv$size,cv$dev,type="b")

## regression tree
library(MASS)
set.seed(123)
tree.b=tree(Y_6 ~ .,data=training)
pred.tree.b=predict(tree.b,newdata=validation)
table(pred.tree.b[,1],validation$Y_6)



##xgboost doesn't not deal with factor all factor have to be transforme to dummy variable voir vd?o youtube (xgboost tutorial)
y=as.numeric(training$Y_6)-1## trainig
X=training%>%select(-Y_6)## training
str(X)
## tranformation des factor en dummy variable
install.packages("fastDummies")
library(fastDummies)
X=dummy_cols(X,remove_first_dummy = TRUE)

X=X[,-c(1:15)]

##seeting parameters

params=list(set.seed=123,
                     eval_metric="auc",
                     objective="binary:logistic")


## runing xgboost
library(xgboost)

xgb=xgboost(data=as.matrix(X),
            label=y,
            params=params,
            nrounds=20,
            verbose=1,
            )

pred.gbm <- predict(xgb, newdata=validationM)

## shap values
xgb.plot.shap(data=as.matrix(X),
              model=xgb,
              top_n=5)




#TRAIN ==> LOGITBOOST

logit_boost = train(Y_6~., data = training, method = "adaboost",  nIter = c(1:100))
yelogit_boost$trainingData
logit_boost$finalModel
predict(logit_boost$finalModel, )


pred = predict(logit_boost,validation)

table(pred)

colnames(validation)
colnames(training)


#GBM ==> 
install.packages("gbm")
library("gbm")
ctrl = trainControl(method = "repeatedcv", number = 10, repeats = 10)
gbmGrid =expand.grid(interaction.depth = c(1,5,9), n.trees = (1:30)*5, shrinkage = 0.1, n.minobsinnode = 20)


gbm = train(Y_6~., data = training, method = "gbm")
gbm2 = train(Y_6~., data = training, method = "gbm", trControl = ctrl, tuneGrid = gbmGrid)

plot(gbm2)

gbm2$results

table(predict(gbm2))


(predict(gbm2, validation))

#Arbres ==>
library(rpart)
colnames(training)
test_arbres = rpart(Y_6~ PROMO + NOMI + AGE + ANC_EMPLOI + ABS_AUTRES  , 
                      data = training, method = "class", minsplit = 2, minbucket = 1,control = rpart.control(cp = 0.01))
test_arbres
##☼ courbe de roc de la premier modélisation avec les logit


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

