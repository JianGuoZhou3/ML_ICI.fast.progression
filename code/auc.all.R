#####1.glm####
# train
x.test <-data.frame(train.data[,-1])
probabilities <-predict(model.glm,x.test)
FP.auc<-roc(train.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( train.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
glm.train.overall<-cbind(class,overall)
glm.train.overall<-data.frame(glm.train.overall)
glm.train.overall$id<-"glm"
glm.train.overall$dataset<-"train"

# test1
x.test <-data.frame(test1.data[,-1])
probabilities <-predict(model.glm,x.test)
FP.auc<-roc(test1.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test1.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
glm.test1.overall<-cbind(class,overall)
glm.test1.overall<-data.frame(glm.test1.overall)
glm.test1.overall$id<-"glm"
glm.test1.overall$dataset<-"test1"

# test2 
x.test <-data.frame(test2.data[,-1])
probabilities <-predict(model.glm,x.test)
FP.auc<-roc(test2.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test2.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
glm.test2.overall<-cbind(class,overall)
glm.test2.overall<-data.frame(glm.test2.overall)
glm.test2.overall$id<-"glm"
glm.test2.overall$dataset<-"test2"

#####2.lasso####
# train
x.test <-data.matrix(train.data[,-1])
probabilities <-predict(model.lasso,newx =x.test)
FP.auc<-roc(train.data[,1],probabilities[,1],ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities[,1]>cut.off$threshold,1,0) )
ref<-as.factor( train.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
lasso.train.overall<-cbind(class,overall)
lasso.train.overall<-data.frame(lasso.train.overall)
lasso.train.overall$id<-"lasso"
lasso.train.overall$dataset<-"train"

# test1
x.test <-data.matrix(test1.data[,-1])
probabilities <-predict(model.lasso,newx =x.test)
FP.auc<-roc(test1.data[,1],probabilities[,1],ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities[,1]>cut.off$threshold,1,0) )
ref<-as.factor( test1.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
lasso.test1.overall<-cbind(class,overall)
lasso.test1.overall<-data.frame(lasso.test1.overall)
lasso.test1.overall$id<-"lasso"
lasso.test1.overall$dataset<-"test1"

# test2 
x.test <-data.matrix(test2.data[,-1])
probabilities <-predict(model.lasso,newx =x.test)
FP.auc<-roc(test2.data[,1],probabilities[,1],ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities[,1]>cut.off$threshold,1,0) )
ref<-as.factor( test2.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
lasso.test2.overall<-cbind(class,overall)
lasso.test2.overall<-data.frame(lasso.test2.overall)
lasso.test2.overall$id<-"lasso"
lasso.test2.overall$dataset<-"test2"

#####3.svm####
# train
x.test <-data.matrix(train.data[,-1])
probabilities <-predict(model.svm,x.test)
FP.auc<-roc(train.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( train.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
svm.train.overall<-cbind(class,overall)
svm.train.overall<-data.frame(svm.train.overall)
svm.train.overall$id<-"svm"
svm.train.overall$dataset<-"train"

# test1
x.test <-data.matrix(test1.data[,-1])
probabilities <-predict(model.svm,x.test)
FP.auc<-roc(test1.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test1.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
svm.test1.overall<-cbind(class,overall)
svm.test1.overall<-data.frame(svm.test1.overall)
svm.test1.overall$id<-"svm"
svm.test1.overall$dataset<-"test1"

# test2 
x.test <-data.matrix(test2.data[,-1])
probabilities <-predict(model.svm,x.test)
FP.auc<-roc(test2.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test2.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
svm.test2.overall<-cbind(class,overall)
svm.test2.overall<-data.frame(svm.test2.overall)
svm.test2.overall$id<-"svm"
svm.test2.overall$dataset<-"test2"
#####4.dt######
###train
probabilities1 <- predict(model.dt,train.data[,-1],type="class")
FP.auc1<-roc(train.data[,1],as.numeric(probabilities1),ci= TRUE)
cut.off<-coords(FP.auc1,"best")
predi<-as.factor(ifelse(as.numeric(probabilities1)>cut.off$threshold,1,0) )
ref<-as.factor( train.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
dt.train.overall<-cbind(class,overall)
dt.train.overall<-data.frame(dt.train.overall)
dt.train.overall$id<-"dt"
dt.train.overall$dataset<-"train"

###test1
probabilities1 <- predict(model.dt,test1.data[,-1],type="class")
FP.auc1<-roc(test1.data[,1],as.numeric(probabilities1),ci= TRUE)
cut.off<-coords(FP.auc1,"best")
predi<-as.factor(ifelse(as.numeric(probabilities1)>cut.off$threshold,1,0) )
ref<-as.factor( test1.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
dt.test1.overall<-cbind(class,overall)
dt.test1.overall<-data.frame(dt.test1.overall)
dt.test1.overall$id<-"dt"
dt.test1.overall$dataset<-"test1"
###test2
probabilities1 <- predict(model.dt,test2.data[,-1],type="class")
FP.auc1<-roc(test2.data[,1],as.numeric(probabilities1),ci= TRUE)
cut.off<-coords(FP.auc1,"best")
predi<-as.factor(ifelse(as.numeric(probabilities1)>cut.off$threshold,1,0) )
ref<-as.factor( test2.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
dt.test2.overall<-cbind(class,overall)
dt.test2.overall<-data.frame(dt.test2.overall)
dt.test2.overall$id<-"dt"
dt.test2.overall$dataset<-"test2"


#####5.rf####
# train
x.test <-data.matrix(train.data[,-1])
probabilities <-predict(model.rf,x.test,type="class")
probabilities <- ifelse(probabilities==1,1,0)
FP.auc<-roc(train.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( train.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
rf.train.overall<-cbind(class,overall)
rf.train.overall<-data.frame(rf.train.overall)
rf.train.overall$id<-"rf"
rf.train.overall$dataset<-"train"

# test1
x.test <-data.matrix(test1.data[,-1])
probabilities <-predict(model.rf,x.test)
probabilities <- ifelse(probabilities==1,1,0)
FP.auc<-roc(test1.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test1.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
rf.test1.overall<-cbind(class,overall)
rf.test1.overall<-data.frame(rf.test1.overall)
rf.test1.overall$id<-"rf"
rf.test1.overall$dataset<-"test1"

# test2 
x.test <-data.matrix(test2.data[,-1])
probabilities <-predict(model.rf,x.test)
probabilities <- ifelse(probabilities==1,1,0)
FP.auc<-roc(test2.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test2.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
rf.test2.overall<-cbind(class,overall)
rf.test2.overall<-data.frame(rf.test2.overall)
rf.test2.overall$id<-"rf"
rf.test2.overall$dataset<-"test2"

#####6.xgb####
# train
x.test <-as.matrix(train.data[,-1])
probabilities <-predict(model.xgb,x.test)
FP.auc<-roc(train.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( train.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
xgb.train.overall<-cbind(class,overall)
xgb.train.overall<-data.frame(xgb.train.overall)
xgb.train.overall$id<-"xgb"
xgb.train.overall$dataset<-"train"

# test1
x.test <-data.matrix(test1.data[,-1])
probabilities <-predict(model.xgb,x.test)
FP.auc<-roc(test1.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test1.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
xgb.test1.overall<-cbind(class,overall)
xgb.test1.overall<-data.frame(xgb.test1.overall)
xgb.test1.overall$id<-"xgb"
xgb.test1.overall$dataset<-"test1"

# test2 
x.test <-data.matrix(test2.data[,-1])
probabilities <-predict(model.xgb,x.test)
FP.auc<-roc(test2.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test2.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
xgb.test2.overall<-cbind(class,overall)
xgb.test2.overall<-data.frame(xgb.test2.overall)
xgb.test2.overall$id<-"xgb"
xgb.test2.overall$dataset<-"test2"
#####6.gbm####
# train
x.test <-data.frame(train.data[,-1])
probabilities <-predict(model.gbm,x.test,n.trees = best.iter, type = "link")
FP.auc<-roc(train.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( train.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
gbm.train.overall<-cbind(class,overall)
gbm.train.overall<-data.frame(gbm.train.overall)
gbm.train.overall$id<-"gbm"
gbm.train.overall$dataset<-"train"

# test1
x.test <-data.frame(test1.data[,-1])
probabilities <-predict(model.gbm,x.test,n.trees = best.iter, type = "link")
FP.auc<-roc(test1.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test1.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
gbm.test1.overall<-cbind(class,overall)
gbm.test1.overall<-data.frame(gbm.test1.overall)
gbm.test1.overall$id<-"gbm"
gbm.test1.overall$dataset<-"test1"

# test2 
x.test <-data.frame(test2.data[,-1])
probabilities <-predict(model.gbm,x.test,n.trees = best.iter, type = "link")
FP.auc<-roc(test2.data[,1],probabilities,ci= TRUE)
cut.off<-coords(FP.auc,"best")
predi<-as.factor(ifelse(probabilities>cut.off$threshold,1,0) )
ref<-as.factor( test2.data[,1])
cm<-confusionMatrix(predi,reference = ref)
class<-cm[["byClass"]]
class<-t(class)
overall<-  cm[["overall"]]
overall<-t(overall)
gbm.test2.overall<-cbind(class,overall)
gbm.test2.overall<-data.frame(gbm.test2.overall)
gbm.test2.overall$id<-"gbm"
gbm.test2.overall$dataset<-"test2"
###overall
auc.overall<-data.frame(rbind(glm.train.overall,lasso.train.overall,svm.train.overall,
                              dt.train.overall,rf.train.overall,xgb.train.overall,gbm.train.overall,
                              glm.test1.overall,lasso.test1.overall,svm.test1.overall,dt.test1.overall,
                              rf.test1.overall,xgb.test1.overall,gbm.test1.overall,
                              glm.test2.overall,lasso.test2.overall,svm.test2.overall,dt.test2.overall,
                              rf.test2.overall,xgb.test2.overall,gbm.test2.overall))
