library(tidyverse)
library(FNN)
library(glmnet)
library(caret)

setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_8")

Trees = read.csv("TreesTransformed.csv")
Trees

x = model.matrix(Volume~.,data=Trees)[,-1]
x
y = Trees[,1]
n = dim(x)[1]
lambdalist = exp((-1000:500)/100)
alphalist = c(0,.1,.2,.4,.6,.8,.9,1); n.alpha = length(alphalist)


################ Step 1. ##############
# split into cvgroups
nfolds = 5
groups = rep(1:nfolds,length=n)
set.seed(8)
cvgroups = sample(groups,n)

# run cross-validation for model selection 
alllambdabest = rep(NA,n.alpha)
allcv5best = rep(NA,n.alpha)
for (which.alpha in 1:n.alpha) {
        cvfit = cv.glmnet(x, y, lambda=lambdalist, alpha = alphalist[which.alpha], 
                          nfolds=nfolds, foldid=cvgroups)
        plot(cvfit$lambda, cvfit$cvm, xlim=c(0,2),ylim=c(0,15) ) 
        abline(v=cvfit$lambda[order(cvfit$cvm)[1]],col = "red")
        allcv5best[which.alpha] = cvfit$cvm[order(cvfit$cvm)[1]]
        alllambdabest[which.alpha] = cvfit$lambda[order(cvfit$cvm)[1]]
}
whichmodel = order(allcv5best)[1]
################ Step 2. ##############
bestalpha = alphalist[whichmodel]
bestlambda = alllambdabest[whichmodel]
bestmodel = glmnet(x, y, alpha = bestalpha,lambda=lambdalist)
############################################################


################ Step 1. ##############
# set up training method
set.seed(8)
training = trainControl(method = "cv", number = 5)

# cross-validation of penalized regression
dataused = Trees
fit_caret_penalized = train(Volume ~ . ,
                            data = dataused,
                            method = "glmnet",
                            trControl = training,
                            tuneGrid = expand.grid(alpha=alphalist,lambda=lambdalist))
min(fit_caret_penalized$results$RMSE)
plot(fit_caret_penalized$results$RMSE,ylim=c(2.5,3.5))

################ Step 2. ##############
fit_caret_penalized$bestTune
fit_caret_penalized$finalModel


#/////////////////


finallambda = fit_caret_penalized$bestTune$lambda
finalalpha = fit_caret_penalized$bestTune$alpha
finalfit.penalized <- glmnet(x, y, alpha = finalalpha,lambda=lambdalist)
finalcoef.penalized <- coef(finalfit.penalized,s=finallambda)




#//////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////


##### model assessment OUTER shell #####
# produce loops for 5-fold cross-validation for model ASSESSMENT
nfolds = 5
groups = rep(1:nfolds,length=n)  #produces list of group labels
set.seed(8)
cvgroups = sample(groups,n)  #orders randomly
# set up storage for predicted values from the double-cross-validation
allpredictedCV = rep(NA,n)
# set up storage to see what models are "best" from the inner loops
allbestPars = vector("list",nfolds)
# loop through outer splits
for (j in 1:nfolds)  {  #be careful not to re-use loop indices
        groupj = (cvgroups == j)
        traindata = Trees[!groupj,]
        trainx = model.matrix(Volume ~ ., data = traindata)[,-1]
        trainy = traindata$Volume
        validdata = Trees[groupj,]
        validx = model.matrix(Volume ~ ., data = validdata)[,-1]
        validy = validdata$Volume
        
        #specify data to be used
        dataused=traindata
        
        ################ Step 1. ##############
        # set up training method
        # set.seed(8)
        training = trainControl(method = "cv", number = 5)
        
        # cross-validation of penalized regression
        fit_caret_penalized = train(Volume ~ . ,
                                    data = dataused,
                                    method = "glmnet",
                                    trControl = training,
                                    tuneGrid = expand.grid(alpha=alphalist,lambda=lambdalist))
        min(fit_caret_penalized$results$RMSE)
        plot(fit_caret_penalized$results$RMSE,
             ylim=c(.95*min(fit_caret_penalized$results$RMSE),
                    1.2*min(fit_caret_penalized$results$RMSE)))
        
        ################ Step 2. (RENAMED) ##############
        best_Pars = fit_caret_penalized$bestTune
        print(min(fit_caret_penalized$results$RMSE))
        ################ Step 3. (RENAMED) ##############
        best_Model <- glmnet(x, y, alpha = best_Pars$alpha,lambda=lambdalist)
        ############################################################
        
        
        
        # best type must be a penalized regression model
        allbestPars[[j]] = best_Pars
        # only considering penalized regression models, so specified the same
        lambda.out = best_Pars$lambda
        alpha.out = best_Pars$alpha
        allpredictedCV[groupj]  = predict(best_Model,newx=validx,s=lambda.out)
}


y = Trees$Volume
CV.assess = mean((allpredictedCV-y)^2); CV.assess
R2.assess = 1 - sum((allpredictedCV-y)^2)/sum((y-mean(y))^2); R2.assess




#///////////////////////////////////////////////


input = read.csv("Heart_Disease_Cleveland.csv")

names(input)
heart = input[,c(1,4,5,8,10)]
heart$HD = rep(0, length(input$DiseaseStatus))
heart$HD[which(input$DiseaseStatus > 0)] = 1
heart$HD = factor(heart$HD)
table(heart$HD)



#////////////////////////////////////////////


set.seed(8)
library(MASS)
##############################
##entire model-fitting process##
xy.in = heart
n.in = dim(xy.in)[1]
k.in = 10
groups.in = rep(1:k.in,length=n.in)
cvgroups.in = sample(groups.in,n.in)
# with model selection 
allpredictedcv10 = matrix(,ncol=6,nrow=n.in)
for (i in 1:k.in) {
        # split out the test set
        newdata.in = xy.in[cvgroups.in==i,]
        
        #fit LDA on 2 predictors, for training set (cvgroups.in!=i)
        lda2fit = lda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
        allpredictedcv10[cvgroups.in==i,1] = predict(lda2fit,newdata.in)$class
        
        #fit LDA on 5 predictors, for training set (cvgroups.in!=i)
        lda5fit = lda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
        allpredictedcv10[cvgroups.in==i,2] = predict(lda5fit,newdata.in)$class
        
        #fit QDA on 2 predictors, for training set (cvgroups.in!=i)
        qda2fit = qda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
        allpredictedcv10[cvgroups.in==i,3] = predict(qda2fit,newdata.in)$class
        
        #fit QDA on 5 predictors, for training set (cvgroups.in!=i)
        qda5fit = qda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
        allpredictedcv10[cvgroups.in==i,4] = predict(qda5fit,newdata.in)$class
        
        #fit logistic on 2 predictors, for training set (cvgroups.in!=i)
        log2fit = glm(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i), family=binomial)
        log2prob = predict(log2fit,newdata.in,type="response")
        log2fact = rep(1,dim(newdata.in)[1]); log2fact[log2prob > 0.5] = 2
        allpredictedcv10[cvgroups.in==i,5] = log2fact
        
        #fit logistic on 5 predictors, for training set (cvgroups.in!=i)
        log5fit = glm(HD ~., data= xy.in, subset=(cvgroups.in!=i),family=binomial)
        log5prob = predict(log5fit,newdata.in,type="response")
        log5fact = rep(1,dim(newdata.in)[1]); log5fact[log5prob > 0.5] = 2
        allpredictedcv10[cvgroups.in==i,6] = log5fact
}
#relabel as original values, not factor levels
allpredictedcv10 = allpredictedcv10-1  # now a table of predicted 0-1 values for HD

#compute the CV values
allcv10 = rep(0,6)
for (m in 1:6) allcv10[m] = sum(xy.in$HD!=allpredictedcv10[,m])/n.in
bestmodels = (1:6)[allcv10 == min(allcv10)]
##############################










#///////////////////////////////////////////////////////////////////////////////////////////////////


##### model assessment OUTER shell #####
nvalid = 100
xy.out = heart
n.out = dim(xy.out)[1]

#define the validation set
set.seed(8)
validset = sample(1:n.out,nvalid)
trainxy.out = xy.out[-validset,]
testxy.out = xy.out[validset,]
###        inputs trainxy.out       ###
###		:	:	:	:	:	###
###   entire model-fitting process  ###
###		:	:	:	:	:	###
###      resulting in bestmodels     ###
bestmodel = ifelse(length(bestmodels)==1,bestmodels,sample(bestmodels,1))

# take the single selected best model and fit to the validation set
if (bestmodel == 1)  {
        lda2fit.train = lda(HD ~ MaxHeartRate + STdepress, data=trainxy.out)
        predictvalid = as.numeric(predict(lda2fit.train, testxy.out)$class)
}
if (bestmodel == 2)  {
        lda5fit.train = lda(HD ~ ., data=trainxy.out)
        predictvalid = as.numeric(predict(lda5fit.train, testxy.out)$class)
}
if (bestmodel == 3)  {
        qda2fit.train = qda(HD ~ MaxHeartRate + STdepress, data=trainxy.out)
        predictvalid = as.numeric(predict(qda2fit.train, testxy.out)$class)
}
if (bestmodel == 4)  {
        qda5fit.train = qda(HD ~ ., data=trainxy.out)
        predictvalid = as.numeric(predict(qda5fit.train, testxy.out)$class)
}
if (bestmodel == 5)  {
        log2fit.train = glm(HD ~ MaxHeartRate + STdepress, data= trainxy.out, family=binomial)
        log2prob.test = predict(log2fit.train,testxy.out,type="response")
        predictvalid = rep(1,dim(testxy.out)[1]); predictvalid[log2prob.test > 0.5] = 2
}
if (bestmodel == 6)  {
        log5fit.train = glm(HD ~ ., data= trainxy.out, family=binomial)
        log5prob.test = predict(log5fit.train,testxy.out,type="response")
        predictvalid = rep(1,dim(testxy.out)[1]); predictvalid[log5prob.test > 0.5] = 2
}

#relabel as original values, not factor levels
predictvalid = predictvalid-1  # now a vector of predicted 0-1 values for HD in validation set

#assessment
CV.valid = sum(testxy.out$HD!=predictvalid)/nvalid
p.valid = 1-CV.valid