library(tidyverse)
library(glmnet)
library(MASS)

setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_5")

heart <- read_csv(file = "Heart_disease_Cleveland.csv") %>%
        slice(-c(88, 167, 193, 267, 288, 303)) %>%
        mutate(log.STdepress = log(STdepress),
               Sex = factor(Sex),
               ChestPain = factor(ChestPain),
               HighBloodSugar = factor(HighBloodSugar),
               ECG = factor(ECG),
               ExerAngina = factor(ExerAngina),
               Slope = factor(Slope),
               Thal = factor(Thal),
               DiseaseStatus = factor(DiseaseStatus)) %>%
        select(-STdepress)
heart

heart %>% select(where(is.numeric)) %>% cor()
heart %>% select(where(is.numeric)) %>% 
        ggpairs(., upper = list(continuous = wrap("cor", size = 2)))


#/////////////////


BostonNew <- Boston %>%
        mutate(log.crim= log(crim),log.zn= log(zn+1), chas = factor(chas)) %>%
        dplyr::select(-zn,-crim)

x = model.matrix(log.crim~.,data=BostonNew)[,-1]
y = BostonNew$log.crim


lambdalist = 1:1000/1000
RRfit = glmnet(x, y, lambda=lambdalist, alpha = 0)
coef(RRfit,s=0.05)



LASSOfit = glmnet(x, y, lambda=lambdalist, alpha = 1)
coef(LASSOfit,s=0.05)



#///////////////



n=506
ncv = 10
groups=rep(1:10,length=n)
RNGkind(sample.kind = "Rejection")
set.seed(5)
cvgroups = sample(groups,n)


cvRR = cv.glmnet(x, y, lambda=lambdalist, alpha = 0, nfolds=ncv, foldid=cvgroups)
min(cvRR$cvm)
RRbestlambda = cvRR$lambda.min
RRbestlambda


cvlasso = cv.glmnet(x, y, lambda=lambdalist, alpha = 1, nfolds=ncv, foldid=cvgroups)
min(cvlasso$cvm)
lassobestlambda = cvlasso$lambda.min
lassobestlambda

cvelastic_net = cv.glmnet(x, y, lambda=lambdalist, alpha = .5, nfolds=ncv, foldid=cvgroups)
min(cvelastic_net$cvm)
elastic_netbestlambda = cvelastic_net$lambda.min
elastic_netbestlambda

coef(RRfit,s=RRbestlambda)
coef(LASSOfit,s=lassobestlambda)
coef(ENET50fit,s=elastic_netbestlambda)


#////////////////


beta.fn.penalized = function(yxdata,index,bestlambda,lambdalist,alpha) {
        yboot = yxdata[index,1]
        xboot = yxdata[index,-1]
        penfitboot = glmnet(xboot, yboot, alpha = alpha,lambda=lambdalist)
        return(coef(penfitboot,s=bestlambda)[,1])
}


library(boot)
RNGkind(sample.kind = "Rejection")

set.seed(5)
RRbootoutput = boot(cbind(y,x),beta.fn.penalized,R=1000,
                    bestlambda=0.012, lambdalist=lambdalist, alpha=0)
print(RRbootoutput)

RRbootoutput %>% mutate()

RNGkind(sample.kind = "Rejection")
set.seed(5)
RRbootoutput = boot(cbind(y,x),beta.fn.penalized,R=1000,
                    bestlambda=0.012, lambdalist=lambdalist, alpha=0)
print(RRbootoutput)
apply(RRbootoutput$t,2,sd)
apply(RRbootoutput$t,2,sd)[10]
apply(RRbootoutput$t,2,sd)[14]



RNGkind(sample.kind = "Rejection")
set.seed(5)
LASSObootoutput = boot(cbind(y,x),beta.fn.penalized,R=1000,
                    bestlambda=0.015, lambdalist=lambdalist, alpha=1)
print(LASSObootoutput)
apply(LASSObootoutput$t,2,sd)
apply(LASSObootoutput$t,2,sd)[10]
apply(LASSObootoutput$t,2,sd)[14]


RNGkind(sample.kind = "Rejection")
set.seed(5)
ENETbootoutput = boot(cbind(y,x),beta.fn.penalized,R=1000,
                       bestlambda=0.028, lambdalist=lambdalist, alpha=.5)
print(ENETbootoutput)
apply(ENETbootoutput$t,2,sd)
apply(ENETbootoutput$t,2,sd)[10]
apply(ENETbootoutput$t,2,sd)[14]


#////////////////////////////////////////////////////////////////////////////////////////////


# homework ####

# problem 1 ####
Trees <- read.csv("TreesTransformed.csv")  
x <- model.matrix(Volume ~ ., data = Trees)[ , -1]
y <- Trees$Volume

# inspect
Trees
x
y


#//////////////


# fit models

# get lambdalist
lambdalist <- c((1:100)/100)
lambdalist

# fit linear regression model
lm_model <- lm(formula = Volume ~ ., data = Trees)
summary(lm_model)

# fit ridge regression model
rr_model <- glmnet(x = x, y = y, lambda = lambdalist, alpha = 0)
# coef(rr_model, s = 0.05)

# fit lasso model
lasso_model <- glmnet(x = x, y = y, lambda = lambdalist, alpha = 1)
coef(lasso_model, s = 0.1)

# fit elasticnet_model
elasticnet_model <- glmnet(x = x, y = y, lambda = lambdalist, alpha = .7)


#///////////////


lambdalist = 1:100/100
nfolds = 5; groups = rep(1:nfolds,length=dim(trees)[1]) 
set.seed(5); cvgroups = sample(groups,n) 

RRcv = cv.glmnet(x, y, alpha = 0,lambda=lambdalist,nfolds=nfolds, foldid=cvgroups)
LASSOcv = cv.glmnet(x, y, alpha = 1,lambda=lambdalist,nfolds=nfolds, foldid=cvgroups)
ENETcv = cv.glmnet(x, y, alpha = 0.7,lambda=lambdalist,nfolds=nfolds, foldid=cvgroups)


#/////////////////


library(ISLR)

CollegeT <- College %>% as_tibble() %>% mutate(log.Enroll = log(Enroll),
                                               log.Apps = log(Apps),
                                               log.Accept = log(Accept),
                                               log.F.Undergrad = log(F.Undergrad),
                                               log.P.Undergrad = log(P.Undergrad),
                                               Private = factor(Private)) %>% 
        dplyr::select(-c(Enroll, Apps, Accept, F.Undergrad, P.Undergrad))
CollegeT %>% glimpse()

CollegeT %>% ggplot(data = ., mapping = aes(x = log.Enroll)) + geom_density()

ggpairs(data = CollegeT)
CollegeT %>% dplyr::select(where(is.numeric)) %>% correlate() %>% 
        dplyr::select(term, log.Enroll) %>% arrange(desc(log.Enroll))
CollegeT %>% dplyr::select(where(is.numeric)) %>% correlate() %>% rplot()


#///////////////////


# get x and y
x <- model.matrix(log.Enroll ~ ., data = CollegeT)[ , -1]
x
y <- CollegeT$log.Enroll
y

# get lambdalist
lambdalist <- seq(from = .001, to = 1, by = .001)
lambdalist

# fit lasso model
lasso_model <- glmnet(x = x, y = y, lambda = lambdalist, alpha = 1)
coef(lasso_model, s = 0.02)
coef(lasso_model, s = 0.03)
coef(lasso_model, s = 0.05)
coef(lasso_model, s = 0.5)


#///////////////////


# get cvgroups
ncollege <- dim(CollegeT)[1]
groups = rep(1:10, length.out = ncollege)
RNGkind(sample.kind = "Rejection")
set.seed(5)
cvgroups <- sample(groups, size = ncollege)

# fit elasticnet model
elasticnet_cv_model <- cv.glmnet(x, y, alpha = 0.75, lambda = lambdalist, nfolds = nfolds, foldid = cvgroups)

# plot cv MSE vs. lambda
plot(elasticnet_cv_model)  # vertical lines at lambda.min and lambda.1se

# get best lambda values and corresponding lowest cv MSE
elasticnet_cv_model 
elasticnet_cv_model$lambda.min
min(elasticnet_cv_model$cvm)


