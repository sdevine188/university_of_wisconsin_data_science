library(tidyverse)
library(kernlab)
library(caret)
library(ggformula)

setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_9")

bank = read_delim("bank-additional.csv", delim=";")
bank <- bank %>%
        mutate(scale_duration = scale(duration),
               scale_emp.var.rate = scale(emp.var.rate))
bank %>%
        gf_point(scale_duration ~ scale_emp.var.rate, 
                 color =~ y, pch =~ y)


set.seed(999)
data_used = bank
ctrl = trainControl(method = "cv", number = 10)
fit_bank = train(y ~ emp.var.rate + duration,
                 data = data_used,
                 method = "svmLinear",
                 tuneGrid = expand.grid(C = 1),
                 preProcess = c("center","scale"),
                 trControl = ctrl)
fit_bank
confusionMatrix(fit_bank)
fit_bank %>% attributes()
pred <- predict(object = fit_bank$finalModel, newdata = bank)
data_used <- data_used %>% mutate(pred = factor(pred, levels = c("no", "yes")),
                                  y = factor(y, levels = c("no", "yes")))
confusionMatrix(data = data_used$pred, reference = data_used$y)



bank <- bank %>%
        tibble::rownames_to_column("Row")
bank <- bank %>%
        mutate(is_SV = Row %in% attr(fit_bank$finalModel, "SVindex"))
b = attr(fit_bank$finalModel, "b")
coefs = attr(fit_bank$finalModel, "coef")[[1]]
bank_SV <- bank %>%
        filter(is_SV) %>%
        select(c(scale_emp.var.rate, scale_duration)) %>%
        as.matrix()
w = colSums(coefs * bank_SV) # beta_1, ... beta_p
bank %>%
        gf_point(scale_duration ~ scale_emp.var.rate, 
                 color =~ y, pch =~ y) %>%
        gf_abline(intercept = b/w[2], slope = -w[1]/w[2]) %>%
        gf_abline(intercept = (b+1)/w[2], slope = -w[1]/w[2], lty = 2) %>%
        gf_abline(intercept = (b-1)/w[2], slope = -w[1]/w[2], lty = 2)



#///////////////////////////////////



set.seed(999)
data_used = bank
ctrl = trainControl(method = "cv", number = 5)
fit_radial = train(y ~ emp.var.rate + duration,
                   data = data_used,
                   method = "svmRadial",
                   tuneGrid = expand.grid(C = c(1, 10, 100),
                                          sigma = c(1, 2, 3, 4)),
                   preProcess = c("center","scale"),
                   prob.model = TRUE,
                   trControl = ctrl)
fit_radial
fit_radial$bestTune
confusionMatrix(fit_radial)

newClient = data.frame(emp.var.rate = 1, duration = 250) 
pred <- predict(object = fit_radial$finalModel, newdata = newClient)
pred <- predict(object = fit_radial$finalModel, newdata = bank)
data_used <- bank %>% mutate(pred = factor(pred, levels = "no", "yes"),
                             y = factor(y, levels = "no", "yes"))
confusionMatrix(data = data_used$pred, reference = data_used$y)


#//////////////////


preds = predict(fit_radial, newdata = xgrid, type = "prob")
head(preds)
xgrid <- xgrid %>%
        mutate(prob_no = preds[ ,1])

ggplot(xgrid, aes(emp.var.rate, duration, z = prob_no)) +
        geom_contour(breaks = .5) +
        geom_point(aes(emp.var.rate, duration, shape = y, color = y),
                   data = bank, 
                   inherit.aes = FALSE)



#/////////////////


emp.var.rate = seq(-3.4, 1.4, by = .01)


#/////////////////////////////////////////////////////////////////////////////


# homework

oak <- read.csv("Oak_log.csv") %>% as_tibble() %>%
        mutate(logSize_std = scale(logSize, center = TRUE, scale = TRUE)[, 1],
               logRange_std = scale(logRange, center = TRUE, scale = TRUE)[, 1]) 
oak
oak %>% glimpse()
oak %>% count(Region)

# scatterplot
oak %>% ggplot(data = ., mapping = aes(x = logSize_std, y = logRange_std, color = Region, shape = Region)) +
        geom_point()



#/////////////////////////


set.seed(9)
train_control <- trainControl(method = "LOOCV")
oak_linear_svm <- train(Region ~ logSize + logRange,
                 data = oak,
                 method = "svmLinear",
                 tuneGrid = expand.grid(C = c( .001, .01, .1, 1, 5, 10, 100)),
                 preProcess = c("center","scale"),
                 trControl = train_control)

# inspect
oak_linear_svm

# predict
pred <- predict(object = oak_linear_svm$finalModel, newdata = oak %>% select(logSize, logRange))
pred
oak <- oak %>% mutate(pred = pred)
oak %>% glimpse()
confusionMatrix(data = oak %>% mutate(pred = factor(pred, levels = c("Atlantic", "California"))) %>% pull(pred), 
                reference = oak %>% mutate(Region = factor(Region, levels = c("Atlantic", "California"))) %>% pull(Region))








#/////////////////////



###################################################################
##### Double cross-validation for modeling-process assessment #####				 
###################################################################

##### model assessment OUTER shell #####
# produce loops for 10-fold cross-validation for model ASSESSMENT
n <- oak %>% nrow()
nfolds = 10
groups = rep(1:nfolds,length=n)  #produces list of group labels
set.seed(9)
cvgroups = sample(groups, size = n)  #orders randomly

# set up storage for predicted values from the double-cross-validation
allpredictedCV = factor(rep(NA,n), levels = c("Atlantic", "California"))
# set up storage to see what models are "best" on the inner loops
allbestTypes = rep(NA,nfolds)
allbestPars = vector("list",nfolds)

# loop through outer splits
for (j in 1:nfolds)  {  #be careful not to re-use loop indices
        
        # print loop j
        print(str_c("currently on loop ", j))
        
        # get training and validation data
        groupj = (cvgroups == j)
        traindata = oak[!groupj,]
        validdata = oak[groupj,]
        
        #specify data to be used
        dataused=traindata
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////
        
        
        train_control <- trainControl(method = "LOOCV")
        fit_linear_svm <- train(Region ~ logSize + logRange,
                                data = dataused,
                                method = "svmLinear",
                                tuneGrid = expand.grid(C = c( .001, .01, .1, 1, 5, 10, 100)),
                                preProcess = c("center","scale"),
                                trControl = train_control)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////
        #//////////////////////////////////////////////////////////////////////////////////////////
        
        
        ############# identify selected model to fit to full data #############
        
        
        # all best models
        # note that for kNN, fit_caret_kNN is the actual model; for most others like rf, it's fit_caret_rf$finalModel
        all_best_Types = c("linear_svm")
        all_best_Pars = list(fit_linear_svm$bestTune)
        all_best_Models = list(fit_linear_svm$finalModel)
        all_best_RMSE = c(max(fit_linear_svm$results$Accuracy))
        
        # get one best model/type/pars
        one_best_Type = all_best_Types[which.max(all_best_RMSE)]
        one_best_Pars = all_best_Pars[which.max(all_best_RMSE)]
        one_best_Model = all_best_Models[[which.max(all_best_RMSE)]]
        
        # store one best model type/pars
        allbestTypes[j] = one_best_Type
        allbestPars[[j]] = one_best_Pars
        
        
        #////////////////////////////////////////////////////////////////////////////////
        
        
        # predict validation data with one_best_model
        if (one_best_Type == "kNN") {  # then best is one of kNN models
                allpredictedCV[groupj] = one_best_Model %>% predict(validdata)
        } else if (one_best_Type == "rf") {  # then best is one of linear models
                allpredictedCV[groupj] = one_best_Model %>% predict(validdata)
        } else if(one_best_Type == "linear_svm") {  
                allpredictedCV[groupj] = one_best_Model %>% predict(newdata = validdata %>% select(logSize, logRange))
        }
}

# for curiosity / consistency, we can see the models that were "best" on each of the inner splits
allbestTypes
allbestPars
# print individually
for (j in 1:nfolds) {
        writemodel = paste("The best model at loop", j, 
                           "is of type", allbestTypes[j],
                           "with parameter(s)",allbestPars[j])
        print(writemodel, quote = FALSE)
}

# assessment
# the model selection process has an accuracy of .698, which is well-above the no-information rate of .396
# the model performs about equally well at predicting all three Sport_group classes

# the model's predictions are good enough to be used for targeting advertising, where the cost of a false positive is
# relatively low

# further improvemnts could likely be made by gathering more information on athletes age, nationality, and maybe
# physical performance stats like running speed, endurance, weight-lifting capacity, etc

y = oak %>% mutate(Region = factor(Region, levels = c("Atlantic", "California"))) %>% pull(Region)
confusionMatrix(data = allpredictedCV, reference = y)



#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////

library(ISLR)
auto <- Auto %>% mutate(origin = factor(origin),
                        median_mpg = median(mpg),
                        mpg_level = case_when(mpg >= median_mpg ~ "high",
                                              mpg < median_mpg ~ "low"),
                        mpg_level = factor(mpg_level)) %>%
        select(-c(mpg, name, median_mpg)) %>%
        as_tibble()
auto 
auto %>% glimpse()


#/////////////////////////////


# fit radial_svm

set.seed(9)
train_control <- trainControl(method = "cv", number = 10)
fit_radial_svm <- train(mpg_level ~ .,
                   data = auto,
                   method = "svmRadial",
                   tuneGrid = expand.grid(C = c(.001, .01, .1, 1, 5, 10, 100), 
                                          sigma = c(0.5, 1, 2, 3, 4)), 
                   preProcess = c("center","scale"),
                   prob.model = TRUE,
                   trControl = train_control)
fit_radial_svm


#/////////////////////


new_car <- tibble(cylinders = 4,
                  displacement = 132.5,
                  horsepower = 155,
                  weight = 2910,
                  acceleration = 8.3,
                  year = 77,
                  origin = factor(1))
new_car

# predict new car
pred <- predict(object = fit_radial_svm, newdata = new_car, type = "prob")
pred


#/////////////////////////


auto_expand_grid <- expand.grid(weight = seq(min(Auto$weight), max(Auto$weight), length = 100),
            cylinders = 4, 
            origin = 1,
            displacement = median(auto$displacement),
            horsepower = median(auto$horsepower),
            acceleration = median(auto$acceleration),
            year = median(auto$year)) %>% 
        as_tibble() %>% mutate(origin = factor(origin))
auto_expand_grid

pred <- predict(object = fit_radial_svm, newdata = auto_expand_grid, type = "prob")
pred


#////////////////////


auto_expand_grid %>% bind_cols(., pred) %>%
        ggplot(data = ., mapping = aes(x = weight, y = high)) + geom_line() +
        labs(title = "Predicted gas mileage decreases with increasing weight")

