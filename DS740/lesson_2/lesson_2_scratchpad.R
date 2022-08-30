library(tidyverse)
library(ggformula)
library(GGally)
library(boot)
library(MASS)
library(skimr)

setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_2")
options(scipen=999)

# webwork

# load boston data from MASS package

Boston
boston <- Boston %>% as_tibble()
boston %>% glimpse()
boston %>% nrow() # 506
boston %>% skim(crim, age, rad)
?Boston

# get boston_std
boston_std <- boston %>% dplyr::select(age, rad) %>%
        mutate(across(.cols = c(age, rad), .fns = ~ scale(.x)[ , 1], .names = "{.col}_std"))
boston_std

# boston_std <- boston %>% dplyr::select(age, rad) %>% 
#         scale() 
# boston_std %>% as_tibble() %>% glimpse()
# attributes(boston_std)
# attr(boston_std, "scaled:center")
# attr(boston_std, "scaled:scale")

# add on response variable crim
boston_std <- boston_std %>% mutate(crim = boston$crim)
boston_std %>% glimpse()

# attr(x_train, "scaled:center")
# attr(x_train, "scaled:scale")
# x_test = scale(x_test, center = attr(x_train, "scaled:center"), 
#                scale = attr(x_train, "scaled:scale"))

# get x_std
x_std <- boston_std %>% dplyr::select(age_std, rad_std)

# fit knn
predictions = knn.reg(train = x_std, 
                      test = x_std,
                      y = boston_std$crim,
                      k = 25)

mean((predictions$pred - boston_std$crim)^2)


#//////////////////


# using course recommended code for compatibility
BostonStd <- Boston %>% 
        mutate(age.std = scale(age),rad.std = scale(rad)) %>% 
        dplyr::select(age,rad,age.std,rad.std,crim)
BostonStd
x.std <- BostonStd %>%
        dplyr::select(age.std,rad.std)
y <- BostonStd$crim
predictions = knn.reg(train = x.std, test = x.std, y = y, k = 25)
mean( (y - predictions$pred)^2 )

x <- BostonStd %>% dplyr::select(age,rad)
x
y <- BostonStd %>% dplyr::select(crim)
y


#//////////////


# webwork problem 2
BostonStd <- Boston %>% 
        mutate(age.std = scale(age),rad.std = scale(rad)) %>% 
        dplyr::select(age,rad,age.std,rad.std,crim)
BostonStd %>% glimpse()

x <- BostonStd %>% dplyr::select(age,rad)
x
y <- BostonStd %>% dplyr::select(crim)
y

# use LOO cross-validation to get MSE
n <- BostonStd %>% nrow()
n
nfolds <- n
nfolds
cvgroups <- (1:n)
cvgroups


# loop to do cross-validation
allpredicted <- rep(NA, n)   # storage for honest predictions
for(ii in 1:nfolds) {    # ii is an easier string to search for index
        
        # get train and test set
        groupii <- (cvgroups == ii)     # logical vector for group ii
        trainset <- x[!groupii,]  # all data EXCEPT for group ii
        ytrain <- y[!groupii, ]
        testset <- x[groupii, ]   # data in group ii
        
        # standardize predictors
        trainset <- scale(trainset)
        testset <- scale(testset, center = attr(trainset, "scaled:center"), 
                       scale = attr(trainset, "scaled:scale"))
        
        # fit knn model, get predictions, and store them
        predictions <- knn.reg(train = trainset, test = testset, y = ytrain, k = 25)
        allpredicted[groupii] = predictions$pred              # store in ordered locations
}

# get cv_value
allpredicted %>% tibble(predictions = .)
cv_value_loo <- mean((allpredicted - y[, 1])^2)
cv_value_loo # 40.69515


#/////////////////////


# webwork problem 3

# clean and standardize data
BostonStd <- Boston %>% 
        mutate(age.std = scale(age),rad.std = scale(rad)) %>% 
        dplyr::select(age,rad,age.std,rad.std,crim)
BostonStd %>% glimpse()

x <- BostonStd %>% dplyr::select(age,rad)
x
y <- BostonStd %>% dplyr::select(crim)
y

# get nfolds and cvgroups
nfolds <- 10
nfolds
n <- Boston %>% as_tibble() %>% nrow()
n
groups <- rep(1:nfolds, length.out = n)
groups

RNGkind(sample.kind = "Rejection")
set.seed(2)
cvgroups <- sample(x = groups, size = n, replace = FALSE)
cvgroups


# loop to do cross-validation
allpredicted <- rep(NA, n)   # storage for honest predictions
for(ii in 1:nfolds) {    # ii is an easier string to search for index
        
        # get train and test set
        groupii <- (cvgroups == ii)     # logical vector for group ii
        trainset <- x[!groupii,]  # all data EXCEPT for group ii
        ytrain <- y[!groupii, ]
        testset <- x[groupii, ]   # data in group ii
        
        # standardize predictors
        trainset <- scale(trainset)
        testset <- scale(testset, center = attr(trainset, "scaled:center"), 
                         scale = attr(trainset, "scaled:scale"))
        
        # fit knn model, get predictions, and store them
        predictions <- knn.reg(train = trainset, test = testset, y = ytrain, k = 25)
        allpredicted[groupii] = predictions$pred              # store in ordered locations
}

# get cv_value
allpredicted %>% tibble(predictions = .)
cv_value_10_fold <- mean((allpredicted - y[, 1])^2)
cv_value_10_fold # 41.18455


#/////////////////////


# webwork problem 5

# get data
x <- Boston
x %>% glimpse()

# get models and model_list
model_1 <- crim ~ rad
model_2 <- crim ~ rad + age
model_3 <- crim ~ .
model_list <- c(model_1, model_2, model_3)
model_list

# 10-fold CV
n <- Boston %>% nrow()
n
nfolds <- 10
RNGkind(sample.kind = "Rejection")
set.seed(2)
groups = rep(1:nfolds, length.out = n)  # nfolds is number of folds
groups
cvgroups <- sample(x = groups, size = n, replace = FALSE)  
cvgroups


#///////////////////


# loop through models getting cross-validated MSE
model_cv_values <- c()
for(m in 1:length(model_list)) {
        
        # loop to do cross-validation
        allpredicted <- rep(NA, n)   # storage for honest predictions
        # predictions <- tibble(row_number = 1:n)
        for(ii in 1: nfolds) {    # ii is an easier string to search for index
                
                # get train and test set
                groupii <- (cvgroups == ii)     # logical vector for group ii
                trainset <- x[!groupii,]  # all data EXCEPT for group ii
                testset <- x[groupii, ]   # data in group ii
                
                
                # fit model, get predictions, and store them
                current_model_fit <- lm(formula = model_list[[m]], data = trainset) # fit to train set
                current_predictions <- predict(object = current_model_fit, newdata = testset)
                allpredicted[groupii] <- current_predictions             # store in ordered locations
                # predictions <- predictions %>% mutate(!!sym(str_c("model_", ii, "_predictions")) := current_predictions)
                
        }
        
        # get cv_value
        allpredicted %>% tibble(predictions = .)
        model_cv_values[m] <- mean((allpredicted - Boston %>% pull(crim))^2)
}

# inspect
model_cv_values


#///////////////////////


# webwork problem 6

# get data
boston <- Boston %>% mutate(crim_log = log(crim))

# fit model
original_model_fit <- lm(formula = crim_log ~ age + rad, data = boston)
original_model_fit
attributes(model_fit)
summary(original_model_fit)
summary(original_model_fit) %>% attributes()
summary(original_model_fit) %>% str()
summary(original_model_fit)$coefficients %>% as_tibble()
original_model_fit$coefficients

# create get_bootstrapped_std_errors()
get_bootstrapped_std_errors <- function(data, index) {
        
        # get current bootstrapped data
        current_data <- data %>% slice(index)
        
        # fit model and return coefficients
        model_fit <- lm(formula = crim_log ~ age + rad, data = current_data)
        return(model_fit$coefficients)
        
}

# run boostrap on get_bootstrapped_std_errors
RNGkind(sample.kind = "Rejection")
set.seed(2)
boot_output <- boot(data = boston, statistic = get_bootstrapped_std_errors, R = 5000)
boot_output
boot_output %>% attributes()
# note that in bootstrap output: 
# the original variable is the model coefficients for the predictors on original full data
summary(original_model_fit)
boot_output
# the bias variable is the avg difference between the original_model coefficient and each boostrap sample coefficient
summary(original_model_fit)
boot_output
boot_output$t %>% as_tibble() 
boot_output$t %>% as_tibble() %>% 
        mutate(V1_original = -4.226193, 
               V1_bias_for_sample = V1 - V1_original,
               V1_bias_avg = mean(V1_bias_for_sample))
# the std. error variable is just the sd of the bootstrap sample coefficients
boot_output
boot_output$t %>% as_tibble() %>%
        mutate(std_error = sd(V1))

# get rounded str. error
round(sd(bootoutput$t[ , 1]), digits = 6)
round(sd(bootoutput$t[ , 2]), digits = 6)
round(sd(bootoutput$t[ , 3]), digits = 6)

#/////////////////


# instructor code for webwork problem 6
BostonT <- Boston %>% 
        mutate(log.crim = log(crim)) %>% 
        dplyr::select(age,rad,log.crim)

beta.fn = function(inputdata,index) {
        lmfitboot = lm(formula = log.crim ~., data= inputdata[index,])
        return(lmfitboot$coef)
} 
beta.fn(BostonT,1:n)  # outputs coefficients of model fit on full dataset (observations 1 to n)


RNGkind(sample.kind = "Rejection")
set.seed(2)
bootoutput = boot(BostonT,beta.fn,R=5000)
print(bootoutput)
# standard error as estimated via simulation
round(sd((bootoutput$t)[,1]),6); hist((bootoutput$t)[,1])

round(sd((bootoutput$t)[,2]),6); hist((bootoutput$t)[,2])

round(sd((bootoutput$t)[,3]),6); hist((bootoutput$t)[,3])


