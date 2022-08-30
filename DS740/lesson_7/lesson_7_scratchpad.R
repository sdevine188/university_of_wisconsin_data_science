library(tidyverse)
library(tree)
library(gbm)
library(ISLR)
library(skimr)
library(GGally)
library(randomForest)

setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_7")


# get oj data
data(OJ)
OJ 
oj <- OJ %>% as_tibble() %>% mutate(STORE = factor(STORE), StoreID = factor(StoreID))
oj
oj %>% glimpse()
oj %>% nrow()
oj %>% ncol()
oj %>% skim
?OJ

# split data to training and testing
RNGkind(sample.kind = "Rejection")
set.seed(7)
groups = c(rep(1, 800), rep(0, 270)) # 1 represents the training set
random_groups = sample(groups, 1070)
in_train = (random_groups == 1)

oj_train <- oj %>% filter(in_train)
oj_test <- oj %>% filter(!in_train)


# n = 208
# k = 10 # Using 10-fold CV
# groups = rep(1:k, length = n)
# groups
# RNGkind(sample.kind = "Rejection")
# set.seed(729)
# cvgroups = sample(groups, n)
# 
# groupii <- (cvgroups == ii)
# train_set <- auto2[!groupii, ] %>% select(-name)
# test_set <- auto2[groupii, ] %>% select(-name)


# fit tree on training data
tree_1 <- tree(Purchase ~ ., data = oj_train)
summary(tree_1)
plot(tree_1)
text(tree_1, pretty = 0)

# predict testing data
tree_1_test_pred_class <- predict(tree_1, newdata = oj_test, type = "class")
tree_1_test_pred_prob <- predict(tree_1, newdata = oj_test, type = "vector")

# get confusion matrix
conf_mat <- table(tree_1_test_pred_class, oj_test$Purchase)
conf_mat

sum(diag(conf_mat)) / (oj_test %>% nrow())  # Accuracy
(131 + 87) / (131 + 87 + 18 + 34)
1 - sum(diag(conf_mat)) / (oj_test %>% nrow()) # Error rate


#//////////////////////


# use cross validation to select optimal number of leaves
# for regression, use prune.tree
tree_1_cv <- cv.tree(tree_1, FUN = prune.misclass)
tree_1_cv

# plot
plot(tree_1_cv)

# get optimal number of leaves
min(tree_1_cv$dev)
which(tree_1_cv$dev == min(tree_1_cv$dev))
tree_1_cv$size[ which(tree_1_cv$dev == min(tree_1_cv$dev)) ]

# get pruned tree
tree_1_cv_pruned <- prune.misclass(tree_1, best = 4)
plot(tree_1_cv_pruned)
text(tree_1_cv_pruned, pretty = 0)


#///////////////////


# predict with pruned tree
tree_1_cv_pruned_test_pred_class <- predict(tree_1_cv_pruned, newdata = oj_test, type = "class")
tree_1_cv_pruned_test_pred_prob <- predict(tree_1_cv_pruned, newdata = oj_test, type = "vector")

# get confusion matrix
conf_mat <- table(tree_1_cv_pruned_test_pred_class, oj_test$Purchase)
conf_mat

sum(diag(conf_mat)) / (oj_test %>% nrow())  # Accuracy
(131 + 87) / (131 + 87 + 18 + 34)
1 - sum(diag(conf_mat)) / (oj_test %>% nrow()) # Error rate


#////////////////////////////////////////////////////////////////////////////


data(Hitters)
hitters <- Hitters %>% filter(!is.na(Salary)) %>% mutate(salary_log = log(Salary)) %>% select(-Salary)
hitters 
hitters %>% glimpse()
hitters %>% nrow()
hitters %>% ncol()

hitters %>% skim(Salary)


#/////////////////


# create boosted tree
# for distribution arg, use "gaussian" for a regression problem; "bernoulli" for classification
boosted_tree_1 <- gbm(salary_log ~ ., data = hitters, 
            distribution = "gaussian", n.trees = 5000,
            shrinkage = .001, interaction.depth = 4)

# predict(boost, newdata = sonar, n.trees = 1000, type = "response")

# inspect
boosted_tree_1 %>% attributes()
boosted_tree_1 

boost_summary <- summary(boosted_tree_1)
boost_summary


#////////////////////////


# compare boosting to linear regression w cross validation

# get cv groups
n <- hitters %>% nrow()
k <- 10 # Using 10-fold CV
groups <- rep(1:k, length.out = n)
groups
RNGkind(sample.kind = "Rejection")
set.seed(7)
cvgroups <- sample(groups, n)

# create storage for boost and lm predictions
boost_predict <- numeric(length = n)
lm_predict <- numeric(length = n)
data_used <- hitters


# run cv loop
for(ii in 1:k){
        
        # set groupi testing data
        groupi <- (cvgroups == ii)
        
        # fit boost model on training data
        boost <- gbm(salary_log ~ ., data = data_used[!groupi, ], 
                    distribution = "gaussian", n.trees = 5000,
                    shrinkage = .001, interaction.depth = 4)
        
        # get boost predictions for testing data
        boost_predict[groupi] <- predict(object = boost, newdata = data_used[groupi, ], n.trees = 5000, type = "response")
        
        
        #/////////////////
        
        
        # fit lm model on training data
        lm_model <- lm(formula = salary_log ~ ., data = data_used[!groupi, ])
        
        # get lm predictions for testing data
        lm_predict[groupi] <- predict(object = lm_model, newdata = data_used[groupi, ])
        
}


#///////////////


# get MSE
boost_predict %>% tibble(pred = .) %>% 
        mutate(truth = hitters$salary_log,
               error = pred - truth,
               error_squared = error^2,
               mse = num(mean(error_squared), digits = 5))

lm_predict %>% tibble(pred = .) %>% 
        mutate(truth = hitters$salary_log,
               error = pred - truth,
               error_squared = error^2,
               mse = num(mean(error_squared), digits = 5))
        

#///////////////////////////////////////////////////////


# question 15

# get correlation 
ggcorr(hitters, method = c("everything", "pearson"))


#/////////////


# fit bagging_model_1
RNGkind(sample.kind = "Rejection")
set.seed(7)

bagging_model_1 <- randomForest(salary_log ~ ., data = hitters, mtry = ((hitters %>% ncol()) - 1), importance = TRUE)


# inspect
bagging_model_1
bagging_model_1 %>% attributes()
bagging_model_1 %>% summary()
plot(bagging_model_1)
importance(bagging_model_1) %>% as.data.frame() %>% 
        rownames_to_column(var = "rowname") %>% 
        arrange(desc(IncNodePurity))
varImpPlot(bagging_model_1)


#//////////////////


# fit rf_model_1
RNGkind(sample.kind = "Rejection")
set.seed(7)

rf_model_1 <- randomForest(salary_log ~ ., data = hitters, mtry = 6, importance = TRUE)

# inspect
rf_model_1
rf_model_1 %>% attributes()
rf_model_1 %>% summary()
plot(rf_model_1)
importance(rf_model_1) %>% as.data.frame() %>% 
        rownames_to_column(var = "rowname") %>% 
        arrange(desc(IncNodePurity))
varImpPlot(rf_model_1)

partialPlot(rf_model_1, pred.data = hitters, x.var = CAtBat)



