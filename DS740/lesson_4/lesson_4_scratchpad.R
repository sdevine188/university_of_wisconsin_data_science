library(tidyverse)
library(pROC)
library(MASS)  #help(lda)
library(caret)

setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_4")

# lesson 4 scratchpad

# webwork problem 1

# Let Y denote the outcome, Y=1 for "yes" (dividend issued) and Y=0 for "no" (dividend not issued).
# 80% of companies issue dividends this year.
# For companies that issue a dividend, the distribution of X is normal with mean 10 and standard deviation 6.
# For companies that do not issue a dividend, the distribution of X is normal with mean 0 and standard deviation 6.

# p(y = 0 | x = x)
x <- 2
1 / (1 +  (4 * exp((20*x - 100) / 72)))

# p(y = 1 } x = x)
x <- 2
(4 * exp((20*x - 100) / 72)) / (1 + (4 * exp((20*x - 100) / 72)))


#////////////////////////////


# webwork problem 2

# y = 1
10 / (6^2)
log(.8) - (10^2 / (2*(6^2)))
x <- -.009
x <- -.1
x <- .009
x <- .008
x <- .1
x*(10 / (6^2)) + log(.8) - (10^2 / (2*(6^2)))

y = 0
0 / (6^2)
log(.2) - (0^2 / (2*(6^2)))
x <- 0.009
x <- -.1
x <- .009
x <- .008
x <- .1
x*(0 / (6^2)) + log(.2) - (0^2 / (2*(6^2)))


#/////////////////


# webwork - problem 4
dividends <- read_csv(file = "Dividends.csv")
dividends

lda_roc_1 <- roc(response = dividends %>% pull(y), predictor = dividends %>% pull(x))
lda_roc_1
plot.roc(lda_roc_1)

# run lda
lda_fit <- lda(formula = y ~ x, data = dividends)
lda_fit

# posterior probability 
lda_prob <- predict(object = lda_fit, data = dividends)$posterior[ , 2]
lda_prob %>% as_tibble()

# posterior probability way of ROC curve for LDA
lda_roc_2 <- roc(response = dividends %>% pull(y), predictor = lda_prob)
lda_roc_2
plot.roc(lda_roc_2)  # exact same curve as above

# get class predictions
lda_pred <- predict(object = lda_fit, data = dividends)$class
lda_pred

confusionMatrix(data = factor(lda_pred, levels = c(0, 1)), 
                reference = factor(dividends %>% pull(y), levels = c(0, 1)),
                positive = "1")
table(dividends %>% pull(y), lda_pred)
# accuracy
(107 + 762) / (107 + 93 + 38 + 762)
# TPR / sensitivity
762 / (762 + 38)
# TNR / specificity
107 / (107 + 93)


lda_prob <- predict(object = lda_fit, data = dividends)$posterior[ , 2]
lda_roc_3 <- roc(response = dividends %>% pull(y), predictor = lda_prob)
plot(lda_roc_3)


#//////////////////////////


heart <- read_csv(file = "Heart_disease_Cleveland.csv") %>%
        dplyr::select(Age, BloodPressure, Chol, MaxHeartRate, STdepress, DiseaseStatus)
heart %>% glimpse()
heart %>% count(DiseaseStatus)

# lda_model_1
lda_model_1 <- lda(formula = DiseaseStatus ~ MaxHeartRate + STdepress, data = heart)
lda_model_1

# get class predictions
lda_model_1_pred <- predict(object = lda_model_1, newdata = heart)$class
lda_model_1_pred

# number of parameters estimated
# (k + (p/2)) * (p + 1)
k <- 5
p <- 2
(k + (p/2)) * (p + 1)

# get overall accuracy
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = lda_model_1_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n())

# get accuracy by class
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = lda_model_1_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        group_by(truth) %>% summarize(n = n(),
                                      n_correct = sum(pred_correct),
                                      accuracy = sum(pred_correct) / n())

# predict individual
predict(object = lda_model_1, newdata = tibble(MaxHeartRate = 130, STdepress = 2))$class


#//////////////


# lda_model_2
lda_model_2 <- lda(formula = DiseaseStatus ~ ., data = heart)
lda_model_2

# get class predictions
lda_model_2_pred <- predict(object = lda_model_2, newdata = heart)$class
lda_model_2_pred

# get overall accuracy
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = lda_model_2_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n())

# get accuracy by class
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = lda_model_2_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        group_by(truth) %>% summarize(n = n(),
                                      n_correct = sum(pred_correct),
                                      accuracy = sum(pred_correct) / n())

# number of parameters estimated
# (k + (p/2)) * (p + 1)
k <- 5
p <- 5
(k + (p/2)) * (p + 1)


#////////////////


# cross-validation for model selection
n <- heart %>% nrow()
n
ngroups <- 10 # using 10-fold cross-validation
groups <- rep(1:ngroups, length.out = n)
groups

RNGkind(sample.kind = "Rejection")
set.seed(4)
cvgroups <- sample(groups, size = n)
cvgroups


#/////////////


# get lda_model_1_cv_pred 
lda_model_1_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- heart[!groupii, ] 
        test_set <- heart[groupii, ] 
        
        # train model
        lda_model_1 <- lda(formula = DiseaseStatus ~ MaxHeartRate + STdepress, data = train_set)
        
        # get class predictions
        lda_model_1_pred <- predict(object = lda_model_1, newdata = test_set)$class
        lda_model_1_cv_pred[groupii] <- lda_model_1_pred %>% as.character() %>% as.numeric()
}

# get error rate
tibble(truth = heart %>% pull(DiseaseStatus), pred = lda_model_1_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())


#////////////////


# get lda_model_2_cv_pred 
lda_model_2_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- heart[!groupii, ] 
        test_set <- heart[groupii, ] 
        
        # train model
        lda_model_2 <- lda(formula = DiseaseStatus ~ ., data = train_set)

        # get class predictions
        lda_model_2_pred <- predict(object = lda_model_2, newdata = test_set)$class
        lda_model_2_cv_pred[groupii] <- lda_model_2_pred %>% as.character() %>% as.numeric()
}

# get error rate
tibble(truth = heart %>% pull(DiseaseStatus), pred = lda_model_2_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())


#/////////////////


# get qda_model_3
qda_model_3 <- qda(formula = DiseaseStatus ~ MaxHeartRate + STdepress, data = heart)
qda_model_3

# check variability of STdepress by DiseaseStatus
heart %>% count(DiseaseStatus)
heart %>% group_by(DiseaseStatus) %>% mutate(STdepress_sd = sd(STdepress)) %>% 
        ungroup() %>% distinct(DiseaseStatus, STdepress_sd)

# get class predictions
qda_model_3_pred <- predict(object = qda_model_3, newdata = heart)$class
qda_model_3_pred

# number of parameters estimated
# k(p + 1) * ((p / 2) + 1)
k <- 5
p <- 2
(k * (p + 1)) * ((p / 2) + 1)

# get overall accuracy
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = qda_model_3_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n())

# get accuracy by class
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = qda_model_3_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        group_by(truth) %>% summarize(n = n(),
                                      n_correct = sum(pred_correct),
                                      accuracy = sum(pred_correct) / n())

# predict individual
predict(object = qda_model_3, newdata = tibble(MaxHeartRate = 130, STdepress = 2))$class


#//////////////////////////


# get qda_model_4
qda_model_4 <- qda(formula = DiseaseStatus ~ ., data = heart)
qda_model_4

# get class predictions
qda_model_4_pred <- predict(object = qda_model_4, newdata = heart)$class
qda_model_4_pred

# number of parameters estimated
# k(p + 1) * ((p / 2) + 1)
k <- 5
p <- 5
(k * (p + 1)) * ((p / 2) + 1)

# get overall accuracy
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = qda_model_4_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n())

# get accuracy by class
tibble(truth = heart %>% pull(DiseaseStatus), 
       pred = qda_model_4_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        group_by(truth) %>% summarize(n = n(),
                                      n_correct = sum(pred_correct),
                                      accuracy = sum(pred_correct) / n())

# predict individual
predict(object = lda_model_1, newdata = tibble(MaxHeartRate = 130, STdepress = 2))$class


#/////////////


# get qda_model_3_cv_pred 
qda_model_3_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- heart[!groupii, ] 
        test_set <- heart[groupii, ] 
        
        # train model
        qda_model_3 <- qda(formula = DiseaseStatus ~ MaxHeartRate + STdepress, data = train_set)
        
        # get class predictions
        qda_model_3_pred <- predict(object = qda_model_3, newdata = test_set)$class
        qda_model_3_cv_pred[groupii] <- qda_model_3_pred %>% as.character() %>% as.numeric()
}

# get error rate
tibble(truth = heart %>% pull(DiseaseStatus), pred = qda_model_3_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())


#////////////////


# get qda_model_4_cv_pred 
qda_model_4_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- heart[!groupii, ] 
        test_set <- heart[groupii, ] 
        
        # train model
        qda_model_4 <- qda(formula = DiseaseStatus ~ ., data = train_set)
        
        # get class predictions
        qda_model_4_pred <- predict(object = qda_model_4, newdata = test_set)$class
        qda_model_4_cv_pred[groupii] <- qda_model_4_pred %>% as.character() %>% as.numeric()
}

# get error rate
tibble(truth = heart %>% pull(DiseaseStatus), pred = qda_model_4_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())


#///////////////////////////////////////////////////////////////////////////////////


# lesson 4 homework


auto %>% ggplot(data = ., mapping = aes(x = factor(Domestic), y = mpg)) + geom_boxplot()
auto %>% ggplot(data = ., mapping = aes(x = mpg, color = factor(Domestic))) + geom_density()

# test normality
mpg_when_domestic_1 <- auto %>% filter(Domestic == 1) %>% pull(mpg)
mpg_when_domestic_0 <- auto %>% filter(Domestic == 0) %>% pull(mpg)

shapiro.test(mpg_when_domestic_1)
shapiro.test(mpg_when_domestic_0)

# test sd
bartlett.test(x = auto %>% pull(mpg), g = auto %>% pull(Domestic))
auto %>% group_by(Domestic) %>% summarize(mpg_sd = sd(mpg))

# lda_model_1
lda_model_1 <- lda(formula = Domestic ~ mpg, data = auto)
lda_model_1

# get class predictions
lda_model_1_pred <- predict(object = lda_model_1, newdata = auto)$class
lda_model_1_pred

# get posterior probabilities
lda_model_1_pred_prob <- predict(object = lda_model_1, newdata = auto)$posterior[ , 2]
lda_model_1_pred_prob

# get overall accuracy
tibble(truth = auto %>% pull(Domestic), 
       pred = lda_model_1_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n())

# confusion matrix
table(auto %>% pull(Domestic), lda_model_1_pred)
confusionMatrix(data = factor(lda_model_1_pred, levels = c(0, 1)), 
                reference = factor(auto %>% pull(Domestic), levels = c(0, 1)),
                positive = "1")

# get sensitivity
204 / (204 + 41)

# get specificity
90 / (90 + 57)

# get roc and auc
lda_roc_1 <- roc(response = auto %>% pull(Domestic),  predictor = lda_model_1_pred_prob, 
                 direction = "<", levels = c(0, 1),)
plot(lda_roc_1)
auc(lda_roc_1)


#/////////////////


# homework 4 question 9
auto %>% dplyr::select(-name) %>% ggpairs()
auto %>% count(displacement)
auto %>% ggplot(data = ., mapping = aes(x = factor(Domestic), y = displacement)) + geom_boxplot()
auto %>% ggplot(data = ., mapping = aes(x = displacement, color = factor(Domestic))) + geom_density()
auto %>% ggplot(data = ., mapping = aes(x = mpg, color = factor(Domestic))) + geom_density()


# test normality
displacement_when_domestic_1 <- auto %>% filter(Domestic == 1) %>% pull(displacement)
displacement_when_domestic_0 <- auto %>% filter(Domestic == 0) %>% pull(displacement)

shapiro.test(displacement_when_domestic_1)
shapiro.test(displacement_when_domestic_0)

# test sd
bartlett.test(x = auto %>% pull(displacement), g = auto %>% pull(Domestic))
auto %>% group_by(Domestic) %>% summarize(displacement_sd = sd(displacement))

# qda_model_1
qda_model_1 <- qda(formula = Domestic ~ displacement, data = auto)
qda_model_1

# get class predictions
qda_model_1_pred <- predict(object = qda_model_1, newdata = auto)$class
qda_model_1_pred <- as.numeric(as.character(qda_model_1_pred ))
qda_model_1_pred 

# get posterior class probabilities
qda_model_1_pred_prob <- predict(object = qda_model_1, newdata = auto)$posterior[ , 2]
qda_model_1_pred_prob

# get overall accuracy
tibble(truth = auto %>% pull(Domestic), 
       pred = qda_model_1_pred) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n())

# confusion matrix
table(auto %>% pull(Domestic), qda_model_1_pred)
confusionMatrix(data = factor(qda_model_1_pred, levels = c(0, 1)), 
                reference = factor(auto %>% pull(Domestic), levels = c(0, 1)),
                positive = "1")

# get roc and auc
qda_roc_1 <- roc(response = auto %>% pull(Domestic),  predictor = qda_model_1_pred_prob, 
                 direction = "<", levels = c(0, 1),)
plot(qda_roc_1)
auc(qda_roc_1)


#////////////////////


# problem 2

auto <- Auto %>% mutate(origin = factor(origin))
auto %>% glimpse()
auto %>% count(origin)

# plot
auto %>% ggplot(data = ., mapping = aes(x = displacement, y = mpg, color = origin)) + geom_point()
auto %>% dplyr::select(-name) %>% ggpairs(data = ., mapping = aes(color = origin))


# test normality
# mpg_when_domestic_1 <- auto %>% filter(Domestic == 1) %>% pull(mpg)
# mpg_when_domestic_0 <- auto %>% filter(Domestic == 0) %>% pull(mpg)
# 
# shapiro.test(mpg_when_domestic_1)
# shapiro.test(mpg_when_domestic_0)

# test sd
# bartlett.test(x = auto %>% pull(mpg), g = auto %>% pull(Domestic))
# auto %>% group_by(Domestic) %>% summarize(mpg_sd = sd(mpg))

# lda_model_2
lda_model_2 <- lda(formula = origin ~ mpg + cylinders + displacement + horsepower + weight, data = auto)
lda_model_2

# get class predictions
lda_model_2_pred_class <- predict(object = lda_model_2, newdata = auto)$class
lda_model_2_pred_class

# get posterior probabilities
lda_model_2_pred_prob <- predict(object = lda_model_2, newdata = auto)$posterior[ , 2]
lda_model_2_pred_prob

# get overall accuracy
tibble(truth = auto %>% pull(origin), 
       pred = lda_model_2_pred_class) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = (n - n_correct) / n())

# get accuracy by origin class
tibble(truth = auto %>% pull(origin), 
       pred = lda_model_2_pred_class) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               origin = case_when(truth == 1 ~ "American",
                                  truth == 2 ~ "European",
                                  truth == 3 ~ "Japanese")) %>%
        group_by(origin, truth) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = (n - n_correct) / n())

# confusion matrix
?Auto
table(auto %>% pull(origin), lda_model_2_pred_class)
# confusionMatrix(data = factor(lda_model_2_pred, levels = c(0, 1)), 
#                 reference = factor(auto %>% pull(Domestic), levels = c(0, 1)),
#                 positive = "1")


# get roc and auc
# lda_roc_1 <- roc(response = auto %>% pull(Domestic),  predictor = lda_model_2_pred_prob, 
#                  direction = "<", levels = c(0, 1),)
# plot(lda_roc_1)
# auc(lda_roc_1)


#//////////////////


auto <- Auto %>% mutate(origin = factor(origin))
auto %>% glimpse()
auto %>% count(origin)

# plot
auto %>% ggplot(data = ., mapping = aes(x = displacement, y = mpg, color = origin)) + geom_point()
auto %>% dplyr::select(-name) %>% ggpairs(data = ., mapping = aes(color = origin))


# test normality
# mpg_when_domestic_1 <- auto %>% filter(Domestic == 1) %>% pull(mpg)
# mpg_when_domestic_0 <- auto %>% filter(Domestic == 0) %>% pull(mpg)
# 
# shapiro.test(mpg_when_domestic_1)
# shapiro.test(mpg_when_domestic_0)

# test sd
# bartlett.test(x = auto %>% pull(mpg), g = auto %>% pull(Domestic))
# auto %>% group_by(Domestic) %>% summarize(mpg_sd = sd(mpg))

# qda_model_2
qda_model_2 <- qda(formula = origin ~ mpg + cylinders + displacement + horsepower + weight, data = auto)
qda_model_2

# get class predictions
qda_model_2_pred_class <- predict(object = qda_model_2, newdata = auto)$class
qda_model_2_pred_class

# get posterior probabilities
qda_model_2_pred_prob <- predict(object = qda_model_2, newdata = auto)$posterior[ , 2]
qda_model_2_pred_prob

# get overall accuracy
tibble(truth = auto %>% pull(origin), 
       pred = qda_model_2_pred_class) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = (n - n_correct) / n())

# get accuracy by origin class
tibble(truth = auto %>% pull(origin), 
       pred = qda_model_2_pred_class) %>% 
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               origin = case_when(truth == 1 ~ "American",
                                  truth == 2 ~ "European",
                                  truth == 3 ~ "Japanese")) %>%
        group_by(origin, truth) %>%
        summarize(n = n(),
                  n_correct = sum(pred_correct),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = (n - n_correct) / n())

# confusion matrix
?Auto
table(auto %>% pull(origin), qda_model_2_pred_class)
# confusionMatrix(data = factor(qda_model_2_pred, levels = c(0, 1)), 
#                 reference = factor(auto %>% pull(Domestic), levels = c(0, 1)),
#                 positive = "1")


# get roc and auc
# lda_roc_1 <- roc(response = auto %>% pull(Domestic),  predictor = qda_model_2_pred_prob, 
#                  direction = "<", levels = c(0, 1),)
# plot(lda_roc_1)
# auc(lda_roc_1)

# predict individual
predict(object = qda_model_2, 
        newdata = tibble(mpg = 20, cylinders = 8, displacement = 320, horsepower = 280, weight = 3600))$class



#///////////////////////////////////////////////////////////////////////////////////////


# homework 4 - problem 3 ####

# cross-validation for model selection
n <- auto %>% nrow()
n
ngroups <- 10 # using 10-fold cross-validation
groups <- rep(1:ngroups, length.out = n)
groups

RNGkind(sample.kind = "Rejection")
set.seed(4)
cvgroups <- sample(groups, size = n)
cvgroups


#/////////////


# get lda_model_1_cv_pred 
lda_model_1_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- auto[!groupii, ] 
        test_set <- auto[groupii, ] 
        
        # train model
        lda_model_1 <- lda(formula = origin ~ displacement, data = train_set)
        
        # get class predictions
        lda_model_1_pred <- predict(object = lda_model_1, newdata = test_set)$class
        lda_model_1_cv_pred[groupii] <- lda_model_1_pred %>% as.character() %>% as.numeric()
}

# get error rate
lda_model_1_cv_pred_summary <- tibble(truth = auto %>% pull(origin), pred = lda_model_1_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())
lda_model_1_cv_pred_summary


#////////////////


# get lda_model_2_cv_pred 
lda_model_2_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- auto[!groupii, ] 
        test_set <- auto[groupii, ] 
        
        # train model
        lda_model_2 <- lda(formula = origin ~ mpg + cylinders + displacement + horsepower + weight, data = train_set)
        
        # get class predictions
        lda_model_2_pred <- predict(object = lda_model_2, newdata = test_set)$class
        lda_model_2_cv_pred[groupii] <- lda_model_2_pred %>% as.character() %>% as.numeric()
}

# get error rate
lda_model_2_cv_pred_summary <- tibble(truth = auto %>% pull(origin), pred = lda_model_2_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())
lda_model_2_cv_pred_summary


#/////////////////


# get qda_model_3_cv_pred 
qda_model_3_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- auto[!groupii, ] 
        test_set <- auto[groupii, ] 
        
        # train model
        qda_model_3 <- qda(formula = origin ~ displacement, data = train_set)
        
        # get class predictions
        qda_model_3_pred <- predict(object = qda_model_3, newdata = test_set)$class
        qda_model_3_cv_pred[groupii] <- qda_model_3_pred %>% as.character() %>% as.numeric()
}

# get error rate
qda_model_3_cv_pred_summary <- tibble(truth = auto %>% pull(origin), pred = qda_model_3_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())
qda_model_3_cv_pred_summary


#////////////////


# get qda_model_4_cv_pred 
qda_model_4_cv_pred <- numeric(length = n)

for(ii in 1:ngroups) {
        
        groupii <- (cvgroups == ii)
        train_set <- auto[!groupii, ] 
        test_set <- auto[groupii, ] 
        
        # train model
        qda_model_4 <- qda(formula = origin ~ mpg + cylinders + displacement + horsepower + weight, data = train_set)
        
        # get class predictions
        qda_model_4_pred <- predict(object = qda_model_4, newdata = test_set)$class
        qda_model_4_cv_pred[groupii] <- qda_model_4_pred %>% as.character() %>% as.numeric()
}

# get error rate
qda_model_4_cv_pred_summary <- tibble(truth = auto %>% pull(origin), pred = qda_model_4_cv_pred) %>%
        mutate(pred_correct = case_when(truth == pred ~ 1, TRUE ~ 0),
               pred_incorrect = case_when(truth != pred ~ 1, TRUE ~ 0)) %>%
        summarize(n = n(),
                  sum_pred_correct = sum(pred_correct),
                  sum_pred_incorrect = sum(pred_incorrect),
                  accuracy = sum(pred_correct) / n(),
                  error_rate = sum_pred_incorrect / n())
qda_model_4_cv_pred_summary


#//////////////////


# lda_model_4 parameters
# number of parameters estimated
# (k + (p/2)) * (p + 1)
k <- 3
p <- 5
(k + (p/2)) * (p + 1)

# qda_model_4 parameters
# number of parameters estimated
# k(p + 1) * ((p / 2) + 1)
k <- 3
p <- 5
(k * (p + 1)) * ((p / 2) + 1)


#////////////////////////


# test assumptions 


#////////////////////


# test asumptions for mpg

auto %>% ggplot(data = ., mapping = aes(x = mpg, color = origin)) + geom_density()

# test normality
mpg_when_origin_1 <- auto %>% filter(origin == 1) %>% pull(mpg)
mpg_when_origin_2 <- auto %>% filter(origin == 2) %>% pull(mpg)
mpg_when_origin_3 <- auto %>% filter(origin == 3) %>% pull(mpg)

shapiro.test(mpg_when_origin_1)
shapiro.test(mpg_when_origin_2)
shapiro.test(mpg_when_origin_3)

# test sd
bartlett.test(x = auto %>% pull(mpg), g = auto %>% pull(origin))


#////////////////////


# test asumptions for cylinders

auto %>% ggplot(data = ., mapping = aes(x = cylinders, color = origin)) + geom_density()

# test normality
cylinders_when_origin_1 <- auto %>% filter(origin == 1) %>% pull(cylinders)
cylinders_when_origin_2 <- auto %>% filter(origin == 2) %>% pull(cylinders)
cylinders_when_origin_3 <- auto %>% filter(origin == 3) %>% pull(cylinders)

shapiro.test(cylinders_when_origin_1)
shapiro.test(cylinders_when_origin_2)
shapiro.test(cylinders_when_origin_3)

# test sd
bartlett.test(x = auto %>% pull(cylinders), g = auto %>% pull(origin))


#////////////////////


# test asumptions for displacement

auto %>% ggplot(data = ., mapping = aes(x = displacement, color = origin)) + geom_density()

# test normality
displacement_when_origin_1 <- auto %>% filter(origin == 1) %>% pull(displacement)
displacement_when_origin_2 <- auto %>% filter(origin == 2) %>% pull(displacement)
displacement_when_origin_3 <- auto %>% filter(origin == 3) %>% pull(displacement)

shapiro.test(displacement_when_origin_1)
shapiro.test(displacement_when_origin_2)
shapiro.test(displacement_when_origin_3)

# test sd
bartlett.test(x = auto %>% pull(displacement), g = auto %>% pull(origin))


#////////////////////


# test asumptions for horsepower

auto %>% ggplot(data = ., mapping = aes(x = horsepower, color = origin)) + geom_density()

# test normality
horsepower_when_origin_1 <- auto %>% filter(origin == 1) %>% pull(horsepower)
horsepower_when_origin_2 <- auto %>% filter(origin == 2) %>% pull(horsepower)
horsepower_when_origin_3 <- auto %>% filter(origin == 3) %>% pull(horsepower)

shapiro.test(horsepower_when_origin_1)
shapiro.test(horsepower_when_origin_2)
shapiro.test(horsepower_when_origin_3)

# test sd
bartlett.test(x = auto %>% pull(horsepower), g = auto %>% pull(origin))


#////////////////////


# test asumptions for weight

auto %>% ggplot(data = ., mapping = aes(x = weight, color = origin)) + geom_density()

# test normality
weight_when_origin_1 <- auto %>% filter(origin == 1) %>% pull(weight)
weight_when_origin_2 <- auto %>% filter(origin == 2) %>% pull(weight)
weight_when_origin_3 <- auto %>% filter(origin == 3) %>% pull(weight)

shapiro.test(weight_when_origin_1)
shapiro.test(weight_when_origin_2)
shapiro.test(weight_when_origin_3)

# test sd
bartlett.test(x = auto %>% pull(weight), g = auto %>% pull(origin))
