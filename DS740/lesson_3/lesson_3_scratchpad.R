library(tidyverse)
library(pROC)

# setwd
setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_3")


#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////


# webwork problem 8
bad_regression <- read_csv(file = "BadRegression.csv")
bad_regression
bad_regression %>% glimpse()


# get roc
roc_1 <- roc(response = bad_regression$y, 
             predictor = bad_regression$predictvals)
roc_1
plot.roc(roc_1)

# boxplot
bad_regression %>% ggplot(data = ., mapping = aes(x = factor(y), y = predictvals)) + geom_boxplot()

# get roc after specifying direction of comparison for threshold prob to predicted prob
# this forces roc() to associate higher predicted probs with y values of 1 (aka "positives")
# roc() uses the direction like: if threshold prob is "<" predicted prob, then classify as a "positive"
roc_2 <- roc(response = bad_regression$y, 
             predictor = bad_regression$predictvals,
             direction = "<")
roc_2
plot.roc(roc_2)


#///////////////////////////////////////////////////////////////////////////


# webwork problem 9
heart <- read_csv(file = "Heart_disease_Cleveland.csv")
heart
heart %>% glimpse()

heart %>% count(Sex)

# set factors
heart <- heart %>% 
        mutate(Sex = factor(Sex),
               ChestPain = factor(ChestPain),
               HighBloodSugar = factor(HighBloodSugar),
               ECG = factor(ECG),
               ExerAngina = factor(ExerAngina),
               Slope = factor(Slope),
               Thal = factor(Thal),
               HD = case_when(DiseaseStatus == 0 ~ 0,
                              TRUE ~ 1))

heart %>% glimpse()

# run logistic regression model
logistic_fit <- glm(formula = HD ~ ., data = heart, family = "binomial")
logistic_fit


model_1 <- glm(formula = HD ~ . - DiseaseStatus, data = heart, family = "binomial")
model_1

model_2 <- glm(formula = HD ~ BloodPressure + Chol + Thal, data = heart, family = "binomial")
model_2



fit <- lm(formula = STdepress ~ . - HD, data = heart)
fit

par(mfrow = c(2, 2))
plot(fit) # where fit is what you called the regression model

heart %>% ggplot(data = ., mapping = aes(x = STdepress)) + geom_boxplot()
heart %>% ggplot(data = ., mapping = aes(x = STdepress)) + geom_histogram()
heart %>% ggplot(data = ., mapping = aes(x = STdepress)) + geom_density()


fit2 <- lm(log(STdepress + 1) ~ . - HD, data = heart)
fit2
par(mfrow = c(2, 2))
plot(fit2) # where fit is what you called the regression model


AIC(fit, fit2)



#//////////////////////////////////////////////////////////////////////

MSE_overall <- apply(group_error, 2, mean)
MSE_overall
tibble(mse_avg = MSE_overall, num_predictors = seq(from = 1, to = 39, by = 1)) %>%
        ggplot(data = ., mapping = aes(x = num_predictors, y = mse_avg)) + geom_line()
low_MSE_model = which.min(MSE_overall)
low_MSE_model

std_err = apply(group_error, 2, sd)/sqrt(ngroups)
std_err[low_MSE_model]
which(MSE_overall <= MSE_overall[low_MSE_model] +
              std_err[low_MSE_model])

group_error_tbl <- group_error %>% as_tibble() %>% mutate(fold = row_number()) %>% 
        relocate(fold, .before = everything()) %>%
        rename_with(.cols = -fold, .fn = ~ str_replace(string = .x, pattern = "V", replacement = "")) %>%
        pivot_longer(cols = -fold, names_to = "num_predictors", values_to = "mse") %>%
        mutate(num_predictors = as.numeric(num_predictors)) %>%
        group_by(num_predictors) %>% 
        mutate(avg_mse = mean(mse),
               sd_mse = sd(mse) / sqrt(ngroups)) %>%
        ungroup() %>%
        mutate(min_avg_mse = min(avg_mse),
               best_num_predictors_flag = case_when(avg_mse == min_avg_mse ~ 1, TRUE ~ 0),
               sd_mse_for_best_num_predictors = case_when(best_num_predictors_flag == 1 ~ sd_mse, TRUE ~ NA_real_)) %>%
        fill(sd_mse_for_best_num_predictors, .direction = "updown") %>%
        mutate(one_sd_above_min_avg_mse = min_avg_mse + sd_mse_for_best_num_predictors,
               among_best_model_given_num_predictors_flag = case_when(avg_mse < one_sd_above_min_avg_mse ~ 1,
                                                 TRUE ~ 0)) %>%
        ungroup() %>% arrange(num_predictors)

group_error_tbl %>% print(n = 20)
group_error_tbl %>% distinct(num_predictors, avg_mse, sd_mse) %>% arrange(avg_mse)
group_error_tbl %>% filter(among_best_model_given_num_predictors_flag == 1) %>% arrange(num_predictors)       
group_error_tbl %>% distinct(num_predictors, avg_mse, sd_mse, min_avg_mse, best_num_predictors_flag,
                             sd_mse_for_best_num_predictors, one_sd_above_min_avg_mse,
                             among_best_model_given_num_predictors_flag) %>% arrange(avg_mse) %>% 
        print(n = nrow(.))



#//////////////////////////////////////


reg_subsets <- regsubsets(PERNP_log ~ . - PERNP - WKHP, data = wisconsin,
                      method = "exhaustive", nvmax = 4)
reg_subsets_summary <- summary(reg_subsets)

# inspect
reg_subsets
reg_subsets_summary %>% attributes()
reg_subsets_summary$bic
reg_subsets_summary$adjr2

plot(reg_subsets)
plot(reg_subsets, scale = "adjr2")

# get best model
which.min(reg_subsets_summary$adjr2)
which.min(reg_subsets_summary$bic)

reg_subsets_summary$which[1,] %>% enframe() %>% filter(value == TRUE)
reg_subsets_summary$which[19,] %>% enframe() %>% filter(value == TRUE) %>% print(n = nrow(.))
reg_subsets_summary$which[4,] %>% enframe() %>% filter(value == TRUE)

model_w_4_predictors <- wisconsin %>% 
        mutate(COW6 = case_when(COW == 6 ~ 1, TRUE ~ 0)) %>%
        lm(formula = PERNP_log ~ COW6 + SEX + Education + WKHP_log, data = .)
summary(model_w_4_predictors)



#////////////////////////////////////


auto %>% select(where(is.numeric)) %>% correlate()
auto %>% glimpse()


model_1 <- glm(gas_mileage_fct ~ . - name - mpg - mpg_median, data = auto, family = "binomial")
summary(model_1)

vif(model_1)


#///////////
 

n <- auto %>% nrow()
ngroups <- 10 # using 10-fold cross-validation
groups <- rep(1:ngroups, length.out = n)

RNGkind(sample.kind = "Rejection")
set.seed(3)
cvgroups <- sample(groups, size = n)
all_predicted = numeric(length = n)

# convert gas_mileage_fct to numeric
auto %>% pull(gas_mileage_fct) %>% levels()
auto2 <- auto %>%
        mutate(gas_mileage_fct = case_when(gas_mileage_fct == "high" ~ 1, 
                                           gas_mileage_fct == "low" ~ 0))
auto2 %>% glimpse()

for(ii in 1:ngroups){
        groupii <- (cvgroups == ii)
        train_set <- auto2[!groupii, ] %>% select(-name)
        test_set <- auto2[groupii, ] %>% select(-name)
        
        model_fit <- glm(gas_mileage_fct ~ . - mpg - mpg_median, data = train_set, family = "binomial")
        predicted <- predict(model_fit, newdata = test_set, type = "response")
        all_predicted[groupii] <- predicted
}

all_predicted
roc_logistic <- roc(response = auto2$gas_mileage_fct, predictor = all_predicted, direction = "<")
roc_logistic
roc_logistic %>% attributes()
plot.roc(roc_logistic)
auc(roc_logistic)


#//////////////


auto_knn_roc <- read_csv(file = "Homework_3_KNN.csv") %>% mutate(model = "knn")
auto_knn_roc

tibble(sensitivities = roc_logistic$sensitivities,
                specificities = roc_logistic$specificities,
                thresholds = roc_logistic$thresholds) %>%
        mutate(model = "logistic") %>%
        bind_rows(., auto_knn_roc) %>%
        mutate(specificities = specificities * -1) %>%
        ggplot(data = ., mapping = aes(x = specificities, y = sensitivities, color = model)) + geom_line() +
        scale_x_continuous(labels = seq(from = 1, to = 0, by = -.25))


#//////////////////////


RNGkind(sample.kind = "Rejection")
set.seed(3)
cvgroups <- sample(groups, size = n)
all_predicted_knn = numeric(length = n)

# Convert gas_mileage_fct to numeric
auto %>% pull(gas_mileage_fct) %>% levels()
auto2 <- auto %>%
        mutate(gas_mileage_fct = case_when(gas_mileage_fct == "high" ~ 1, 
                                           gas_mileage_fct == "low" ~ 0),
               origin_1 = case_when(origin == 1 ~ 1, TRUE ~ 0),
               origin_2 = case_when(origin == 2 ~ 1, TRUE ~ 0),
               origin_3 = case_when(origin == 3 ~ 1, TRUE ~ 0)) %>%
        select(-c(origin, name, mpg, mpg_median))
auto2 %>% glimpse()

for(ii in 1:ngroups){
        groupii <- (cvgroups == ii)
        train_set <- auto2[!groupii, ] %>% select(-name)
        test_set <- auto2[groupii, ] %>% select(-name)
        
        # Scale balance in both sets
        train_std <- train_set %>% select(-gas_mileage_fct) %>% scale(.)

        test_std = test_set %>% select(-gas_mileage_fct) %>% 
                scale(., center = attr(train_std, "scaled:center"),
                                 scale = attr(train_std, "scaled:scale"))

        predicted <- knn(train = train_std, 
                        test = test_std,
                        cl = train_set$gas_mileage_fct,
                        k = 49,
                        prob = TRUE) # Set prob = TRUE to extract predicted probabilities
        
        prob <- attr(predicted, "prob")
        
        # Prob contains the probability of belonging to the predicted class.
        # We want to convert this into the probability of defaulting.
        prob_of_default <- ifelse(predicted == "No", 1-prob, prob)
        all_predicted_knn[groupii] <- prob_of_default
}

