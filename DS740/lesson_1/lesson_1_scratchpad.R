library(tidyverse)
library(testthat)
library(ISLR)
library(FNN)
library(skimr)
library(caret)
library(MASS)
library(ggformula)

# DS740 - lesson 1


#/////////////////////////////////////////////////////////////////////////////////////


# view data
default_tbl <- Default %>% as_tibble() %>% 
        mutate(student_flag = case_when(student == "Yes" ~ 1,
                                        TRUE ~ 0))


#//////////////////////


# inspect
default_tbl
default_tbl %>% glimpse()
default_tbl %>% nrow() # 10000
default_tbl %>% ncol() # 5


default_tbl %>% skim()
default_tbl %>% count(student, student_flag)


#/////////////////////////////////////////////////////////////////////////////////////


# get training/test set
set.seed(123)
in_train <- createDataPartition(y = default_tbl$default, times = 1, p = .66, list = FALSE)

default_train <- default_tbl %>% mutate(row_number = row_number()) %>%
        filter(row_number %in% in_train) 
default_test <- default_tbl %>% mutate(row_number = row_number()) %>%
        filter(!(row_number %in% in_train))


# ds740 method for getting in_train
set.seed(123)
groups = c(rep(1, 6666), rep(2, 3334)) # 1 represents the training set
random_groups = sample(groups, 10000)
in_train_alt = (random_groups == 1)

default_train <- default_tbl %>% filter(in_train_alt == TRUE)
default_test <- default_tbl %>% filter(in_train_alt == FALSE)


#////////////////////


# inspect
default_train
default_train %>% glimpse()
default_train %>% nrow() # 6601
default_train %>% ncol() # 6

default_train %>% count(default)

default_test
default_test %>% glimpse()
default_test %>% nrow() # 3399
default_test %>% ncol() # 6

in_train_alt %>% length()


#/////////////////////////////////////////////////////////////////////////////////////


# center and scale
# note that to ensure compatibility, they recommend using mean/sd from training data to center/scale testing data
# so that a value of 0 represents the same amount in standardized training and testing data
# note that converting dataframe output from scale() to a tibble loses its attributes including scaled:center/scale,
# so the scale() attributes are extracted and then default_train_std is converted to tibble
# note that variables are run through as.vector to get rid of lingering attributes, which mess up tests
default_train_std = scale(default_train %>% select(balance, income), center = TRUE, scale = TRUE)
default_train_std_mean <- attr(default_train_std, "scaled:center")
default_train_std_sd <- attr(default_train_std, "scaled:scale")
default_train_std <- default_train_std %>% as_tibble() %>%
        rename(balance_std = balance,
               income_std = income) %>%
        mutate(across(.cols = everything(), .fns = ~ as.vector(.x))) %>%
        bind_cols(default_train %>% as_tibble(), .)


default_test_std <- scale(default_test %>% select(balance, income), 
                        center = default_train_std_mean %>% as_tibble() %>% pull(value),
                        scale = default_train_std_sd %>% as_tibble() %>% pull(value)) %>%
        as_tibble() %>%
        rename(balance_std = balance,
               income_std = income) %>%
        mutate(across(.cols = everything(), .fns = ~ as.vector(.x))) %>%
        bind_cols(default_test %>% as_tibble(), .)


#//////////////////


# note that you can call scale() directly on variables in mutate()
starwars %>% select(height, mass) 

# get height_std and mass_std outside of a tibble
height_std <- starwars %>% pull(height) %>% scale(.)
height_std
mass_std <- starwars %>% pull(mass) %>% scale(.)
mass_std

# create scale_in_tbl()
scale_in_tbl <- function(x, center = TRUE, scale = TRUE) {
    
    
}

# get height_std_in_tbl and weight_std_in_tbl and compare
starwars %>%
    select(name, height, mass) %>%
    mutate(across(.cols = c(height, mass), .fns = ~ scale(x = .x, center = TRUE, scale = TRUE), 
                  .names = "{.col}_std_in_tbl"))
    mutate(height_std = height_std,
           equal_flag = case_when(is.na(height_std_in_tbl) & is.na(height_std) ~ 1,
                                  height_std_in_tbl == height_std ~ 1,
                                  TRUE ~ 0)) %>%
    # filter(equal_flag == 0)
    count(equal_flag)

#/////////////////


# inspect
default_train_std
default_train_std %>% glimpse()
default_train_std %>% nrow() # 6601
default_train_std %>% ncol() # 8
default_train_std_mean
default_train_std_sd

default_test_std
default_test_std %>% glimpse()
default_test_std %>% nrow() # 3399
default_test_std %>% ncol() # 8


#/////////////////


# test to confirm that scale() is just standardizing by subtracting mean and dividing by sd
test_train_test_standardization <- function() {

        # balance_std
        expect_equal(object = default_train_std %>% pull(balance_std),
                     expected = default_train %>% 
                             mutate(across(.cols = balance:income, .fns = ~ mean(.x), .names = "{.col}_mean")) %>%
                             mutate(across(.cols = balance:income, .fns = ~ sd(.x), .names = "{.col}_sd")) %>%
                             mutate(balance_std = (balance - balance_mean) / balance_sd,
                                    income_std = (income - income_mean) / income_sd) %>%
                             pull(balance_std))
        
        # income_std
        expect_equal(object = default_train_std %>% pull(income_std),
                     expected = default_train %>% 
                             mutate(across(.cols = balance:income, .fns = ~ mean(.x), .names = "{.col}_mean")) %>%
                             mutate(across(.cols = balance:income, .fns = ~ sd(.x), .names = "{.col}_sd")) %>%
                             mutate(balance_std = (balance - balance_mean) / balance_sd,
                                    income_std = (income - income_mean) / income_sd) %>%
                             pull(income_std))
}
test_train_test_standardization()

        
#/////////////////////////////////////////////////////////////////////////////////////


# predict
set.seed(123)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 
default_train_std_knn_model <- default_train_std %>% train(default ~ income_std + balance_std,
              data = .,
              method = "knn", trControl = train_control)
predictions <- predict(object = default_train_std_knn_model, 
                       newdata = default_test_std)


# using knn()
predictions_knn <- knn(train = default_train_std %>% select(balance_std, income_std),
    test = default_test_std %>% select(balance_std, income_std),
    cl = default_train_std %>% pull(default),
    k = 1)


#/////////////////


# inspect
default_train_std_knn_model
plot(default_train_std_knn_model)

predictions %>% head()
predictions %>% length() # 3399

predictions_knn %>% head()
predictions_knn %>% length() # 3399

# confusion matrix
confusionMatrix(data = predictions, reference = default_test_std %>% pull(default))
confusionMatrix(data = predictions_knn, reference = default_test_std %>% pull(default))


#/////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////


# webwork

# load boston data from MASS package

Boston
boston <- Boston %>% as_tibble()
boston %>% glimpse()
boston %>% nrow() # 506
boston %>% skim(crim, age, rad)
?Boston

boston %>% count()


# set.seed(123)
# groups = c(rep(1, 6666), rep(2, 3334)) # 1 represents the training set
# random_groups = sample(groups, 10000)
# in_train_alt = (random_groups == 1)
# 
# default_train <- default_tbl %>% filter(in_train_alt == TRUE)
# default_test <- default_tbl %>% filter(in_train_alt == FALSE)

options(scipen=999)

# note the instructor told me to use RNGkind because it seemed like the random number generator in my console
# had somehow gotten set to use the old (pre-R version 3.6) "rounding" method???
?RNGkind
RNGkind(sample.kind = "Rejection")
set.seed(100)
groups = c(rep(1, 350), rep(2, 156)) # 1 represents the training set
random_groups = sample(groups, 506)
in_train = (random_groups == 1)

x_train <- Boston %>%
        dplyr::select(c(age, rad)) %>%
        filter(in_train)
x_test <- Boston %>%
        dplyr::select(c(age, rad)) %>%
        filter(!in_train)

x_train %>% as_tibble() %>% summarize(rad_mean = mean(rad))

x_train = scale(x_train)
attr(x_train, "scaled:center")
attr(x_train, "scaled:scale")

x_test = scale(x_test, center = attr(x_train, "scaled:center"), 
               scale = attr(x_train, "scaled:scale"))


predictions = knn.reg(train = x_train, 
                      test  = x_test,
                      y = boston$crim[in_train],
                      k = 25)

mean( (predictions$pred - boston$crim[!in_train])^2 )


age_to_check = seq(20, 100, by = 1)
rad_to_check = c(seq(1, 8, by = 1), 24)
example_data = expand.grid(age_to_check, 
                           rad_to_check)
example_data <- example_data %>%
    rename(age = Var1,
           rad = Var2)


x_example = scale(example_data, 
                  center = attr(x_train, "scaled:center"),
                  scale = attr(x_train, "scaled:scale"))
predictions = knn.reg(train = x_train, 
                      test  = x_example,
                      y = Boston$crim[in_train],
                      k = 25)


example_data <- example_data %>%
    mutate(pred = predictions$pred)
example_data %>%
    gf_point(rad ~ age, color =~ pred)


example_data %>%
    filter(rad %in% c(1,8,24)) %>%
    gf_line(pred ~ age, color =~ factor(rad))
