library(tidyverse)
library(caret)
library(nnet)
library(NeuralNetTools)
library(ISLR)
library(skimr)

OJ
oj <- OJ %>% as_tibble()
oj %>% glimpse()
oj %>% count(Purchase)
oj %>% distinct(Purchase) %>% pull(Purchase)
oj %>% count(LoyalCH)
?OJ

set.seed(10)

train_control <- trainControl(method = "cv", number = 5)
oj_ann_1 <- train(Purchase ~ LoyalCH + SalePriceMM + PriceDiff,
                    data = oj,
                    method = "nnet",
                    tuneGrid = expand.grid(size = 1, decay = 0),
                    preProc = c("center", "scale"),
                    trControl = train_control)

oj_ann_1 
oj_ann_1 %>% summary()
oj_ann_1$finalModel
oj_ann_1$finalModel$convergence

# plot net
par(mar = c(.1, .1, .1, .1))
plotnet(oj_ann_1)


# predict 
predicted_purchase_probs <- predict(object = oj_ann_1, newdata = oj, type = "prob")
predicted_purchase_probs

oj %>% mutate(predicted_purchase = predicted_purchase[ , "MM"]) %>%
        select(Purchase, predicted_purchase)

# get classification error rate
oj <- oj %>% mutate(predicted_purchase = predicted_purchase_probs[ , "MM"],
              predicted_purchase_class = case_when(predicted_purchase >= .5 ~ "MM",
                                                   TRUE ~ "CH"),
              predicted_purchase_class = factor(predicted_purchase_class, levels = c("CH", "MM")))

oj %>% select(Purchase, predicted_purchase, predicted_purchase_class)


confusionMatrix(data = oj$predicted_purchase_class, reference = oj$Purchase)
(96 + 86) / (oj %>% nrow())        
(567 + 321) / 1070
0.1700935 + 0.8299065


# alternate threshold
oj <- oj %>% mutate(predicted_purchase = predicted_purchase_probs[ , "MM"],
                    predicted_purchase_class = case_when(predicted_purchase >= .9 ~ "MM",
                                                         predicted_purchase <= .1 ~ "CH",
                                                         TRUE ~ NA_character_),
                    predicted_purchase_class = factor(predicted_purchase_class, levels = c("CH", "MM")))

oj %>% select(Purchase, predicted_purchase, predicted_purchase_class)
oj %>% count(predicted_purchase_class)

confusionMatrix(data = oj$predicted_purchase_class, reference = oj$Purchase)
351 + 93 + 14 + 9
1 - .9507
(14 + 9) / (14 + 9 + 351 + 93)


#////////////////////////////


# plot to diagnose model
lekprofile(oj_ann_1)


#//////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////


# problem 2

hitters <- Hitters %>% as_tibble() %>% 
        filter(!is.na(Salary)) %>%
        mutate(League01 = case_when(League == "A" ~ 0, 
                                    League == "N" ~ 1),
               Division01 = case_when(Division == "E" ~ 0,
                                      Division == "W" ~ 1),
               NewLeague01 = case_when(NewLeague == "A" ~ 0,
                                       NewLeague == "N" ~ 1)) %>%
        select(-c(League, Division, NewLeague))
hitters %>% glimpse()


#////////////////////


# fit ann model
set.seed(10)

train_control <- trainControl(method = "cv", number = 10)
hitters_ann_1 <- train(Salary ~ .,
                  data = hitters,
                  method = "nnet",
                  linout = TRUE,
                  maxit = 2000,
                  trace = FALSE,
                  tuneGrid = expand.grid(size = 5, decay = seq(from = 1, to = 2, by = .1)),
                  preProc = c("center", "scale"),
                  trControl = train_control)

hitters_ann_1 
hitters_ann_1 %>% summary()
hitters_ann_1$results
hitters_ann_1$finalModel
hitters_ann_1$finalModel$convergence


#/////////////////////////////


# plot rmse by decay values
hitters_ann_1$results %>% mutate(min_rmse_flag = case_when(RMSE == min(RMSE) ~ 1, 
                                                           TRUE ~ 0)) %>%
        arrange(RMSE)
hitters_ann_1$results %>% 
        ggplot(data = ., mapping = aes(x = decay, y = RMSE)) + geom_line() + geom_point() +
        geom_segment(x = 1.6, xend = 1.6, y = 339, yend = 385, color = "red")


#/////////////////////////////


garson(hitters_ann_1)


#/////////////////////////////


# example points for partial dependence plot
hitters %>% skim(Hits)
hitters %>% glimpse()

hitters_example <- hitters %>% 
                        mutate(across(.cols = everything(), .fns = ~ median(.x))) %>%
                        slice(1:max(hitters$Hits)) %>%
                        mutate(Hits = seq(from = min(hitters$Hits), to = max(hitters$Hits), by = 1))
hitters_example
hitters_example %>% glimpse()
hitters %>% glimpse()

predicted_salary <- predict(object = hitters_ann_1, newdata = hitters_example)
predicted_salary

hitters_example <- hitters_example %>% mutate(predicted_salary = predicted_salary)
hitters_example %>% select(Hits, Salary, predicted_salary) %>% 
        ggplot(data = ., mapping = aes(x = Hits, y = predicted_salary)) + geom_line()






