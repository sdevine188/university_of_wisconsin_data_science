library(tidyverse)
library(fs)
library(arules)

# setwd
setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_11")


# load data
dir_ls()
ames <- read.csv(file = "AmesSimple.csv") %>% as_tibble() %>%
        mutate(Lot.Shape = ifelse(Lot.Shape == "Reg", "Reg", "Ir"))

# inspect
ames
ames %>% glimpse()
ames %>% count(Lot.Shape)


#/////////////////////////////


# discretize continuous variables
ames$Bedroom.AbvGr = discretize(ames$Bedroom.AbvGr, breaks=2, ordered=T, method="interval")
ames %>% count(Bedroom.AbvGr)


ames$Full.Bath <- discretize(ames$Full.Bath, breaks=2, ordered=T, method="interval")
ames %>% count(Full.Bath)


living_area <- discretize(ames$Gr.Liv.Area, breaks=3, ordered=T, method="interval") 
living_area %>% as_tibble() %>% count(value)
living_area %>% as_tibble() %>% count(value) %>% mutate(support = num(n / sum(n), digits = 10))

# distcretize skewed variable with fixe boundaries at quantiles, instead of interval length, 
# which can lead to intervals with very low support in outlying tails of distribution
summary(ames$Gr.Liv.Area)
ames$Gr.Liv.Area = discretize(ames$Gr.Liv.Area, method = "fixed", 
                              breaks=c(334, 1126, 1743, 5642), 
                              ordered=T)

# discretize 
ames %>% ggplot(data = ., mapping = aes(x = SalePrice)) + geom_histogram()
ames$SalePrice = discretize(ames$SalePrice, method = "fixed", 
                            breaks=c(12789, 129500, 213500, 755000), 
                            ordered=T)


# treat all other variables as factors as well
ames <- ames %>%
        mutate(across(where(is.character), factor)) %>%
        mutate(across(where(is.double), factor)) %>%
        mutate(across(where(is.integer), factor))

ames %>% glimpse()

# convert dataframe to transactions structure
ames_trans = as(ames, "transactions")

# inspect
ames_trans
ames_trans %>% glimpse()
summary(ames_trans)


#///////////////////////////


# note that support = p(A & B) with a = antecedent and b = consequent
# confidence = p(B|A) 
# lift = p(B|A) / p(B) aka how much more likely is consequent given antecedent, than the consequentn is in overall population

# get rules
rules <- apriori(data = ames, parameter = list(support = .05, 
                                          confidence = 0.5, 
                                          maxlen = 12))
?inspect
?head
rules %>% summary()
rules %>% attributes()
# rules %>% inspect()
rules[1:5] %>% inspect() %>% data.frame() %>% as_tibble() 

head(rules, n = 3, by = "lift", decreasing = TRUE) %>% inspect()



#//////////////////////////////


# mine for particular antecedent (left-hand side aka lhs) or consequent (right-hand side aka rhs)
rules2 = apriori(ames_trans, parameter = list(support = .05, confidence = 0.5, maxlen = 12), 
                 appearance = list(rhs = c("SalePrice=[2.14e+05,7.55e+05]"), default = "lhs") )
rules2 %>% summary()


# find non-redundant rules
(interestMeasure(rules2, measure = "improvement", quality_measure = "confidence") > 0)

non_redundant = !is.redundant(rules2)
rules3 <- rules2[non_redundant]
rules3 %>% summary()



#///////////////////////////////


# look at subset of rules
rules4 = subset( rules3, subset = lhs %in% c("Bldg.Type.simple=1Fam", "Bldg.Type.simple=2Fam") )
rules4 %>% summary()

head(rules4, n = 3, by = "lift", decreasing = TRUE) %>% inspect()


high_lift = subset(rules3, subset = lift > 3.5 & confidence > .95)
high_lift %>% summary()
head(high_lift, n = 3, by = "lift", decreasing = TRUE) %>% inspect()


#//////////////////////////////////


# filter rules with only a single antecedent
mylhs = lhs(rules3)
singleAnt = which( size(mylhs) == 1 )
inspect( rules3[singleAnt] )


# look for specific subsets of rules
subset_1 = subset( rules3, subset = lhs %in% c("Bedroom.AbvGr=[4,8]") )
subset_1 %>% summary()
head(subset_1, n = 3, by = "lift", decreasing = TRUE) %>% inspect()


#/////////////////////////////////////


# note the transactions object is an efficient way to store a sparse one-hot encoded matrix
# but you can also convert the transactions object back to the original sparse matrix
mylhs_mat = as(mylhs, Class = "matrix")
mylhs_mat %>% str()
mylhs_mat
mylhs_mat %>% data.frame() %>% as_tibble()
mylhs_mat %>% data.frame() %>% as_tibble() %>% glimpse()


#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////


library(arules)

data(Groceries)
?Groceries

groceries <- Groceries

groceries
groceries %>% inspect()
groceries %>% summary()
groceries %>% attributes()
groceries[1:5] %>% inspect() %>% data.frame() %>% as_tibble() 


groceries_matrix <- as(groceries, Class = "matrix")
groceries_matrix %>% str()
groceries_matrix
groceries_matrix %>% data.frame() %>% as_tibble()
groceries_matrix %>% data.frame() %>% as_tibble() %>% glimpse()

groceries <- Groceries
groceries_matrix <- as(groceries, Class = "matrix")
groceries_matrix %>% data.frame() %>% as_tibble() %>%
        mutate(across(.cols = everything(), .fns = ~ sum(.x))) %>%
        distinct() %>% 
        pivot_longer(cols = everything(), names_to = "product", values_to = "transaction_count") %>%
        arrange(transaction_count) %>%
        mutate(share_of_transactions_w_product = transaction_count / 
                       (groceries_matrix %>% data.frame() %>% as_tibble() %>% nrow())) %>%
        filter(share_of_transactions_w_product >= .05) %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = product, .x = transaction_count), 
                                       y = transaction_count)) + 
        geom_col() +
        theme(
                axis.text.x = element_text(angle = 45)
        )


#////////////////////////////////////////////////////////////////////////////////////////////////


# get rules
rules <- apriori(data = groceries, parameter = list(support = .001, 
                                               confidence = 0.5
                                               # maxlen = 12
                                               ))
?inspect
?head
rules %>% summary()
rules %>% attributes()
# rules %>% inspect()
rules[1:5] %>% inspect() %>% data.frame() %>% as_tibble() 

head(rules, n = 10, by = "lift", decreasing = TRUE) %>% inspect()

?subset
subset(rules, subset = lhs %in% c("soda", "popcorn", "salty snack")) %>% inspect()
subset(rules, subset = lhs %ain% c("soda", "popcorn", "salty snack")) %>% inspect()


subset(rules, subset = lhs %ain% c("ham", "processed cheese") &
                        rhs %ain% c("white bread")) %>% inspect()


non_redundant <- !is.redundant(rules)
rules2 <- rules[non_redundant]
rules2 %>% summary()


subset(rules2, subset = rhs %in% c("pastry")) %>% inspect()



#///////////////////////////////////////////////////////////////////////////////////////////


# part 2

heart <- read_csv(file = "HeartDisease.csv") %>% 
        mutate(hasCP = case_when(ChestPain == 4 ~ "yes",
                                 TRUE ~ "no"),
               hasCP = factor(hasCP)) %>%
        select(-ChestPain)
heart


#////////////////////////////////////////


heart %>% skim(Age)
heart$Age <- discretize(heart$Age, breaks = 3, ordered_result = TRUE, method = "interval")
heart %>% count(Age)

heart %>% skim(BloodPressure)
heart$BloodPressure = discretize(heart$BloodPressure, method = "fixed", 
                              breaks = c(-Inf, 120, 140, Inf), 
                              ordered_result = TRUE)
heart %>% count(BloodPressure)


#////////////////////////////////////////


# treat all other variables as factors as well
heart <- heart %>%
        mutate(across(where(is.character), factor)) %>%
        mutate(across(where(is.double), factor)) %>%
        mutate(across(where(is.integer), factor))

heart %>% glimpse()

# convert dataframe to transactions structure
heart_trans <- as(heart, Class = "transactions")

# inspect
heart_trans
heart_trans %>% glimpse()
summary(heart_trans)

heart_matrix <- as(heart, Class = "matrix")
heart_matrix %>% as_tibble() %>% glimpse()


#////////////////////////////////////////



heart_rules <- apriori(data = heart, parameter = list(support = .03, 
                                                    confidence = 0.5,
                                                    maxlen = 15),
                 appearance = list(rhs = c("hasHD=1"), default = "lhs"))


heart_rules %>% summary()


heart_rules_w_lhs_female <- subset(heart_rules, subset = lhs %in% c("Sex=0"))
heart_rules_w_lhs_female
heart_rules_w_lhs_female %>% summary()


head(heart_rules_w_lhs_female, n = 10, by = "lift", decreasing = TRUE) %>% inspect()


read_csv(file = "HeartDisease.csv") %>% 
        mutate(hasCP = case_when(ChestPain == 4 ~ "yes",
                                 TRUE ~ "no"),
               hasCP = factor(hasCP)) %>%
        select(-ChestPain) %>% count(Sex)
