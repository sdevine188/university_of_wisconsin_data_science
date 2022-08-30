library(tidyverse)
library(MASS)
library(nlme)

setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS740/lesson_6")

# lesson 6

x <- tibble(r = seq(from = -6, to = 6, by = 1)) %>%
                    mutate(w = case_when(abs(r) <= 3 ~ 1 - (abs(r) / 3),
                                         abs(r) > 3 ~ 0),
                           Tukey = (pmax(1-(r/4.685)^2,0))^2)
x

x %>% ggplot(data = ., mapping = aes(x = r, y = w)) + geom_line()
x %>% ggplot(data = ., mapping = aes(x = w, y = Tukey)) + geom_point()

bodyfat <- read_csv(file = "bodyfat.csv")
bodyfat


fit_bisquare = rlm(BodyFatBrozek ~ Weight, data = bodyfat, psi = psi.bisquare)
fit_bisquare


#////////////////////

fat <- bodyfat
fit.w = lm(BodyFatBrozek ~ Weight, data = fat)
oldcoef = rep(0, length(fit.w$coef))
newcoef = fit.w$coef
iter = 0

while(sum(abs(oldcoef-newcoef)) > .0001 & iter < 100){
        w = pmax(1-abs(fit.w$residuals)/3, 0)
        fit.w = lm(BodyFatBrozek ~ Weight, data = fat, weights=w)
        
        iter = iter + 1
        oldcoef = newcoef
        newcoef = fit.w$coef 
}

summary(fit.w)
tibble(w = w) %>% mutate(row_number = row_number()) %>%
        ggplot(data = ., mapping = aes(x = row_number, y = w)) + geom_line()


#/////////////////////


n = 100
Sigmax = matrix(NA, nr=n, nc=n) # initialize the covariance matrix
Sigmax[1,1] = 1
for(i in 2:n){
        Sigmax[i,i] = 1
        for(j in 1:(i-1)){
                Sigmax[i, j] = .9^abs(i-j)
                Sigmax[j, i] = Sigmax[i, j] # make the covariance matrix 
                # symmetric across the diagonal
        } #end iter over j
} # end iter over i


RNGkind(sample.kind = "Rejection")
set.seed(15)
x = runif(n, 0, 1)
y = 2*x + 3 + mvrnorm(1, rep(0,n), Sigmax) # generate 1 random vector 
# of n noise terms


m1 <- lm(formula = y ~ x)
plot(m1)
summary(m1)

cor(m1$residuals[1:99], m1$residuals[2:100])
acf(m1$residuals)


#/////////////////


m2 = gls(y ~ x, correlation = corAR1(form = ~1))
m2
summary(m2)


#/////////////////


m1_reml = gls(y~x)
m1_reml %>% summary()


AIC(m2, m1_reml)


#/////////////////////////////////////////////////////////////////////////////


# homework

data("crime2005")
crime2005

lm_model_1 <- lm(formula = VI2 ~ PO + ME, data = crime2005)
lm_model_1 %>% summary()
plot(lm_model_1)


# 51, 40, 8
crime2005 %>% slice(8, 40, 51)


#////////////////


fit.w = lm(formula = VI2 ~ PO + ME, data = crime2005)

oldcoef = rep(0,length(fit.w$coef))
newcoef = fit.w$coef
iter = 0

while(sum(abs(oldcoef-newcoef)) > .0001 & iter < 100){
        
        print(iter)
        
        w = 1/(fit.w$fitted.values^2)
        fit.w = lm(formula = VI2 ~ PO + ME, data = crime2005, weights = w)
        
        iter = iter + 1
        oldcoef = newcoef
        newcoef = fit.w$coef
}

fit.w 


#/////////////


fit_bisquare = rlm(VI2 ~ PO + ME, data = crime2005, psi = psi.bisquare)
fit_bisquare
fit_bisquare$w


#////////////


crime2005 %>% slice(51)

crime2005 %>% mutate(tukey_bisquare_weights = fit_bisquare$w,
                     row_number = row_number(),
                     label = case_when(tukey_bisquare_weights < .8 ~ as.character(STATE),
                                       TRUE ~ "")) %>%
        ggplot(data = ., mapping = aes(x = row_number, y = tukey_bisquare_weights, label = label)) + 
        geom_point() + geom_text(vjust = -1)


#////////////////////////////////////////////////////////////////////////


elnino <- read_csv(file = "elnino.csv") %>% drop_na()
elnino %>% glimpse()
elnino %>% skim()
elnino


#//////////////


lm_model_2 <- lm(formula = air.temp ~ zon.winds + mer.winds + humidity + s.s.temp, data = elnino)
lm_model_2 %>% summary()
lm_model_2 %>% plot()


#/////////////////


gls_model_3 = gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp, data = elnino)
gls_model_3 %>% summary()
gls_model_3 %>% attributes()
gls_model_3$residuals

elnino %>% filter(buoy == 3)

elnino %>% mutate(residuals = gls_model_3$residuals) %>%
        group_by(buoy) %>%
        mutate(prior_residuals = lag(residuals, n = 1)) %>%
        ungroup() %>%
        filter(buoy == 3) %>% 
        ggplot(data = ., mapping = aes(x = prior_residuals, y = residuals)) + geom_point()


acf_results = acf(gls_model_3$residuals, ci.type = "ma")
acf_results
acf_results[1] # lag-1 autocorrelation


#////////////////


gls_model_4 <- gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp, data = elnino,
    correlation = corAR1(form = ~day | buoy))
gls_model_4 %>% summary()

elnino_w_gls_model_4_residuals <- elnino %>% mutate(residuals = gls_model_4$residuals) %>%
        group_by(buoy) %>%
        mutate(prior_residuals = lag(residuals, n = 1),
               lag_2_prior_residuals = lag(residuals, n = 2)) %>%
        ungroup()

cor(elnino_w_gls_model_4_residuals$residuals, 
    elnino_w_gls_model_4_residuals$prior_residuals, 
    use = "pairwise.complete.obs")

cor(elnino_w_gls_model_4_residuals$residuals, 
    elnino_w_gls_model_4_residuals$lag_2_prior_residuals, 
    use = "pairwise.complete.obs")

AIC(gls_model_3, gls_model_4)

