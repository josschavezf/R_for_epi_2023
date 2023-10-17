library(modelr)
library(tidyverse)

sinai_covid <- read.csv("Sinai_covid.csv")
covid2 <- read.csv("covid2.csv")

sinai_covid <- full_join(sinai_covid, covid2)

ggplot(sim1, aes(x,y)) +
    geom_point() +
    geom_abline(aes(intercept = 5, slope = 2), color = "blue")+
    geom_abline(aes(intercept = 3, slope = 2.5), color = "green")

model1 <- lm(y~x, data = sim1)
summary(model1)

ggplot(sim1, aes(x,y)) +
    geom_point() +
    geom_abline(aes(intercept = 4.22, slope = 2.05), color = "blue")


ggplot(sinai_covid,
       aes(y = systolic_bp,
           x = age)) +
    geom_point()

model2 <- lm(systolic_bp~age, data = sinai_covid)
model2

ggplot(sinai_covid,
       aes(y = systolic_bp,
           x = age)) +
    geom_point() +
    geom_abline(aes(intercept = 127.41754, slope = 0.07063),
                color = "blue")

summary(model2) # P value 0.261  

model3 <- lm(systolic_bp ~ age + bmi, data = sinai_covid)
summary(model3) # 0.275 0.835  

model4 <- lm(systolic_bp ~ age + diastolic_bp, data = sinai_covid)
summary(model4) # 0.159 <2e-16

# generalized linear models

fit1 <- glm(factor(deceased_indicator) ~ age + bmi,
            data = sinai_covid,
            family = binomial)
summary(fit1)

## to get coefficients
coef(fit1)
exp(coef(fit1))

sinai_covid <- sinai_covid %>% 
    mutate(age_group = case_when(age <= 60 ~ "young",
                                 age > 60 ~ "old")) %>% 
    mutate(age_group = factor(age_group, levels = c("young", "old")))
glimpse(sinai_covid)

fit2 <- glm(deceased_indicator ~ age_group + bmi,
            data = sinai_covid,
            family = binomial)
summary(fit2)
coef(fit2)
exp(coef(fit2)) # it means that being part of the old group increase 
# in 4.524 times the odds of dying for covid compared to young people

fit3 <- glm(factor(asthma) ~ sex,
            data = sinai_covid,
            family = binomial)

summary(fit3) # sex is significantly related to asthma

coef(fit3)
exp(coef(fit3)) # being a male decreases the chance to have asthma in 66%

sinai_covid <- sinai_covid %>% 
    mutate(sex = factor(sex, levels = c("MALE", "FEMALE")))

fit3 <- glm(factor(asthma) ~ sex,
            data = sinai_covid,
            family = binomial)

coef(fit3)
exp(coef(fit3)) # being a female increases 2.942 times the chance to get asthma

t.test(systolic_bp ~ sex, sinai_covid) # p value 0.8462 
# it means there is no significant difference in blood pressure between sex
t.test(sinai_covid$systolic_bp, sinai_covid$diastolic_bp)

one_way <- aov(systolic_bp ~ smoking_status,
               data = sinai_covid)

summary(one_way) # p value 0.696 smoking status is not significantly 
# related to blood pressure 



