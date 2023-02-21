# Session 08 

# Link https://josschavezf.github.io/R_for_epi/session08/session08.html

library(tidyverse)
library(janitor)

sinai_covid <- read_csv("sinai_covid.csv")
sinai_covid

sinai_covid <- clean_names(sinai_covid)
sinai_covid


ggplot(sinai_covid, aes(x = systolic_bp, y = diastolic_bp,
                        color = facility)) +
    geom_point()


ggplot(sinai_covid, aes(x = systolic_bp, y = diastolic_bp,
                        shape = facility, color = facility)) +
    geom_point()


ggplot(sinai_covid, aes(x =systolic_bp, y = diastolic_bp,
                        shape = sex , color = sex)) +
    geom_point()


ggplot(sinai_covid, aes(x = age, y = bmi, shape = smoking_status,
                        color = smoking_status)) +
    geom_point()

colnames(sinai_covid)


ggplot(sinai_covid, aes(x = systolic_bp, y = diastolic_bp, 
                        color = age)) +
    geom_point()

##
count_smoking <- sinai_covid %>% 
    group_by(facility) %>% 
    count(smoking_status)

count_smoking

ggplot(count_smoking, aes(x = smoking_status, y = facility,
                          color = n, size = n )) +
    geom_point()

count_kidneyds <- sinai_covid %>% 
    group_by(facility) %>%
    count(chronic_kidney_disease)
count_kidneyds

ggplot(count_kidneyds, aes(x = factor(chronic_kidney_disease), y = facility, 
                           size = n )) +
    geom_point()


ggplot(count_kidneyds, aes(x = n , y = facility, 
                            fill = factor(chronic_kidney_disease))) +
    geom_col(position = position_dodge())


count_kidneyds$chronic_kidney_disease[count_kidneyds$chronic_kidney_disease == 0] <- "no"
count_kidneyds$chronic_kidney_disease[count_kidneyds$chronic_kidney_disease == 1] <- "yes"

count_kidneyds

ggplot(count_kidneyds, aes(x = n , y = facility, 
                           fill = chronic_kidney_disease)) +
    geom_col(position = position_dodge()) +
    labs(title = "CKD per facility",
         x = "Number of patients",
         y = "",
         fill = "Chronic kidney disease")



ggplot(count_kidneyds, aes(x = n , y = facility, 
                           fill = chronic_kidney_disease)) +
    geom_col(position = position_dodge()) +
    labs(title = "CKD per facility",
         x = "Number of patients",
         y = "",
         fill = "Chronic kidney disease") +
    scale_fill_brewer(palette = 4)


ggplot(count_kidneyds, aes(x = n , y = facility, 
                           fill = chronic_kidney_disease)) +
    geom_col(position = position_dodge()) +
    labs(title = "CKD per facility",
         x = "Number of patients",
         y = "",
         fill = "Chronic kidney disease") +
    scale_fill_manual(values = c("#5f9141", "#de62d3"))


ggplot(count_kidneyds, aes(x = facility, y = n, 
                           fill = chronic_kidney_disease)) +
    geom_col(position = position_dodge()) +
    labs(title = "CKD per facility",
         x = "",
         y = "Number of patients",
         fill = "Chronic kidney disease") +
    scale_fill_manual(values = c("#5f9141", "#de62d3")) +
    theme(axis.text.x = element_text(angle = 90, size = 10),
          plot.title = element_text(hjust = 0.5, size = 18))













