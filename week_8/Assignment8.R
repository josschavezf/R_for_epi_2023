# Assignment week 8

# Exercise 1: Using the sinai_covid dataset, 
# complete the following tasks:

library(tidyverse)
sinai_covid <- read_csv("Sinai_covid.csv")

# - Count the number of patients depending on 
# their smoking status per ethnic group 

count_smoking <- sinai_covid %>% 
    group_by(ethnicity) %>% 
    count(smoking_status)
count_smoking

# - Represent the data on a column plot
ggplot(count_smoking,
       aes(x = n,
           y = ethnicity,
           fill = smoking_status)) +
    geom_col()

# - Split each bar by the smoking status
ggplot(count_smoking,
       aes(x = n,
           y = ethnicity,
           fill = smoking_status)) +
    geom_col(position = position_dodge())

# - Add a plot title, axis title and change the legend 
# title to remove the underscores
ggplot(count_smoking,
       aes(x = n,
           y = ethnicity,
           fill = smoking_status)) +
    geom_col(position = position_dodge()) +
    labs(title = "Smoking status incidence by ethnicity",
         y = "",
         x = "Number of patients",
         fill = "Smoking status")

#  Exercise 2:  Using the sinai_covid dataset, 
# complete the following tasks:

# - Count the number of patients depending on their 
# diabetes status per ethnic group and sex

count_diabetes <- sinai_covid %>% 
    group_by(ethnicity, sex) %>% 
    count(diabetes)

count_diabetes

# - Represent the data on a column plot, with the ethnic 
# groups at x axis

ggplot(count_diabetes,
       aes(x = ethnicity,
           y = n,
           fill = factor(diabetes))) +
    geom_col()

# - Split each bar depending on the diabetes status
ggplot(count_diabetes,
       aes(x = ethnicity,
           y = n,
           fill = factor(diabetes))) +
    geom_col(position = position_dodge())

# - Add a plot title, axis titles and modify the legend title 
ggplot(count_diabetes,
       aes(x = ethnicity,
           y = n,
           fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes incidence by ethnicity",
         fill = "Diabetes",
         y = "Number of patients",
         x = "")

# - Change the filling colors of the bars 
# (use scale_fill_brewer or scale_fill_manual )
ggplot(count_diabetes,
       aes(x = ethnicity,
           y = n,
           fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes incidence by ethnicity",
         fill = "Diabetes",
         y = "Number of patients",
         x = "") +
    scale_fill_brewer(palette = 3)

ggplot(count_diabetes,
       aes(x = ethnicity,
           y = n,
           fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes incidence by ethnicity",
         fill = "Diabetes",
         y = "Number of patients",
         x = "") +
    scale_fill_manual(values = c("blue", "yellow"))

# - Use the theme() layer to modify the angle 
# or size of the axis text 

# - Split the plots by sex (use facets)
ggplot(count_diabetes,
       aes(x = ethnicity,
           y = n,
           fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes incidence by ethnicity",
         fill = "Diabetes",
         y = "Number of patients",
         x = "") +
    scale_fill_manual(values = c("blue", "yellow")) +
    facet_grid(cols = vars(sex))

ggplot(count_diabetes,
       aes(x = ethnicity,
           y = n,
           fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes incidence by ethnicity",
         fill = "Diabetes",
         y = "Number of patients",
         x = "") +
    scale_fill_manual(values = c("blue", "yellow")) +
    facet_wrap(vars(sex), scales = "free_y")
