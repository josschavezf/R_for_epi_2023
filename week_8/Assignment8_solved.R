# Assignment 8

# Exercise 1: Using the sinai_covid dataset, complete the following tasks:

library(tidyverse)
library(janitor)

sinai_covid <- read_csv("sinai_covid.csv")
sinai_covid <- clean_names(sinai_covid)

# - Count the number of patients depending on their smoking status per ethnic group 
smoking <- sinai_covid %>% 
    group_by(ethnicity) %>% 
    count(smoking_status)

smoking
# - Represent the data on a column plot

ggplot(smoking, aes(x = n, y = ethnicity, fill = smoking_status)) +
    geom_col()

# - Split each bar depending on the smoking status

ggplot(smoking, aes(x = n, y = ethnicity, fill = smoking_status)) +
    geom_col(position = position_dodge())

# - Add a plot title, axis title and change the legend title to remove the underscores
ggplot(smoking, aes(x = n, y = ethnicity, fill = smoking_status)) +
    geom_col(position = position_dodge()) +
    labs(title = "Smoking status per ethnicity",
         x = "Number of patients",
         y = "",
         fill = "Smoking status")


#  Exercise 2:  Using the sinai_covid dataset, complete the following tasks:

# - Count the number of patients depending on their diabetes status per ethnic group 

diabetes <- sinai_covid %>% 
    group_by(ethnicity) %>% 
    count(diabetes)

diabetes

# - Represent the data on a column plot, with the etnic groups at x axis

ggplot(diabetes, aes(x = ethnicity, y = n, fill = factor(diabetes))) +
    geom_col()

# - Split each bar depending on the diabetes status

ggplot(diabetes, aes(x = ethnicity, y = n, fill = factor(diabetes))) +
    geom_col(position = position_dodge())

# - Add a plot title, axis titles and modify the legend title 
ggplot(diabetes, aes(x = ethnicity, y = n, fill = factor(diabetes))) +
    geom_col() +
    labs(title = "Diabetes per ethnicity",
         x = "",
         y = "Number of patients",
         fill = "Diabetes status")

# - Change the filling colors of the bars (use scale_fill_brewer or scale_fill_manual )
ggplot(diabetes, aes(x = ethnicity, y = n, fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes per ethnicity",
         x = "",
         y = "Number of patients",
         fill = "Diabetes status") +
    scale_fill_brewer(palette = 2)


# - Explore the functions that start with theme_ and use one of them

ggplot(diabetes, aes(x = ethnicity, y = n, fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes per ethnicity",
         x = "",
         y = "Number of patients",
         fill = "Diabetes status") +
    scale_fill_brewer(palette = 2) +
    theme_classic()


# - Use the theme() layer to modify the angle or size of the axis text 

ggplot(diabetes, aes(x = ethnicity, y = n, fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes per ethnicity",
         x = "",
         y = "Number of patients",
         fill = "Diabetes status") +
    scale_fill_brewer(palette = 2) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))

