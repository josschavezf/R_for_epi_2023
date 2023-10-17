# Session 08 

# Link https://josschavezf.github.io/R_for_epi_2023/week_8/slides.html

library(tidyverse)

sinai_covid <- read_csv("Sinai_covid.csv")

ggplot(sinai_covid,
       aes(x = smoking_status,
           y = facility)) +
    geom_point()


count_smoking <- sinai_covid %>% 
    group_by(facility) %>% 
    count(smoking_status)

colnames(count_smoking)

ggplot(count_smoking,
       aes(x = smoking_status,
           y = facility,
           size = n,
           color = n)) +
    geom_point()


count_obesity <- sinai_covid %>% 
    group_by(facility) %>% 
    count(obesity)
count_obesity

ggplot(count_obesity,
       aes(x = factor(obesity),
           y = facility,
           size = n)) +
    geom_point()

ggplot(count_obesity,
       aes(y = facility,
           x = n,
           fill = factor(obesity))) +
    geom_col(position = position_dodge()) +
    labs(title = "Obsesity incidence per facility",
         x = "Number of patients",
         y = "",
         fill = "Obesity")

# count Race vs diabetes status
# plot with column geom
# fill with the diabetes status
# split the columns
# rename axis and legend titles
# add plot title

count_diabetes <- sinai_covid %>% 
    group_by(race) %>% 
    count(diabetes)
count_diabetes

ggplot(count_diabetes,
       aes(x = n,
           y = race,
           fill = factor(diabetes))) +
    geom_col(position = position_dodge()) +
    labs(title = "Diabetes frecuency by race",
         x = "Number of patients",
         y = "",
         fill = "Diabetes")

count_obesity <- sinai_covid %>% 
    group_by(facility, sex) %>% 
    count(obesity)

View(count_obesity)

ggplot(count_obesity,
       aes(x = n,
           y = facility,
           fill = factor(obesity))) +
    geom_col(position = position_dodge()) +
    facet_grid(rows = vars(obesity),
               cols = vars(sex))
    
count_obesity <- sinai_covid %>% 
    group_by(facility, sex) %>% 
    count(obesity) %>% 
    mutate(obesity = case_when(obesity == 0 ~ "no",
                               obesity == 1 ~ "yes"))

View(count_obesity)


ggplot(count_obesity,
       aes(x = n,
           y = facility,
           fill = obesity)) +
    geom_col(position = position_dodge()) +
    facet_grid(rows = vars(obesity),
               cols = vars(sex),
               labeller = label_both)


ggplot(count_obesity,
       aes(x = n,
           y = facility)) +
    geom_col() +
    facet_wrap(vars(sex, obesity),
               scales = "free_x",
               labeller = label_both)


count_smoking <- sinai_covid %>% 
    group_by(ethnicity, asthma) %>% 
    count(smoking_status) %>% 
    mutate(asthma = case_when(asthma == 0 ~ "no",
                              asthma == 1 ~ "yes"))
count_smoking

ggplot(count_smoking,
       aes(x = n,
           y = ethnicity,
           fill = asthma)) +
    geom_col() +
    labs(title = "Asthma frequency by ethnicity",
         x = "Number of patients",
         y = "",
         fill = "Asthma status") +
    facet_wrap(vars(smoking_status, asthma), 
               labeller = label_both)

ggplot(sinai_covid,
       aes(x = age,
           y = log10(bmi) + 1)) +
    geom_point()

ggplot(sinai_covid,
       aes(x = age,
           y = bmi,
           color = facility)) +
    geom_point() +
    scale_color_manual(values = c("THE MOUNT SINAI HOSPITAL" = "red", 
                                  "MOUNT SINAI QUEENS HOSPITAL" = "blue",
                                  "MOUNT SINAI BI BROOKLYN" = "#428554",
                                  "MOUNT SINAI WEST" = "orange",
                                  "MOUNT SINAI ST. LUKE'S" = "pink"))
    

count_smoking <- sinai_covid %>% 
    group_by(ethnicity, asthma) %>% 
    count(smoking_status) %>% 
    mutate(asthma = case_when(asthma == 0 ~ "no",
                              asthma == 1 ~ "yes"))


ggplot(count_smoking,
       aes(x = n,
           y = ethnicity,
           fill = asthma)) +
    geom_col(position = position_dodge()) +
    labs(title = "Asthma frequency by ethnicity",
         x = "Number of patients",
         y = "",
         fill = "Asthma status") +
    scale_y_discrete(limits = c(rev(unique(sort(count_smoking$ethnicity))[-1]), 
                                "ARGENTINEAN"))

ggplot(count_smoking,
       aes(x = asthma,
           y = n)) +
    geom_col() +
    scale_x_discrete(limits = c("yes", "no"))


# count smoking status vs asthma
# create a column plot
# reverse the order of smoking status 

count_smoking_asthma <- sinai_covid %>% 
    group_by(smoking_status) %>% 
    count(asthma)

count_smoking_asthma    

ggplot(count_smoking_asthma,
       aes(x = n,
           y = smoking_status)) +
    geom_col() +
    scale_y_discrete(limits = c("NEVER",
                                "YES",
                                "QUIT"))





