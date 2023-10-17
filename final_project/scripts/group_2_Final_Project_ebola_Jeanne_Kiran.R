ebola <- read.csv('data/ebola-cases-and-deaths.csv')
View(ebola)

library(tidyverse)

no21 <- select(ebola, Date, Country, Case.def., Total.cases, Total.deaths)
View(no21)

ebola1 <- no21 %>% 
  rename(cases = Total.cases) %>% 
  rename(deaths = Total.deaths)

View(ebola1)

library(dplyr)

ebola2 <- subset(ebola1, Country != 'Liberia2')
View(ebola2)

ebola3 <- subset(ebola2, Country %in% c('Guinea', 'Sierra Leone', 'Liberia'))

ebola4 <- subset(ebola2, !Country %in% c('Guinea', 'Sierra Leone', 'Liberia'))

View(ebola4)
View(ebola3)

#my_dataframe <- mutate_all(my_dataframe, ~replace_na(.,0)

ebola2[is.na(ebola2)] <- 0
View(ebola2)

ebola_all <- subset(ebola2, Case.def. == 'All')
View(ebola_all)

ebola_all_3 <- subset(ebola3, Case.def. == 'All')
ebola_all_4 <- subset(ebola4, Case.def. == 'All')
View(ebola_all_3)
View(ebola_all_4)

ggplot(ebola_all_3,
       aes(x = cases, y = deaths, color = Country))+
  geom_point()+
  theme_classic()+
  geom_abline(aes(intercept = 2.195e+03, slope = 1.648e-01), color = 'red')+
  labs(title = 'Number of Cases and Deaths from Ebola', x = 'Ebola Cases', y = 'Ebola Deaths')

ebola_all_3lr <- lm(deaths~cases, data = ebola_all_3)
summary(ebola_all_3lr)

ggplot(ebola_all_4,
       aes(x = cases, y = deaths, color = Country))+
  geom_point()+
  theme_classic()+
  geom_abline(aes(intercept = -0.17782, slope = 0.45124), color = 'red')+
  labs(title = 'Number of Cases and Deaths from Ebola', x = 'Ebola Cases', y = 'Ebola Deaths')

ebola_all_4lr <- lm(deaths~cases, data = ebola_all_4)
summary(ebola_all_4lr)

library(ggrepel)

ggplot(ebola_all,
       aes(x = cases, y = deaths, color = Country))+
  geom_point()+
  geom_abline(aes(intercept = 148.11491, slope = 0.34340), color = 'red')+
  labs(title = 'Number of Cases and Deaths from Ebola', x = 'Ebola Cases', y = 'Ebola Deaths')+
  theme_classic()

ebola_alllr <- lm(deaths~cases, data = ebola_all)
summary(ebola_alllr)

#############

ebola_all$cases <- as.numeric(as.character(ebola_all$cases))
ebola_all$deaths <- as.numeric(as.character(ebola_all$deaths))

ebola_all$total <- ebola_all$cases + ebola_all$deaths

ggplot(ebola_all, aes(x = Country, y = cases, fill = Country)) +
  geom_col() +
  labs(title = "Histogram of Ebola Deaths by Country") +
  theme_classic()

ebola_all <- ebola_all %>%
  mutate(total = cases + deaths)

ggplot(ebola_all, aes(x = Date)) +
  geom_point(aes(y = deaths + cases, color = total)) +
  scale_color_gradient(name = "Total Cases and Deaths") +
  labs(x = "Date", y = "Number of People", 
       title = "Ebola Cases and Deaths by Country") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(name = "Total Cases and Deaths")

ggplot(ebola_all, aes(x = Country, y = cases, fill = deaths)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Ebola Cases by Country", x = "Country", y = "Cases") +
  theme_classic()

ggplot(ebola_all, aes(x = Country)) +
  geom_bar(aes(y = cases, fill = "cases"), stat = "identity", position = "stack") +
  geom_bar(aes(y = deaths, fill = "deaths"), stat = "identity", position = "stack") +
  scale_fill_manual(name = "Legend", values = c("cases" = "blue", "deaths" = "red")) +
  labs(title = "Ebola Cases and Deaths by Country", x = "Country", y = "Number of People") +
  theme_classic()

ggplot(ebola_all_3, aes(x = Country)) +
  geom_bar(aes(y = cases, fill = "cases"), stat = "identity", position = "stack") +
  geom_bar(aes(y = deaths, fill = "deaths"), stat = "identity", position = "stack") +
  scale_fill_manual(name = "Legend", values = c("cases" = "blue", "deaths" = "red")) +
  labs(title = "Ebola Cases and Deaths by Country", x = "Country", y = "Number of People") +
  theme_classic()

ggplot(ebola_all_4, aes(x = Country)) +
  geom_bar(aes(y = cases, fill = "cases"), stat = "identity", position = "stack") +
  geom_bar(aes(y = deaths, fill = "deaths"), stat = "identity", position = "stack") +
  scale_fill_manual(name = "Legend", values = c("cases" = "blue", "deaths" = "red")) +
  labs(title = "Ebola Cases and Deaths by Country", x = "Country", y = "Number of People") +
  theme_classic()
