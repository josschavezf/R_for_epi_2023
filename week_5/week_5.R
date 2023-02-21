# Week 5

# Link https://josschavezf.github.io/R_for_epi_2023/week_5/slides.html

df <- data.frame(x = letters,
                 y = 1:26,
                 z = sample(c(TRUE, FALSE), 26,
                            replace = TRUE))
df

library(dplyr)

dt <- tibble(x = letters,
             y = 1:26,
             z = sample(c(TRUE, FALSE), 26,
                        replace = TRUE))

df
dt

library(readr)

sinai_covid <- read_csv("Sinai_covid.csv")

View(sinai_covid)

sinai_covid

head_sinai_covid <- head(sinai_covid)

write_csv(head_sinai_covid, "new_tibble.csv")


## Arrange
sinai_covid <- arrange(sinai_covid, age)

sinai_covid 

arrange(sinai_covid, race, sex)

arrange(sinai_covid, desc(age))

## filter

sinai_covid_filtered <- filter(sinai_covid, age > 50)

sinai_covid_filtered

filter(sinai_covid, age > 50 & sex == "FEMALE")

filter(sinai_covid, age > 50, sex == "FEMALE",
       smoking_status == "NEVER")

filter(sinai_covid, age > 50 | sex == "FEMALE")

## select

select(sinai_covid, id, age, facility)


result <- filter(sinai_covid, 
                 facility == "THE MOUNT SINAI HOSPITAL" |
                     facility == "MOUNT SINAI BI BROOKLYN")
result

result <- select(result, id, sex, ethnicity, bmi)

result

result <- arrange(result, desc(bmi))

result

## mutate
result_log <- mutate(result, bmi_log = log10(bmi))

result_log

mutate(result, sex = paste("SEX:", sex))

mutate(result, bmi = bmi - 10)

mutate(result, sex = paste0(sex, ":"))

mutate(result, bmi = bmi - 10)

result_2 <- mutate(result, bmi_10 = bmi - 10)

result
result_2

## multiply bmi by 100, replace bmi content

mutate(result, bmi = bmi*100)
mutate(result, bmi_100 = bmi*100)

tolower("AAA")

result

## replace ethnicity content with lowercase
mutate(result, ethnicity = tolower(ethnicity))
mutate(result, ethnicity_lower = tolower(ethnicity))


1:272
rnorm(272)

result

result_2 <- mutate(result, random_num = rnorm(272))
result_2 <- mutate(result_2, sex = tolower(sex))
result_2 <- mutate(result_2, bmi_10 = bmi*10)
result_2


count(sinai_covid, ethnicity)

# how many males and females?
count_sex <- count(sinai_covid, sex)
count_sex

# how many patients from each facility?
num_facility <- count(sinai_covid, facility)
num_facility

num_facility <- count(sinai_covid, facility, 
                      name = "patients")
num_facility

# how many males and females?
count_sex <- count(sinai_covid, sex, 
                   name = "patients")
count_sex


# extract patients from MOUNT SINAI WEST and count
# smoking status
result_3 <- filter(sinai_covid, 
                   facility == "MOUNT SINAI WEST")
result_3 <- count(result_3, smoking_status,
                  name = "patients")
result_3

