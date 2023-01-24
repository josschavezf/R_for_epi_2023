# Week 2 

# Link https://josschavezf.github.io/R_for_epi_2023/week_2/slides.html

table_csv <- read.csv("table.csv")
table_txt <- read.delim("table.txt")

table_csv
table_txt

View(table_csv)
View(table_txt)

write.csv(table_csv, "exported_table.csv")
write.table(table_txt, "exported_table.txt", sep = "\t")

write.table(table_csv, "exported_table.txt", sep = "")

# creating a vector

my_vector <- 10
my_vector <- "a"
my_vector

my_vector <- "10"
my_vector

my_vector <- c(1, 10, 25, 30)
my_vector

my_vector <- c("a", "b", "c")
my_vector

vector_1 <- 500
vector_2 <- 1:500
vector_3 <- c("a", "e")
vector_4 <- c("Joselyn", "31", "Irapuato")

vector_1
vector_2
vector_3
vector_4

seq(1, 10, 2)

length(vector_2)

seq(from = 10, to = 2)

x_num <- c(1,2,3)
class(x_num)
x_num

x_int <- c(1L, 2L, 3L)
class(x_int)
x_int

x_dbl <- c(1, 2.5, 3.1)
class(x_dbl)
typeof(x_dbl)

x_char <- c("a", "chair", "the window")
x_char
class(x_char)

x_fct <- c("mouse_a", "mouse_b", "mouse_c")
x_fct
class(x_fct)

x_fct <- as.factor(x_fct)
class(x_fct)

x_char
x_fct

x_log <- c(TRUE, FALSE, FALSE)
class(x_log)

x_test <- c("TRUE", "FALSE", "FALSE")


x_log
class(x_log)

x_test
class(x_test)


x_mix <- c(10, "a", TRUE)
x_mix
class(x_mix)


x_test <- c(10,2, TRUE)
class(x_test)

as.logical(x_test)

x <- c(10, 20, 30, 40, 50)
x

x[2]
x[4]
x[3:5]
x[1,3,5]
x["1","3","5"]
x[c(1,3,5)]

my_index <- c(1,3,5)
my_index

x[my_index]

x <- seq(50, 100, 5)
x
x[1:7]
length(x)

x[4:11]

letters
LETTERS

my_letters <- letters[1:11]

my_letters[c(1,3,10)]

