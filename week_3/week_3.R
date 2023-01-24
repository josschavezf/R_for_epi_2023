# Week 3 

x <- c(1,3,10)
names(x)

x <- c("number_a" = 1, 
       "number_b" = 3,
       "number_c" = 10)
x
class(x)

names(x)

x["number_a"]
x["number_c"]


x <- c(1,3,10)
names(x)

names(x) <- c("number_a", "number_b", "number_c")
names(x)
x

x[c("number_a", "number_b")]
x[c("number_b", "number_c")]


x <- 1:10
x

x < 5

x[x <= 5]
x[x >= 5]


x <- c("a", "a", "b", "c", "c", "c")
x

x == "c"

temperature_nyc <- c("Monday" = 55,
                     "Tuesday" = 55,
                     "Wednesday" = 66,
                     "Thursday" = 50,
                     "Friday" = 48,
                     "Saturday" = 45,
                     "Sunday" = 47)
temperature_nyc

temperature_nyc[c("Monday", "Tuesday", "Wednesday")]
temperature_nyc[1:3]

temperature_nyc[c("Tuesday", "Thursday", "Saturday")]
temperature_nyc[c(2,4,6)]

mean(temperature_nyc)

temperature_nyc > mean(temperature_nyc)

temperature_nyc[temperature_nyc > mean(temperature_nyc)]

temperature_nyc[temperature_nyc <= median(temperature_nyc)]

# equal to 50

temperature_nyc[temperature_nyc == 50]

x <- c("a", "b", "c")
x
length(x)

x[4] <- "d"

x

x[-2]

y <- x[-2]

x
y

x <- x[-2]
x

x

x[-3]
x

x <- x[-3]
x

fruits <- c("apples" = 1,
            "cherries" = 10,
            "mangos" = 7)
fruits

fruits[4] <- 20

fruits

names(fruits)[4] <- "peaches"

fruits

fruits["berries"] <- 15
fruits

# create a vector with 4 names
bff <- c("Anna",
         "Rob",
         "Lucas",
         "Rose")
bff

# remove the friend # 2
bff <- bff[-2]
bff

# add a new friend at the end of the vector
bff[4] <- "Paty"

bff

bff[1] <- "Leo"
bff

# replace your third friend with a new one 
bff[3] <- "Valerie"
bff

x <- c("a", "a", "b", "c", "c","c")
x

# replace all c's with a "d"

x[x == "c"] <- "d"

x

# replace a by f
x[x == "a"] <- "f"
x


fruits <- c("apples" = 1,
            "cherries" = 10,
            "mangos" = 7)
fruits

fruits[fruits > 5]

fruits[1] <- 4
fruits

fruits[fruits > 5] <- 20
fruits

my_matrix <- matrix(1:12, nrow = 4)
my_matrix

my_matrix <- matrix(1:12, ncol = 4)
my_matrix

my_matrix <- matrix(1:12, ncol = 4, 
                    byrow = TRUE)
my_matrix

my_matrix <- matrix(1:11, ncol = 4)
my_matrix

my_matrix + 10

my_matrix <- my_matrix + 10

my_matrix

my_matrix * 2

my_matrix1 <- matrix(1:6, nrow = 2)
my_matrix1

my_matrix2 <- matrix(7:12, nrow = 2)
my_matrix2

my_matrix1 + my_matrix2

my_matrix3 <- matrix(1:12, nrow = 3)
my_matrix3

my_matrix1 + my_matrix3

my_new_matrix <- matrix(101:125, nrow = 5)
my_new_matrix
(my_new_matrix + 10) * 3

my_new_matrix

my_new_matrix[5,4]
my_new_matrix[c(1,2,3,4,5), c(1,1,1,1,2)]

my_new_matrix[1:7]

101:125

my_new_matrix[c(1,3,5)]

my_new_matrix[1:2, 1:3]

# rows 2,3,4 & columns 1,2
my_new_matrix[2:4,1:2]


my_new_matrix

my_new_matrix[1,5]

my_new_matrix[1:5, 4:5]
my_new_matrix[,4:5]
my_new_matrix[3:5,]
my_new_matrix[,]

install.packages("BiocManager")
library(BiocManager)
install("Biostrings")

install.packages("remotes")
library(remotes)
install_github("nstrayer/datadrivencv")







