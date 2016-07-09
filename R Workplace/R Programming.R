install.packages("dplyr")
install.packages("tidyr")
library("dplyr")
library("tidyr")
library(datasets)
data(iris)
?iris
str(iris$Species)


iris %>%
    subset(Species=="virginica") %>%
    summarise(round(mean(Sepal.Length)))

x <- subset(iris, iris$Species == "virginica")

round(mean(x$Sepal.Length))
lapply(iris, mean)

apply(iris[, 1:4], 2, mean)


library(datasets)
data(mtcars)

mtcars %>%
    group_by(cyl) %>%
    summarise(mean(mpg))

y <- split(mtcars$mpg,mtcars$cyl)
sapply(y,mean)
mean(mtcars$mpg, mtcars$cyl)
with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
y<-sapply(split(mtcars$mpg, mtcars$cyl), mean)
y
abs(y[1]-y[3])

debug(ls)
tapply(mtcars$hp, mtcars$cyl, mean)
