df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)


x <- runif(1e6)
y <- list(x,x,x)

d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d3 <- d1
d3[1, ] <- d3[1, ] * 3


x <- list(1:10)
x[[2]] <- x



x1 <- array(1:5, c(1, 1, 5))
x2 <- array(1:5, c(1, 5, 1))
x3 <- array(1:5, c(5, 1, 1))


f1 <- factor(letters)
levels(f1) <- rev(levels(f1))

f2 <- rev(factor(letters))

f3 <- factor(letters, levels = rev(letters))

x <- c(2.1, 4.2, 3.3, 5.4)
x[c(3, 1)]

mtcars[mtcars$cyl == 4, ]

mtcars[-c(1:4), ]

mtcars[mtcars$cyl <= 5, ]

mtcars[mtcars$cyl == c(4, 6), ]

x <- matrix(letters[1:25],nrow = 5)
x[upper.tri(x)]



mod <- lm(mpg ~ wt, data = mtcars)
temp <- (summary(mod))


x <- 1:10
if (length(x)) "not empty" else "empty"

x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
g04()



y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1()
y


