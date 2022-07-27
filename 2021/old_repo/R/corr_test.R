N <- 100000
x <- rnorm(N)
y <- rnorm(N)

cor(x,y)
cor(sort(x),sort(y))
cor(sort(x),-sort(y))

hist(x)
hist(y)

rho <- 0.1597
ind <- sample(1:N, rho*N, replace = FALSE)
x <- sort(x)

y[sort(ind)] <- sort(y[ind])
y
cor(x, y)




N <- 100000
x <- rbinom(N, 3, 0.5)
y <- rbinom(N, 10, 0.2)

cor(x,y)
cor(sort(x),sort(y))
cor(sort(x),-sort(y))




x <- sample(1:20, 20, replace = FALSE)
y <- sample(1:20, 20, replace = FALSE)

cor(sort(x), -sort(y), method = "kendall")

rho <- 0.567
ind <- sample(1:20, rho*20, replace = FALSE)
x <- sort(x)
y[sort(ind)] <- sort(y[ind])
cor(x, y, method = "kendall")





#Going from continuous to discrete and N = 20
N <- 1000
x <- rnorm(N)
y <- rnorm(N)

rho <- 0.5
ind <- sample(1:N, rho*N, replace = FALSE)
x <- sort(x)

y[sort(ind)] <- sort(y[ind])
y
cor(x, y, method = "kendall")

#Subset sample size 20
set <- sample(1:N, 20, replace = FALSE)
cor(rank(x[set]), rank(y[set]))
