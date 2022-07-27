# Desired Spearman correlation between bouldering and lead
rho <- 0.5
set.seed(1999)

# Step 1: Generate the marginals with nsim = 100000
nsim <- 100000
speed <- runif(nsim)
lead <- runif(nsim)
bouldering <- runif(nsim)

dat <- data.frame(speed, lead, bouldering)

# Step 2.  The upper and lower bounds are 1 and -1.
# Don't need this here.

# Step 3.
# Pick some indexes
ind <- sample(1:nsim, rho * nsim, replace = FALSE)
dat$lead[ind] <- sort(dat$lead[ind])
dat$bouldering[ind] <- sort(dat$bouldering[ind])
cor(dat, method = "spearman")

ind2 <- sample(1:nsim, 20, replace = FALSE)

dat[ind2, ]


# Now we have the "population" that has the desired correlation.
test <- c()
for (i in 1:10000) {
  ind2 <- sample(1:nsim, 20, replace = FALSE)
  prelims <- dat[ind2, ]
  prelims <- as.data.frame(apply(prelims, 2, rank))
  test[i] <- cor(prelims$lead, prelims$bouldering, method = "spearman")
}

hist(test)
mean(test)
