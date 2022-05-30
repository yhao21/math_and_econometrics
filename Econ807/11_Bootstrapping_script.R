#uniform distribution. 300 numbers, range(0,100)
z <- runif(300,0,100)

B <- 1000
#n = original sample size
n <- length(z)
#使用matrix把列表变成矩阵，B行，n列
#each row is a bootstrap sample, so each bootstrap sample has 300 numbers
mean(z)
boot_samples <- matrix(sample(z,size=B*n,replace = TRUE),B,n)


#compute sample mean
boot_results <- apply(boot_samples, 1, mean)

sd(boot_results)
#sd(boot_results) [1] 1.671707


#standard error of mean
sd(z)/sqrt(length(z))


median(z)

#standard error of median
boot_samples <- matrix(sample(z,size=B*n,replace = TRUE),B,n)
boot_results <- apply(boot_samples, 1, median)
sd(boot_results)


# custom estimator
my_estimator <- function(z){
mean(z) + (1000/length(z))
}
boot_samples <- matrix(sample(z,size=B*n,replace = TRUE),B,n)
boot_results <- apply(boot_samples, 1, my_estimator)
sd(boot_results)


####################################################################

beta0 <- 1
beta1 <- 2
beta2 <- -3

x1 <- runif(1000,0,50)
x2 <- runif(1000,0,50)
epsilon <- rnorm(1000,0,1)

y <- beta0 + beta1*x1 + beta2*x2 + epsilon

df <- data.frame(y,x1,x2)
head(df)
#run regression
summary(lm(df$y~df$x1+df$x2))

#the std.Error is compute by formula. now let's use bootstrap to compute.

B <- 1000
n <- length(y)

#you need to draw the row(draw y and x together)
#1:n表示从1到n，即，整行draw
boot_sample <- lapply(1:B, function(x) df[sample(1:n, B, replace = TRUE),])

