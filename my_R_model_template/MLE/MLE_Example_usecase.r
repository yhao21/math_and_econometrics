source("MLE.r")


set.seed(2)
e = rnorm(n = 10, mean = 5, sd = 2)
x1 = rnorm(n = 10, mean = 3, sd = 1)
x2 = rnorm(n = 10, mean = 9, sd = 2)
y = 10 + 3 * x1 + 5*x2 + e


df = data.frame(
								y = y,
								x1 = x1,
								x2 = x2
)

## prepare x and y
x = as.matrix(df[2:ncol(df)])   # x is a matrix extracting from df
y = as.matrix(df[1])
beta = rep(10,times = ncol(x))  # generate beta column
par = c(1,1,beta)               # generate initial guess c(sigma, const, beta)

mle = optim(par = par,
						fn = llf,											# Here, llf comes from my module: MLE.r
						method = "L-BFGS-B",          # this method lets set lower bounds
        		lower = 0.00001,              # lower limit for parameters
        		control = list(fnscale = -1), # maximize the function
						hessian = T
)
mle
