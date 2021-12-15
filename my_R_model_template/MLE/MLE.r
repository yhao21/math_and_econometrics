### Usage:
#  import this module by using
#  		source("MLE.r")
#  
#  Then, write your own mle = optim(), you can directly use "llf"
#  		mlt = optim(par = par, fn = llf)
#  


# generate data
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



##--------## MLE

## Change x and y when you are using this model

x = as.matrix(df[2:ncol(df)])   # x is a matrix extracting from df
y = as.matrix(df[1])
beta = rep(10,times = ncol(x))  # generate beta column
par = c(1,1,beta)               # generate initial guess c(sigma, const, beta)




llf = function(params){
		const = params[2]
		b = params[3:length(params)]
		xbeta = x%*%b
		inner_content = log(2 * pi) + log(params[1]**2) + ((y - const - xbeta)/params[1])**2
		-1/2 * sum(inner_content)
}

mle = optim(par = par,
						fn = llf,
						method = "L-BFGS-B",          # this method lets set lower bounds
        		lower = 0.00001,              # lower limit for parameters
        		control = list(fnscale = -1), # maximize the function
						hessian = T
)
mle


##-----## Inference

# inverse of the hessian matrix
variance_matrix = -solve(mle$hessian)
variance_matrix


loglikelihood = mle$value                           # Loglik
coef = mle$par[2:length(mle$par)]                   # beta
# Calculate SE from variance_matrix
se = sqrt(diag(variance_matrix))[2:length(mle$par)] # the 1st col is se for sigma
t = (coef - 0)/se                                   # t-statistics


results = cbind(coef, se, t)
variables = c('const',colnames(x))
row.names(results) = variables

results                                             # Regression results




