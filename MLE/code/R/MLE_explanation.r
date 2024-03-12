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


ols = lm(y~x1+x2, data = df)
summary(ols)
#             Estimate Std. Error t value Pr(>|t|)
#(Intercept) 14.942816   0.109199   136.8   <2e-16 ***
#x1           3.027311   0.019755   153.2   <2e-16 ***
#x2           4.999703   0.009984   500.8   <2e-16 ***



##--------## MLE

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


#$par
#[1]  1.999067 14.942974  3.027330  4.999680
#
#$value
#[1] -21116.16
#
#$hessian
#              [,1]          [,2]          [,3]          [,4]
#[1,] -5.004628e+03  2.288198e-02  1.163953e-01 -2.367460e-02
#[2,]  2.288198e-02 -2.502333e+03 -7.505564e+03 -2.251966e+04
#[3,]  1.163953e-01 -7.505564e+03 -2.507564e+04 -6.757426e+04
#[4,] -2.367460e-02 -2.251966e+04 -6.757426e+04 -2.127004e+05





##-----## Inference



# inverse of the hessian matrix
variance_matrix = -solve(mle$hessian)
variance_matrix

# Calculate SE from variance_matrix

# Exactly the same as OLS
#      sigma       const      beta1      beta2
#[1] 0.01413560 0.10918322 0.01975200 0.00998243

#             Estimate Std. Error t value Pr(>|t|)
#(Intercept) 14.942816   0.109199   136.8   <2e-16 ***
#x1           3.027311   0.019755   153.2   <2e-16 ***
#x2           4.999703   0.009984   500.8   <2e-16 ***


loglikelihood = mle$value
coef = mle$par[2:length(mle$par)]
se = sqrt(diag(variance_matrix))[2:length(mle$par)] # the 1st col is se for sigma


t = (coef - 0)/se    # t-statistics
# [1] 136.8615 153.2670 500.8480

results = cbind(coef, se, t)
variables = c('const',colnames(x))
row.names(results) = variables

results




