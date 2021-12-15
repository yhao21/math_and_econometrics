


# generate data
set.seed(2)
e = rnorm(n = 100000, mean = 5, sd = 2)
x = rnorm(n = 100000, mean = 3, sd = 1)
y = 10 + 3 * x + e





#ols = lm(y~x)
#summary(ols)
#             Estimate Std. Error t value Pr(>|t|)
#(Intercept) 15.012837   0.020082   747.6   <2e-16 ***
#x            2.997774   0.006349   472.1   <2e-16 ***





## You must add a constant, otherwise it will not be the same as the one from OLS.
nLLF = function(params){
		# sigma: params[1], b: params[2], constant: params[3]
		miu = params[3] + params[2]*x   # miu = constant + beta * x.   Recall: E(y) = const + b*x = miu
		inner_content = log(2 * pi) + log(params[1]**2) + ((y - miu)/params[1])**2
		# negative LLF
		-1/2 * sum(inner_content)
}


set.seed(200)
MLE = optim(par = c(1,1,1), # initial values for mu and sigma
        fn = nLLF, # function to maximize, !!! do NOT put () after function name
        method = "L-BFGS-B", # this method lets set lower bounds
        lower = 0.00001, # lower limit for parameters
        control = list(fnscale = -1), # maximize the function
        hessian = T # calculate Hessian matricce because we will need for confidence intervals
        )
MLE




## Clearly, beta = 2.99774 from MLE is exactly the same as the one from OLS!!

#$par
#		  sigma      beta     constant
#		 params[1] params[2] params[3]
#[1]  2.001661  2.997774 15.012836
#
#$value
#[1] -211291.6
#
#$counts
#function gradient
#      28       28
#
#$convergence
#[1] 0
#
#$message
#[1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#
#$hessian
#              [,1]          [,2]          [,3]
#[1,] -4.991717e+04 -5.517359e-02 -1.832814e-02
#[2,] -5.517359e-02 -2.496658e+05 -7.491445e+04
#[3,] -1.832814e-02 -7.491445e+04 -2.495854e+04






# inverse of the hessian matrix
variance_matrix = -solve(MLE$hessian)
variance_matrix

# Calculate SE from variance_matrix
se = sqrt(diag(variance_matrix))
se
# Exactly the same as OLS
#	    	  sigma      beta     constant
#[1] 0.004475845 0.006349251 0.020081330













