#source("MLE.r")




df = read.csv('~/my_disk/git/econometrics/MLE/data/cps88.csv')
print(head(df))


df = log(df)
y = as.matrix(df[c('lnwage')])
x = as.matrix(df[c(2,3,4)])
#x = as.matrix(df[c(2,3)])
beta = rep(1, times = ncol(x))
par = c(1,1,beta)

ols = lm(df$lnwage~age+exp2+grade, data = df)
summary(ols)





llf = function(params){
		const = params[2]
		b = params[3:length(params)]
		xbeta = x%*%b
		inner_content = log(2 * pi) + log(params[1]**2) + ((y - const - xbeta)/params[1])**2
		-1/2 * sum(inner_content)
		#dnorm(y, mean = const + xbeta, sd = params[1], log=T)
}

mle = optim(par = par,
						fn = llf,
						method = "L-BFGS-B",          # this method lets set lower bounds
						#method = "",          # this method lets set lower bounds
        		#lower = 0.00001,              # lower limit for parameters
        		#lower = c(-Inf, 0),              # lower limit for parameters
        		control = list(fnscale = -1), # maximize the function
						hessian = T
)
mle$par


##-----## Inference

# inverse of the hessian matrix
variance_matrix = -solve(mle$hessian)
#variance_matrix


loglikelihood = mle$value                           # Loglik
coef = mle$par[2:length(mle$par)]                   # beta
# Calculate SE from variance_matrix
se = sqrt(diag(variance_matrix))[2:length(mle$par)] # the 1st col is se for sigma
t = (coef - 0)/se                                   # t-statistics


results = cbind(coef, se, t)
variables = c('const',colnames(x))
row.names(results) = variables

round(results,5)                                             # Regression results




