
#==============================================================

df = read.csv('~/my_disk/git/data/sample_dataset.csv')


y = df[1]
x = df[4:5]
var_name = c('x1','x2')
model = lm(y1 ~ x1 + x2, data = df)
print(summary(model))




#-------test the equality of two estimators using F-test-------#	
##  test if coef of x1 equals to the coef of x2
#			Hypothesis:
#			x1 - x2 = 0, i.e., x1 = x2
#		matrix form:
#		(0 1 -1)(beta0 beta1 beta2)' = (0)
#
#			Model 1: restricted model   x1 = x2
#			Model 2: y1 ~ x1 + x2				full model
#
#			  Res.Df   RSS Df Sum of Sq      F    Pr(>F)
#			1   9998 53882
#			2   9997 35183  1     18700 5313.4 < 2.2e-16 ***




R = t(matrix(c(0,1,-1)))
beta = matrix(as.numeric(model$coef))
r = matrix(0)
m = 1


### You should NEVER forget to add constant column to x matrix!!!!
ones = rep(1, dim(x)[1])
x = cbind(ones,x)
x = matrix(unlist(x), ncol = dim(x)[2])	# add constant column to x

numerator = (t(R%*%beta-r) %*% solve(R %*% solve(t(x)%*%x) %*% t(R)) %*% (R%*%beta-r))/m
denominator = summary(model)$sigma ** 2
f_value = numerator/denominator		#[1,] 5313.439 This F value is correct
f_value







