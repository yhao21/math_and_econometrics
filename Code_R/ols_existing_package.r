library(car)



df = read.csv('~/my_disk/git/data/sample_dataset.csv')



model = lm(y1 ~ x1 + x2, data = df)
print(summary(model))

#-------test the equality of two estimators using F-test-------#	
## 	Joint hypothesis testing
##  test if coef of x1 equals to the coef of x2
linearHypothesis(model, c('x1=x2'))



#			            Estimate Std. Error t value Pr(>|t|)
#			(Intercept)  5.47081    0.01876  291.58   <2e-16 ***
#			x1           1.04023    0.01872   55.58   <2e-16 ***
#			x2           2.99147    0.01893  158.03   <2e-16 ***

#			Hypothesis:
#			x1 - x2 = 0
#
#			Model 1: restricted model   x1 = x2
#			Model 2: y1 ~ x1 + x2				full model
#
#			  Res.Df   RSS Df Sum of Sq      F    Pr(>F)
#			1   9998 53882
#			2   9997 35183  1     18700 5313.4 < 2.2e-16 ***
# F statistics is very large, reject the null. Great, it says coefs of x1 and x2 are not equal. It is consistent with the result of ols regression, where beta1 = 1.04, beta2=2.99
