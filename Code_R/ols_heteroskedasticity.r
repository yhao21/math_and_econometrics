library(lmtest)
library(car)





#------run BP test using LM (lagrange multiplier statistics)--------#	
#		LM = nR2 ~ chisq(K-1)
#		where 	n: total number of observations
#						R2: R square



df = read.csv('~/my_disk/git/data/sample_dataset.csv')
attach(df)

model = lm(y1~x1+x2)
summary(model)
ee = residuals(model)**2
aux_reg = lm(ee~x1+x2)
LM = summary(aux_reg)$r.squared * nrow(df)
dof = length(aux_reg$coef) - 1		# K-1 
c('LM', 'p-value')
c(LM, pchisq(LM, dof, lower.tail = F))


bptest(model)
#		test if my result is the same as built-in packages
#		data:  model
#		BP = 2.9155, df = 2, p-value = 0.2328





#------run White test using LM (lagrange multiplier statistics)--------#	
### White test just adds all second order terms to BP formula

aux_reg = lm(ee~x1+x2+c(x1**2)+c(x2**2)+x1*x2)
summary(aux_reg)
LM = nrow(df)*summary(aux_reg)$r.squared
LM
dof = length(aux_reg$coef) - 1		# K-1 
c('LM', 'p-value')
c(LM, pchisq(LM, dof, lower.tail = F))






