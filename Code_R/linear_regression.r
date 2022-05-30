
get_statistics = function(beta, x, y){
  y_hat = x%*%beta
  ee = sum((y - y_hat)**2)
  # s2 = sigma2_hat = ee/(n-k), here sigma2_hat is the estimate of sigma2, since we cannot observe sigma2
  sigma2_hat = ee/(dim(x)[1] - dim(x)[2])			#### Var(e) = sigma2 = ee
  Var = sigma2_hat*solve(t(x)%*%x)
  beta_std_err_matrix = sqrt(Var)
  beta_std_err = diag(beta_std_err_matrix)
  t_value = beta/beta_std_err
  
  stats_result = cbind(beta, round(beta_std_err, 5), t_value)
  col_name = c('coef', 'std_err', 't_value')

  # use a list to return multiple variables
  results = list('stats'=stats_result, 'col_name' = col_name)

  return(results)
}


linear_regression = function(x,y,var_name, add_contant = T){
  if (add_contant == T){
    ones = rep(1, dim(x)[1])
    x = cbind(ones,x)
    var_name = c('const', var_name)
  }
  
  x = matrix(unlist(x), ncol = dim(x)[2])
  y = matrix(unlist(y), ncol = dim(y)[2])
  
  beta = (solve(t(x)%*%x)%*%t(x))%*%y
  stats_result = get_statistics(beta, x, y)
  
  #col_name = c('coef', 'std_err')
  colnames(stats_result$stats) = stats_result$col_name
  rownames(stats_result$stats) = var_name
  print(stats_result$stats)
}




#==============================================================

df = read.csv('~/my_disk/git/data/sample_dataset.csv')


y = df[1]
x = df[4:5]
var_name = c('x1','x2')
linear_regression(x,y,var_name)

model = lm(y1 ~ x1 + x2, data = df)
print(summary(model))




#-------test the equality of two estimators using t-test-------#	
##  test if coef of x1 equals to the coef of x2
#			Hypothesis:
#			x1 - x2 = 0, i.e., x1 = x2

# form t statistics   
# [(beta1 - beta2) - 0]/sqrt(Var(beta1 - beta2))
# Var(beta1 - beta2) = Var(beta1)+Var(beta2) - 2Cov(beta1, beta2)


beta = as.numeric(model$coef)
b1 = beta[2]
b2 = beta[3]
var_cov = vcov(model)
var_cov
v_b1 = var_cov[2,2]
v_b2 = var_cov[3,3]
v_b1_b2 = var_cov[2,3]
t_value = (b1 - b2)/(sqrt(v_b1+v_b2-2*v_b1_b2))
t_value  			#[1] -72.89334

