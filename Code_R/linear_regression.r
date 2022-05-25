
get_statistics = function(beta, x, y){
		y_hat = x%*%beta
		ee = sum((y - y_hat)**2)
		sigma2 = ee/(dim(x)[1] - dim(x)[2])
		Var = sigma2*solve(t(x)%*%x)
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
		print(head(x))
		print(head(y))
		print(var_name)

		if (add_contant == T){
				ones = rep(1, dim(x)[1])
				x = cbind(ones,x)
				var_name = c('const', var_name)
		}

		x = matrix(unlist(x), ncol = dim(x)[2])
		y = matrix(unlist(y), ncol = dim(y)[2])

		print(head(x))
		print(head(y))

		beta = (solve(t(x)%*%x)%*%t(x))%*%y
		print(beta)

		stats_result = get_statistics(beta, x, y)

		#col_name = c('coef', 'std_err')
		colnames(stats_result$stats) = stats_result$col_name
		rownames(stats_result$stats) = var_name
		print(stats_result$stats)
}




#==============================================================

df = read.csv('~/my_disk/git/data/sample_dataset.csv')

print(head(df))

head(df[2])
y = df[1]
x = df[4:5]
var_name = c('x1','x2')

linear_regression(x,y,var_name)

a = head(x)
