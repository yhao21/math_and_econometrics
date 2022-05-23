#
##--------------#	 Poisson
#
####--------### CDF
#x = seq(0, 21, length = 10000)
#png('figures/Poisson_cdf.png')
#plot(
#		 x,
#		 ppois(x,lambda = 4),
#		 type = 'l'
#)
#dev.off()
#
#
#
####--------### PDF
## Note, Posisson, x = 0,1,2,3,... so, length of x = b-a + 1
#x = seq(0,21, length = 22)
#png('figures/Poisson_pdf.png')
#plot(
#		 x,
#		 dpois(x, lambda = 10),
#		 type = 'h'
#)
#dev.off()
#
#
#
##--------------#	Beta
#
####--------### PDF
#
#x = seq(0,1, length = 10000)
## c(alpha, beta, line_type)
#parameters = list(c(14,14,2), c(1,4,3))
#legends = c(paste("alpha = 2", "beta = 4"))
#for (i in parameters){
#		legends = c(legends, paste("alpha = ", i[1], "beta = ", i[2]))
#}
#print(legends)
#
#
#png('figures/Beta_pdf.png')
#plot(
#		 x,
#		 dbeta(x,2,4),
#		 ylim = c(0,5),
#		 type = 'l',
#)
#for (item in parameters){
#		lines(
#					x,
#					dbeta(x,item[1],item[2]),
#					lty = item[3]
#		)
#}
#legend('topright',
#			 legend = legends,
#			 lty = c(1,2,3)
#)
#dev.off()
#
#
##--------------#	Uniform
####--------### PDF
## discrete
## x must be non-negative
#x = seq(0, 10, length = 11)
#png('figures/Uniform_pdf_discrete.png')
#plot(
#		 x,
#		 dunif(x, 1,5),
#		 type = 'h',
#		 ylim = c(0,1)
#)
#dev.off()
#
#
## continuous
## x must be non-negative
#x = seq(0, 10, length = 10000)
#png('figures/Uniform_pdf_continuous.png')
#plot(
#		 x,
#		 dunif(x, 1,5),
#		 type = 'l',
#		 ylim = c(0,1)
#)
#dev.off()
#
#
#
####--------### CDF
#
#x = seq(0, 10, length = 10000)
#png('figures/Uniform_cdf_continuous.png')
#plot(
#		 x,
#		 punif(x, 1,5),
#		 type = 'l',
#		 ylim = c(0,1)
#)
#dev.off()
#


#--------------#	Cauchy
###--------### pdf

x = seq(-5,5, length = 1000)
png('figures/Cauchy_pdf.png')
plot(
		 x,
		 dcauchy(x, location = 0, scale = 1),
		 type = 'l',
		 ylim = c(0,0.4)
)
dev.off()


# write cauchy density by myself
cauchy = function(location, scale, x){
		a = ((x - location)/scale)**2
		fx = 1/(pi*scale *(1+a))
		return(fx)
}


fx_list = c()
for(i in x){
		fx = cauchy(0,1,i)
		fx_list = c(fx_list, fx)

}
fx_list


png('figures/cauchy_pdf_manually.png')
plot(
		 x,
		 fx_list,
		 type = 'l'
)
lines(
			x,
			dcauchy(x, 1,1)
)
dev.off()





