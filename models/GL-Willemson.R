#########################################################################################
#
# Willemson Security Probability Breach Functions
# from: Willemson, J. "On the Gordon \& Loeb Model for Information Security Investment"
#
########################

source("./GL.R")

##########
# 
##########
# TODO --- proper comments for each of these 
# TODO --- error conditions (relative to the )

S3W <- function( z=1, v=1, b=1, k=3 ){
	if( z >= b ){
		return( 0 );
	} else {
		return( v * (1 - (z/b)^k ) )
	}
}

S4W <- function( z=1, v=1, b=1, bprime=1, k=1 ){
	#Willemson does not give a formal example of S4 in the paper
	# if( z <= bprime ){
		# return( S3W(z,v,b,k) );
	# } else {
		# return( S3W(b,v,b,k) )
	# }
}

S5W <- function( z=1, v=1, b=1, k=2 ){
	if( z >= b ){
		return( 0 );
	} else {
		return( v * (1 - (z/b)^k ) )
	}
}




## TODO

WillemsonGraph_S <- function( v=0.5, l=16 ){
	# investment = seq(0, 10, by=0.5)
	# startingVul = v
	# loss = l
	
	# alpha = 0.5
	# beta = 1.0
	# phi = 1 
	# gamma = 0.02
	# k_s4 = (2/3)
	# mu = 10^-k_s4
	# k_s5 = 2
	# omega = 10^-k_s5
	# lambda = 0.1

	# outS1 = S1(investment, startingVul, alpha, beta)	
	# outS2 = S2(investment, startingVul, alpha)	
	# outS3 = S3H(investment, startingVul, phi, gamma)	
	# outS4 = sapply(investment, S4H, startingVul, mu, k_s4)	
	# outS5 = sapply(investment, S5H, startingVul, omega, k_s5)	
	# outS6 = sapply(investment, S6H, startingVul, lambda)	


	# plot(investment, outS1, type="o", ylim=range(0,startingVul), lty=1, pch=15, ylab="S(z,v)", xlab="Investment")
	# #abline(0, 0, col = "black")

	# lines(investment, outS2, type="o", lty=1, pch=8)
	# lines(investment, outS3, type="o", lty=1, pch=17)
	# lines(investment, outS4, type="o", lty=1, pch=18)
	# lines(investment, outS5, type="o", lty=1, pch=0)
	# lines(investment, outS6, type="o", lty=1, pch=10)
	
	# legend("top", c("S1", "S2", "S3", "S4", "S5", "S6"), lty=c(1,1,1,1,1,1), pch=c(15,8,17,18,0,10), horiz=TRUE, bty="n", cex=0.65)

}

WillensonGraph_ENBIS <- function(){
	# investment = seq(0, 10, by=0.5)
	# startingVul = 0.5
	# loss = 16
	
	# alpha = 0.5
	# beta = 1.0
	# phi = 1 
	# gamma = 0.02
	# k_s4 = (2/3)
	# mu = 10^-k_s4
	# k_s5 = 2
	# omega = 10^-k_s5
	# lambda = 0.1

	# outS1 = (startingVul - S1(investment, startingVul, alpha, beta)) * loss - investment	
	# outS2 = (startingVul - S2(investment, startingVul, alpha)) * loss - investment	
	# outS3 = (startingVul - S3H(investment, startingVul, phi, gamma)) * loss - investment
	# outS4 = (startingVul - sapply(investment, S4H, startingVul, mu, k_s4)) * loss - investment
	# outS5 = (startingVul - sapply(investment, S5H, startingVul, omega, k_s5)) * loss - investment
	# outS6 = (startingVul - sapply(investment, S6H, startingVul, lambda)) * loss - investment


	# plot(investment, outS1, type="o", ylim=range(-3.5,2.5), lty=1, pch=15, ylab="S(z,v)", xlab="Investment")
	# #abline(0, 0, col = "black")

	# lines(investment, outS2, type="o", lty=1, pch=8)
	# lines(investment, outS3, type="o", lty=1, pch=17)
	# lines(investment, outS4, type="o", lty=1, pch=18)
	# lines(investment, outS5, type="o", lty=1, pch=0)
	# lines(investment, outS6, type="o", lty=1, pch=10)
	
	# legend("top", c("S1", "S2", "S3", "S4", "S5", "S6"), lty=c(1,1,1,1,1,1), pch=c(15,8,17,18,0,10), horiz=TRUE, bty="n", cex=0.65)

}
