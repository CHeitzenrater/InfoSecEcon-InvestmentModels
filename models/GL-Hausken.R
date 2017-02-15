#########################################################################################
#
# Hausken Security Probability Breach Functions
# from: Hausken, K. Returns to information security investment: The effect of alternative
#   information security breach functions on optimal investment and sensitivity to
#   vulnerability. Information Systems Frontiers (2006), 338–349.
#
########################

source("./GL.R")

##########
# 
##########
# TODO --- proper comments for each of these 

S3 <- function( z=1, v=1, phi=1, gamma=1 ){
	return( v / (1 + ( gamma * (exp(phi*z) - 1) ) ) )
}

S4 <- function( z=1, v=1, mu=1, k=0.5 ){
	# and 0 < k < 1
	if( z > mu^(-1/k) ){
		return( 0 );
	} else {
		return( v * (1 - mu * z^k ) )
	}
}

S5 <- function( z=1, v=1, omega=1, k=2 ){
	# k > 1
	if( z > omega^(-1/k) ){
		return( 0 );
	} else {
		return( v * (1 - omega * z^k ) )
	}
}

S6 <- function( z=1, v=1, lambda=0.1 ){
	if( z > (1/lambda) ){
		return( 0 );
	} else {
		return( v * (1 - lambda * z ) )
	}
}


HauskenGraph_S <- function( v=0.5, l=16 ){
	investment = seq(0, 10, by=0.5)
	startingVul = v
	loss = l
	
	alpha = 0.5
	beta = 1.0
	phi = 1 
	gamma = 0.02
	k_s4 = (2/3)
	mu = 10^-k_s4
	k_s5 = 2
	omega = 10^-k_s5
	lambda = 0.1

	outS1 = S1(investment, startingVul, alpha, beta)	
	outS2 = S2(investment, startingVul, alpha)	
	outS3 = S3(investment, startingVul, phi, gamma)	
	outS4 = sapply(investment, S4, startingVul, mu, k_s4)	
	outS5 = sapply(investment, S5, startingVul, omega, k_s5)	
	outS6 = sapply(investment, S6, startingVul, lambda)	


	plot(investment, outS1, type="o", ylim=range(0,startingVul), lty=1, pch=15, ylab="S(z,v)", xlab="Investment")
	#abline(0, 0, col = "black")

	lines(investment, outS2, type="o", lty=1, pch=8)
	lines(investment, outS3, type="o", lty=1, pch=17)
	lines(investment, outS4, type="o", lty=1, pch=18)
	lines(investment, outS5, type="o", lty=1, pch=0)
	lines(investment, outS6, type="o", lty=1, pch=10)
	
	legend("top", c("S1", "S2", "S3", "S4", "S5", "S6"), lty=c(1,1,1,1,1,1), pch=c(15,8,17,18,0,10), horiz=TRUE, bty="n", cex=0.65)

}

HauskenGraph_ENBIS <- function(){
	investment = seq(0, 10, by=0.5)
	startingVul = 0.5
	loss = 16
	
	alpha = 0.5
	beta = 1.0
	phi = 1 
	gamma = 0.02
	k_s4 = (2/3)
	mu = 10^-k_s4
	k_s5 = 2
	omega = 10^-k_s5
	lambda = 0.1

	outS1 = (startingVul - S1(investment, startingVul, alpha, beta)) * loss - investment	
	outS2 = (startingVul - S2(investment, startingVul, alpha)) * loss - investment	
	outS3 = (startingVul - S3(investment, startingVul, phi, gamma)) * loss - investment
	outS4 = (startingVul - sapply(investment, S4, startingVul, mu, k_s4)) * loss - investment
	outS5 = (startingVul - sapply(investment, S5, startingVul, omega, k_s5)) * loss - investment
	outS6 = (startingVul - sapply(investment, S6, startingVul, lambda)) * loss - investment


	plot(investment, outS1, type="o", ylim=range(-3.5,2.5), lty=1, pch=15, ylab="S(z,v)", xlab="Investment")
	#abline(0, 0, col = "black")

	lines(investment, outS2, type="o", lty=1, pch=8)
	lines(investment, outS3, type="o", lty=1, pch=17)
	lines(investment, outS4, type="o", lty=1, pch=18)
	lines(investment, outS5, type="o", lty=1, pch=0)
	lines(investment, outS6, type="o", lty=1, pch=10)
	
	legend("top", c("S1", "S2", "S3", "S4", "S5", "S6"), lty=c(1,1,1,1,1,1), pch=c(15,8,17,18,0,10), horiz=TRUE, bty="n", cex=0.65)

}
