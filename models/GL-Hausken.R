################################################################################
##
## Hausken Security Probability Breach Functions.  from: 
## [1] "Returns to information security investment: The effect of alternative 
##  information security breach functions on optimal investment and sensitivity
##  to vulnerability" by K. Hausken. 
##  Information Systems Frontiers (2006), 338–349.
##
################################################################################

## Dependant files
source("./GL.R")

################################################################################
## Hausken Security Breach Probability Functions
################################################################################

####
## Hausken security breach probability function S3H; from [1]
## z:     Investment in cybersecurity
## v:     Vulnerability
## phi:   S3H parameter (productivity?)
## gamma: S3H parameter (productivity?)
## 
S3H <- function( z=1, v=1, phi=1, gamma=1 ){
	return( v / (1 + ( gamma * (exp(phi*z) - 1) ) ) )
}


####
## Hausken security breach probability function S4H; from [1]
## z:     Investment in cybersecurity
## v:     Vulnerability
## mu:    S3H parameter (productivity?)
## k:     S3H parameter (productivity?)
## 
S4H <- function( z=1, v=1, mu=1, k=0.5 ){
	# and 0 < k < 1
	if( z > mu^(-1/k) ){
		return( 0 );
	} else {
		return( v * (1 - mu * z^k ) )
	}
}


####
## Hausken security breach probability function S5H; from [1]
## z:     Investment in cybersecurity
## v:     Vulnerability
## omega: S3H parameter (productivity?)
## k:     S3H parameter (productivity?)
## 
S5H <- function( z=1, v=1, omega=1, k=2 ){
	# k > 1
	if( z > omega^(-1/k) ){
		return( 0 );
	} else {
		return( v * (1 - omega * z^k ) )
	}
}


####
## Hausken security breach probability function S6H; from [1]
## z:     Investment in cybersecurity
## v:     Vulnerability
## omega: S3H parameter (productivity?)
## k:     S3H parameter (productivity?)
## 
S6H <- function( z=1, v=1, lambda=0.1 ){
	if( z > (1/lambda) ){
		return( 0 );
	} else {
		return( v * (1 - lambda * z ) )
	}
}


################################################################################
## HELPER FUNCTIONS
################################################################################

####
## Reproduces graph of security breach probability functions from [1] to verify 
## correct operation
## v:	starting vulnerability
## l: 	loss
##
HauskenGraph_S <- function( v=0.5, l=16 ){
	investment = seq(0, 10, by=0.5)  # range of investment
	startingVul = v                  # starting vulnerability
	loss = l                         # loss
	
	## set up all the breach probability function parameters
	alpha = 0.5
	beta = 1.0
	phi = 1 
	gamma = 0.02
	k_s4 = (2/3)
	mu = 10^-k_s4
	k_s5 = 2
	omega = 10^-k_s5
	lambda = 0.1

	## Calculate the output for each function
	outS1 = S1(investment, startingVul, alpha, beta)	
	outS2 = S2(investment, startingVul, alpha)	
	outS3 = S3H(investment, startingVul, phi, gamma)	
	outS4 = sapply(investment, S4H, startingVul, mu, k_s4)	
	outS5 = sapply(investment, S5H, startingVul, omega, k_s5)	
	outS6 = sapply(investment, S6H, startingVul, lambda)	

	## Produce the graph
	plot(investment, outS1, type="o", ylim=range(0,startingVul), lty=1, pch=15,
	  ylab="S(z,v)", xlab="Investment")
	#abline(0, 0, col = "black")

	lines(investment, outS2, type="o", lty=1, pch=8)
	lines(investment, outS3, type="o", lty=1, pch=17)
	lines(investment, outS4, type="o", lty=1, pch=18)
	lines(investment, outS5, type="o", lty=1, pch=0)
	lines(investment, outS6, type="o", lty=1, pch=10)
	
	legend("top", c("S1", "S2", "S3", "S4", "S5", "S6"), lty=c(1,1,1,1,1,1), 
	  pch=c(15,8,17,18,0,10), horiz=TRUE, bty="n", cex=0.65)

}


####
## Reproduces graph of ENBIS from [1] to verify correct operation
## v:	starting vulnerability
## l: 	loss
##
HauskenGraph_ENBIS <- function(){
	investment = seq(0, 10, by=0.5)  # range of investment
	startingVul = 0.5				 # starting vulnerability
	loss = 16						 # loss
	
	## set up all the breach probability function parameters
	alpha = 0.5
	beta = 1.0
	phi = 1 
	gamma = 0.02
	k_s4 = (2/3)
	mu = 10^-k_s4
	k_s5 = 2
	omega = 10^-k_s5
	lambda = 0.1

	## Calculate the ENBIS for each of the security breach probability functions
	outS1 = (startingVul - 
	  S1(investment, startingVul, alpha, beta)) * loss - investment	
	outS2 = (startingVul - 
	  S2(investment, startingVul, alpha)) * loss - investment	
	outS3 = (startingVul - 
	  S3H(investment, startingVul, phi, gamma)) * loss - investment
	outS4 = (startingVul - 
	  sapply(investment, S4H, startingVul, mu, k_s4)) * loss - investment
	outS5 = (startingVul - 
	  sapply(investment, S5H, startingVul, omega, k_s5)) * loss - investment
	outS6 = (startingVul - 
	  sapply(investment, S6H, startingVul, lambda)) * loss - investment
	  
	## Produce graph
	plot(investment, outS1, type="o", ylim=range(-3.5,2.5), lty=1, pch=15, ylab="S(z,v)", xlab="Investment")
	#abline(0, 0, col = "black")

	lines(investment, outS2, type="o", lty=1, pch=8)
	lines(investment, outS3, type="o", lty=1, pch=17)
	lines(investment, outS4, type="o", lty=1, pch=18)
	lines(investment, outS5, type="o", lty=1, pch=0)
	lines(investment, outS6, type="o", lty=1, pch=10)
	
	legend("top", c("S1", "S2", "S3", "S4", "S5", "S6"), lty=c(1,1,1,1,1,1), pch=c(15,8,17,18,0,10), horiz=TRUE, bty="n", cex=0.65)

}
