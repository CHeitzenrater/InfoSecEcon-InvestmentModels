################################################################################
##
## Gordon & Loeb Base Model
##
## Code based off version of model in, "Investing in Cybersecurity: Insights 
##  from the Gordon-Loeb Model" by Lawrence A. Gordon, Martin P. Loeb, Lei Zhou, 
##  Journal of Information Security, 2016, 7, 49-59
## Original model in, "The economics of information security investment" by 
##  L. A. Gordon, and M. P. Loeb, ACM Transactions of Information Systems 
##  Security 5, 4 (November 2002),438–457.
##
################################################################################


################################################################################
## Security Breach Probability Functions
################################################################################

####
## Security breach probability function S1
## v:     vulnerability, "probability that a breach to a specific information 
##         set will occur under current conditions" 0 <= v <= 1
## z:     Investment in cybersecurity.  Assumed that "z will decrease v based 
##         on the productivity of the investment" 
## alpha: Parameter for productivity of the investment. In [GLZ16], alpha=1
## beta:  Parameter for productivity of the investment. In [GLZ16], beta 
##         increases as v increases [1,3] 
##
S1 <- function( z=1, v=1, alpha=1, beta=1 ){
	return( v/(((alpha*z) + 1)^beta) )
}
####
## Version of S1 that employs settings from [GLZ16]
## form:	  The qualitative name for the settings employed in [GLZ16].
##        ["GLZ16-LOW", "GLZ16-MED", "GLZ16-HIGH"] 
##
S1_Var <- function( z=1, v=1, form ){
	if( form == "GLZ16-LOW" ){
		alpha = 1;
		beta = 1;
	} else if( form == "GLZ16-MED" ) {
		alpha = 1;
		beta = 2;
	} else if( form == "GLZ16-HIGH" ) {
		alpha = 1;
		beta = 3;
	} 
	return( S1(z,v,alpha,beta) )
}


###
## Security breach probability function S2
## v:     vulnerability, "probability that a breach to a specific information 
##         set will occur under current conditions" 0 <= v <= 1
## z:     Investment in cybersecurity.  Assumed that "z will decrease v based 
##         on the productivity of the investment" 
## alpha: Parameter for productivity of the investment. 
##
S2 <- function( z=1, v=1, alpha ){
	return( v^( alpha * z + 1 ) )
}


################################################################################
## Expected Net Benefit of Information Security (ENBIS) Functions
################################################################################

####
## Expected Net Benefit of Information Security (ENBIS) for the GL model using 
##  security breach probability function S1 
## v:     Vulnerability, "probability that a breach to a specific information 
##         set will occur under current conditions" 0 <= v <= 1
## z:     Investment in cybersecurity.  Assumed that "z will decrease v based 
##         on the productivity of the investment" 
## t:     The threat level, as a percentage 0 <= t <= 1
## L:     "Potential loss (i.e., the cost of the breach), [...] expressed as a 
##         monetary value"
## alpha: Parameter for productivity of the investment
## beta:  Parameter for productivity of the investment
##
GL_ENBIS_S1 <- function( z=1, v=1, alpha=1, beta=1, t=1, L ){
	return( (v - S1(z,v,alpha,beta))*(t*L) - z )
}


####
## Expected Net Benefit of Information Security (ENBIS) for the GL model using 
##  security breach probability function S2 
## v:     Vulnerability, "probability that a breach to a specific information 
##         set will occur under current conditions" 0 <= v <= 1
## z:     Investment in cybersecurity.  Assumed that "z will decrease v based 
##         on the productivity of the investment" 
## t:     The threat level, as a percentage 0 <= t <= 1
## L:     "Potential loss (i.e., the cost of the breach), [...] expressed as a 
##         monetary value"
## alpha: Parameter for productivity of the investment
GL_ENBIS_S2 <- function( z=1, v=1, alpha=1, t=1, L ){
	return( (v - S2(z,v,alpha))*(t*L) - z )
}


###
# Version of the Expected Net Benefit of Cybersecurity (ENBIS) for the GL model
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# s_zv: already calculated value(s) for the loss function
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment"
# t: the threat level, as a percentage 0 <= t <= 1
# L: "Potential loss (i.e., the cost of the breach), [...] expressed as a monetary value"
#
GL_ENBIS_SZV <- function( z=1, v=1, s_zv=1, t=1, L ){
	return( (v - s_zv)*(t*L) - z )
}


################################################################################
## First Order Condition (FOC)
################################################################################

####
## First Order Condition (FOC) for the S1 security breach probability function.
## Returns the optimal value of z
## v:     Vulnerability, "probability that a breach to a specific information 
##         set will occur under current conditions" 0 <= v <= 1
## t:     The threat level, as a percentage 0 <= t <= 1
## L:     "Potential loss (i.e., the cost of the breach), [...] expressed as a 
##         monetary value"
## alpha: Parameter for productivity of the investment
## beta:  Parameter for productivity of the investment
##
GL_ZstarI <- function( v=1, alpha=1, beta=1, t=1, L ){
	return( ( ( (v * beta * alpha * (t*L) ) ^ (1/(beta+1) ) ) - 1 ) / alpha )
}
####
## HELPER FUNCTION: Supplies the v at the FOC for S1.  Same params as orig
##
GL_ZstarI_v <- function( v=1, alpha=1, beta=1, t=1, L ){
	foc_z = GL_ZstarI(v, alpha, beta, t, L)
	return( S1(foc_z, v, alpha, beta) )
}


####
## First Order Condition (FOC) for the S2 security breach probability function.
## Returns the optimal value of z
## v:     Vulnerability, "probability that a breach to a specific information 
##         set will occur under current conditions" 0 <= v <= 1
## t:     The threat level, as a percentage 0 <= t <= 1
## L:     "Potential loss (i.e., the cost of the breach), [...] expressed as a 
##         monetary value"
## alpha: Parameter for productivity of the investment
##
GL_ZstarII <- function( v=1, alpha=1, t=1, L ){
	return( (log(1/(-alpha * v * (t*L) * (log(v)) ))) / (alpha * log(v)) )
}
####
## HELPER FUNCTION: Supplies the v at the FOC for S1.  Same params as orig
##
GL_ZstarII_v <- function( v=1, alpha=1, t=1, L ){
	foc_z = GL_ZstarII(v, alpha, t, L)
	return( S2(foc_z, v, alpha) )
}


################################################################################
## Supporting Functions
################################################################################

####
## Loss given vulnerability.  From [GLZ16], "vL is equal to the expected loss 
##  prior to an investment in additional cybersecurity activities"
## v:     Vulnerability, "probability that a breach to a specific information 
##         set will occur under current conditions" 0 <= v <= 1
## L:     "Potential loss (i.e., the cost of the breach), [...] expressed as a 
##         monetary value"
loss <- function( v=1, L ){
	return( v*L )
}

