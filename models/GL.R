########################
#
# Base model
# Code based off version of model in, "Investing in Cybersecurity: Insights from the Gordon-Loeb Model" by 
#  Lawrence A. Gordon, Martin P. Loeb, Lei Zhou, Journal of Information Security, 2016, 7, 49-59
#
########################


###
# Security breach probability function S1
# Diminishing returns
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment" 
# alpha: control parameter
# beta: control parameter.  In [GLZ16], beta increases as 
#
S1 <- function( z=1, v=1, alpha=1, beta=1 ){
	return( v/((alpha*z) + 1)^beta )
}
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
# Security breach probability function S2
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment" 
# alpha: control parameter
#
S2 <- function( z=1, v=1, alpha ){
	return( v^( alpha * z + 1 ) )
}


###
# Expected Net Benefit of Cybersecurity (ENBIS) for the GL model
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment"
# L: "Potential loss (i.e., the cost of the breach), [...] expressed as a monetary value"
#
#
GL_ENBIS <- function( z=1, v=1, alpha=1, beta=1, L ){
	return( (v - S1(z,v,alpha,beta))*L - z )
}

###
# Expected Net Benefit of Cybersecurity (ENBIS) for the GL model
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# s_zv: already calculated value(s) for the loss function
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment"
# L: "Potential loss (i.e., the cost of the breach), [...] expressed as a monetary value"
#
#
GL_ENBIS_SZV <- function( z=1, v=1, s_zv=1, L ){
	return( (v - s_zv)*L - z )
}

####################################
# Supporting functions

###
# loss given vulnerability: "vL is equal to the expected loss prior to an investment in additional cybersecurity activities"
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# L: "Potential loss (i.e., the cost of the breach), [...] expressed as a monetary value"
#
loss <- function( v=1, L ){
	return( v*L )
}

