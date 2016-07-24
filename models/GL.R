########################
#
# Base model
# Code based off version of model in, "Investing in Cybersecurity: Insights from the Gordon-Loeb Model" by 
#  Lawrence A. Gordon, Martin P. Loeb, Lei Zhou, Journal of Information Security, 2016, 7, 49-59
#
########################


###
# Security breach probability function
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment" 
#
s <- function( z=1, v=1, form ){
	if( form == "GLZ16-LOW" ){
		return( v/(1+z) )
	} else if( form == "GLZ16-MED" ) {
		return( v/(1+z)^2 )
	} else if( form == "GLZ16-HIGH" ) {
		return( v/(1+z)^3 )
	}
}


###
# Expected Net Benefit of Cybersecurity (ENBIS) for the GL model
# v: vulnerability, "probability that a breach to a specific information set will occur under current conditions"
#		0 <= v <= 1
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment"
# L: "Potential loss (i.e., the cost of the breach), [...] expressed as a monetary value"
#
#
GL_ENBIS <- function( z=1, v=1, L ){
	return( (v - s(z,v))*L - z )
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

