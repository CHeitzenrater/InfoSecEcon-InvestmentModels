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
s <- function( z=1, v, form ){
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
GL_ENBIS <- function( z=1, v, L ){
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



####################################################################################################
#
# GLZ16 Example
# Impelemntation of example within, "Investing in Cybersecurity: Insights from the Gordon-Loeb Model" by 
#  Lawrence A. Gordon, Martin P. Loeb, Lei Zhou, Journal of Information Security, 2016, 7, 49-59
#
########################

###
# Security breach reduction
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment" 
#
s_BPF <- function( z=1, form ){
	if( form == "GLZ16-LOW" ){
		return( round( 1/(1+z), digits=3 ) )
	} else if( form == "GLZ16-MED" ) {
		return( round( 1/(1+z)^2, digits=3 ) )
	} else if( form == "GLZ16-HIGH" ) {
		return( round( 1/(1+z)^3, digits=3 ) )
	}
}
s_BPF_Reduction <- function( z=1, form ){
	if( form == "GLZ16-LOW" ){
		return( s_BPF(z-1,"GLZ16-LOW") - s_BPF(z,"GLZ16-LOW") )
	} else if( form == "GLZ16-MED" ) {
		return( s_BPF(z-1,"GLZ16-MED") - s_BPF(z,"GLZ16-MED") )
	} else if( form == "GLZ16-HIGH" ) {
		return( s_BPF(z-1,"GLZ16-HIGH") - s_BPF(z,"GLZ16-HIGH") )
	}
}


## STEP 1
is <- c(20, 40, 60, 80, 100)
#infoSetValuesM <- c(20, 40, 60, 80, 100)
infoSetValuesM <- matrix( c(is,is,is,is), ncol=5, nrow=4, byrow=TRUE)

#STEP 2
scores <- c(0.2, 0.4, 0.6, 0.8)
vScores <- matrix( c(scores, scores, scores, scores, scores), nrow=4, ncol=5 )


#STEP 3
Figure2 <- matrix( vScores * infoSetValuesM, ncol=5, nrow=4 )

#STEP 4
unitOfInvestment <- 1000000

investmentRange <- c(0,1,2,3,4,5,6)

lowBPF <- s_BPF(investmentRange,"GLZ16-LOW")
medBPF <- s_BPF(investmentRange,"GLZ16-MED")
highBPF <- s_BPF(investmentRange,"GLZ16-HIGH")

lowBPFReduction <- s_BPF_Reduction(investmentRange,"GLZ16-LOW")
medBPFReduction <- s_BPF_Reduction(investmentRange,"GLZ16-MED")
highBPFReduction <- s_BPF_Reduction(investmentRange,"GLZ16-HIGH")

Figure3 <- matrix( c(lowBPF,lowBPFReduction,medBPF,medBPFReduction,highBPF,highBPFReduction), ncol=6, nrow=7 )

Figure3_1M <- matrix( c(lowBPFReduction[2], medBPFReduction[2], medBPFReduction[2], highBPFReduction[2]), ncol=5, nrow=4, )

Figure4 <- Figure2 * Figure3_1M





