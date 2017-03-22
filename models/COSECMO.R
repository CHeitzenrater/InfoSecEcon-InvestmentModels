# Implementation of the COSECMO models, based off: 
# Colbert, E. and Boehm, B.  "Cost Estimation for Secure Software & Systems", ISPA/SCEA 2008 Joint International Conference, 2008

# This code can be used to generate the data in the COSECMO_Data.R file. 


#### BASE COCOMO

# The 1.18 are multiplier factors to account for a 6% inception and 12% transition; typical COCOMO values
EffortDevel = val_EffortEC * 1.18

###
# A				Effort coefficient; nominally 2.94, can be calibrated to a specific organisation
# size			??
# val_E			The value for E, as defined by the E() function
# val_EM		The multiplication of the various effort multipliers
#
EffortEC <- function(A=2.94, size=1, val_E, val_EM=1){
	val_EffortEC = A * size * val_E * val_EM
}

###
# B 				Scaling exponent; nominally 0.91, can be calibrated to a specific organisation
# val_ScaleFactors	The summation of the scale factor values
#
E <- function( B=0.91, val_ScaleFactors=1 ){
	return( B + 0.01 * val_ScaleFactors )
}

Cost <- function( val_Effort, val_LaborRate ){
	return( val_Effort * val_LaborRate ) 
}


####### COSECMO Extensions

EffortAssuredDevel = val_EffortAssuredEC * 1.18

EffortAssuredEC = val_EffortEC + val_EffortIntAssure

###
# val_EffortEC			Effort in person-months
# val_EffortPercentAL	Percent additional effort for developer assurance, beyond an "ordinary-highly reliable system"
# AL is the assurance level, realted to the EAL
#
EffortIntAssure <- function( val_EffortEC, val_EffortPerAL ){
	return( val_EffortEC * val_EffortPerAL )
}

###
# effort3		percent additional effort at EAL 3; see Figure 1 of text
# SECU			the effort multiplier for a 1 EAL increase (i.e. Effort(EAL n+1)=Effort(EAL n)*SECU) for EAL > 3
# EAL 			Evaluation Assurance Level
#
EffortPercentAL <- function( effort3, SECU=2.5, EAL ){
	ret = 0
	if(EAL >= 3){
		ret = effort3 * SECU ^ (EAL-3);
	}
	return( ret )
}

TotalAssuredCost <- function( val_CostAssureDevel, val_CostIndAssure ){
	return( val_CostAssureDevel + val_CostIndAssure ) 
}











