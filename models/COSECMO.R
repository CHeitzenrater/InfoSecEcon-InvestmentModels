################################################################################
##
## Implementation of the COSECMO model, based off: 
## [1] Colbert, E. and Boehm, B.  "Cost Estimation for Secure Software & 
##   Systems", ISPA/SCEA 2008 Joint International Conference, 2008
##
## This code can be used to generate the data in the Data/COSECMO_Data.R file. 
##
################################################################################

################################################################################
## BASE COCOMO FUNCTIONS
################################################################################

####
## Calculate the effort in person-months for "Elaboration and Construction"
## phases of development
## A:	   Effort coefficient; nominally 2.94, can be calibrated to a specific 
##           organisation
## size:   Size of the codebase (LOC)
## val_E:  The value for E, as defined by the E() function
## val_EM: The multiplication of the various effort multipliers
#
EffortEC <- function(A=2.94, size=1, val_E, val_EM=1){
	val_EffortEC = A * size * val_E * val_EM
}

####
## Function for calculating the effort
## B 		         Scaling exponent; nominally 0.91, can be calibrated to a 
##					  specific organisation
## val_ScaleFactors: The summation of the scale factor values
##
E <- function( B=0.91, val_ScaleFactors=1 ){
	return( B + 0.01 * val_ScaleFactors )
}

####
## Calculation of the cost as the effort x labor rate
## val_Effort:    Value of the effort, in man-months
## val_LaborRate: Value of the labor rate, units undertermined (pounds, dollars)
##
Cost <- function( val_Effort, val_LaborRate ){
	return( val_Effort * val_LaborRate ) 
}


################################################################################
## COSECOMO EXTENSIONS
################################################################################

####
## Calculate effort for assured development, based on the effort for assured 
## elaboration and construction (EC) by a multiplicative factor
## val_EffAssuredEC:    Value for effort in assured eleboration and construction
## val_effAssureECMult: Multiplier factor to account for assured development.
##                       Nominally, this is set to 1.18 to accoutn for a 6% 
##						 inception and 12% transition; typical COCOMO values
##
EffortAssuredDevel <- function( val_EffAssuredEC, val_effAssureECMult = 1.18 ){
	return( val_EffAssuredEC * val_effAssureECMult ) 
}

####
## Calculate the effort on assured elaboration and construction, based on the 
## calculation of effort for internal assurance and the overall effort for 
## elaboration and construction
## val_EffortEC:		Effort in person-months
## val_EffortIntAssure:	Effort spent on internal assurance	
##
EffortAssureEC <- function( val_EffortEC, val_EffortIntAssure ){
	return( val_EffortEC + val_EffortIntAssure )
}

####
## Calculate the effort for internal assurance, as the effort for internal 
## assurance multiplied for a percentage increase in effort based on assurance
## level
## val_EffortEC:		Effort in person-months
## val_EffortPercentAL:	Percent additional effort for developer assurance, 
##                       "beyond an ordinary-highly reliable system"
##                      AL is the assurance level (e.g. realted to the EAL)
##
EffortIntAssure <- function( val_EffortEC, val_EffortPerAL ){
	return( val_EffortEC * val_EffortPerAL )
}

####
## Calculate the percentage increase in effort relative to the assurance level,
## for the EAl levels employed in Common Criteria
## effort3:	Percent additional effort at EAL 3; see Figure 1 of text
## SECU:	The effort multiplier for a 1 EAL increase 
##               (i.e. Effort(EAL n+1)=Effort(EAL n)*SECU) for EAL > 3
## EAL: 	Evaluation Assurance Level
##
EffortPercentAL <- function( effort3, SECU=2.5, EAL ){
	ret = 0
	if(EAL >= 3){
		ret = effort3 * SECU ^ (EAL-3);
	}
	return( ret )
}


################################################################################
## COSECOMO COST CALCULATIONS
################################################################################

####
## Calculate the cost of assured develeopment, given the base effort, 
## assurance  level, and labor rate; under the Common Criteria framework
## effortEC:  Base effort (in person-months) for development
## effort3:	  The percent additional effort for EAL 3 
##             (Common Criteria framework) 
## SECU:	  The effort multiplier for a one level increase in EAL 
##             (Common Criteria framework)
## EAL:		  Desired evaluation assurance level (Common Criteria framework)
## laborRate: The cost of labor
##
CalculateCostAssureDevel <- function( effortEC, effort3, SECU=2.5, EAL, laborRate ){
	eff_ia = EffortIntAssure( effortEC, EffortPercentAL( effort3, SECU, EAL) ) 
	eff_aEC = EffortAssureEC( effortEC, eff_ia )
	eff_aDevel = EffortAssuredDevel( eff_aEC )
	return( Cost( eff_aDevel, laborRate )  )	
}


####
## Calculate the internal costs of assured develeopment, given the base effort, 
## assurance  level, and labor rate; under the Common Criteria framework
## effortEC:  Base effort (in person-months) for development
## effort3:	  The percent additional effort for EAL 3 
##             (Common Criteria framework) 
## SECU:	  The effort multiplier for a one level increase in EAL 
##             (Common Criteria framework)
## EAL:		  Desired evaluation assurance level (Common Criteria framework)
## laborRate: The cost of labor
##
CalculateCostIntAssure <- function( effortEC, effort3, SECU=2.5, EAL, laborRate ){
	eff_ia = EffortIntAssure( effortEC, EffortPercentAL( effort3, SECU, EAL) ) 
	return( Cost( eff_ia, laborRate )  )
}


####
## Calculate the internal effort (person-months) for assured development
## effortEC:  Base effort (in person-months) for development
## effort3:	  The percent additional effort for EAL 3 
##             (Common Criteria framework) 
## SECU:	  The effort multiplier for a one level increase in EAL 
##             (Common Criteria framework)
## EAL:		  Desired evaluation assurance level (Common Criteria framework)
## laborRate: The cost of labor
##
CalculateEffIntAssure <- function( effortEC, effort3, SECU=2.5, EAL ){
	eff_ia = EffortIntAssure( effortEC, EffortPercentAL( effort3, SECU, EAL) ) 
	return( eff_ia )
}

####
## Calculate the total costs, given the costs for assured develeopment and 
##  independant assurance
## val_CostAssureDevel: Cost of assured development
## val_CostIndAssure:   Cost of independant assurance
##
TotalAssuredCost <- function( val_CostAssureDevel, val_CostIndAssure ){
	return( val_CostAssureDevel + val_CostIndAssure ) 
}




