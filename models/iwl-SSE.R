################################################################################
##
## Implements the IWL-SSE model.  This work was published in the paper:
## [1] "The days before zero day: Investment models for secure software 
##  engineering" by C. Heitzenrater, R. Böhme, and A. C. Simpson.  
##  In Proceedings of the 15th Workshop on the Economics of Information Security 
##  (WEIS 2016) (June 2016).
##
## This code leverages code provided by Rainer Böhme, Feb 2016. (IWL-alt.R)
##  See file "iwl-Alt.R" for information on that code
##
################################################################################

## Dependent source files
source("./iwl-Alt.R",local=TRUE)


################################################################
#	IWL-SSE MODEL
################################################################ 

####
## Calculates the phase cost of review
## iter		Number of review iterations (default=0)
## iterCost	Cost per review iteration (default=1)
## probSucc	Probability of success of each review (effectiveness) (default=1)
## fixCost	Cost incurred when a review is successful
##
phaseCostRev <- function( iter=0, iterCost=1, probSucc=1, fixCost=1 ){
	
	### DEBUG
	#message("review=",(iter * iterCost) + iter * (probSucc * fixCost))
	
	return( (iter * iterCost) + iter * (probSucc * fixCost) )
}


####
## Calculates the phase cost of test
## iter		Number of test iterations (default=0)
## iterCost	Cost per test iteration (default=1)
## probSucc	Probability of success of each test (test effectiveness) (default=1)
## prevSucc	Probability of success of reviews (review effectiveness) (default=1)
## prevIter	The number of review iterations undertaken previously (default=0)
## costBug	Cost incurred when a bug is found
## costFlaw	Cost incurred when a flaw is found
##
phaseCostTest <- function( iter=0, iterCost=1, probSucc=1, prevSucc=1, prevIter=0, costBug=1, costFlaw=10 ){
	
	### DEBUG
	#message("test=",(iter * iterCost) + 
	# iter * (probSucc * ((costBug+costFlaw)/2)))
	
	return( (iter * iterCost) + 
	  iter * (probSucc * ( (costFlaw) / (2^(prevIter*prevSucc)) + costBug ) ) ) 
}


####
## Calculates the overall uncertaintly resulting after the execution of software 
##  process steps
## sigmaMax		starting uncertainty (default=16)
## alpha		effectiveness of reviews (default=1)
## beta 		effectiveness of tests (default=1)
## revIter		number of iterations in the review phase (default=0)
## testIter		number of iterations in the test phase (default=0)
##
setOverallUncertainty <- function( sigmaMax=16, alpha=1, beta=1, revIter=0, 
                                   testIter=0 ){
	return( sigmaMax <- sigmaMax - 
	  setPhaseUncertainty(sigmaPhase=(sigmaMax/2), eff=alpha, iter=revIter) - 
	  setPhaseUncertainty(sigmaPhase=(sigmaMax/2), eff=beta, iter=testIter) );
}


####
## Calculates the uncertaintly resulting after the execution of one phase of 
##   software process
## sigmaPhase	starting uncertainty for that phase (default=8)
## eff			effectiveness of the process in this phase (default=1)
## iter			number of iterations in this phase (default=0)
##
setPhaseUncertainty <- function( sigmaPhase=8, eff=1, iter=0 ){
		return( sigmaPhase * (eff) ^ (1/iter) )
}


####
## Calculates the current attack gradient 
## revEff		effectiveness of the process in the review phase (default=1)
## testEff		effectiveness of the process in the test phase (default=1)
## revIter		number of iterations in the review phase (default=0)
## testIter		number of iterations in the test phase (default=0)
##
setGradientOfAttack <- function( revEff=1, testEff=1, revIter=0, testIter=0 ){
	return( sqrt( 1 + revEff*revIter + testEff*testIter) )
}


####
## Calculates the costs of the software process 
## revIter		number of iterations in the review phase
## testIter		number of iterations in the test phase
## revEff		effectiveness of the process in the review phase
## testEff		effectiveness of the process in the test phase
## revCost		cost per iteration of the process in the review phase
## testCost		cost per iteration of the process in the test phase
## sigmaMax		starting uncertainty (default=16)
## x0			starting attack cost (default=15, based on IWL)
##
CalculateSWProcessCosts <- function( revIter=0, testIter=0, revEff=1, testEff=1, revCost=1, testCost=1, sigmaMax=16, x0=15 ){
	
	cr=0;		## phase costs for review
	ct=0;		## phase costs for test
	uncert=0;	## overall uncertainty
	deltaX=0;	## overall gradient of attack cost	
	
	## Review phase (AD), t = -2
	cr <- phaseCostRev( iter=revIter, iterCost=revCost, probSucc=revEff, 
	   fixCost=0.01 )
		
	## Test phase (IT), t = -1
	ct <- phaseCostTest( iter=testIter, iterCost=testCost, probSucc=testEff,
	   prevSucc=revEff, prevIter=revIter, costBug=0.01, costFlaw=0.1 )
	
	## set overall costs, based on per-phase costs
	costs <- (cr + ct);
	
	### DEBUG
	#message("overall costs=",costs)
	
	## set overall Uncertainty, based on per-phase effectiveness & 
	##  number of iterations
	uncert <- setOverallUncertainty(sigmaMax=sigmaMax, alpha=revEff,
	          beta=testEff, revIter=revIter, testIter=testIter)
	
	## calculate gradient of attack, beased on per-phase effectiveness & 
	## number of iterations
	deltaX <- setGradientOfAttack( revEff=revEff, testEff=testEff, 
	          revIter=revIter, testIter=testIter )
	
	## return the software process results as a list
	##	costs 	Costs incurred by the process
	##	sigma	The residual uncertainty after the process
	##	dx		The resulting attack gradient after the process 
	return(list(costs=costs,sigma=uncert,dx=deltaX))
}


###
# Calculates the costs of the software process in conjunction with the IWL
# revIter		number of iterations in the review phase
# testIter		number of iterations in the test phase
# revEff		effectiveness of the process in the review phase
# testEff		effectiveness of the process in the test phase
# revCost		cost per iteration of the process in the review phase
# testCost		cost per iteration of the process in the test phase
# sigmaMax		starting uncertainty (default=16)
# x0			starting attack cost (default=15, based on IWL)
#
CalculateOverallCosts <- function( revIter=0, testIter=0, revEff=1, testEff=1, revCost=1, testCost=1, sigmaMax=16, x0=15 ){
	
	cr=0;		## phase costs for review
	ct=0;		## phase costs for test
	uncert=0;	## overall uncertainty
	deltaX=0;	## overall gradient of attack cost	
	
	### Review phase (AD), t = -2
	cr <- phaseCostRev( iter=revIter, iterCost=revCost, probSucc=revEff, 
	      fixCost=0.01 )
		
	### Test phase (IT), t = -1
	ct <- phaseCostTest( iter=testIter, iterCost=testCost, probSucc=testEff,
	      prevSucc=revEff, prevIter=revIter, costBug=0.01, costFlaw=0.1 )
	
	## set overall costs, based on per-phase costs
	costs <- (cr + ct);
	
	### DEBUG
	#message("overall costs=",costs)
	
	## set overall Uncertainty, based on per-phase effectiveness & 
	##  number of iterations
	uncert <- setOverallUncertainty(sigmaMax=sigmaMax, alpha=revEff, 
	          beta=testEff, revIter=revIter, testIter=testIter)
	
	## calculate gradient of attack, beased on per-phase effectiveness & 
	##  number of iterations
	deltaX <- setGradientOfAttack( revEff=revEff, testEff=testEff, 
	          revIter=revIter, testIter=testIter )

	dynamicVals <- dynamic.revenue( n=25, a=1000, z=.025, r=.05,
	               rho=.1, dx=deltaX, x0, sigma=uncert, lambda=0, cp=costs )
	
	## DEBUG --- testing code
	#staticVals <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=deltaX,x0=15,sigma=uncert,cp=costs)
	
	#### Code commented out ######################################
	#v <- as.numeric(dynamicVals$rev[dynamicVals$k+1])
	#message(dynamicVals$rev[dynamicVals$k+1])
		
	#produceDynamicRevenueGraphs( initialCTB=15, uncert=uncert, 
	#  processCosts=costs, revIter=revIter, testIter=testIter, deltaX=deltaX )
	#produceDynamicRevenueGraphs( initialCTB=15, uncert=uncert, 
	# processCosts=costs, revIter=revIter, testIter=testIter, deltaX=deltaX, 
	# dynamicVals=dynamicVals, staticVals=staticVals )	
	### this is the one to fix to make it work
	
	## DEBUG
	#return(list(vals=dynamicVals$rev[dynamicVals$k+1],k=k+1))
	############################################################	
	
	return(dynamicVals)	
}


####
## Calculates the Return on Secure Software Process (ROSSP)
## ROSI_SSE		The return on investment with SSE
## ROSI_NOSSE	The return on investment with no SSE
##
ROSSP <- function( ROSI_SSE=0, ROSI_NOSSE=0 ){
	return( ROSI_SSE - ROSI_NOSSE )
}


################################################################
##	OPTIMAL INVESTMENTS
################################################################ 

####
## Uses CalculateOverallCosts in order to identify the optimal SSE investment
## maxRevIter	Maximum number of iterations in the review phase 
##			     (outer loop: 0 to max; default = 0)
## maxTestIter	Maximum number of iterations in the test phase 
##               (inner loop: 0 to max; default = 0)
## revEff		effectiveness of the process in the review phase (default=1)
## testEff		effectiveness of the process in the test phase (default=1)
## revCost		cost per iteration of the process in the review phase 
##				 (default=1)
## testCost		cost per iteration of the process in the test phase (default=1)
## sigmaMax		starting uncertainty (default=16)
## x0			starting attack cost (default=15, based on IWL)
##
FindOptimalInvestmentState <- function( maxRevIter=0, maxTestIter=0, revEff=1, testEff=1, revCost=1, testCost=1, sigmaMax=16, x0=15 ){
	
	dynamicVals <- CalculateOverallCosts( maxRevIter, maxTestIter, revEff,
	               testEff, revCost, testCost, sigmaMax, x0 )
	return(dynamicVals$rev[dynamicVals$k+1])
}


####
## Loops over a number of review and test iterations to identify the optimal 
## investment
## maxRevIter	Maximum number of iterations in the review phase 
##				 (outer loop: 0 to max; default = 0)
## maxTestIter	Maximum number of iterations in the test phase 
##				 (inner loop: 0 to max; default = 0)
## revEff		effectiveness of the process in the review phase (default=1)
## testEff		effectiveness of the process in the test phase (default=1)
## revCost		cost per iteration of the process in the review phase 
##				 (default=1)
## testCost		cost per iteration of the process in the test phase (default=1)
## sigmaMax		starting uncertainty (default=16)
## x0			starting attack cost (default=15, based on IWL)
##
FindOptimalSWInvestment <- function( maxRevIter=0, maxTestIter=0, revEff=1, testEff=1, revCost=1, testCost=1, sigmaMax=16, x0=15 ){
	
	## matrix to hold results
	vals <- matrix(data=NA,nrow=maxRevIter, ncol=maxTestIter)
	## number of initial defences
	k <- 0;			 
	
	## loop over reviews, then tests....
	for( i in 0:maxRevIter ){
		for( j in 0:maxTestIter ){
			## Set the i,j value equal to the effectiveness of the integrated 
			## IWL-SSE process for that number of reviews, phases
			vals[i,j] <- FindOptimalInvestmentState( i, j, revEff=revEff, 
			             testEff=testEff, revCost=revCost, testCost=testCost,
			             sigmaMax=sigmaMax, x0=x0 )
			### DEBUG
			#message("---- iteration ", i, " ", j)
			#message(vals[i+1,j+1])
		}		
	}
	
	## Identify the index that points to the maximal value of return
	ind <- which(vals == max(vals), arr.ind = TRUE)
	
	## Output the max value of reviews, tests and return
	paste("Best is ", ind[1], " review(s) and ", ind[2], 
	  " test(s), with return ", format(vals[ind],digits=8), sep="")
}



################################################################################
##
## LEGACY CODE
## NOTE: commented out --- produces previously used graph
##
################################################################################

################################################################ 
## UNUSED --- Used with old graph generation functions above
##
# produceDynamicRevenueGraphs <- function( initialCTB=15, uncert=1, 
# processCosts=0, revIter, testIter, deltaX=1, dynamicVals, staticVals ){
# 	plot(NA,NA,las=1, 
#  	 xlab=c("number of defenses in place (k)", 
#     paste("probAttack=", format(prob.tatt(n=1:25, x0=initialCTB, dx=deltaX, 
#	  sigma=uncert, a=1000,z=.025),digits=8),sep="")),
#    ylab=c( paste("Exp Return: cp =",format(processCosts,digits=8),", 
#     delta x=",format(deltaX,digits=8),", sigma=", format(uncert,digits=8), 
#     sep=""), paste("rev=",revIter,", test=",testIter,", dynamic= ", 
#	  format(max(dynamicVals$rev), digits=8),", static= ", 
#	  format(max(staticVals$rev), digits=8), sep="") ),
# 	ylim=range(-10:50),xlim=c(0,25) )	
#  rect(-80,-80,30,0,col=gray(.85),lty=3)
	
#  points(staticVals$k,staticVals$rev[staticVals$k+1],pch=16,cex=2,col="yellow")
#  lines(0:25,staticVals$rev,type="b",col="black",pch=16)	
	
# points(dynamicVals$k,dynamicVals$rev[dynamicVals$k+1],pch=15,cex=2,
#  col="yellow")
# lines(0:25,dynamicVals$rev,type="b",col="black",pch=15)
	
# legend("right","top", c("Static","Dynamic"), pch=c(16,15))
# Sys.sleep(0.2)
# }






