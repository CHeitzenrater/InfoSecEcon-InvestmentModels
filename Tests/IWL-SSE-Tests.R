
source("./models/iwl-SSE.R", local=TRUE)


###
# Helper function --- creates a graph of costs
# revIter		number of iterations in the review phase
# testIter		number of iterations in the test phase
# revEff		effectiveness of the process in the review phase
# testEff		effectiveness of the process in the test phase
# revCost		cost per iteration of the process in the review phase
# testCost		cost per iteration of the process in the test phase
# sigmaMax		starting uncertainty (default=16)
# x0			starting attack cost (default=15, based on IWL)
# revMax
# testMax
#
SWProcessCosts <- function( revEff=1, testEff=1, revCost=1, testCost=1, sigmaMax=16, x0=15, revMax=10, testMax=10 ){

	u <- vector(mode="numeric", length=0)	# uncertainty values
	d <- vector(mode="numeric", length=0)	# attack gradient values
	costs <- vector(mode="numeric", length=0)
	y <- vector(mode="numeric", length=0)
			
	# sets the range for the x axis (attack gradient)
	dxMax <- sqrt(12*2+1)	

	for( i in 0:revMax ){
		for( j in 0:testMax ){
			vals <- CalculateSWProcessCosts( i, j, revEff, testEff, revCost, testCost, sigmaMax )
			u <- c(u, as.numeric(vals$sigma))
			d <- c(d, as.numeric(vals$dx))
			costs <- c(costs, as.numeric(vals$cost) )	
			y <- c(y, as.numeric(vals$sigma/vals$dx))
		}
	}
	sort(y)
	plot(costs,y,las=1,ylim=rev(range(0:16)), 
	  #ylab=expression(paste("Benefit: Normalised Uncertainty (", sigma, "/", Delta, "x)")),
	  ylab=" ", xlab = " ",
	  pch=15)	
}



CalculateOverallCostsSunk <- function( revIter=0, testIter=0, revEff=1, testEff=1, revCost=1, testCost=1, sigmaMax=16, x0=15, lambdaV=0 ){
	
	cr=0;		#phase costs for review
	ct=0;		#phase costs for test
	uncert=0;	#overall uncertainty
	deltaX=0;	#overall gradient of attack cost	
	
	### Review phase (AD), t = -2
	cr <- phaseCostRev( iter=revIter, iterCost=revCost, probSucc=revEff, fixCost=0.01 )
		
	### Test phase (IT), t = -1
	ct <- phaseCostTest( iter=testIter, iterCost=testCost, probSucc=testEff, prevSucc=revEff, prevIter=revIter, costBug=0.01, costFlaw=0.1 )
	
	# set overall costs, based on per-phase costs
	costs <- (cr + ct);
	### DEBUG
	#message("overall costs=",costs)
	
	# set overall Uncertainty, based on per-phase effectiveness & number of iterations
	uncert <- setOverallUncertainty(sigmaMax=sigmaMax, alpha=revEff, beta=testEff, revIter=revIter, testIter=testIter)
	
	# calculate gradient of attack, beased on per-phase effectiveness & number of iterations
	deltaX <- setGradientOfAttack( revEff=revEff, testEff=testEff, revIter=revIter, testIter=testIter )

	dynamicVals <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=deltaX,x0=15,sigma=uncert,lambda=lambdaV,cp=costs )
	#staticVals <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=deltaX,x0=15,sigma=uncert,cp=costs)
	
	#v <- as.numeric(dynamicVals$rev[dynamicVals$k+1])
	#message(dynamicVals$rev[dynamicVals$k+1])
		
	#####produceDynamicRevenueGraphs( initialCTB=15, uncert=uncert, processCosts=costs, revIter=revIter, testIter=testIter, deltaX=deltaX )
	#produceDynamicRevenueGraphs( initialCTB=15, uncert=uncert, processCosts=costs, revIter=revIter, testIter=testIter, deltaX=deltaX, dynamicVals=dynamicVals, staticVals=staticVals )	
	### this is the one to fix to make it work	
	
	return(dynamicVals)

	#return(list(vals=dynamicVals$rev[dynamicVals$k+1],k=k+1))	
}

CalculateOverallCostsRho <- function( revIter=0, testIter=0, revEff=1, testEff=1, revCost=1, testCost=1, sigmaMax=16, x0=15, rhoOne=0 ){
	
	cr=0;		#phase costs for review
	ct=0;		#phase costs for test
	uncert=0;	#overall uncertainty
	deltaX=0;	#overall gradient of attack cost	
	
	### Review phase (AD), t = -2
	cr <- phaseCostRev( iter=revIter, iterCost=revCost, probSucc=revEff, fixCost=0.01 )
		
	### Test phase (IT), t = -1
	ct <- phaseCostTest( iter=testIter, iterCost=testCost, probSucc=testEff, prevSucc=revEff, prevIter=revIter, costBug=0.01, costFlaw=0.1 )
	
	# set overall costs, based on per-phase costs
	costs <- (cr + ct);
	### DEBUG
	#message("overall costs=",costs)
	
	# set overall Uncertainty, based on per-phase effectiveness & number of iterations
	uncert <- setOverallUncertainty(sigmaMax=sigmaMax, alpha=revEff, beta=testEff, revIter=revIter, testIter=testIter)
	
	# calculate gradient of attack, beased on per-phase effectiveness & number of iterations
	deltaX <- setGradientOfAttack( revEff=revEff, testEff=testEff, revIter=revIter, testIter=testIter )

	dynamicVals <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=rhoOne,dx=deltaX,x0=15,sigma=uncert,lambda=0,cp=costs )
	#staticVals <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=deltaX,x0=15,sigma=uncert,cp=costs)
	
	#v <- as.numeric(dynamicVals$rev[dynamicVals$k+1])
	#message(dynamicVals$rev[dynamicVals$k+1])
		
	#####produceDynamicRevenueGraphs( initialCTB=15, uncert=uncert, processCosts=costs, revIter=revIter, testIter=testIter, deltaX=deltaX )
	#produceDynamicRevenueGraphs( initialCTB=15, uncert=uncert, processCosts=costs, revIter=revIter, testIter=testIter, deltaX=deltaX, dynamicVals=dynamicVals, staticVals=staticVals )	
	### this is the one to fix to make it work	
	
	return(dynamicVals)

	#return(list(vals=dynamicVals$rev[dynamicVals$k+1],k=k+1))	
}


Figure7Sunk <- function(){
	# Vectors to hold the values for various combinations of review and test
	vals0 <- vector(mode="numeric", length=0)
	#vals0_2 <- vector(mode="numeric", length=0)
	vals1 <- vector(mode="numeric", length=0)
	vals5 <- vector(mode="numeric", length=0)
	vals10 <- vector(mode="numeric", length=0)	
	vals25 <- vector(mode="numeric", length=0)
	#vals26 <- vector(mode="numeric", length=0)	
	
	# Baseline value --- no uncertainty, no SSE
	valsNo <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0, testEff=0, revCost=0, testCost=0, sigmaMax=0 )
	
	# Various combinations of review and test
	vals0 <- CalculateOverallCostsSunk( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=7, 0 )
	vals1 <- CalculateOverallCostsSunk( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=7,0 )
	vals5 <- CalculateOverallCostsSunk( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=7, 0.001 )
	vals10 <- CalculateOverallCostsSunk( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=7, 0.005 )
	vals20 <- CalculateOverallCostsSunk( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=7, 0.01 )
	# Should put in here a comparison with no process and sigma = 16
	
	# # Establishes the plot
	# layout(rbind(1,2), heights=c(6,1))
	# plot(NA,NA,las=1, 
		# xlab="Number of deployed proactive defences (k)",
		# ylab="Return on Secure Software Process (ROSSP)",
		# ylim=range(-15:20),xlim=c(0,25))	
	# rect(-80,-80,30,0,col=gray(.85),lty=3)
	# par(mai=c(0.92,0.96,0.82,0.42))
	
	# Draw the lines for various parameters
	# lines(0:25,ROSSP(vals0$rev,valsNo$rev),type="b",col="black",pch=12)
	# #lines(0:25,ROSSP(vals0_2$rev,valsNo$rev),type="b",col="black",pch=10)
	# lines(0:25,ROSSP(vals1$rev,valsNo$rev),type="b",col="black",pch=15)
	# lines(0:25,ROSSP(vals5$rev,valsNo$rev),type="b",col="black",pch=16)
	# lines(0:25,ROSSP(vals10$rev,valsNo$rev),type="b",col="black",pch=17)
	# lines(0:25,ROSSP(vals20$rev,valsNo$rev),type="b",col="black",pch=18)
	# #lines(0:25,ROSSP(vals26$rev,valsNo$rev),type="b",col="black",pch=8)
	
	
		# Establishes the plot	
	layout(rbind(1,2), heights=c(10,1))
	plot(0:25,valsNo$rev,las=1, 
		xlab="Number of deployed proactive defences (k)",
		ylab="Return on Security Investment (ROSI)",
		ylim=range(-15:50),xlim=c(0,25))	
	rect(-80,-80,30,0,col=gray(.85),lty=3)
	par(mai=c(0.92,0.96,0.82,0.42))
	
	lines(0:25,vals0$rev,type="b",col="black",pch=12)
	lines(0:25,vals1$rev,type="b",col="black",pch=15)
	lines(0:25,vals5$rev,type="b",col="black",pch=16)
	lines(0:25,vals10$rev,type="b",col="black",pch=17)
	lines(0:25,vals20$rev,type="b",col="black",pch=18)
	
	# Draws the legend
#	par(mar=c(0, 0, 0, 0))
#	plot.new()
#	legend("top", c("1 review, 1 test", "5 reviews, 1 test", "1 review, 5 tests", "8 reviews, 24 tests", "25 reviews, 25 tests"), 
#		pch=c(15,16,17,18,8), horiz=TRUE, bty="n", cex=0.75)
#	legend("center", c( expression(paste("0 reviews, 0 tests, ", sigma, " = 0"))	, expression(paste("0 reviews, 0 tests, ", sigma, " = 16"))	), pch=c(12,10), horiz=TRUE, bty="n", cex=0.75)

}





Figure7Rho <- function(){
	# Vectors to hold the values for various combinations of review and test
	vals0 <- vector(mode="numeric", length=0)
	#vals0_2 <- vector(mode="numeric", length=0)
	vals1 <- vector(mode="numeric", length=0)
	vals5 <- vector(mode="numeric", length=0)
	vals10 <- vector(mode="numeric", length=0)	
	vals25 <- vector(mode="numeric", length=0)
	#vals26 <- vector(mode="numeric", length=0)	
	
	# Baseline value --- no uncertainty, no SSE
	valsNo <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0, testEff=0, revCost=0, testCost=0, sigmaMax=0 )
	
	# Various combinations of review and test
	vals0 <- CalculateOverallCostsRho( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=16, 0 )
	vals1 <- CalculateOverallCostsRho( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=16,0 )
	vals5 <- CalculateOverallCostsRho( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=16, 1 )
	vals10 <- CalculateOverallCostsRho( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=16, 5 )
	vals20 <- CalculateOverallCostsRho( revIter=1, testIter=1, revEff=0.6, testEff=0.3, revCost=3, testCost=1, sigmaMax=16, 10 )
	# Should put in here a comparison with no process and sigma = 16
	
	# # Establishes the plot
	# layout(rbind(1,2), heights=c(6,1))
	# plot(NA,NA,las=1, 
		# xlab="Number of deployed proactive defences (k)",
		# ylab="Return on Secure Software Process (ROSSP)",
		# ylim=range(-15:20),xlim=c(0,25))	
	# rect(-80,-80,30,0,col=gray(.85),lty=3)
	# par(mai=c(0.92,0.96,0.82,0.42))
	
	# Draw the lines for various parameters
	# lines(0:25,ROSSP(vals0$rev,valsNo$rev),type="b",col="black",pch=12)
	# #lines(0:25,ROSSP(vals0_2$rev,valsNo$rev),type="b",col="black",pch=10)
	# lines(0:25,ROSSP(vals1$rev,valsNo$rev),type="b",col="black",pch=15)
	# lines(0:25,ROSSP(vals5$rev,valsNo$rev),type="b",col="black",pch=16)
	# lines(0:25,ROSSP(vals10$rev,valsNo$rev),type="b",col="black",pch=17)
	# lines(0:25,ROSSP(vals20$rev,valsNo$rev),type="b",col="black",pch=18)
	# #lines(0:25,ROSSP(vals26$rev,valsNo$rev),type="b",col="black",pch=8)
	
	
		# Establishes the plot	
	layout(rbind(1,2), heights=c(10,1))
	plot(0:25,valsNo$rev,las=1, 
		xlab="Number of deployed proactive defences (k)",
		ylab="Return on Security Investment (ROSI)",
		ylim=range(-15:50),xlim=c(0,25))	
	rect(-80,-80,30,0,col=gray(.85),lty=3)
	par(mai=c(0.92,0.96,0.82,0.42))
	
	lines(0:25,vals0$rev,type="b",col="black",pch=12)
	lines(0:25,vals1$rev,type="b",col="black",pch=15)
	lines(0:25,vals5$rev,type="b",col="black",pch=16)
	lines(0:25,vals10$rev,type="b",col="black",pch=17)
	lines(0:25,vals20$rev,type="b",col="black",pch=18)
	
	# Draws the legend
#	par(mar=c(0, 0, 0, 0))
#	plot.new()
#	legend("top", c("1 review, 1 test", "5 reviews, 1 test", "1 review, 5 tests", "8 reviews, 24 tests", "25 reviews, 25 tests"), 
#		pch=c(15,16,17,18,8), horiz=TRUE, bty="n", cex=0.75)
#	legend("center", c( expression(paste("0 reviews, 0 tests, ", sigma, " = 0"))	, expression(paste("0 reviews, 0 tests, ", sigma, " = 16"))	), pch=c(12,10), horiz=TRUE, bty="n", cex=0.75)

}



