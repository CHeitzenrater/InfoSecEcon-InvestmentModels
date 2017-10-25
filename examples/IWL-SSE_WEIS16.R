################################################################################
##
## Generates the IWL-SSE graphs for the paper:
## [1] "The days before zero day: Investment models for secure software 
##  engineering" by C. Heitzenrater, R. Böhme, and A. C. Simpson.  
##  In Proceedings of the 15th Workshop on the Economics of Information Security 
##  (WEIS 2016) (June 2016).
##
################################################################################

## Required source files
#source("./models/iwl-SSE.R", local=TRUE)

################################################################################
##
## Generate plots
##
################################################################################

####
## Examines the space of potential values (senitivity analysis) and produces 
##  the graph of the ROI for established numbers of reviews and tests.
## sigmaMax: Starting uncertainty value (default is 16)
## x0:		Starting attack gradient (default is 15)
##
Figure6 <- function( sigmaMax=16, x0=15 ){
	u <- vector(mode="numeric", length=0)	## uncertainty values
	d <- vector(mode="numeric", length=0)	## attack gradient values
			
	## sets the range for the x axis (attack gradient)
	dxMax <- sqrt(12*2+1)	
	#dxMin <- 1				
	
	## layout with two parts: graph (top) and ledgend (bottom)
	layout(rbind(1,2), heights=c(8,2))
	plot(NA,NA,las=1, 
	xlab=expression(paste("Attack Gradient (", Delta, "x)")),
	ylab=expression(paste("Normalised Uncertainty (", sigma, "/", Delta, "x)")), 	  # | Uncertainty (sigma) / delta X",
	  ylim=range(0:sigmaMax),xlim=range(0.75:dxMax))

	## Calculate various points within the trade-space
	vals <- CalculateSWProcessCosts( revIter=0, testIter=0, revEff=0, testEff=0,
	  revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
			
	## points with best outcomes 
	vals <- CalculateSWProcessCosts( revIter=0, testIter=0, revEff=0.001, 
	  testEff=0.001, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
					
	vals <- CalculateSWProcessCosts( revIter=0, testIter=0, revEff=0.99, 
	  testEff=0.99, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
				
	vals <- CalculateSWProcessCosts( revIter=1, testIter=1, revEff=0.0001, 
	  testEff=0.0001, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
			
	vals <- CalculateSWProcessCosts( revIter=2, testIter=2, revEff=0.35, 
	  testEff=0.35, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
					
	vals <- CalculateSWProcessCosts( revIter=1, testIter=1, revEff=0.99,
	  testEff=0.99, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
		
	vals <- CalculateSWProcessCosts( revIter=100, testIter=100, revEff=0,
	  testEff=0, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
			
	vals <- CalculateSWProcessCosts( revIter=100, testIter=100, revEff=0.0001,
	  testEff=0.0001, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
					
	vals <- CalculateSWProcessCosts( revIter=10, testIter=10, revEff=0.9999, 
	  testEff=0.9999, revCost=1, testCost=1, sigmaMax=sigmaMax )
	u <- c(u, as.numeric(vals$sigma))
	d <- c(d, as.numeric(vals$dx))
	
	## Convert the uncertainty and attack gradient values into matricies		
	u <- matrix(u,3,3,byrow=TRUE)
	d <- matrix(d,3,3,byrow=TRUE)
	
	#### DEBUG	
	#message(u)
	#message(d)
		
	abline(v=1,untf=FALSE,col="grey")
	abline(h=0,untf=FALSE,col="grey")

	### Unused points; left for exploration later
	#lines(d[1,1],u[1,1],type="b",col="red",pch=8)	
	#lines(d[1,2],u[1,2],type="b",col="red",pch=8)
	#lines(d[1,3],u[1,3],type="b",col="red",pch=8)
	#lines(d[3,1],u[3,1],type="b",col="red",pch=8)	
			
	## Sets the points on the graph that correspond to the text
	lines(d[2,1],u[2,1]/d[2,1],type="b",col="black",pch=15)	# upper left
	lines(d[2,2],u[2,2]/d[2,2],type="b",col="black",pch=8)	# "expected" point
	lines(d[2,3],u[2,3]/d[2,3],type="b",col="black",pch=16)	# lower left 
	lines(d[3,2],u[3,2]/d[3,2],type="b",col="black",pch=17)	# lower left
	lines(d[3,3],u[3,3]/d[3,3],type="b",col="black",pch=18)	# lower right

	## Mark the four points on the graph by number (corresponding to the 
	##  notional uncertainty-gradient graph) in the previous section of 
	##  the paper
	text(4.45,15,labels=c("4"),cex=2.5) 
	#expression(paste("4: Defined By Initial ",Delta,"x"))))
	text(1.25,15,labels=c("1"),cex=2.5)
	text(1.25,1.1,labels=c("2"),cex=2.5)
	text(4.45,1.1,labels=c("3"),cex=2.5)			
		
	## Set up the legend for the graph
	par(mar=c(0, 0, 0, 0))
	plot.new()
	legend("top", c( "1 review & 1 test, both 0.01% effective",
	                 "1 review & 1 test, both 99.99% effective"), 
	  pch=c(15,16), horiz=TRUE, bty="n", cex=0.85)	
	legend("center", c( "10 reviews & 10 tests, both 0.01% effective",
	                    "10 reviews & 10 tests, both 99.99% effective"), 
	  pch=c(17,18), horiz=TRUE, bty="n", cex=0.85)			
	legend("bottom", c( "2 reviews & 2 tests, both 35% effective"), pch=c(8), 
	  horiz=TRUE, bty="n", cex=0.85)			
}


####
## Evaluates the ROSI for a series of SSE process values, and produces the 
##  graph of ROSI values in Section 5
## Evaluated points:
## revIter=0,testIter=0,revEff=0,testEff=0,revCost=0,testCost=0,sigmaMax=0 
##   (baseline)
## revIter=0,testIter=0,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigmaMax=0 
## revIter=0,testIter=0,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=1,testIter=1,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=5,testIter=1,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=1,testIter=5,revEff=0.6,testEff=0.3,revCost=3,testCost=1, sigMax=16 
## revIter=8,testIter=24,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=25,testIter=25,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
##
Figure7 <- function(){
	## Vectors to hold the values for various combinations of review and test
	vals0 <- vector(mode="numeric", length=0)
	vals1 <- vector(mode="numeric", length=0)
	vals5 <- vector(mode="numeric", length=0)
	vals10 <- vector(mode="numeric", length=0)	
	vals25 <- vector(mode="numeric", length=0)
	vals26 <- vector(mode="numeric", length=0)	
	
	## Baseline value --- no uncertainty, no SSE
	valsNo <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0, 
	  testEff=0, revCost=0, testCost=0, sigmaMax=0 )

	## Various combinations of review and test
	vals0 <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0.6,
	 testEff=0.3, revCost=3, testCost=1, sigmaMax=0 )
	vals0_2 <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0.6,
	 testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals1 <- CalculateOverallCosts( revIter=1, testIter=1, revEff=0.6, 
	 testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals5 <- CalculateOverallCosts( revIter=5, testIter=1, revEff=0.6,
	 testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals10 <- CalculateOverallCosts( revIter=1, testIter=5, revEff=0.6, 
	 testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals25 <- CalculateOverallCosts( revIter=8, testIter=24, revEff=0.6, 
	 testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals26 <- CalculateOverallCosts( revIter=25, testIter=25, revEff=0.6,
	 testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )

	## Establish the plot	
	layout(rbind(1,2), heights=c(10,1))
	plot(0:25,valsNo$rev,las=1, 
		xlab="Number of deployed proactive defences (k)",
		ylab="Return on Security Investment (ROSI)",
		ylim=range(-15:50),xlim=c(0,25))	
	rect(-80,-80,30,0,col=gray(.85),lty=3)
	par(mai=c(0.92,0.96,0.82,0.42))

	## Highlights the optimal points for each parameter set
	points(vals0$k,vals0$rev[vals0$k+1],pch=16,cex=2,col="yellow")
	points(vals0_2$k,vals0_2$rev[vals0_2$k+1],pch=15,cex=2,col="yellow")
	points(vals1$k,vals1$rev[vals1$k+1],pch=15,cex=2,col="yellow")	
	points(vals5$k,vals5$rev[vals5$k+1],pch=16,cex=2,col="yellow")
	points(vals10$k,vals10$rev[vals10$k+1],pch=17,cex=2,col="yellow")
	points(vals26$k,vals26$rev[vals26$k+1],pch=16,cex=2,col="yellow")
	points(vals25$k,vals25$rev[vals25$k+1],pch=18,cex=2,col="yellow")

	## Draw the lines for various parameters
	lines(0:25,vals0$rev,type="b",col="black",pch=12)
	lines(0:25,vals0_2$rev,type="b",col="black",pch=10)
	lines(0:25,vals1$rev,type="b",col="black",pch=15)
	lines(0:25,vals5$rev,type="b",col="black",pch=16)
	lines(0:25,vals10$rev,type="b",col="black",pch=17)
	lines(0:25,vals25$rev,type="b",col="black",pch=18)
	lines(0:25,vals26$rev,type="b",col="black",pch=8)	
	
	message("0r and 0t sigma 0 ", vals0$rev[vals0$k+1], " ", vals0$k)
	message("0r and 0t sigma 16 ", vals0_2$rev[vals0_2$k+1], " ", vals0_2$k)	
	message("1r and 1t ", vals1$rev[vals1$k+1], " ", vals1$k)	
	message("5r and 1t ", vals5$rev[vals5$k+1], " ", vals5$k)
	message("1r and 5t ", vals10$rev[vals10$k+1], " ", vals10$k)
	message("8r and 24t ", vals25$rev[vals25$k+1], " ", vals25$k)
	message("25r and 25t ", vals26$rev[vals26$k+1], " ", vals26$k)	
		
	## Draws the legend		
	par(mar=c(0, 0, 0, 0))
	plot.new()
	legend("top", c( "1 review, 1 test", "5 reviews, 1 test", 
	                 "1 review, 5 tests", "8 reviews, 24 tests", 
	                 "25 reviews, 25 tests"), 
	  pch=c(15,16,17,18,8), horiz=TRUE, bty="n", cex=0.75)
	legend("center", c( expression(paste("0 reviews, 0 tests, ",
	  sigma," = 0")),   expression(paste("0 reviews, 0 tests, ", 
	  sigma, " = 16"))), pch=c(12,10), horiz=TRUE, bty="n", cex=0.75)			
}


####
## Evaluates the ROSSP for a series of SSE process values, and produces the
##  graph of ROSSP values in Section 5
## Evaluated points:
## revIter=0,testIter=0,revEff=0,testEff=0,revCost=0,testCost=0,sigmaMax=0 
##   (baseline)
## revIter=0,testIter=0,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigmaMax=0 
## revIter=0,testIter=0,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=1,testIter=1,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=5,testIter=1,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=1,testIter=5,revEff=0.6,testEff=0.3,revCost=3,testCost=1, sigMax=16 
## revIter=8,testIter=24,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
## revIter=25,testIter=25,revEff=0.6,testEff=0.3,revCost=3,testCost=1,sigMax=16 
##	
Figure8 <- function(){
	## Vectors to hold the values for various combinations of review and test
	vals0 <- vector(mode="numeric", length=0)
	vals0_2 <- vector(mode="numeric", length=0)
	vals1 <- vector(mode="numeric", length=0)
	vals5 <- vector(mode="numeric", length=0)
	vals10 <- vector(mode="numeric", length=0)	
	vals25 <- vector(mode="numeric", length=0)
	vals26 <- vector(mode="numeric", length=0)	
	
	## Baseline value --- no uncertainty, no SSE
	valsNo <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0, testEff=0, revCost=0, testCost=0, sigmaMax=0 )
	
	## Various combinations of review and test
	vals0 <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=0 )
	vals0_2 <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals1 <- CalculateOverallCosts( revIter=1, testIter=1, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals5 <- CalculateOverallCosts( revIter=5, testIter=1, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals10 <- CalculateOverallCosts( revIter=1, testIter=5, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals25 <- CalculateOverallCosts( revIter=8, testIter=24, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	vals26 <- CalculateOverallCosts( revIter=25, testIter=25, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )
	
	## Establishes the plot
	layout(rbind(1,2), heights=c(6,1))
	plot(NA,NA,las=1, 
		xlab="Number of deployed proactive defences (k)",
		ylab="Return on Secure Software Process (ROSSP)",
		ylim=range(-15:20),xlim=c(0,25))	
	rect(-80,-80,30,0,col=gray(.85),lty=3)
	par(mai=c(0.92,0.96,0.82,0.42))
	
	## Draw the lines for various parameters
	lines(0:25,ROSSP(vals0$rev,valsNo$rev),type="b",col="black",pch=12)
	lines(0:25,ROSSP(vals0_2$rev,valsNo$rev),type="b",col="black",pch=10)
	lines(0:25,ROSSP(vals1$rev,valsNo$rev),type="b",col="black",pch=15)
	lines(0:25,ROSSP(vals5$rev,valsNo$rev),type="b",col="black",pch=16)
	lines(0:25,ROSSP(vals10$rev,valsNo$rev),type="b",col="black",pch=17)
	lines(0:25,ROSSP(vals25$rev,valsNo$rev),type="b",col="black",pch=18)
	lines(0:25,ROSSP(vals26$rev,valsNo$rev),type="b",col="black",pch=8)
	
	# #Draws the legend
	par(mar=c(0, 0, 0, 0))
	plot.new()
	legend("top", c( "1 review, 1 test", "5 reviews, 1 test", 
	                 "1 review, 5 tests", "8 reviews, 24 tests", 
	                 "25 reviews, 25 tests"), 
	  pch=c(15,16,17,18,8), horiz=TRUE, bty="n", cex=0.75)
	legend("center", c( expression(paste("0 reviews, 0 tests, ", 
	  sigma, " = 0")),  expression(paste("0 reviews, 0 tests, ", 
	  sigma, " = 16"))), pch=c(12,10), horiz=TRUE, bty="n", cex=0.75)
	
	##Calculate the ROSSP values
	a <- ROSSP(vals0$rev[vals0$k+1],vals0$rev[vals0$k+1])
	b <- ROSSP(vals0_2$rev[vals0_2$k+1],vals0$rev[vals0$k+1])
	c <- ROSSP(vals1$rev[vals1$k+1],vals0$rev[vals0$k+1])
	d <- ROSSP(vals5$rev[vals5$k+1],vals0$rev[vals0$k+1])
	e <- ROSSP(vals10$rev[vals10$k+1],vals0$rev[vals0$k+1])
	f <- ROSSP(vals25$rev[vals25$k+1],vals0$rev[vals0$k+1])
	g <- ROSSP(vals26$rev[vals26$k+1],vals0$rev[vals0$k+1])
	
	## Output the ROSSP values to the user
	message("0r and 0t sigma 0 ", a, " ", vals0$k)
	message("0r and 0t sigma 16 ", b, " ", vals0_2$k)	
	message("1r and 1t ", c, " ", vals1$k)	
	message("5r and 1t ", d, " ", vals5$k)
	message("1r and 5t ", e, " ", vals10$k)
	message("8r and 24t ", f, " ", vals25$k)
	message("25r and 25t ", g, " ", vals26$k)	
}


################################################################################
## HELPER FUNCTIONS
################################################################################

####
## Evaluates the baseline values 
##  (sanity check --- not used in paper)
##
evaluateOrig <- function(){
	## Vectors to hold the values for various combinations of review and test
	vals0 <- vector(mode="numeric", length=0)
	vals0_2 <- vector(mode="numeric", length=0)

	## Baseline value --- no uncertainty, no SSE
	valsNo <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0, 
	  testEff=0, revCost=0, testCost=0, sigmaMax=0 )

	## Various combinations of review and test
	vals0 <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0.6, 
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=0 )
	vals0_2 <- CalculateOverallCosts( revIter=0, testIter=0, revEff=0.6,
	  testEff=0.3, revCost=3, testCost=1, sigmaMax=16 )

	## Establishes the plot	
	layout(rbind(1,2), heights=c(10,1))
	plot(0:25,valsNo$rev,las=1, 
		xlab="Number of deployed proactive defences (k)",
		ylab="Return on Security Investment (ROSI)",
		ylim=range(-15:50),xlim=c(0,25))	
	rect(-80,-80,30,0,col=gray(.85),lty=3)
	par(mai=c(0.92,0.96,0.82,0.42))

	## Highlights the optimal points for each parameter set
	points(vals0$k,vals0$rev[vals0$k+1],pch=16,cex=2,col="yellow")
	points(vals0_2$k,vals0_2$rev[vals0_2$k+1],pch=15,cex=2,col="yellow")

	## Draw the lines for various parameters
	lines(0:25,vals0$rev,type="b",col="black",pch=12)
	lines(0:25,vals0_2$rev,type="b",col="black",pch=10)
	
	#### DEBUG
	#message("0r and 0t sigma 0 ", vals0$rev[vals0$k+1], " ", vals0$k)
	#message("0r and 0t sigma 16 ", vals0_2$rev[vals0_2$k+1], " ", vals0_2$k)	
		
	## Draws the legend		
	par(mar=c(0, 0, 0, 0))
	plot.new()
	legend("center", c( expression(paste("0 reviews, 0 tests, ", 
	  sigma, " = 0")),  expression(paste("0 reviews, 0 tests, ", 
	  sigma, " = 16"))), pch=c(12,10), horiz=TRUE, bty="n", cex=0.75)	
}

