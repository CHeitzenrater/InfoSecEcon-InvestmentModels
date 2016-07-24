####################################################################################################
#
# GLZ16 Example
# Impelemntation of example within, "Investing in Cybersecurity: Insights from the Gordon-Loeb Model" by 
#  Lawrence A. Gordon, Martin P. Loeb, Lei Zhou, Journal of Information Security, 2016, 7, 49-59
#
########################

### Requires source("../models/GL.R")

###
# Security breach reduction; returns the difference between the value of 
# z: Investment in cybersecurity.  Assumed that "z will decrease v based on the productivity of the investment" 
# v: 
# form: Form for s to use; see definition of s() in GL.R for forms
#
s_Reduction <- function( z=1, v=1, form ){
	return( s(z-1, v, form) - s(z, v, form) )
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
vConst <- 1

investmentRange <- c(0,1,2,3,4,5,6)

lowBPF <-  round( s(investmentRange, vConst, "GLZ16-LOW"),  3 )
medBPF <-  round( s(investmentRange, vConst, "GLZ16-MED"),  3 )
highBPF <- round( s(investmentRange, vConst, "GLZ16-HIGH"), 3 ) 

lowBPFReduction <-  round( s_Reduction(investmentRange, vConst, "GLZ16-LOW"), 3 )
medBPFReduction <-  round( s_Reduction(investmentRange, vConst, "GLZ16-MED"), 3 )
highBPFReduction <- round( s_Reduction(investmentRange, vConst, "GLZ16-HIGH"), 3 )

Figure3 <- matrix( c(lowBPF,lowBPFReduction,medBPF,medBPFReduction,highBPF,highBPFReduction), ncol=6, nrow=7 )

Figure3_1M <- matrix( c(lowBPFReduction[2], medBPFReduction[2], medBPFReduction[2], highBPFReduction[2]), ncol=5, nrow=4, )
Figure4 <- Figure2 * Figure3_1M

Figure3_2M <- matrix( c(lowBPFReduction[3], medBPFReduction[3], medBPFReduction[3], highBPFReduction[3]), ncol=5, nrow=4, )
Figure5 <- Figure2 * Figure3_2M

Figure3_3M <- matrix( c(lowBPFReduction[4], medBPFReduction[4], medBPFReduction[4], highBPFReduction[4]), ncol=5, nrow=4, )
Figure6 <- Figure2 * Figure3_3M

Figure3_4M <- matrix( c(lowBPFReduction[5], medBPFReduction[5], medBPFReduction[5], highBPFReduction[5]), ncol=5, nrow=4, )
Figure7 <- Figure2 * Figure3_4M


investmentLevel <- matrix( ">4M", ncol=5, nrow=4 )
for(i in 1:5){
	for(j in 1:4){
		if(Figure7[j,i] < 1){ investmentLevel[j,i] <- "<4M" }
		if(Figure7[j,i] == 1){ investmentLevel[j,i] <- "4M" }
		if(Figure6[j,i] < 1){ investmentLevel[j,i] <- "<3M" }
		if(Figure6[j,i] == 1){ investmentLevel[j,i] <- "3M" }
		if(Figure5[j,i] < 1){ investmentLevel[j,i] <- "<2M" }
		if(Figure5[j,i] == 1){ investmentLevel[j,i] <- "2M" }
		if(Figure4[j,i] < 1){ investmentLevel[j,i] <- "<1M" }
		if(Figure4[j,i] == 1){ investmentLevel[j,i] <- "1M" }		
	}
}

print(investmentLevel)
