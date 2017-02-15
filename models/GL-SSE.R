###########################################################
#
# Implementation of the GL-SSE variant
#
########################

source("./models/GL.R")
source("./models/GL-Hausken.R")


GL_SSE <- function(sVul=1,z1,z2,loss,delta,SPRE,SPOST,prePar,postPar){
	
	pre = sVul - SPRE(z1, sVul, preParams) + delta
	post = pre - SPOST(z2, pre, postParams)
	
	return( post * loss - (z1 + z2) );	
	
}

GL_SSE(1,3,4,5,0.1,S1,S2,list(z=1, v=1, alpha=1, beta=1),list(z=1, v=1, alpha=1))

## TO DO LIST:
# Figure out how to pass function names to other functions
# figure out how to pass lists of parameters 
# how do you mandate that the function is executed over all of a list?