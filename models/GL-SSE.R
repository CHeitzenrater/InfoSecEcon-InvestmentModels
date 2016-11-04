###########################################################
#
# Implementation of the GL-SSE variant
#
########################

source("./models/GL.R")
source("./models/GL-Hausken.R")


GL-SSE <- function( starting_v=1, z_1, z_2, loss, S_PRE, S_POST pre_params, post_params ){
	
	pre = starting_v - S_PRE(z_1, startingVul, pre_params)
	post = pre - S_POST(z_2, pre, post_params)
	
	return( post * loss - (z_1 + z_2) );	
	
}

## TO DO LIST:
# Figure out how to pass function names to other functions
# figureout how to pass lists of parameters 
# how do you mandate that the function is executed over all of a list?