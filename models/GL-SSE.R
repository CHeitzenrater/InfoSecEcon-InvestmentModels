###########################################################
#
# Implementation of the GL-SSE variant
#
########################

source("./GL.R")
source("./GL-Hausken.R")


GL_SSE <- function(sVul=1,z1,z2,t=1,lambda,delta=0,SPRE,SPOST,prePar,postPar){
	
	start = sVul-delta
	pre = start - do.call( SPRE, list(z1, start, as.numeric(as.character(prePar[1])), as.numeric(as.character(prePar[2]))) ) + delta
	#if(pre > 1){pre <- 1}
	#message(pre)
	
	post = pre - do.call( SPOST, list(z2, pre, as.numeric(as.character(postPar[1]))) )
	#message(post)
	
	return( post * (t*lambda) - (z1 + z2) );	
}


GL_SSE_S1_S2 <- function(sVul=1,z1,z2,t=1,lambda,delta=0,alpha1=1,beta1=1,alpha2=1){
	start = sVul-delta
	pre = start - S1(z1, start, alpha1, beta1) + delta
	#if(pre > 1){pre <- 1}
	#message(pre)
	
	post = pre - S2(z2, pre, alpha2) 
	#message(post)
		
	return( post * (t*lambda) - (z1 + z2) );	
}


#z1seq = seq(0,600000)
#deltaSeq = seq(0.01, 0.25, by=0.02)
#GL_SSE(1,1,3.7,10,0.1,S1,S2,list(1,1),list(1))
