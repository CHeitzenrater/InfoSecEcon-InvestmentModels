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



GL_SSE_S3H_S2 <- function(sVul=1,z1,z2,t=1,lambda,delta=0,phi=1,gamma=1,alpha2=1){
	start = sVul-delta
	pre = start - S3H(z1, start, phi, gamma) + delta
	#if(pre > 1){pre <- 1}
	#message(pre)
	
	post = pre - S2(z2, pre, alpha2) 
	#message(post)
		
	return( post * (t*lambda) - (z1 + z2) );	
}


GL_SSE_S3H_S1 <- function(sVul=1,z1,z2,t=1,lambda,delta=0,phi=1,gamma=1,alpha=1,beta=1){
	start = sVul-delta
	pre = start - S3H(z1, start, phi, gamma) + delta
	#if(pre > 1){pre <- 1}
	#message(pre)
	
	post = pre - S1(z2, pre, alpha, beta) 
	#message(post)
		
	return( post * (t*lambda) - (z1 + z2) );	
}


############################################
## 
## Helper function for thesis/analysis
## Returns the arrays of vulnerability values
## Order: Vpre, Vpst, S1, S2
#
GL_SSE_S1_S2_retVs <- function(sVul=1,z1,z2,t=1,lambda,delta=0,alpha1=1,beta1=1,alpha2=1){
	start = sVul-delta
	firstZ = S1(z1, start, alpha1, beta1)
	#message(c("s1 = ", firstZ))
	pre = start - firstZ + delta
	#message(c("pre = ", pre))

	secondZ = S2(z2, pre, alpha2)
	#message(c("S2 = ", pre))	
	#plot(z1,secondZ)
	post = pre - secondZ
	#message(c("post = ", pre))	

	return( list(v1=pre,v2=post,v3=firstZ,v4=secondZ) );	
}

