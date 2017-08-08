################################################################################
##
## Gordon & Loeb Secure Software Engineering (GL-SSE) model
## 
## In support of the paper: C. Heitzenrater & A. Simpson, "Modelling Software 
##   Security Investment", planned for submission to ICSE 2018 (under PA review)
##
################################################################################

## Dependent source files
source("./GL.R")
source("./GL-Hausken.R")
source("./GL-Willemson.R")

################################################################################
## ENBIS function
################################################################################

####
## Implements the GL-SSE ENBIS Function
##
## sVul:	Starting vulnerability; default=1
## z1:		SwSec (pre-deployment) investment  						
## z2:		Post-deployment investment
## t:		Threat; default=1
## lambda:	Loss													
## delta:	Component of vulnerability for configuration issues; default=0.1
## SPRE:	Function to be employed for $S_{pre}$
## SPOST:	Function to be employed for $S_{post}$
## prePar:  Parameter list for the function SPRE
## postPar:	Parameter list for the function SPOST
##
GL_SSE <- function(sVul=1,z1,z2,t=1,lambda,delta=0,SPRE,SPOST,prePar,postPar){
	
	start 	= sVul-delta		# Set starting vulnerability
	
	## Calculate the pre-deployment vulnerability reduction using the supplied 
	##  security breach probability function SPRE and parameters prePar
	## TODO: this needs to be generalised properly
	pre 	= 	start - do.call( SPRE, list(z1, start, 
				as.numeric(as.character(prePar[1])), 
				as.numeric(as.character(prePar[2]))) ) + delta
	## DEBUG
	# message(pre)

	## Calculate the post-deployment vulnerability reduction using the supplied 
	##  security breach probability function SPOST and parameters postPar
	## TODO: this needs to be generalised properly	
	post = 	pre - do.call( SPOST, list(z2, pre, 
			as.numeric(as.character(postPar[1]))) )
	## DEBUG
	# message(post)
	
	## Return ENBIS value: Vulnerability * Threat * Loss - Investment 
	return( post * (t*lambda) - (z1 + z2) );	
}


################################################################################
## Specific combinations of $S_{pre}$ and $S_{post}$
################################################################################

####
## Earned Net Benefit (ENBIS) for GL-SSE with $S_{pre} = S^{I}$ and 
##  $S_{post} = S^{II}$
## sVul:	Starting vulnerability; default=1
## z1:		SwSec (pre-deployment) investment  						
## z2:		Post-deployment investment
## t:		Threat; default=1
## lambda:	Loss													
## delta:	Component of vulnerability for configuration issues; default=0.1
## alpha1:	Value for $\alpha$ in $S_{pre}=S^{I}$; default=1
## beta:	Value for $\beta$ in $S_{pre}=S^{I}$; default=1
## alpha2:	Value for $\alpha$ in $S_{post}=S^{II}$; default=1
##
GL_SSE_S1_S2 <- function(sVul=1,z1,z2,t=1,lambda,delta=0,alpha1=1,beta1=1,alpha2=1){
	
	start = sVul-delta		# Set starting vulnerability
	
	## Pre-deployment vulnerability reduction provided by z1 under 
	##  $S_{pre} = S^{I}$ 
	pre = start - S1(z1, start, alpha1, beta1) + delta
	## DEBUG
	# message(pre)
	
	## Post-deployment vulnerability reduction provided by z2 under 
	##  $S_{post} = S^{II}$ 	
	post = pre - S2(z2, pre, alpha2) 
	## DEBUG
	# message(post)
		
	## Return ENBIS value: Vulnerability * Threat * Loss - Investment 	
	return( post * (t*lambda) - (z1 + z2) );	
}


####
## Earned Net Benefit (ENBIS) for GL-SSE with $S_{pre} = S^{IIIH}$ and 
##  $S_{post} = S^{II}$
## sVul:	Starting vulnerability; default=1
## z1:		SwSec (pre-deployment) investment  						
## z2:		Post-deployment investment
## t:		Threat; default=1
## lambda:	Loss													
## delta:	Component of vulnerability for configuration issues; default=0.1
## phi:		Value for $\phi$ in $S_{pre}=S^{IIIH}$; default=1
## gamma:	Value for $\gamma$ in $S_{pre}=S^{IIIH}$; default=1
## alpha2:	Value for $\alpha$ in $S_{post}=S^{II}$; default=1
##
GL_SSE_S3H_S2 <- function(sVul=1,z1,z2,t=1,lambda,delta=0,phi=1,gamma=1,alpha2=1){
	start = sVul-delta			# Set starting vulnerability
	
	## Pre-deployment vulnerability reduction provided by z1 under 
	##  $S_{pre} = S^{IIIH}$ 
	pre = start - S3H(z1, start, phi, gamma) + delta
	## DEBUG
	# message(pre)
	
	## Post-deployment vulnerability reduction provided by z2 under 
	##  $S_{post} = S^{II}$ 	
	post = pre - S2(z2, pre, alpha2) 
	## DEBUG
	# message(post)
	
	## Return ENBIS value: Vulnerability * Threat * Loss - Investment 	
	return( post * (t*lambda) - (z1 + z2) );	
}


####
## Earned Net Benefit (ENBIS) for GL-SSE with $S_{pre} = S^{IIIH}$ and 
##  $S_{post} = S^{I}$
## sVul:	Starting vulnerability; default=1
## z1:		SwSec (pre-deployment) investment  						
## z2:		Post-deployment investment
## t:		Threat; default=1
## lambda:	Loss													
## delta:	Component of vulnerability for configuration issues; default=0.1
## phi:		Value for $\phi$ in $S_{pre}=S^{IIIH}$; default=1
## gamma:	Value for $\gamma$ in $S_{pre}=S^{IIIH}$; default=1
## alpha:	Value for $\alpha$ in $S_{pst}=S^{I}$; default=1
## beta:	Value for $\beta$ in $S_{pst}=S^{I}$; default=1
##
GL_SSE_S3H_S1 <- function(sVul=1,z1,z2,t=1,lambda,delta=0,phi=1,gamma=1,alpha=1,beta=1){
	start = sVul-delta     # Set starting vulnerability
	
	pre = start - S3H(z1, start, phi, gamma) + delta
	
	post = pre - S1(z2, pre, alpha, beta) 
		
	return( post * (t*lambda) - (z1 + z2) );	
}


################################################################################
## HELPER FUNCTIONS
################################################################################

####
## 
## Returns the arrays of vulnerability values
## Order: Vpre, Vpst, S1 output, S2 output
## sVul:	Starting vulnerability; default=1
## z1:		SwSec (pre-deployment) investment  						
## z2:		Post-deployment investment
## t:		Threat; default=1
## lambda:	Loss													
## delta:	Component of vulnerability for configuration issues; default=0.1
## alpha1:	Value for $\alpha$ in $S_{pre}=S^{I}$; default=1
## beta:	Value for $\beta$ in $S_{pre}=S^{I}$; default=1
## alpha2:	Value for $\alpha$ in $S_{post}=S^{II}$; default=1
##
GL_SSE_S1_S2_retVs <- function(sVul=1,z1,z2,t=1,lambda,delta=0,alpha1=1,beta1=1,alpha2=1){
	start = sVul-delta   # Set starting vulnerability
	
	firstZ = S1(z1, start, alpha1, beta1)
	pre = start - firstZ + delta

	secondZ = S2(z2, pre, alpha2)
	post = pre - secondZ	

	return( list(v1=pre,v2=post,v3=firstZ,v4=secondZ) );	
}

