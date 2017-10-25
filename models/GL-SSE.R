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
	
	v_pre 	= sVul-delta		# Set starting vulnerability
	
	
	## Calculate the pre-deployment vulnerability reduction using the supplied 
	##  security breach probability function SPRE and parameters prePar
	## TODO: this needs to be generalised properly
	v_dep 	= 	v_pre - do.call( SPRE, list(z1, v_pre, 
				as.numeric(as.character(prePar[1])), 
				as.numeric(as.character(prePar[2]))) ) + delta
	## DEBUG
	# message(pre)

	## Calculate the post-deployment vulnerability reduction using the supplied 
	##  security breach probability function SPOST and parameters postPar
	## TODO: this needs to be generalised properly	
	v_pst = pre - do.call( SPOST, list(z2, v_dep, 
			as.numeric(as.character(postPar[1]))) )
	## DEBUG
	# message(post)
	
	## Return ENBIS value: Vulnerability * Threat * Loss - Investment 
	return( v_pst * (t*lambda) - (z1 + z2) );	
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
	
	v_pre = sVul-delta		# Set starting vulnerability
	
	## Pre-deployment vulnerability reduction provided by z1 under 
	##  $S_{pre} = S^{I}$ 
	#v_dep = (v_pre - S1(z1, v_pre, alpha1, beta1)) + delta
	v_dep = S1(z1, v_pre, alpha1, beta1) + delta
	
	## DEBUG
	# message(v_dep)
	
	## Post-deployment vulnerability reduction provided by z2 under 
	##  $S_{post} = S^{II}$ 	
	#v_pst = v_dep - S2(z2, v_dep, alpha2) 
	v_pst = S2(z2, v_dep, alpha2) 
	
	## DEBUG
	# message(v_pst)
		
	## Return ENBIS value: Vulnerability * Threat * Loss - Investment 	
	return( (sVul - v_pst) * (t*lambda) - (z1 + z2) );	
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
	v_pre = sVul-delta			# Set starting vulnerability
	
	## Pre-deployment vulnerability reduction provided by z1 under 
	##  $S_{pre} = S^{IIIH}$ 
	v_dep = v_pre - S3H(z1, v_pre, phi, gamma) + delta
	## DEBUG
	# message(v_dep)
	
	## Post-deployment vulnerability reduction provided by z2 under 
	##  $S_{post} = S^{II}$ 	
	v_pst = v_dep - S2(z2, v_dep, alpha2) 
	## DEBUG
	# message(v_pst)
	
	## Return ENBIS value: Vulnerability * Threat * Loss - Investment 	
	return( v_pst * (t*lambda) - (z1 + z2) );	
} ### TODO - update


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
	v_pre = sVul-delta     # Set starting vulnerability
	
	v_dep = v_pre - S3H(z1, v_pre, phi, gamma) + delta
	
	v_pst = v_dep - S1(z2, v_dep, alpha, beta) 
		
	return( v_pst * (t*lambda) - (z1 + z2) );	
} ## TODO -- update


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
	v_pre = sVul - delta   # Set starting vulnerability
	
	firstS = S1(z1, v_pre, alpha1, beta1)
	#v_dep = (v_pre - firstS) + delta
	v_dep = firstS + delta

	secondS = S2(z2, v_dep, alpha2)
	v_pst = sVul - secondS    #v_dep - secondS	

	return( list(v1=v_dep,v2=v_pst,v3=firstS,v4=secondS) );	
}

