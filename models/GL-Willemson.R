################################################################################
##
## Willemson Security Probability Breach Functions from: 
## [1] "On the Gordon \& Loeb Model for Information Security Investment", J.
##  Willemson, in 5th Annual Workshop on the Economics of Information Security 
##  (WEIS 2006) (June 2006).
## Further expounded on in:## [2] "Extending the Gordon and Loeb model for information security investment." 
##  J. Willemson, in 5th International Conference on Availability, Reliability 
##  and Security (ARES 2010) (February 2010), pp. 258–261.
##
################################################################################

## Dependent source files
source("./GL.R")

## TODO --- error conditions
## TODO --- fix the if..else conditions
## TODO --- implement the functional graph checks
##  WillemsonGraph_S <- function(){}
##  WillensonGraph_ENBIS <- function(){}

################################################################################
## Willemson Security Breach Probability Functions
################################################################################

####
## Willemson security breach probability function S3W; from [1]
## z:     Investment in cybersecurity
## v:     Vulnerability
## b:     S3W parameter --- investment inflection point
## k:     S3W parameter
## 
S3W <- function( z=1, v=1, b=1, k=3 ){
	if( z >= b ){
		return( 0 );
	} else {
		return( v * (1 - (z/b)^k ) )
	}
}


####
## Willemson security breach probability function S4W; from [1]
## z:      Investment in cybersecurity
## v:      Vulnerability
## b:      S4W parameter --- investment inflection point
## bprime: S4W parameter --- Secondary inflection point
## k:      S4W parameter
## 
S4W <- function( z=1, v=1, b=1, bprime=1, k=1 ){
	## Willemson does not give a formal example of S4 in the paper
	## Commented out pending further investigation
	
	# if( z <= bprime ){
		# return( S3W(z,v,b,k) );
	# } else {
		# return( S3W(b,v,b,k) )
	# }
}


####
## Willemson security breach probability function S5W; from [1]
## z:      Investment in cybersecurity
## v:      Vulnerability
## b:      S5W parameter --- investment inflection point
## k:      S5W parameter
## 
S5W <- function( z=1, v=1, b=1, k=2 ){
	if( z >= b ){
		return( 0 );
	} else {
		return( v * (1 - (z/b)^k ) )
	}
}


