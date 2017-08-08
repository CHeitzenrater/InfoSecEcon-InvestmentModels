################################################################################
## Based on the r-into.R file 09/20/2012 Tyler Moore
##
## Basic security metrics functions
##
################################################################################

## TODO --- source each of these to their papers

################################################################################
## ANNUAL LOSS EXPECTANCY (ALE) 
################################################################################
## The ANNUAL LOSS EXPECTANCY (ALE) is the expected loss per period due to 
##  information security failures given security level s: ALE <- function();
##  Equal to the loss distribution function under security, E(L_s)
##  Using the BERNOULLI LOSS ASSUMPTION: Assume two outcomes: {0, lambda}
## p      : Probability of the event occurring
## lambda : Fixed loss amount incurred with probability p_s = L_s(lambda).  
##          With probability 1-p_s = L_s(0), no loss is suffered
##
ALE <- function( p, lambda )
  (p * lambda + (1 - p) * 0);


################################################################################
## EXPECTED BENEFIT OF INFORMAITON SECURITY (EBIS)
################################################################################  
## The EXPECTED BENEFIT OF INFORMAITON SECURITY (EBIS): difference between loss 
##  expectancy without security and the loss expectancy given security level s
##  (assuming efficient investment, c ~ security)
## ALE_0 : The ALE with no security investment
## ALE_s : The ALE with security investment security
##
ENBS_ALE <- function(ALE_0, ALE_s)
  ALE_0 - ALE_s;

################################################################################
## EXPECTED NET BENEFIT OF INFORMAITON SECURITY (ENBIS)
################################################################################  
   
####
## The EXPECTED BENEFIT OF INFORMAITON SECURITY (ENBIS): expected benefit of 
##  information security minus cost of the investment to reach security level s
##  (assuming efficient investment, c ~ security)
## p_0    : Probability of the event occurring without security 
## p_s    : Probability of the event occurring with security 
## lambda : Bernoulli Loss
## s      : Cost of security
##
ENBIS <- function( p_0, lambda, p_s, s)  #|
  ((p_0 * lambda) - (p_s * lambda) - s); #|-> def.  in original R file by Moore
  
## In this case, calculated given the ALE values
## ALE_0 : The ALE with no security investment
## ALE_s : The ALE with security investment security
## s      : Cost of security
##
ENBIS_ALE <- function(ALE_0, ALE_s, s)  
  (ALE_0 - ALE_s - s);  

## In this case, calculated given the EBIS with security
## EBIS_s : Expected Net Benefit of Security at security level s
## s      : Cost of security
##
ENBIS_EBIS <- function(EBIS_s, s)
  (EBIS_s - s);
  

################################################################################
## RETURN ON SECURITY INVESTMENT (ROSI)
################################################################################  
   
####
## The RETURN ON SECURITY INVESTMENT(ROSI) is the ratio of the expected net 
##  benefit over the cost of security
##  (assuming efficient investment, c ~ security)
## p_0    : Probability of the event occurring without security 
## p_s    : Probability of the event occurring with security 
## lambda : Bernoulli Loss
## s      : Cost of security
##
ROSI <- function(p_0, lambda, p_s, s)
  (ENBIS(p_0, lambda, p_s, s) / s);

## In this case, calculated given the EBIS with security
## EBIS_s : Expected Net Benefit of Security at security level s
## s      : Cost of security
##
ROSI_ENBIS <- function(ENBIS_s, s) 
  (ENBIS_s / s);

## In this case, calculated given the ALE with and without security
## ALE_0 : The ALE with no security investment
## ALE_s : The ALE with security investment security
## s      : Cost of security
##
ROSI_ALE <- function(ALE_0, ALE_s, s)
  ((ALE_0 - ALE_s - c) / c);

  
################################################################################
## NET PRESENT VALUE (NPV)
################################################################################  
## The NET PRESENT VALUE (NPV) aggregates the expected net benefit of 
##  information security over multiple future periods into a monetary equivalent 
##  at present
## c_0    : the one-off cost of security at time 0
## c_t    : are the recurring costs of security in period t (if any)
## ALE_s  : the loss expectancy in period t (if any) at security level s
## r      : the discount rate
## t      : the number of periods.  The default is set to 10
##
NPV <- function(r, c_0, c_t, ALE_0, ALE_s, t=10)
  (-c_0 + sum( (ALE_0 - ALE_s - c_t) / ( (1 + r)^(1:t) ) ) );

  
################################################################################
## INITIAL RATE OF RETURN (IRR)
################################################################################  
## The INITIAL RATE OF RETURN (IRR): The "break even" point of return in an NPV
## calculation; i.e. where NPV_s = 0
## c_0      : the one-off cost of security at time 0
## c_t      : are the recurring costs of security in period t (if any)
## ALE_0    : the loss expectancy with no security spending
## ALE_s    : the loss expectancy with security spending at level s
## interval : the interval (a,b) over which the IRR is considered
##
IRR <- function( c_0, c_t, ALE_0, ALE_s, interval )
  uniroot( NPV, interval, c_0, c_t, ALE_0, ALE_s)$root;  
  

################################################################################
## RETURN ON ATTACK (ROA) 
################################################################################

#### 
## The RETURN ON ATTACK (ROA) base definition
## GI	: Expected attacker gain from the incident
## CA	: Expected cost for the attacker to succeed (presumably at level GI)
## eff	: The efficiency of the defensive measure; 1-eff = eff', the attacker 
##         efficiency
##
ROA <- function( GI, CA, eff )
  (GI/CA) * (1-eff);
  
####
## Alternate definition: pre-calculated gain
## gain	: Approximation of the GI/CA term; unit gain per attack cost
## eff  : As above
## 
ALT_ROA <- function( gain, eff )
  gain * (1-eff);
  
####
## ROA ALE --- TODO: needs to be verified
## 
#ROA_ALE <- function( eff, CI, CS )
#  ((eff * CI)/CS)-1;
  


################################################################################
################################################################################
##
## LEGACY CODE
## NOTE: This code is commented due to further developments elsewhere
##
################################################################################
################################################################################

################################################################################
## VARIABLE DEFINITIONS
################################################################################

##p_0 - the probability of the event occurring without security 
#p_0 = 1;

##p_s - the probability of the event occurring with security
#p_s = 1;

## The SECURITY LEVEL.  definition:
## Cost of security (c), security level (s): The cost of security c is the 
##  amount spent to reach a security level s. No security investment (c = 0) 
##  implies  s = 0, and for any c > 0, s increases monotonically in c.
## Effective security investment: If security investment is effective, the 
##  security level can be approximated by the cost of security; i.e. s
##  approx.=c.
#cost = 1;
#s = cost;

## LOSS DISTRIBUTION FUNCTION: family of probability distribution functions 
##  describing the monetary losses incurred from insecurity for a given security 
##  level s
##Loss at no security
#L_0 = 1;
##Loss at investment c (security level s)
#L_s = 1;


################################################################################
## GORDON-LOEB MODEL
################################################################################

#### 
## Assumes the security investment is effective and unit loss lambda = 1
## s   : Cost
## v   : Vulnerability level, on the interval [0, 1]
## alpha, beta : alpha>0 and beta>1 capture security productivity 
##  (measures how the security investment reduced the probability of loss)
##
#GB_BPF_FD <- function(c,v,alpha,beta) (v / ((alpha*c + 1)^beta) ); 
#GB_BPF_SD <- function(c,v,alpha,beta) ( v^(alpha*c+1) ); 

#### 
## LINEAR BREACH PROBABILITY FUNCTION  
## Assumes the security investment is effective and unit loss lambda = 1
## s   : Security level, on the interval [0,1] 
## v   : Vulnerability level, on the interval [0, 1]
#LINEAR_BPF <- function(s,v) (v * (1 - s)); 

####
## EXPONENTIAL BREACH PROBABILITY FUNCTION  
## s   : Security level, on the interval [0,1]
## v   : Vulnerability level, on the interval [0, 1]
## beta: How effectively the investment reduces probability of loss; must be > 1
##
#EXP_BPF <- function(s, v, beta) (v * beta ^ (-s));
##------ The first order condition for selecting optimality
#EXP_BPF.FOC <- function(s,v) (log( v * log(s) ) / log(s) );
