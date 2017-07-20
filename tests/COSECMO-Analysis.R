#############################################################################
#
# Analysis of COSECMO data against GL SBPF
#
#############################################################################

#source("../data/COSECMO_Data.R", local=FALSE)
#source("../models/COSECMO.R", local=FALSE)
#source("../models/GL.R", local=FALSE)
#source("../models/GL-Hausken.R", local=FALSE)
#source("../models/GL.R", local=TRUE)

#############################################################################

# domain of the input costs (man months)
rng = seq(0,32.25)

# range of the EAL output; as levels (eal) and mapped to vulnerability levels (eal_alt), by 1.0/eal
# XXX0 -- indicates items with starting values at 0
eal = seq(1,7)
eal0 = seq(0,7)

eal_alt = c(0.143,0.286,0.429,0.571,0.714,0.857,1.0)
eal_alt0 = c(0,0.143,0.286,0.429,0.571,0.714,0.857,1.0)

# range of vulnerability values
vul = seq(0,1)


#############################################################################
# Functions to perform the residual calculations for various 
#
S1_resid <- function( z=1, v=1, alpha=1, beta=1 ){
	return( 1 - S1(z,v,alpha,beta) )
}
S2_resid <- function( z=1, v=1, alpha ){
	return( 1 - S2(z,v,alpha) )
}
###
S3H_resid <- function( z=1, v=1, phi=1, gamma=1 ){
	return( 1 - S3H(z, v, phi, gamma) )
}
S4H_resid <- function( z=1, v=1, mu=1, k=0.5 ){
	#return( 1 - S4H(z, v, mu, k) )
	#vals <- sapply(z, S4H, v, mu, k);
	return( 1 - sapply(z, S4H, v, mu, k))
}
S5H_resid <- function( z=1, v=1, omega=1, k=2 ){
	#return( 1 - S5H(z, v, omega, k) )
	return( 1 - sapply(z, S5H, v, omega, k))
}
S6H_resid <- function( z=1, v=1, lambda=0.1 ){
	#return( 1 - S6H(z, v, lambda) )
	return( 1 - sapply(z, S6H, v, lambda))
}
###
S3W_resid <- function( z=1, v=1, b=1, k=3 ){
	return( 1 - sapply(z, S3W, v, b, k))
}
# S4W_resid <- function( z=1, v=1, b=1, k=3 ){
	# return( 1 - sapply(z, S4W, v, b, k))
# }
S5W_resid <- function( z=1, v=1, b=1, k=2 ){
	return( 1 - sapply(z, S5W, v, b, k))
}

#############################################################################
# best-fit calculations
#    
######## S1
# all variable are free
NLS_S1_5K <- nls(eal_alt~S1_resid(COSECMO_5KLOC,v,a,b), start=list(v=1,a=0.001,b=0.001), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
NLS_S1_20K <- nls(eal_alt~S1_resid(COSECMO_20KLOC,v,a,b), start=list(v=1,a=0.001,b=0.001), algorithm="port", lower=c(0,0,0), upper=c(1,10,1000))
NLS_S1_100K <- nls(eal_alt~S1_resid(COSECMO_100KLOC,v,a,b), start=list(v=1,a=0.001,b=0.001), algorithm="port", lower=c(0,0,0), upper=c(1,10,1000))
NLS_S1_1M <-nls(eal_alt~S1_resid(COSECMO_1MLOC,v,a,b), start=list(v=1,a=0.001,b=0.001), algorithm="port", lower=c(0,0,0), upper=c(1,10,1000))

# Hold beta = 1
NLS_S1_5K_beta <- nls(eal_alt~S1_resid(COSECMO_5KLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
NLS_S1_20K_beta <- nls(eal_alt~S1_resid(COSECMO_20KLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
NLS_S1_100K_beta <- nls(eal_alt~S1_resid(COSECMO_100KLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
NLS_S1_1M_beta <-nls(eal_alt~S1_resid(COSECMO_1MLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))


######## S2
# all variables are free
NLS_S2_5K <- nls(eal_alt~S2_resid(COSECMO_5KLOC,v,a), start=list(v=0.1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S2_20K <- nls(eal_alt~S2_resid(COSECMO_20KLOC,v,a), start=list(v=0.1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S2_100K <- nls(eal_alt~S2_resid(COSECMO_100KLOC,v,a), start=list(v=0.1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S2_1M <-nls(eal_alt~S2_resid(COSECMO_1MLOC,v,a), start=list(v=0.1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,10))
# v=0.99
NLS_S2_5K_z <- nls(eal_alt~S2_resid(COSECMO_5KLOC,0.99,a), start=list(a=0.001), algorithm="port", lower=c(0), upper=c(100))
NLS_S2_20K_z <- nls(eal_alt~S2_resid(COSECMO_20KLOC,0.99,a), start=list(a=0.001), algorithm="port", lower=c(0), upper=c(100))
NLS_S2_100K_z <- nls(eal_alt~S2_resid(COSECMO_100KLOC,0.99,a), start=list(a=0.001), algorithm="port", lower=c(0), upper=c(100))
NLS_S2_1M_z <-nls(eal_alt~S2_resid(COSECMO_1MLOC,0.99,a), start=list(a=0.001), algorithm="port", lower=c(0), upper=c(100))

# as the v value runs from 0.99 toward 0, a runs from 1 to 0.977 (v=0.75) and 0.2055 (v=0.5); residual goes up, then down


######## S3H
# all variables are free
NLS_S3H_5K <- nls(eal_alt~S3H_resid(COSECMO_5KLOC,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
NLS_S3H_20K <- nls(eal_alt~S3H_resid(COSECMO_20KLOC,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
NLS_S3H_100K <- nls(eal_alt~S3H_resid(COSECMO_100KLOC,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
NLS_S3H_1M <-nls(eal_alt~S3H_resid(COSECMO_1MLOC,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
## what if v is started out lower?


######## S4H
# all variables are free
NLS_S4H_5K <- nls(eal_alt~S4H_resid(COSECMO_5KLOC,v,mu,k), start=list(v=0.5,mu=0.5,k=0.25), algorithm="port", lower=c(0,0,0), upper=c(1,10,1))
NLS_S4H_20K <- nls(eal_alt~S4H_resid(COSECMO_20KLOC,v,mu,k), start=list(v=1,mu=0.5,k=0.25), algorithm="port", lower=c(0,0,0), upper=c(1,10,1))
NLS_S4H_100K <- nls(eal_alt~S4H_resid(COSECMO_100KLOC,v,mu,k), start=list(v=1,mu=0.5,k=0.25), algorithm="port", lower=c(0,0,0), upper=c(1,10,1))
NLS_S4H_1M <-nls(eal_alt~S4H_resid(COSECMO_1MLOC,v,mu,k), start=list(v=1,mu=0.5,k=0.25), algorithm="port", lower=c(0,0,0), upper=c(1,10,1))


######## S5H
# all variables are free
NLS_S5H_5K <- nls(eal_alt~S5H_resid(COSECMO_5KLOC,v,omega,k), start=list(v=1,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
NLS_S5H_20K <- nls(eal_alt~S5H_resid(COSECMO_20KLOC,v,omega,k), start=list(v=1,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
NLS_S5H_100K <- nls(eal_alt~S5H_resid(COSECMO_100KLOC,v,omega,k), start=list(v=1,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
NLS_S5H_1M <-nls(eal_alt~S5H_resid(COSECMO_1MLOC,v,omega,k), start=list(v=1,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
## S4H and S5H differ in the value of k...


######## S6H
# all variables are free
NLS_S6H_5K <- nls(eal_alt~S6H_resid(COSECMO_5KLOC,v,lambda), start=list(v=1,lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S6H_20K <- nls(eal_alt~S6H_resid(COSECMO_20KLOC,v,lambda), start=list(v=1,lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S6H_100K <- nls(eal_alt~S6H_resid(COSECMO_100KLOC,v,lambda), start=list(v=1,lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S6H_1M <-nls(eal_alt~S6H_resid(COSECMO_1MLOC,v,lambda), start=list(v=1,lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))
# v=1
NLS_S6H_5K_v <- nls(eal_alt~S6H_resid(COSECMO_5KLOC,1,lambda), start=list(lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S6H_20K_v <- nls(eal_alt~S6H_resid(COSECMO_20KLOC,1,lambda), start=list(lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S6H_100K_v <- nls(eal_alt~S6H_resid(COSECMO_100KLOC,1,lambda), start=list(lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))
NLS_S6H_1M_v <-nls(eal_alt~S6H_resid(COSECMO_1MLOC,1,lambda), start=list(lambda=0.1), algorithm="port", lower=c(0,0), upper=c(1,10))


######## S3W
# all variables are free
NLS_S3W_5K <- nls(eal_alt~S3W_resid(COSECMO_5KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
NLS_S3W_20K <- nls(eal_alt~S3W_resid(COSECMO_20KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
NLS_S3W_100K <- nls(eal_alt~S3W_resid(COSECMO_100KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
NLS_S3W_1M <-nls(eal_alt~S3W_resid(COSECMO_1MLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
# v=1
NLS_S3W_5K_v1 <- nls(eal_alt~S3W_resid(COSECMO_5KLOC,1,b,k), start=list(b=5,k=3), algorithm="port", lower=c(0.01,2.01), upper=c(35,10))
NLS_S3W_20K_v1 <- nls(eal_alt~S3W_resid(COSECMO_20KLOC,1,b,k), start=list(b=5,k=3), algorithm="port", lower=c(0.01,2.01), upper=c(35,10))
NLS_S3W_100K_v1 <- nls(eal_alt~S3W_resid(COSECMO_100KLOC,1,b,k), start=list(b=5,k=3), algorithm="port", lower=c(0.01,2.01), upper=c(35,10))
NLS_S3W_1M_v1 <-nls(eal_alt~S3W_resid(COSECMO_1MLOC,1,b,k), start=list(b=5,k=3), algorithm="port", lower=c(0.01,2.01), upper=c(35,10))


######## S4W
# all variables are free
# NLS_S4W_5K <- nls(eal_alt~S4W_resid(COSECMO_5KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
# NLS_S4W_20K <- nls(eal_alt~S4W_resid(COSECMO_20KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
# NLS_S4W_100K <- nls(eal_alt~S4W_resid(COSECMO_100KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
# NLS_S4W_1M <-nls(eal_alt~S4W_resid(COSECMO_1MLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))


######## S5W
# all variables are free
NLS_S5W_5K <- nls(eal_alt~S5W_resid(COSECMO_5KLOC,v,b,k), start=list(v=1,b=5,k=2), algorithm="port", lower=c(0,0.01,1.01), upper=c(1,35,10))
NLS_S5W_20K <- nls(eal_alt~S5W_resid(COSECMO_20KLOC,v,b,k), start=list(v=1,b=5,k=2), algorithm="port", lower=c(0,0.01,1.01), upper=c(1,35,10))
NLS_S5W_100K <- nls(eal_alt~S5W_resid(COSECMO_100KLOC,v,b,k), start=list(v=1,b=5,k=2), algorithm="port", lower=c(0,0.01,1.01), upper=c(1,35,10))
NLS_S5W_1M <-nls(eal_alt~S5W_resid(COSECMO_1MLOC,v,b,k), start=list(v=1,b=5,k=2), algorithm="port", lower=c(0,0.01,1.01), upper=c(1,35,10))
# v=1
NLS_S5W_5K_v <- nls(eal_alt~S5W_resid(COSECMO_5KLOC,1,b,k), start=list(b=5,k=2), algorithm="port", lower=c(0.01,1.01), upper=c(35,10))
NLS_S5W_20K_v <- nls(eal_alt~S5W_resid(COSECMO_20KLOC,1,b,k), start=list(b=5,k=2), algorithm="port", lower=c(0.01,1.01), upper=c(35,10))
NLS_S5W_100K_v <- nls(eal_alt~S5W_resid(COSECMO_100KLOC,1,b,k), start=list(b=5,k=2), algorithm="port", lower=c(0.01,1.01), upper=c(35,10))
NLS_S5W_1M_v <-nls(eal_alt~S5W_resid(COSECMO_1MLOC,1,b,k), start=list(b=5,k=2), algorithm="port", lower=c(0.01,1.01), upper=c(35,10))

#############################################################################
# Best Fit Plots
s1_best = S1_resid(COSECMO_1MLOC, 1, 0.1711, 1.8385)
s1_best_beta = S1_resid(COSECMO_1MLOC, 1, 0.3837, 1)
s2_best = S2_resid(COSECMO_1MLOC, 0.921, 2.545)
#s2_best_v = S2_resid(COSECMO_1MLOC, 0.921, 1)
s3H_best = S3H_resid(COSECMO_1MLOC, 1, 0.07637, 4.36077) 
s4H_best = S4H_resid(COSECMO_1MLOC, 1, 0.3004, 0.4276) 
s6H_best = S6H_resid(COSECMO_1MLOC, 0.8223, 0.1192) 
s6H_best_v = S6H_resid(COSECMO_1MLOC, 1, 0.1433)
s3W_best = S3W_resid(COSECMO_1MLOC, 0.7976, 4.2684, 2.0100) 
s3W_best_v = S3W_resid(COSECMO_1MLOC, 1, 2.616, 2.010)
s5W_best = S5W_resid(COSECMO_1MLOC, 0.9472, 5.2355, 1.0100) 
s5W_best_v = S3W_resid(COSECMO_1MLOC, 1, 4.851, 1.010)

plot(COSECMO_1MLOC, eal_alt, type="b", xlab="Additional Cost Factor", ylab="Vulnerability Reduction [(1-S(z,v)]", lty=1, pch=0 )
lines(COSECMO_1MLOC, s1_best, type="b", lty=2, pch=19)
lines(COSECMO_1MLOC, s1_best_beta, type="b", lty=3, pch=19)
lines(COSECMO_1MLOC, s2_best, type="b", lty=2, pch=18)
#lines(COSECMO_1MLOC, s2_best_v, type="b", lty=3, pch=18)
lines(COSECMO_1MLOC, s3H_best, type="b", lty=2, pch=17)
lines(COSECMO_1MLOC, s4H_best, type="b", lty=2, pch=16)
lines(COSECMO_1MLOC, s6H_best, type="b", lty=2, pch=15)
lines(COSECMO_1MLOC, s6H_best_v, type="b", lty=3, pch=15)
lines(COSECMO_1MLOC, s3W_best, type="b", lty=2, pch=14)
lines(COSECMO_1MLOC, s3W_best_v, type="b", lty=3, pch=14)
lines(COSECMO_1MLOC, s5W_best, type="b", lty=2, pch=13)
lines(COSECMO_1MLOC, s5W_best_v, type="b", lty=3, pch=13)
# legend("bottomright",
       # c("COSECMO 1M LOC","S1","S1-beta","S2","S2-z","S3H","S4H","S6H","S6H-z"), 
       # lty=c(1,2,3,2,3,2,2,2,3),
       # pch=c(0,19,19,18,18,17,16,15,15))
       
legend("bottomright",
       c("COSECMO 1M LOC","S1","S1-beta","S2","S3H","S4H","S6H","S6H-z","3W","3W-z","5W","5W-z"), 
       lty=c(1,2,3,2,2,2,2,3,2,3,2,3),
       pch=c(0,19,19,18,17,16,15,15,14,14,13,13))
       
#S2-z is not showing up because it is so far off the others


######################################################################################################
######################################################################################################
######################################################################################################

# Code for the COCOMO projected size

# SECU --- effort multiplier for 1 EAL increase > 3
SECU = 2.5
# The percent multipliers for each EAL
COSECMO_Percent3 = c(0.20,0.40,0.60,0.80)

# domain of the input costs (man months)
baseEffortReq_5K 	= 2.2
baseEffortReq_20K 	= 10.4
baseEffortReq_100K 	= 61.2
baseEffortReq_1M 	= 770.4
#baseEffortReq 		= c(baseEffortReq_5K, baseEffortReq_20K, baseEffortReq_100K, baseEffortReq_1M)
baseEffortReq 		= c(baseEffortReq_5K/5, baseEffortReq_20K/20, baseEffortReq_100K/100, 
						baseEffortReq_1M/1000)

baseEffortDes_5K 	= 3.9
baseEffortDes_20K 	= 17.7
baseEffortDes_100K 	= 104.3
baseEffortDes_1M 	= 312.4
#baseEffortDes 		= c(baseEffortDes_5K, baseEffortDes_20K, baseEffortDes_100K, baseEffortDes_1M)
baseEffortDes 		= c(	baseEffortDes_5K/5, baseEffortDes_20K/20, baseEffortDes_100K/100, 
						baseEffortDes_1M/1000)

baseEffortDev_5K 	= 5.5
baseEffortDev_20K 	= 25.2
baseEffortDev_100K 	= 147.5
baseEffortDev_1M 	= 1856.9
#baseEffortDev 		= c(baseEffortDev_5K, baseEffortDev_20K, baseEffortDev_100K, baseEffortDev_1M)
baseEffortDev 		= c(	baseEffortDev_5K/5, baseEffortDev_20K/20, baseEffortDev_100K/100,
						baseEffortDev_1M/1000)
						
baseEffortAll_5K	= (baseEffortReq_5K + baseEffortDes_5K + baseEffortDev_5K)/5
baseEffortAll_20K	= (baseEffortReq_20K + baseEffortDes_20K + baseEffortDev_20K)/20
baseEffortAll_100K	= (baseEffortReq_100K + baseEffortDes_100K + baseEffortDev_100K)/100
baseEffortAll_1M	= (baseEffortReq_1M + baseEffortDes_1M + baseEffortDev_1M)/1000
baseEffortAll 		= c(	baseEffortAll_5K, baseEffortAll_20K, baseEffortAll_100K,
						baseEffortAll_1M)
						

EffAssuredReq_5K 	= vector(mode="numeric", length=7)
EffAssuredReq_20K 	= vector(mode="numeric", length=7)
EffAssuredReq_100K 	= vector(mode="numeric", length=7)
EffAssuredReq_1M 	= vector(mode="numeric", length=7)

EffAssuredDes_5K 	= vector(mode="numeric", length=7)
EffAssuredDes_20K 	= vector(mode="numeric", length=7)
EffAssuredDes_100K 	= vector(mode="numeric", length=7)
EffAssuredDes_1M 	= vector(mode="numeric", length=7)

EffAssuredDev_5K 	= vector(mode="numeric", length=7)
EffAssuredDev_20K 	= vector(mode="numeric", length=7)
EffAssuredDev_100K 	= vector(mode="numeric", length=7)
EffAssuredDev_1M 	= vector(mode="numeric", length=7)

EffAssured_5K 	= vector(mode="numeric", length=7)
EffAssured_20K 	= vector(mode="numeric", length=7)
EffAssured_100K = vector(mode="numeric", length=7)
EffAssured_1M 	= vector(mode="numeric", length=7)

for(i in 1:7){	
	 EffAssuredReq_5K[i]	= CalculateEffIntAssure(baseEffortReq[1], COSECMO_Percent3[1], SECU, i)
	 EffAssuredReq_20K[i] 	= CalculateEffIntAssure(baseEffortReq[2], COSECMO_Percent3[2], SECU, i)
	 EffAssuredReq_100K[i] 	= CalculateEffIntAssure(baseEffortReq[3], COSECMO_Percent3[3], SECU, i)
	 EffAssuredReq_1M[i] 	= CalculateEffIntAssure(baseEffortReq[4], COSECMO_Percent3[4], SECU, i)
	 
	 EffAssuredDes_5K[i] 	= CalculateEffIntAssure(baseEffortDes[1], COSECMO_Percent3[1], SECU, i)
	 EffAssuredDes_20K[i] 	= CalculateEffIntAssure(baseEffortDes[2], COSECMO_Percent3[2], SECU, i)
	 EffAssuredDes_100K[i] 	= CalculateEffIntAssure(baseEffortDes[3], COSECMO_Percent3[3], SECU, i)
	 EffAssuredDes_1M[i] 	= CalculateEffIntAssure(baseEffortDes[4], COSECMO_Percent3[4], SECU, i)
	 
	 EffAssuredDev_5K[i] 	= CalculateEffIntAssure(baseEffortDev[1], COSECMO_Percent3[1], SECU, i)
	 EffAssuredDev_20K[i] 	= CalculateEffIntAssure(baseEffortDev[2], COSECMO_Percent3[2], SECU, i)
	 EffAssuredDev_100K[i] 	= CalculateEffIntAssure(baseEffortDev[3], COSECMO_Percent3[3], SECU, i)
	 EffAssuredDev_1M[i] 	= CalculateEffIntAssure(baseEffortDev[4], COSECMO_Percent3[4], SECU, i)
	 
	 EffAssured_5K[i] 	= CalculateEffIntAssure(baseEffortAll[1], COSECMO_Percent3[1], SECU, i)
	 EffAssured_20K[i] 	= CalculateEffIntAssure(baseEffortAll[2], COSECMO_Percent3[2], SECU, i)
	 EffAssured_100K[i] 	= CalculateEffIntAssure(baseEffortAll[3], COSECMO_Percent3[3], SECU, i)
	 EffAssured_1M[i] 	= CalculateEffIntAssure(baseEffortAll[4], COSECMO_Percent3[4], SECU, i)	 
}

s1_best_beta_eff_req 	= S1_resid(EffAssuredReq_1M, 1, 0.3837, 1)
s3H_best_eff_req 		= S3H_resid(EffAssuredReq_1M,1, 0.07637, 4.36077) 

s1_best_beta_eff_des 	= S1_resid(EffAssuredDes_1M, 1, 0.3837, 1)
s3H_best_eff_des 		= S3H_resid(EffAssuredDes_1M,1, 0.07637, 4.36077) 

s1_best_beta_eff_dev 	= S1_resid(EffAssuredDev_1M, 1, 0.3837, 1)
s3H_best_eff_dev 		= S3H_resid(EffAssuredDev_1M,1, 0.07637, 4.36077) 


plot(EffAssuredDev_1M, eal_alt, type="b", xlab="Man Months / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=1, pch=0 )
lines(EffAssuredDev_1M, s1_best_beta_eff_dev, 	type="b", lty=1, pch=1)
lines(EffAssuredDev_1M, s3H_best_eff_dev, 	 	type="b", lty=1, pch=2)

lines(EffAssuredReq_1M, eal_alt, 				type="b", lty=2, pch=0)
lines(EffAssuredReq_1M, s1_best_beta_eff_req, 	type="b", lty=2, pch=1)
lines(EffAssuredReq_1M, s3H_best_eff_req, 		type="b", lty=2, pch=2)

lines(EffAssuredDes_1M, eal_alt, 				type="b", lty=3, pch=0)
lines(EffAssuredDes_1M, s1_best_beta_eff_des, 	type="b", lty=3, pch=1)
lines(EffAssuredDes_1M, s3H_best_eff_des, 		type="b", lty=3, pch=2)


legend(	"bottomright",
		c(	"COSECMO-Requirements", "S1-Requirements", "S3H-Requirements", "COSECMO-Design", "S1-Design", 
			"S3H-Design", "COSECMO-Implementation", "S1-Implementation", "S3H-Implementation"), 
		lty=c(2,2,2,3,3,3,1,1,1),
		pch=c(0,1,2,0,1,2,0,1,2))

#######################
## 5K plot
s1_best_beta_eff_req_5k 	= S1_resid(EffAssuredReq_5K, 1, 0.6575, 1)
s3H_best_eff_req_5k 		= S3H_resid(EffAssuredReq_5K,1, 1.1987, 0.1811) 

s1_best_beta_eff_des_5k 	= S1_resid(EffAssuredDes_5K, 1, 0.6575, 1)
s3H_best_eff_des_5k 		= S3H_resid(EffAssuredDes_5K,1, 1.1987, 0.1811) 

s1_best_beta_eff_dev_5k 	= S1_resid(EffAssuredDev_5K, 1, 0.6575, 1)
s3H_best_eff_dev_5k 		= S3H_resid(EffAssuredDev_5K,1, 1.1987, 0.1811) 

#s1_best_beta_eff_req_5k 	= S1_resid(EffAssuredReq_5K, 1, 0.3837, 1)
#s3H_best_eff_req_5k 		= S3H_resid(EffAssuredReq_5K,1, 0.07637, 4.36077) 

#s1_best_beta_eff_des_5k 	= S1_resid(EffAssuredDes_5K, 1, 0.3837, 1)
#s3H_best_eff_des_5k 		= S3H_resid(EffAssuredDes_5K,1, 0.07637, 4.36077) 

#s1_best_beta_eff_dev_5k 	= S1_resid(EffAssuredDev_5K, 1, 0.3837, 1)
#s3H_best_eff_dev_5k 		= S3H_resid(EffAssuredDev_5K,1, 0.07637, 4.36077) 

		
plot(EffAssuredDevel_5K, eal_alt, type="b", xlab="Man Months / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=1, pch=0 )
lines(EffAssuredDev_5K, s1_best_beta_eff_dev_5k, 	type="b", lty=1, pch=1)
lines(EffAssuredDev_5K, s3H_best_eff_dev_5k, 		type="b", lty=1, pch=2)

lines(EffAssuredReq_5K, eal_alt, 					type="b", lty=2, pch=0)
lines(EffAssuredReq_5K, s1_best_beta_eff_req_5k, 	type="b", lty=2, pch=1)
lines(EffAssuredReq_5K, s3H_best_eff_req_5k, 		type="b", lty=2, pch=2)

lines(EffAssuredDes_5K, eal_alt, 					type="b", lty=3, pch=0)
lines(EffAssuredDes_5K, s1_best_beta_eff_des_5k, 	type="b", lty=3, pch=1)
lines(EffAssuredDes_5K, s3H_best_eff_des_5k, 		type="b", lty=3, pch=2)

legend(	"bottomright",
		c(	"COSECMO-Requirements", "S1-Requirements", "S3H-Requirements", "COSECMO-Design", "S1-Design", 
			"S3H-Design", "COSECMO-Implementation", "S1-Implementation", "S3H-Implementation"), 
		lty=c(2,2,2,3,3,3,1,1,1),
		pch=c(0,1,2,0,1,2,0,1,2))
		
		
		
####
# All plot

s1_best_beta_eff_5K 	= S1_resid(EffAssured_5K, 1, 0.6575, 1)
s3H_best_eff_5K 		= S3H_resid(EffAssured_5K,1, 1.1987, 0.1811) 

s1_best_beta_eff_20K 	= S1_resid(EffAssured_20K, 1, 0.5142, 1)
s3H_best_eff_20K 		= S3H_resid(EffAssured_20K,1, 0.3395, 1.0031)

s1_best_beta_eff_100K 	= S1_resid(EffAssured_100K, 1, 0.4361, 1)
s3H_best_eff_100K 		= S3H_resid(EffAssured_100K,1, 0.1474, 2.3619)

s1_best_beta_eff_1M 	= S1_resid(EffAssured_1M, 1, 0.3837, 1)
s3H_best_eff_1M 		= S3H_resid(EffAssured_1M,1, 0.07637, 4.36077)


plot(EffAssured_1M, eal_alt, type="b", xlab="Man Months / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=1, pch=0 )
lines(EffAssured_1M, s1_best_beta_eff_1M, 	type="b", lty=1, pch=1)
lines(EffAssured_1M, s3H_best_eff_1M, 		type="b", lty=1, pch=2)

plot(EffAssured_100K, eal_alt, type="b", xlab="Man Months / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=3, pch=0 )
lines(EffAssured_100K, eal_alt, 				type="b", lty=3, pch=0)
lines(EffAssured_100K, s1_best_beta_eff_100K, 	type="b", lty=3, pch=1)
lines(EffAssured_100K, s3H_best_eff_100K, 		type="b", lty=3, pch=2)

plot(EffAssured_20K, eal_alt, type="b", xlab="Man Months / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=2, pch=0 )
lines(EffAssured_20K, eal_alt, 				type="b", lty=2, pch=0)
lines(EffAssured_20K, s1_best_beta_eff_20K, 	type="b", lty=2, pch=1)
lines(EffAssured_20K, s3H_best_eff_20K, 		type="b", lty=2, pch=2)

plot(EffAssured_5K, eal_alt, type="b", xlab="Man Months / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=4, pch=0 )
lines(EffAssured_5K, eal_alt, 				type="b", lty=4, pch=0)
lines(EffAssured_5K, s1_best_beta_eff_5K, 	type="b", lty=4, pch=1)
lines(EffAssured_5K, s3H_best_eff_5K, 		type="b", lty=4, pch=2)

legend(	"bottomright",
		c(	"COSECMO-5K", 	"S1-5K", 	"S3H-5K", 
			"COSECMO-20K", 	"S1-20K", 	"S3H-20K",
			"COSECMO-100K", 	"S1-100K", 	"S3H-100K",
			"COSECMO-1M", 	"S1-1M", 	"S3H-1M" ),
		lty=c(4,4,4,2,2,2,3,3,3,1,1,1),
		pch=c(0,1,2,0,1,2,0,1,2,0,1,2))

	
		
######################################################################################################
######################################################################################################
######################################################################################################

#Monthly Rates
consultantCostMonthUK 	= 4046.42
architectCostMonthUK 	= 4437.17
developerCostMonthUK 	= 2560.58
#Hourly Rates
developerCostHourUKLow 	= 29.69 #12.77
developerCostHourUKMed 	= 18.36
developerCostHourUKHigh = 92.59 #29.69
#conversionRate
convRate = 0.814



#Yearly Rates --- www.salary.com.  US National Averages
					#median rate for Software Engineer I (entry), III, and V
developerCostYearUS 	= (64527 + 103871 + 139453)/3
#Monthly Rates
consultantCostMonthUS 	= 0
architectCostMonthUS 	= 0
developerCostMonthUS	= developerCostYearUS/12
#Hourly Rates
developerCostHourUSLow 	= 64527/2087
developerCostHourUSMed 	= 103871/2087
developerCostHourUSHigh = 139453/2087
#https://www.opm.gov/policy-data-oversight/pay-leave/pay-administration/fact-sheets/computing-hourly-rates-of-pay-using-the-2087-hour-divisor/


developerCostMonth	 	= developerCostMonthUS

developerCostHourLow 	= developerCostHourUSLow
developerCostHourMed 	= developerCostHourUSMed
developerCostHourHigh 	= developerCostHourUSHigh


securityProfCostMed = 29.00



# code review and testing plot

#size = 1000000
size = 10000
vulnRate = 0.02
remediationTime = 5 #hours
defectRate = 100/1000

defects = size * defectRate
vulns = defects * vulnRate
vulnsMan = defects * vulnRate
FPClassificationTime = 0.1


# coverity
coverityFP = 0.15
coverityEffect_h = 0.45  
#coverityFPRatio = 3950
coverityFoundVuln = vulns * coverityEffect
coverityCostKLOC_h = ( 
	(0.09 * size) + 											#coverity license costs
	(coverityFoundVuln * remediationTime * developerCostHourHigh) +  		#fix costs
	(defects * coverityFP * remediationTime * developerCostHourHigh) #+ 	#FP costs
#	(coverityFPRatio * FPClassificationTime * developerCostHourHigh) 		#classification costs 
	) / 10		# per KLOC
	
coverityEffect_l = 0.06
coverityFoundVuln_l = vulns * coverityEffect_l
coverityCostKLOC_l = ( 
	(0.09 * size) + 											#coverity license costs
	(coverityFoundVuln_l * remediationTime * developerCostHourLow) +  		#fix costs
	(defects * coverityFP * remediationTime * developerCostHourLow) #+ 		#FP costs
#	(coverityFPRatio * FPClassificationTime * developerCostHourLow) 		#classification costs 
	) / 10		# per KLOC

coverityAndDefensicsEffect = 0.45
coverityAndDefensicsFP = 0.15
coverityAndDefensicsCostKLOC = ( 
	(0.09 * size) * 3 + 											#coverity license costs
	(11151 * 32) * 3 + 												#defensics license costs
	#(35000) +   													#onsite consulting
	(vulns * coverityAndDefensicsEffect * remediationTime * developerCostHourHigh) + 	#fix costs
	(defects * coverityAndDefensicsFP * remediationTime * developerCostHourHigh)    	#+ 	#FP costs
#	(coverityFPRatio * FPClassificationTime * developerCostHourHigh) 					#classification costs 
	) / 10    # per KLOC
	
# # fortify
# fortifyEffect = 0.40
# fortifyFPRatio = 22744.12
# fortifyCostKLOC = ( 
	# (6000 * convRate) + 													#fortify license costs
	# (vulns * fortifyEffect * remediationTime * developerCost) +  			#fix costs
# #	(1000 * fortifyEffect * fortifyFP * remediationTime * developerCost) + 	#FP costs
	# (fortifyFPRatio * FPClassificationTime * developerCost) 				#classification costs 
	# ) / 1000		# per KLOC
	
#from 2008 paper
AFP = 0.221
AEffect = 0.375
AFPRatio = 2658.33
ACostKLOCLow = ( 
	(vulns * AEffect * remediationTime * developerCostHourLow) + 			#fix costs
	(defects * AFP * remediationTime * developerCostHourLow) #+				#FPcosts 
#	(AFPRatio * FPClassificationTime * developerCostHourLow)				#classification costs
	) / 10																# per KLOC	

ACostKLOCHigh = ( 
	(vulns * AEffect * remediationTime * developerCostHourHigh) + 			#fix costs
	(defects * AFP * remediationTime * developerCostHourHigh) #+			#FPcosts 
#	(AFPRatio * FPClassificationTime * developerCostHourHigh)				#classification costs
	) / 10	
	
BFP = 0.053
BEffect = 0.286
BFPRatio = 132 
BCostKLOCLow = ( 
	(vulns * BEffect * remediationTime * developerCostHourLow) +  			#fix costs
	(defects * BFP * remediationTime * developerCostHourLow) #+	 			#FPcosts
#	(BFPRatio * FPClassificationTime * developerCostHourLow) 				#classification costs 
	) / 10																# per KLOC	

BCostKLOCHigh = ( 
	(vulns * BEffect * remediationTime * developerCostHourHigh) +  			#fix costs
	(defects * BFP * remediationTime * developerCostHourHigh) 	#+		 	#FPcosts
#	(BFPRatio * FPClassificationTime * developerCostHourHigh) 				#classification costs 
	) / 10	
	
CFP = 0.06
CEffect = 0.25
CFPRatio = 480
CCostKLOCLow = ( 
	(vulns * CEffect * remediationTime * developerCostHourLow) +  			#fix costs
	(defects * CFP * remediationTime * developerCostHourLow)  #+ 			#FPcosts
#	(CFPRatio * FPClassificationTime * developerCostHourLow) 				#classification costs  
	) / 10																# per KLOC	
CCostKLOCHigh = ( 
	(vulns * CEffect * remediationTime * developerCostHourHigh) +  			#fix costs
	(defects * CFP * remediationTime * developerCostHourHigh) #+ 			#FPcosts
#	(CFPRatio * FPClassificationTime * developerCostHourHigh) 				#classification costs  
	) / 10																# per KLOC	
	
	

paper2015FP_l = 0.35
paper2015Effect_l = 0.21
paper2015FPRatio_l = 4293.33
paper2015CostKLOC_l = ( 
	(vulns * paper2015Effect_l * remediationTime * developerCostHourHigh) +  	#fix costs
	(defects * paper2015FP_l * remediationTime * developerCostHourHigh) #+ 		#FPcosts
#	(paper2015FPRatio_l * FPClassificationTime * developerCostHourHigh) 		#classification costs  
	) / 10		# per KLOC	
	
paper2015FP_h = 0.01
paper2015Effect_h = 0
paper2015FPRatio_h = 2581.67
paper2015CostKLOC_h = ( 
	(vulns * paper2015Effect_h * remediationTime * developerCostHourHigh) +  	#fix costs
	(defects * paper2015FP_h * remediationTime * developerCostHourHigh) #+ 		#FPcosts
#	(paper2015FPRatio_h * FPClassificationTime * developerCostHourHigh) 		#classification costs  
	) / 10		# per KLOC		

# #manual
# manualEffect = 0.60
# manualCostKLOC = ( 
	# ((size/125)*securityProfCost*4) + 			# review costs
	# ((size/100)*securityProfCost*4) + 			# prep time
	# (vulns * manualEffect * remediationTime * developerCost) 	# fix costs
	# ) / 1000
	
# manualEffect_h = 0.90
# manualCostKLOC_h = ( 
	# ((size/125)*securityProfCost*4) + 			# review costs
	# ((size/100)*securityProfCost*4) + 			# prep time
	# (vulns * manualEffect * remediationTime * developerCost) 	# fix costs
	# ) / 1000
	
# manualEffect_l = 0.30
# manualCostKLOC_l = ( 
	# ((size/125)*securityProfCost*4) + 			# review costs
	# ((size/100)*securityProfCost*4) + 			# prep time
	# (vulns * manualEffect * remediationTime * developerCost) 	# fix costs
	# ) / 1000


# costsSynopsys = c(coverityCostKLOC_h, coverityCostKLOC_l, coverityAndDefensicsCostKLOC)
# effectsSynopsys = c(coverityEffect_h, coverityEffect_l, coverityAndDefensicsEffect)

# costsFortify = c(fortifyCostKLOC)
# effectsFortify = c(fortifyEffect)

costsCoverity = c(coverityCostKLOC_h)
effectsCoverity = c(coverityEffect_h)

costs2008Paper = c(ACostKLOCLow, ACostKLOCHigh, BCostKLOCLow, BCostKLOCHigh, CCostKLOCLow, CCostKLOCHigh)
effects2008Paper = c(AEffect, AEffect, BEffect, BEffect, CEffect, CEffect)

costs2015Paper = c(paper2015CostKLOC_l, paper2015CostKLOC_h)
effects2015Paper = c(paper2015Effect_l, paper2015Effect_h)

#costsManual = c(manualCostKLOC_h, manualCostKLOC, manualCostKLOC_l)
#effectsManual = c(manualEffect_h, manualEffect, manualEffect_l)


#allCosts = c(costsSynopsys,costsFortify,costs2008Paper,costs2015Paper)
#allEff = c(effectsSynopsys,effectsFortify,effects2008Paper,effects2015Paper)
#test_S3H <-nls(allEff~S3H_resid(allCosts,1,phi,gamma), start=list(phi=1,gamma=0.02), algorithm="port", lower=c(0,0), upper=c(10,10))
#test_S1 <-nls(allEff~S1_resid(allCosts,1,a,1), start=list(a=0.001), algorithm="port", lower=c(0), upper=c(100))
#s1_test = S1_resid(allCosts, 1, 0.0008768/developerCostUK, 1)

costsNasa = c(20491.80 * 1000)
effectsNasa = c(0.999)




#costsSEL4 = c(78*1000*convRate,127*1000*convRate,141.57*1000)
#effectsSEL4 = c(0.99,0.99,0.99)
# (28.6*49500)/10000 = 141.57
# $78/SLOC * 10,000 SLOC = 780K, div 4.1py = 190,243.90 $/py
# in pounds this is 190,243.90 * 0.814 = 154,858.50 $/py -- ~155K
# average developers are ~50K.  50K / 0.814 = 61,425.06.  x 4.1 = 251,842.80, or $25.18/SLOC 

#costsSEL4 = c(25.18*1000*convRate,78*1000*convRate)
#costs of security proof only, costs of correctness plus security proofs
costsSEL4 = c((78*1000), (190000*6)/10+(78*1000)) #, (190000*11)/10+(78*1000))
effectsSEL4 = c(1.0, 1.0) #, 1.0)

#costsRuleOfThumb = c(1000*1000*convRate)
#costsRuleOfThumb = c(1000*1000)
#effectsRuleOfThumb = c(1-(1.0/6))


# s1_best_beta_cost_dev 	= S1_resid(EffAssuredDev_1M, 1, 0.3837, 1)
# s3H_best_cost_dev 		= S3H_resid(EffAssuredDev_1M,1, 0.07637, 4.36077) 
#CostAssuredDev_1M = EffAssuredDev_1M * developerCostMonth
s1_best_beta_cost_dev 	= S1_resid(EffAssuredDev_20K, 1, 0.5142, 1)
s3H_best_cost_dev 		= S3H_resid(EffAssuredDev_20K,1, 0.3395, 1.0031) 
CostAssuredDev_20K = EffAssuredDev_20K * developerCostMonth
CostAssuredDev_5K = EffAssuredDev_5K * developerCostMonth

plot(CostAssuredDev_20K, eal_alt, type="b", xlab="Cost / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), xlim=range(0:220000), lty=1, pch=0 )
#lines(CostAssuredDev_5K, eal_alt, type="b", lty=2, pch=0)
#xlim=range(0:10000)

# lines(CostAssuredDev_1M, s1_best_beta_cost_dev, type="b", lty=1, pch=1)
# lines(CostAssuredDev_1M, s3H_best_cost_dev, type="b", lty=1, pch=2)
 lines(CostAssuredDev_20K, s1_best_beta_cost_dev, type="b", lty=1, pch=1)
 lines(CostAssuredDev_20K, s3H_best_cost_dev, type="b", lty=1, pch=2)

lines(costsCoverity,effectsCoverity,type="p", lty=2, pch=19)
#lines(costsSynopsys,effectsSynopsys,type="p", lty=2, pch=19)
#segments(costsSynopsys[1],effectsSynopsys[1],costsSynopsys[2],effectsSynopsys[2])

lines(costs2008Paper,effects2008Paper,type="p", lty=2, pch=19)
#segments(costs2008Paper[1],effects2008Paper[1],costs2008Paper[2],effects2008Paper[2])
#segments(costs2008Paper[3],effects2008Paper[3],costs2008Paper[4],effects2008Paper[4])
#segments(costs2008Paper[5],effects2008Paper[5],costs2008Paper[6],effects2008Paper[6])


#lines(costsManual,effectsManual,type="p", lty=2, pch=14)
#segments(costsManual[1],effectsManual[1],costsManual[2],effectsManual[2])
#segments(costsManual[2],effectsManual[2],costsManual[3],effectsManual[3])

#lines(costsFortify,effectsFortify,type="p", lty=2, pch=15)

lines(costs2015Paper,effects2015Paper,type="p", lty=2, pch=19)
#segments(costs2015Paper[1],effects2015Paper[1],costs2015Paper[2],effects2015Paper[2])

#lines(costsNasa,effectsNasa,type="p", lty=2, pch=15)

#lines(costsRuleOfThumb,effectsRuleOfThumb,type="p", lty=2, pch=4)
lines(costsSEL4,effectsSEL4,type="p", lty=2, pch=15)
segments(costsSEL4[1],effectsSEL4[1],costsSEL4[2],effectsSEL4[2])


#legend("bottomright",
 #      c("COSECMO EAL", "S1", "S3H", "Synopsys: Coverity (low-high)", "Synopsys: Coverity+Defensics", "2008 SAT Paper", "2015 SAT Paper (low-high)", "Fortify", "Manual Review (low-high)"), 
 #      lty=c(1,1,1,0,0,0,0,0,0),
 #      pch=c(0,1,2,19,19,18,17,15,14))
 
 
 legend("bottomright",
       c("COSECMO EAL", "S1", "S3H", "SEL4 Data", "Static Analysis Data"), 
       lty=c(1,1,1,0,0),
       pch=c(0,1,2,15,19))



		





###########################################################################

# CostAssuredDevel_5K[i] = CalculateCostAssureDevel(baseEffortImpl[1], COSECMO_5KLOC[3],2.5, i, developerCostUK)
# CostAssuredDevel_20K[i] = CalculateCostAssureDevel(baseEffortImpl[2], COSECMO_20KLOC[3],2.5, i, developerCostUK)
# CostAssuredDevel_100K[i] = CalculateCostAssureDevel(baseEffortImpl[3], COSECMO_100KLOC[3],2.5, i, developerCostUK)
# CostAssuredDevel_1M[i] = CalculateCostAssureDevel(baseEffortImpl[4], COSECMO_1MLOC[3],2.5, i, developerCostUK)
	 	
# CostAssuredDevel_5K[i] = CalculateCostIntAssure_PM(baseEffortImpl[1], COSECMO_Percent3[1], 2.5, i)
# CostAssuredDevel_20K[i] = CalculateCostIntAssure_PM(baseEffortImpl[2], COSECMO_Percent3[2],2.5, i)
# CostAssuredDevel_100K[i] = CalculateCostIntAssure_PM(baseEffortImpl[3], COSECMO_Percent3[3], 2.5, i)
# CostAssuredDevel_1M[i] = CalculateCostIntAssure_PM(baseEffortImpl[4], COSECMO_Percent3[4], 2.5, i)

### why am I dividing alpha by the per man month cost here????
## these work...?
# s1_best_beta_cost_req = S1_resid(CostAssuredReqs_1M, 1, 0.3837/consultantCostUK, 1)
# s3H_best_cost_req = S3H_resid(CostAssuredReqs_1M, 1, 0.07637/consultantCostUK, 4.36077) 
# s1_best_beta_cost_des = S1_resid(CostAssuredDes_1M, 1, 0.3837/architectCostUK, 1)
# s3H_best_cost_des = S3H_resid(CostAssuredDes_1M, 1, 0.07637/architectCostUK, 4.36077) 
# s1_best_beta_cost_dev = S1_resid(CostAssuredDevel_1M, 1, 0.3837/developerCostUK, 1)
# s3H_best_cost_dev = S3H_resid(CostAssuredDevel_1M, 1, 0.07637/developerCostUK, 4.36077) 
###

#s1_best_cost = S1_resid(CostAssuredDevel_1M, 1, 0.1711, 1.8385)
#s2_best_cost = S2_resid(CostAssuredDevel_1M, 0.921, 2.545)
#s2_best_v_cost = S2_resid(CostAssuredDevel_1M, 0.921, 1)
#s4H_best_cost = S4H_resid(CostAssuredDevel_1M, 1, 0.3004, 0.4276) 
#s6H_best_cost = S6H_resid(CostAssuredDevel_1M, 0.8223, 0.1192) 
#s6H_best_v_cost = S6H_resid(CostAssuredDevel_1M, 1, 0.1433)
#s3W_best_cost = S3W_resid(CostAssuredDevel_1M, 0.7976, 4.2684, 2.0100) 
#s3W_best_v_cost = S3W_resid(CostAssuredDevel_1M, 1, 2.616, 2.010)
#s5W_best_cost = S5W_resid(CostAssuredDevel_1M, 0.9472, 5.2355, 1.0100) 
#s5W_best_v_cost = S3W_resid(CostAssuredDevel_1M, 1, 4.851, 1.010)


#lines(CostAssuredDevel_1M, s1_best_cost, type="b", lty=2, pch=19)
#lines(CostAssuredDevel_1M, s2_best_cost, type="b", lty=2, pch=18)
#lines(CostAssuredDevel_1M, s2_best_v_cost, type="b", lty=3, pch=18)
#lines(CostAssuredDevel_1M, s4H_best_cost, type="b", lty=2, pch=16)
#lines(CostAssuredDevel_1M, s6H_best_cost, type="b", lty=2, pch=15)
#lines(CostAssuredDevel_1M, s6H_best_v_cost, type="b", lty=3, pch=15)
#lines(CostAssuredDevel_1M, s3W_best_cost, type="b", lty=2, pch=14)
#lines(CostAssuredDevel_1M, s3W_best_v_cost, type="b", lty=3, pch=14)
#lines(CostAssuredDevel_1M, s5W_best_cost, type="b", lty=2, pch=13)
#lines(CostAssuredDevel_1M, s5W_best_v_cost, type="b", lty=3, pch=13)


# NLS_S1_1M_beta_reqs <-nls(eal_alt~S1_resid(CostAssuredReqs_1M,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
# NLS_S1_1M_beta_des <-nls(eal_alt~S1_resid(CostAssuredDes_1M,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
# NLS_S1_1M_beta_devel <-nls(eal_alt~S1_resid(CostAssuredDevel_1M,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
# NLS_S3H_1M_reqs <-nls(eal_alt~S3H_resid(CostAssuredReqs_1M,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
# NLS_S3H_1M_des <-nls(eal_alt~S3H_resid(CostAssuredDes_1M,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
# NLS_S3H_1M_devel <-nls(eal_alt~S3H_resid(CostAssuredDevel_1M,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))

# NLS_S1_5K <- nls(eal_alt~S1_resid(COSECMO_5KLOC,v,a,b))
# NLS_S1_20K <- nls(eal_alt~S1_resid(COSECMO_20KLOC,v,a,b))
# NLS_S1_100K <- nls(eal_alt~S1_resid(COSECMO_100KLOC,v,a,b))
# NLS_S1_1M <- nls(eal_alt~S1_resid(COSECMO_1MLOC,v,a,b))
# Hold alpha
# NLS_S1_5K_alpha <- nls(eal_alt~(1-b/(COSECMO_5KLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
# NLS_S1_20K_alpha <- nls(eal_alt~(1-b/(COSECMO_20KLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
# NLS_S1_100K_alpha <- nls(eal_alt~(1-b/(COSECMO_100KLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
# NLS_S1_1M_alpha <- nls(eal_alt~(1-b/(COSECMO_1MLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
#  These all have fairly poor residuals (0.2012 -- 0.2779)










# #############################################################################
# #############################################################################
# #############################################################################
# ### older code the rest of the way down...



# #############################################################################

# #s1_vals = S1_resid(rng, 0.99, 1, 1)  #0.4, 1
# #s2_vals = S2_resid(rng, 0.75, 1)
# #s3_vals = S3_resid(rng, 0.99, 1, 0.1) 

# s1_vals = S1_resid(COSECMO_1MLOC, 0.99, 1, 1)
# s2_vals = S2_resid(COSECMO_1MLOC, 0.99, 1)
# s3H_vals = S3H_resid(COSECMO_1MLOC, 0.99, 1, 0.1) 



# #############################################################################
# # Plots
# #

# # plot(rng, s1_vals, type="l", xlab="Cost (Factor)", ylab="Vulnerability (v)", lty=2)
# # lines(rng, s2_vals, type="l", lty=3)
# # lines(rng, s3_vals, type="l", lty=4)
# plot(COSECMO_1MLOC, eal_alt, type="b", xlab="Cost / Factor", ylab="Residual Vulnerability v-S()", pch=13, lty=1 )
# #plot(COSECMO_1MLOC, s1_vals, type="l", xlab="Cost (Factor)", ylab="Vulnerability (v)", lty=2)
# lines(COSECMO_1MLOC, s1_vals, type="l", lty=2)
# lines(COSECMO_1MLOC, s2_vals, type="l", lty=3)
# lines(COSECMO_1MLOC, s3_vals, type="l", lty=4)

# lines(COSECMO_5KLOC, eal_alt, type="b", pch=16, lty=1 )
# lines(COSECMO_20KLOC, eal_alt, type="b", pch=15, lty=1 )
# lines(COSECMO_100KLOC, eal_alt, type="b", pch=14, lty=1 )
# #lines(COSECMO_1MLOC, eal_alt, type="b", pch=13, lty=1 )
# par(new = T)
# plot(COSECMO_1MLOC, eal, type="n", axes=F, xlab=NA, ylab=NA, lty=1 ) 
# axis(side = 4)
# mtext(side = 4, line=-2, "EAL")

# # par(new = T)
# # plot(COSECMO_5KLOC, eal, type="b", pch=16, axes=F, xlab=NA, ylab=NA, lty=1 )
# # par(new = T)
# # plot(COSECMO_20KLOC, eal, type="b", pch=15, axes=F, xlab=NA, ylab=NA, lty=1 )
# # par(new = T)
# # plot(COSECMO_100KLOC, eal, type="b", pch=14, axes=F, xlab=NA, ylab=NA, lty=1 )
 # # par(new = T)
 # # plot(COSECMO_1MLOC, eal, type="b", pch=13, axes=F, xlab=NA, ylab=NA, lty=1 )
 # # axis(side = 4)
 # # mtext(side = 4, line=-2, "EAL")

# legend("bottomright",
       # c("S1","S2","S3","COSECMO-5K","COSECMO-20K","COSECMO-100K","COSECMO-1M"), 
       # lty=c(2,3,4,1,1,1,1),
       # pch=c(0,0,0,16,15,14,13))
       










# ###################################
# # some test code

# seq1 <- seq(0,1, by=0.01)
# seq2 <- seq(0,10, by=0.1)

# varyPhi1 <- S3H(COSECMO_1MLOC, 1, 0.1, 0.02)
# varyPhi2 <- S3H(COSECMO_1MLOC, 1, 0.5, 0.02)
# varyPhi3 <- S3H(COSECMO_1MLOC, 1, 0.75, 0.02)
# varyPhi4 <- S3H(COSECMO_1MLOC, 1, 1, 0.02)
# varyPhi5 <- S3H(COSECMO_1MLOC, 1, 1.5, 0.02)

# plot(COSECMO_1MLOC,varyPhi5, type="b", pch=16)
# lines(COSECMO_1MLOC,varyPhi4, type="b", pch=15)
# lines(COSECMO_1MLOC,varyPhi3, type="b", pch=14)
# lines(COSECMO_1MLOC,varyPhi2, type="b", pch=13)
# lines(COSECMO_1MLOC,varyPhi1, type="b", pch=12)

# varyGamma <- S3H(100, 1, 1, seq2)





