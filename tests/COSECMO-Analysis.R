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
eal_alt = c(0.143,0.286,0.429,0.571,0.714,0.857,1.0)
eal0 = seq(0,7)
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
# NLS_S1_5K <- nls(eal_alt~S1_resid(COSECMO_5KLOC,v,a,b))
# NLS_S1_20K <- nls(eal_alt~S1_resid(COSECMO_20KLOC,v,a,b))
# NLS_S1_100K <- nls(eal_alt~S1_resid(COSECMO_100KLOC,v,a,b))
# NLS_S1_1M <- nls(eal_alt~S1_resid(COSECMO_1MLOC,v,a,b))

# Hold beta = 1
NLS_S1_5K_beta <- nls(eal_alt~S1_resid(COSECMO_5KLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
NLS_S1_20K_beta <- nls(eal_alt~S1_resid(COSECMO_20KLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
NLS_S1_100K_beta <- nls(eal_alt~S1_resid(COSECMO_100KLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
NLS_S1_1M_beta <-nls(eal_alt~S1_resid(COSECMO_1MLOC,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))

# Hold alpha
# NLS_S1_5K_alpha <- nls(eal_alt~(1-b/(COSECMO_5KLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
# NLS_S1_20K_alpha <- nls(eal_alt~(1-b/(COSECMO_20KLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
# NLS_S1_100K_alpha <- nls(eal_alt~(1-b/(COSECMO_100KLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
# NLS_S1_1M_alpha <- nls(eal_alt~(1-b/(COSECMO_1MLOC + 1)^c), start=list(b=1,c=1), algorithm="port", lower=c(0,1), upper=c(1,10))
#  These all have fairly poor residuals (0.2012 -- 0.2779)


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
NLS_S5H_5K <- nls(eal_alt~S5H_resid(COSECMO_5KLOC,v,omega,k), start=list(v=0,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
NLS_S5H_20K <- nls(eal_alt~S5H_resid(COSECMO_20KLOC,v,omega,k), start=list(v=0,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
NLS_S5H_100K <- nls(eal_alt~S5H_resid(COSECMO_100KLOC,v,omega,k), start=list(v=0,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
NLS_S5H_1M <-nls(eal_alt~S5H_resid(COSECMO_1MLOC,v,omega,k), start=list(v=0,omega=0.01,k=2), algorithm="port", lower=c(0,0,1.001), upper=c(1,10,10))
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
## 
######## S4W
# all variables are free
# NLS_S4W_5K <- nls(eal_alt~S4W_resid(COSECMO_5KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
# NLS_S4W_20K <- nls(eal_alt~S4W_resid(COSECMO_20KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
# NLS_S4W_100K <- nls(eal_alt~S4W_resid(COSECMO_100KLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
# NLS_S4W_1M <-nls(eal_alt~S4W_resid(COSECMO_1MLOC,v,b,k), start=list(v=1,b=5,k=3), algorithm="port", lower=c(0,0.01,2.01), upper=c(1,35,10))
#
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


#############################################################################
#############################################################################
#############################################################################
# Code for the COCOMO projected size

# domain of the input costs (man months)
#rng_Code = seq(0,32.25)

#baseEffortReq_5K = 2.2
#baseEffortReq_20K = 10.4
#baseEffortReq_100K = 61.2
#baseEffortReq_1M = 770.4
#baseEffortReq = c(2.2,10.4,61.2,770.4)
baseEffortReqs = c(2.2/5,10.4/20,61.2/100,770.4/1000)

#baseEffortDes_5K = 3.9
#baseEffortDes_20K = 17.7
#baseEffortDes_100K = 104.3
#baseEffortDes_1M = 312.4
#baseEffortDes = c(3.9,17.7,104.3,312.4)
baseEffortDes = c(3.9/5,17.7/20,104.3/100,312.4/1000)

#baseEffortImpl_5K = 5.5
#baseEffortImpl_20K = 25.2
#baseEffortImpl_100K = 147.5
#baseEffortImpl_1M = 1856.9
#baseEffortImpl = c(5.5,25.2,147.5,1856.9)
baseEffortImpl = c(5.5/5,25.2/20,147.5/100,1856.9/1000)
#baseEffortImpl = c(2816.64,3226.33,3776.86,4754.74)

COSECMO_Percent3 = c(0.20,0.40,0.60,0.80)

consultantCostUK = 4046.42
architectCostUK = 4437.17
developerCostUK = 2560.58

CostAssuredReqs_5K = vector(mode="numeric", length=7)
CostAssuredReqs_20K = vector(mode="numeric", length=7)
CostAssuredReqs_100K = vector(mode="numeric", length=7)
CostAssuredReqs_1M = vector(mode="numeric", length=7)

CostAssuredDes_5K = vector(mode="numeric", length=7)
CostAssuredDes_20K = vector(mode="numeric", length=7)
CostAssuredDes_100K = vector(mode="numeric", length=7)
CostAssuredDes_1M = vector(mode="numeric", length=7)

CostAssuredDevel_5K = vector(mode="numeric", length=7)
CostAssuredDevel_20K = vector(mode="numeric", length=7)
CostAssuredDevel_100K = vector(mode="numeric", length=7)
CostAssuredDevel_1M = vector(mode="numeric", length=7)

for(i in 1:7){
	# CostAssuredDevel_5K[i] = CalculateCostAssureDevel(baseEffortImpl[1], COSECMO_5KLOC[3],2.5, i, developerCostUK)
	# CostAssuredDevel_20K[i] = CalculateCostAssureDevel(baseEffortImpl[2], COSECMO_20KLOC[3],2.5, i, developerCostUK)
	# CostAssuredDevel_100K[i] = CalculateCostAssureDevel(baseEffortImpl[3], COSECMO_100KLOC[3],2.5, i, developerCostUK)
	# CostAssuredDevel_1M[i] = CalculateCostAssureDevel(baseEffortImpl[4], COSECMO_1MLOC[3],2.5, i, developerCostUK)
	
	 CostAssuredReqs_5K[i] = CalculateCostIntAssure(baseEffortReqs[1], COSECMO_Percent3[1], 2.5, i, consultantCostUK)
	 CostAssuredReqs_20K[i] = CalculateCostIntAssure(baseEffortReqs[2], COSECMO_Percent3[2],2.5, i, consultantCostUK)
	 CostAssuredReqs_100K[i] = CalculateCostIntAssure(baseEffortReqs[3], COSECMO_Percent3[3], 2.5, i, consultantCostUK)
	 CostAssuredReqs_1M[i] = CalculateCostIntAssure(baseEffortReqs[4], COSECMO_Percent3[4], 2.5, i, consultantCostUK)	
	 
	 CostAssuredDes_5K[i] = CalculateCostIntAssure(baseEffortDes[1], COSECMO_Percent3[1], 2.5, i, architectCostUK)
	 CostAssuredDes_20K[i] = CalculateCostIntAssure(baseEffortDes[2], COSECMO_Percent3[2],2.5, i, architectCostUK)
	 CostAssuredDes_100K[i] = CalculateCostIntAssure(baseEffortDes[3], COSECMO_Percent3[3], 2.5, i, architectCostUK)
	 CostAssuredDes_1M[i] = CalculateCostIntAssure(baseEffortDes[4], COSECMO_Percent3[4], 2.5, i, architectCostUK)
	
	 CostAssuredDevel_5K[i] = CalculateCostIntAssure(baseEffortImpl[1], COSECMO_Percent3[1], 2.5, i, developerCostUK)
	 CostAssuredDevel_20K[i] = CalculateCostIntAssure(baseEffortImpl[2], COSECMO_Percent3[2],2.5, i, developerCostUK)
	 CostAssuredDevel_100K[i] = CalculateCostIntAssure(baseEffortImpl[3], COSECMO_Percent3[3], 2.5, i, developerCostUK)
	 CostAssuredDevel_1M[i] = CalculateCostIntAssure(baseEffortImpl[4], COSECMO_Percent3[4], 2.5, i, developerCostUK)
	 
	
	# CostAssuredDevel_5K[i] = CalculateCostIntAssure_PM(baseEffortImpl[1], COSECMO_Percent3[1], 2.5, i)
	# CostAssuredDevel_20K[i] = CalculateCostIntAssure_PM(baseEffortImpl[2], COSECMO_Percent3[2],2.5, i)
	# CostAssuredDevel_100K[i] = CalculateCostIntAssure_PM(baseEffortImpl[3], COSECMO_Percent3[3], 2.5, i)
	# CostAssuredDevel_1M[i] = CalculateCostIntAssure_PM(baseEffortImpl[4], COSECMO_Percent3[4], 2.5, i)
}

s1_best_beta_cost_req = S1_resid(CostAssuredReqs_1M, 1, 0.3837/consultantCostUK, 1)
s3H_best_cost_req = S3H_resid(CostAssuredReqs_1M, 1, 0.07637/consultantCostUK, 4.36077) 

s1_best_beta_cost_des = S1_resid(CostAssuredDes_1M, 1, 0.3837/architectCostUK, 1)
s3H_best_cost_des = S3H_resid(CostAssuredDes_1M, 1, 0.07637/architectCostUK, 4.36077) 

s1_best_beta_cost_dev = S1_resid(CostAssuredDevel_1M, 1, 0.3837/developerCostUK, 1)
s3H_best_cost_dev = S3H_resid(CostAssuredDevel_1M, 1, 0.07637/developerCostUK, 4.36077) 

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

plot(CostAssuredDevel_1M, eal_alt, type="b", xlab="Cost / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=1, pch=0 )

lines(CostAssuredDevel_1M, s1_best_beta_cost_dev, type="b", lty=1, pch=1)
lines(CostAssuredDevel_1M, s3H_best_cost_dev, type="b", lty=1, pch=2)

lines(CostAssuredReqs_1M, eal_alt, type="b", lty=2, pch=0)
lines(CostAssuredReqs_1M, s1_best_beta_cost_req, type="b", lty=2, pch=1)
lines(CostAssuredReqs_1M, s3H_best_cost_req, type="b", lty=2, pch=2)

lines(CostAssuredDes_1M, eal_alt, type="b", lty=3, pch=0)
lines(CostAssuredDes_1M, s1_best_beta_cost_dev, type="b", lty=3, pch=1)
lines(CostAssuredDes_1M, s3H_best_cost_dev, type="b", lty=3, pch=2)


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
legend("bottomright",
       c("Requirements", "S1-reqs", "S3H-reqs", "Design", "S1-des", "S3H-des", "Development", "S1-devel", "S3H-devel"), 
       lty=c(2,2,2,3,3,3,1,1,1),
       pch=c(1,2,3,1,2,3,1,2,3))


# NLS_S1_1M_beta_reqs <-nls(eal_alt~S1_resid(CostAssuredReqs_1M,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
 
# NLS_S1_1M_beta_des <-nls(eal_alt~S1_resid(CostAssuredDes_1M,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))

# NLS_S1_1M_beta_devel <-nls(eal_alt~S1_resid(CostAssuredDevel_1M,v,a,1), start=list(v=1,a=0.001), algorithm="port", lower=c(0,0), upper=c(1,100))
 
# NLS_S3H_1M_reqs <-nls(eal_alt~S3H_resid(CostAssuredReqs_1M,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))
  
# NLS_S3H_1M_des <-nls(eal_alt~S3H_resid(CostAssuredDes_1M,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))

# NLS_S3H_1M_devel <-nls(eal_alt~S3H_resid(CostAssuredDevel_1M,v,phi,gamma), start=list(v=1,phi=1,gamma=0.02), algorithm="port", lower=c(0,0,0), upper=c(1,10,10))




#############################################################################
#############################################################################
# code review and testing plot
size = 1000000
developerCost = 18.36
vulnRate = 0.02
remediationTime = 5
defectRate = 1/1000
manDefectRate = 110/1000

vulns = size * defectRate * vulnRate
vulnsMan = size * manDefectRate * vulnRate

# coverity
coverityEffect = 0.35 * 0.5
coverityFP = 0.15
coverityCostKLOC = ( 
	(0.09 * 1.22 * size) *3 + 			#coverity license costs
	(vulns * coverityEffect * remediationTime * developerCost) +  #fix costs
	((size*defectRate) * coverityEffect * coverityFP * remediationTime * developerCost) #FPcosts 
	) / 1000		# per KLOC

coverityAndDefensicsEffect = 0.50
coverityAndDefensicsCostKLOC = ( 
	(0.09 * 1.22 * size) *3 + 		#coverity license costs
	(11151 * 1.22 * 32) *3 + 		#defensics license costs
	(35000 * 1.22) +   				# onsite consulting
	(vulns * coverityAndDefensicsEffect * remediationTime * developerCost) + #fix costs
	(1000 * coverityEffect * coverityFP * remediationTime * developerCost) 	 #FP costs
	) / 1000    # per KLOC
	
#from 2008 paper
AEffect = 0.375 * 0.5
AFP = 0.221
ACostKLOC = ( 
	(600000 * defectRate * vulnRate * AEffect * remediationTime * developerCost) +  #fix costs
	((600000*defectRate) * AEffect * AFP * remediationTime * developerCost) #FPcosts 
	) / 1000		# per KLOC	
BEffect = 0.286 * 0.5
BFP = 0.053
BCostKLOC = ( 
	(600000 * defectRate * vulnRate * BEffect * remediationTime * developerCost) +  #fix costs
	((600000*defectRate) * AEffect * BFP * remediationTime * developerCost) #FPcosts 
	) / 1000		# per KLOC	
CEffect = 0.25 * 0.5
CFP = 0.06
CCostKLOC = ( 
	(600000 * defectRate * vulnRate * CEffect * remediationTime * developerCost) +  #fix costs
	((600000*defectRate) * CEffect * CFP * remediationTime * developerCost) #FPcosts 
	) / 1000		# per KLOC	

#manual
manualEffect = 0.60
manualCostKLOC = ( 
	((size/125)*developerCost*4) + 			# review costs
	((size/200)*developerCost*4) + 			# prep time
	(vulnsMan * manualEffect * remediationTime * developerCost) 	# fix costs
	) / 1000


costsSynopsys = c(coverityCostKLOC, coverityAndDefensicsCostKLOC)
effectsSynopsys = c(coverityEffect, coverityAndDefensicsEffect)

costs2008Paper = c(ACostKLOC, BCostKLOC, CCostKLOC)
effects2008Paper = c(AEffect, BEffect, CEffect)

costsManual = c(manualCostKLOC)
effectsManual = c(manualEffect)

plot(CostAssuredDevel_1M, eal_alt, type="b", xlab="Cost / KLOC", ylab="Vulnerability Reduction [(1-S(z,v)]", ylim=range(0:1), lty=1, pch=0 )

lines(CostAssuredDevel_1M, s1_best_beta_cost_dev, type="b", lty=1, pch=1)
lines(CostAssuredDevel_1M, s3H_best_cost_dev, type="b", lty=1, pch=2)

lines(costsSynopsys,effectsSynopsys,type="p", lty=2, pch=19)
lines(costs2008Paper,effects2008Paper,type="p", lty=2, pch=18)
lines(costsManual,effectsManual,type="p", lty=2, pch=17)

legend("bottomright",
       c("COSECMO EAL", "S1", "S3H", "Synopsys: Coverity, Coverity+Defensics", "2008 SAT Paper", "Manual Review"), 
       lty=c(1,1,1,0,0,0),
       pch=c(0,1,2,19,18,17))











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
       










###################################
# some test code

seq1 <- seq(0,1, by=0.01)
seq2 <- seq(0,10, by=0.1)

varyPhi1 <- S3H(COSECMO_1MLOC, 1, 0.1, 0.02)
varyPhi2 <- S3H(COSECMO_1MLOC, 1, 0.5, 0.02)
varyPhi3 <- S3H(COSECMO_1MLOC, 1, 0.75, 0.02)
varyPhi4 <- S3H(COSECMO_1MLOC, 1, 1, 0.02)
varyPhi5 <- S3H(COSECMO_1MLOC, 1, 1.5, 0.02)

plot(COSECMO_1MLOC,varyPhi5, type="b", pch=16)
lines(COSECMO_1MLOC,varyPhi4, type="b", pch=15)
lines(COSECMO_1MLOC,varyPhi3, type="b", pch=14)
lines(COSECMO_1MLOC,varyPhi2, type="b", pch=13)
lines(COSECMO_1MLOC,varyPhi1, type="b", pch=12)

varyGamma <- S3H(100, 1, 1, seq2)





