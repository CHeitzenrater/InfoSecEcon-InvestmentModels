# Costs from GAO Report
# EAL					1		2		3			4			5			6			7
# Effort Relative		N/A		1		1.2-3.0		1.7-4.0		N/A			N/A			N/A
# COSECMO				1		1		1.2-1.8		1.5-3.0		2.25-6.0	4.13-13.5	8.8-32.25

COSECMO_High = c(0,1,1,1.8,3.0,6.0,13.5,32.25)
COSECMO_Low = c(0,1,1,1.2,1.5,2.25,4.13,8.81)

COSECMO_5KLOC = c(0,1,1,1.2,1.5,2.25,4.13,8.81)
COSECMO_20KLOC = c(0,1,1,1.4,2.0,3.5,7.25,16.63)
COSECMO_100KLOC = c(0,1,1,1.6,2.5,4.75,10.38,24.44)
COSECMO_1MLOC = c(0,1,1,1.8,3.0,6.0,13.5,32.25)

rng = seq(0,32.25)
eal = seq(0,7)
vul = seq(0,1)

S1_resid <- function( z=1, v=1, alpha=1, beta=1 ){
	return( 1 - (v/((alpha*z) + 1)^beta) )
}

S2_resid <- function( z=1, v=1, alpha ){
	return( 1 - (v^( alpha * z + 1 )) )
}
S3_resid <- function( z=1, v=1, phi=1, gamma=1 ){
	return( 1 - (v / (1 + ( gamma * (exp(phi*z) - 1) ) ) ) )
}

s1_vals = S1_resid(rng, 0.99, 1, 1)  #0.4, 1
s2_vals = S2_resid(rng, 0.75, 1)
s3_vals = S3_resid(rng, 0.99, 1, 0.1) 


plot(rng, s1_vals, type="l", xlab="Cost (Factor)", ylab="Vulnerability (v)", lty=2)
lines(rng, s2_vals, type="l", lty=3)
lines(rng, s3_vals, type="l", lty=4)
par(new = T)
plot(COSECMO_5KLOC, eal, type="b", pch=16, axes=F, xlab=NA, ylab=NA, lty=1 )
par(new = T)
plot(COSECMO_20KLOC, eal, type="b", pch=15, axes=F, xlab=NA, ylab=NA, lty=1 )
par(new = T)
plot(COSECMO_100KLOC, eal, type="b", pch=14, axes=F, xlab=NA, ylab=NA, lty=1 )
par(new = T)
plot(COSECMO_1MLOC, eal, type="b", pch=13, axes=F, xlab=NA, ylab=NA, lty=1 )
axis(side = 4)
mtext(side = 4, line=-2, "EAL")

legend("bottomright",
       c("S1","S2","S3","COSECMO-5K","COSECMO-20K","COSECMO-100K","COSECMO-1M"), 
       lty=c(2,3,4,1,1,1,1),
       pch=c(0,0,0,16,15,14,13))
       
       
       
