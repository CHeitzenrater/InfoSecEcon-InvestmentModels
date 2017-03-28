####################################################################################################
#
# Examining various aspects of the GL model
#
########################

source("./models/GL.R")
source("./models/GL-Hausken.R")


S1S2 <- function(){
	investment = seq(0, 2, by=0.1)
	startingVul = 1.0
	alpha = 1.0
	beta = 1.0
	alpha2 = 1.0
	loss = 10

	### S1 followed by S2
	outS1 = S1(investment, startingVul, alpha, beta)
	combined0 = ((outS1) - S2(0, outS1, alpha2))*loss-(investment)
	combined1 = ((outS1) - S2(1, outS1, alpha2))*loss-(investment+1)
	combined2 = ((outS1) - S2(3.7, outS1, alpha2))*loss-(investment+3.7)
	combined3 = ((outS1) - S2(5, outS1, alpha2))*loss-(investment+5)
	combined5 = ((outS1) - S2(10, outS1, alpha2))*loss-(investment+10)
	plot(investment, combined1, type="o", ylim=range(-11,2), lty=1, pch=15, ylab="ENBIS", xlab="SSE Investment")
	abline(0, 0, col = "black")
	lines(investment, combined2, type="o", lty=1, pch=0)
	lines(investment, combined3, type="o", lty=1, pch=16)
	lines(investment, combined5, type="o", lty=1, pch=17)
	lines(investment, combined0, type="o", lty=1, pch=18)
	#lines(investment, outS1, type="l", lty=1)
	legend("top", c("z2=0","z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1,1), pch=c(18,15,0,16,17), horiz=TRUE, bty="n", cex=0.65)
}


S1S2VaryAlpha2 <- function(){
	investment = seq(0, 2.5, by=0.1)
	startingVul = 1.0
	alpha = 1.0
	beta = 1.0	
	loss = 10

	### S1 followed by S2
	outS1 = S1(investment, startingVul, alpha, beta)
	combined0 = ((outS1) - S2(0, outS1, 1.0))*loss-(investment)
	combined1 = ((outS1) - S2(3.7, outS1, 0.1))*loss-(investment+3.7)
	combined2 = ((outS1) - S2(3.7, outS1, 0.5))*loss-(investment+3.7)
	combined3 = ((outS1) - S2(3.7, outS1, 1.0))*loss-(investment+3.7)
	combined5 = ((outS1) - S2(3.7, outS1, 1.2))*loss-(investment+3.7)
	plot(investment, combined1, type="o", ylim=range(-5,5), lty=1, pch=15, ylab="ENBIS", xlab="SSE Investment")
	abline(0, 0, col = "black")
	lines(investment, combined2, type="o", lty=1, pch=0)
	lines(investment, combined3, type="o", lty=1, pch=16)
	lines(investment, combined5, type="o", lty=1, pch=17)
	lines(investment, combined0, type="o", lty=1, pch=18)
	#lines(investment, outS1, type="l", lty=1)
	legend("top", c("No Post","alpha2=0.1","alpha2=0.5", "alpha2=1.0", "alpha2=1.2"), lty=c(1,1,1,1,1), pch=c(18,15,0,16,17), horiz=TRUE, bty="n", cex=0.65)
}


S1S2VaryAlpha <- function(){
	investment = seq(0, 2.0, by=0.1)
	startingVul = 1.0
	alpha2 = 1.0
	beta = 1.0	
	loss = 10

	### S1 followed by S2
	outS1 = S1(investment, startingVul, 1, 1)
	outS11 = S1(investment, startingVul, 0.1, beta)
	outS12 = S1(investment, startingVul, 0.5, beta)
	outS13 = S1(investment, startingVul, 1.0, beta)
	outS14 = S1(investment, startingVul, 2, beta)
	combined0 = ((outS1) - S2(0, outS1, alpha2))*loss-(investment)
	combined1 = ((outS11) - S2(3.7, outS11, alpha2))*loss-(investment+3.7)
	combined2 = ((outS12) - S2(3.7, outS12, alpha2))*loss-(investment+3.7)
	combined3 = ((outS13) - S2(3.7, outS13, alpha2))*loss-(investment+3.7)
	combined5 = ((outS14) - S2(3.7, outS14, alpha2))*loss-(investment+3.7)
	plot(investment, combined1, type="o", ylim=range(-5,2), lty=1, pch=15, ylab="ENBIS", xlab="SSE Investment")
	abline(0, 0, col = "black")
	lines(investment, combined2, type="o", lty=1, pch=0)
	lines(investment, combined3, type="o", lty=1, pch=16)
	lines(investment, combined5, type="o", lty=1, pch=17)
	lines(investment, combined0, type="o", lty=1, pch=18)
	#lines(investment, outS1, type="l", lty=1)
	legend("top", c("No Post","alpha=0.1","alpha=0.5", "alpha=1.0", "alpha=1.2"), lty=c(1,1,1,1,1), pch=c(18,15,0,16,17), horiz=TRUE, bty="n", cex=0.65)
}

S1S2VaryBeta <- function(){
	investment = seq(0, 2.0, by=0.1)
	startingVul = 1.0
	alpha2 = 1.0
	alpha = 1.0	
	loss = 10

	### S1 followed by S2
	outS1 = S1(investment, startingVul, 1, 1)
	outS11 = S1(investment, startingVul, alpha, 1)
	outS12 = S1(investment, startingVul, alpha, 2)
	outS13 = S1(investment, startingVul, alpha, 3)
	outS14 = S1(investment, startingVul, alpha, 4)
	combined0 = ((outS1) - S2(0, outS1, alpha2))*loss-(investment)
	combined1 = ((outS11) - S2(3.7, outS11, alpha2))*loss-(investment+3.7)
	combined2 = ((outS12) - S2(3.7, outS12, alpha2))*loss-(investment+3.7)
	combined3 = ((outS13) - S2(3.7, outS13, alpha2))*loss-(investment+3.7)
	combined5 = ((outS14) - S2(3.7, outS14, alpha2))*loss-(investment+3.7)
	plot(investment, combined1, type="o", ylim=range(-5,2), lty=1, pch=15, ylab="ENBIS", xlab="SSE Investment")
	abline(0, 0, col = "black")
	lines(investment, combined2, type="o", lty=1, pch=0)
	lines(investment, combined3, type="o", lty=1, pch=16)
	lines(investment, combined5, type="o", lty=1, pch=17)
	lines(investment, combined0, type="o", lty=1, pch=18)
	#lines(investment, outS1, type="l", lty=1)
	legend("top", c("No Post","beta=1","beta=2", "beta=3", "beta=4"), lty=c(1,1,1,1,1), pch=c(18,15,0,16,17), horiz=TRUE, bty="n", cex=0.65)
}




#### why are these always negative???

#####################

### S3 followed by S1
investment = seq(0, 10, by=0.5)
startingVul = 1.0
alpha = 1.0
beta = 1.0
alpha2 = 1.0
loss = 10

outS3 = S3H(investment, startingVul, 1, 1)
combined0 = ((outS3) - S1(0, outS3, alpha, beta))*loss-(investment)
combined1 = ((outS3) - S1(1, outS3, alpha, beta))*loss-(investment+1)
combined2 = ((outS3) - S1(3.7, outS3, alpha, beta))*loss-(investment+3.7)
combined3 = ((outS3) - S1(5, outS3, alpha, beta))*loss-(investment+5)
combined5 = ((outS3) - S1(10, outS3, alpha, beta))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
lines(investment, combined0, type="o", lty=1, pch=18)
	#lines(investment, outS1, type="l", lty=1)
	legend("top", c("z2=0","z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1,1), pch=c(18,15,0,16,17), horiz=TRUE, bty="n", cex=0.65)


### S3 followed by S2
outS3 = S3H(investment, startingVul, 1, 1)
combined0 = ((outS3) - S1(0, outS3, alpha, beta))*loss-(investment)
combined1 = ((outS3) - S2(1, outS3, alpha))*loss-(investment+1)
combined2 = ((outS3) - S2(3.7, outS3, alpha))*loss-(investment+3.7)
combined3 = ((outS3) - S2(5, outS3, alpha))*loss-(investment+5)
combined5 = ((outS3) - S2(10, outS3, alpha))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
lines(investment, combined0, type="o", lty=1, pch=18)
	#lines(investment, outS1, type="l", lty=1)
	legend("top", c("z2=0","z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1,1), pch=c(18,15,0,16,17), horiz=TRUE, bty="n", cex=0.65)



#####################

S4_list <- function( z=1, v=1, mu=1, k=0.5 ){
	ans <- vector(mode="numeric", length=0);
	if( length(z) > 1 ){
		for(i in 1:length(z) ){
			ans = c(ans, S4H(z[i], v, mu, k))
		}
	} else {
		ans = S4(z, v, mu, k)
	}
	return(ans)	
}

### S4 followed by S1
outS4 = S4_list(investment, startingVul, 1, 0.5)
combined1 = ((outS4) - S1(1, outS4, alpha, beta))*loss-(investment+1)
combined2 = ((outS4) - S1(3.7, outS4, alpha, beta))*loss-(investment+3.7)
combined3 = ((outS4) - S1(5, outS4, alpha, beta))*loss-(investment+5)
combined5 = ((outS4) - S1(10, outS4, alpha, beta))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
#lines(investment, outS3, type="l", lty=1)
legend("top", c("z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1), pch=c(15,0,16,17), horiz=TRUE, bty="n", cex=0.65)


### S4 followed by S2
outS4 = S4_list(investment, startingVul, 1, 0.5)
combined1 = ((outS4) - S2(1, outS4, alpha))*loss-(investment+1)
combined2 = ((outS4) - S2(3.7, outS4, alpha))*loss-(investment+3.7)
combined3 = ((outS4) - S2(5, outS4, alpha))*loss-(investment+5)
combined5 = ((outS4) - S2(10, outS4, alpha))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
#lines(investment, outS3, type="l", lty=1)
legend("top", c("z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1), pch=c(15,0,16,17), horiz=TRUE, bty="n", cex=0.65)


#####################
S5_list <- function( z=1, v=1, omega=1, k=2  ){
	ans <- vector(mode="numeric", length=0)
	if( length(z) > 1 ){
		for(i in 1:length(z) ){
			ans = c(ans, S5H(z[i], v, omega, k))
		}
	} else {
		ans = S5(z, v, omega, k)
	}
	return(ans)	
}

### S5 followed by S1
outS5 = S5_list(investment, startingVul, 1, 2)
combined1 = ((outS5) - S1(1, outS5, alpha, beta))*loss-(investment+1)
combined2 = ((outS5) - S1(3.7, outS5, alpha, beta))*loss-(investment+3.7)
combined3 = ((outS5) - S1(5, outS5, alpha, beta))*loss-(investment+5)
combined5 = ((outS5) - S1(10, outS5, alpha, beta))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
#lines(investment, outS3, type="l", lty=1)
legend("top", c("z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1), pch=c(15,0,16,17), horiz=TRUE, bty="n", cex=0.65)


### S5 followed by S2
outS5 = S5_list(investment, startingVul, 0.1, 2)
combined1 = ((outS5) - S2(1, outS5, alpha))*loss-(investment+1)
combined2 = ((outS5) - S2(3.7, outS5, alpha))*loss-(investment+3.7)
combined3 = ((outS5) - S2(5, outS5, alpha))*loss-(investment+5)
combined5 = ((outS5) - S2(10, outS5, alpha))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
#lines(investment, outS3, type="l", lty=1)
legend("top", c("z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1), pch=c(15,0,16,17), horiz=TRUE, bty="n", cex=0.65)


#####################
S6_list <- function( z=1, v=1, lambda=0.1  ){
	ans <- vector(mode="numeric", length=0)
	if( length(z) > 1 ){
		for(i in 1:length(z) ){
			ans = c(ans, S6H(z[i], v, lambda))
		}
	} else {
		ans = S6(z, v, lambda)
	}
	return(ans)	
}

### S6 followed by S1
outS6 = S6_list(investment, startingVul, 0.1)
combined1 = ((outS6) - S1(1, outS6, alpha, beta))*loss-(investment+1)
combined2 = ((outS6) - S1(3.7, outS6, alpha, beta))*loss-(investment+3.7)
combined3 = ((outS6) - S1(5, outS6, alpha, beta))*loss-(investment+5)
combined5 = ((outS6) - S1(10, outS6, alpha, beta))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
#lines(investment, outS3, type="l", lty=1)
legend("top", c("z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1), pch=c(15,0,16,17), horiz=TRUE, bty="n", cex=0.65)


### S6 followed by S2
outS6 = S6_list(investment, startingVul, 1, 2)
combined1 = ((outS6) - S2(1, outS6, alpha))*loss-(investment+1)
combined2 = ((outS6) - S2(3.7, outS6, alpha))*loss-(investment+3.7)
combined3 = ((outS6) - S2(5, outS6, alpha))*loss-(investment+5)
combined5 = ((outS6) - S2(10, outS6, alpha))*loss-(investment+10)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=15, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=0)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
#lines(investment, outS3, type="l", lty=1)
legend("top", c("z2=1", "z2=3.7", "z2=5", "z2=10"), lty=c(1,1,1,1), pch=c(15,0,16,17), horiz=TRUE, bty="n", cex=0.65)










	
# outS1 = S3H(investment, 0.5, 1, 0.02)			
# outS2 = S1(investment, 0.5, 0.5, 1)

# #plot(invest, outS1, type="o", ylim=range(0,0.5), lty=1, pch=0, ylab="S(z,v)", xlab="Investment")	
# #lines(invest, outS2, type="o", lty=1, pch=15)
	
# enbisS1 = GL_ENBIS_SZV( investment, 1, outS1, 16 )
# enbisS2 = GL_ENBIS_SZV( investment, 1, outS2, 16 )
	
# plot(investment, enbisS1, type="o", ylim=range(-10,10), lty=1, pch=0, ylab="ENBIS", xlab="Investment")
# lines(investment, enbisS2, type="o", lty=1, pch=15)
	
