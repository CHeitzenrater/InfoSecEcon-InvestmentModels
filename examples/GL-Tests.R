####################################################################################################
#
# Examining various aspects of the GL model
#
########################

source("./models/GL.R")
source("./models/GL-Hausken.R")


investment = seq(0, 10, by=0.5)
startingVul = 1.0
alpha = 1.0
beta = 3.0

outS3 = S3(investment, startingVul, 1, 0.02)	
loss = 10

combined1 = ((outS3) - S1(1, outS3, alpha, beta))*loss-(invest+1)
combined2 = ((outS3) - S1(2, outS3, alpha, beta))*loss-(invest+2)
combined3 = ((outS3) - S1(3, outS3, alpha, beta))*loss-(invest+3)
combined5 = ((outS3) - S1(5, outS3, alpha, beta))*loss-(invest+5)
plot(investment, combined1, type="o", ylim=range(-20,10), lty=1, pch=0, ylab="ENBIS(z1+z2)", xlab="SSE Investment (z1)")
abline(0, 0, col = "black")
lines(investment, combined2, type="o", lty=1, pch=15)
lines(investment, combined3, type="o", lty=1, pch=16)
lines(investment, combined5, type="o", lty=1, pch=17)
lines(investment, outS3, type="l", lty=1)

legend("top", c("z2 = 1", "z2 = 2", "z2 = 3", "z2 = 5"), lty=c(1,1,1,1), pch=c(0,15,16,17), horiz=TRUE, bty="n", cex=0.65)




	
outS1 = S3(investment, 0.5, 1, 0.02)			
outS2 = S1(investment, 0.5, 0.5, 1)

#plot(invest, outS1, type="o", ylim=range(0,0.5), lty=1, pch=0, ylab="S(z,v)", xlab="Investment")	
#lines(invest, outS2, type="o", lty=1, pch=15)
	
enbisS1 = GL_ENBIS_SZV( investment, 1, outS1, 16 )
enbisS2 = GL_ENBIS_SZV( investment, 1, outS2, 16 )
	
plot(investment, enbisS1, type="o", ylim=range(-10,10), lty=1, pch=0, ylab="ENBIS", xlab="Investment")
lines(investment, enbisS2, type="o", lty=1, pch=15)
	
