################################################################################
#
# TODO:   Examining various aspects of the GL-SSE model
#
########################

## Required source files
source("./models/GL.R")
source("./models/GL-Hausken.R")

## out <- GL_SSE_S1_S2(1,b,1,1,100,0.1,0.5,1,1)
## b <- seq(1:100)
## Should alpha be divided by the magnitude of the loss?

####
## 
##
## Execution for Figure XX of [1]:
##    out <- S1S2_ROSI(1000, 0.99, 1, 0.3837, 2.5, 0.1, seq(0, 150, by=5), 
## 		c(0,10,25,50,100,250,370,500), c(-2, 125) )
##
##	  values do get to be negative at some point
##
S1S2_ROSI <- function(loss=10, vuln=1, b=1, a1=0.5, a2=1, d=0.1, preInv, postInv, yrng){
	z1 		= preInv
	z2		= postInv
	v		= vuln
	alpha 	= a1
	beta	= b
	alpha2 	= a2
	delta 	= d
	L 		= loss
	t 		= 1
	
	#rosi <- vector(mode="numeric", length=0)
	rosi <- matrix( ncol=length(z1), nrow=length(z2) )
	
	for(j in 1:length(z2) ){
		for(i in 1:length(z1) ){
			rosi[j,i] = GL_SSE_S1_S2(v,z1[i],z2[j],t,L,delta,alpha,beta,alpha2) / (z1[i]+z2[j])
			#message(c("z1=", z1[i], " z2=", z2[j], " rosi=", rosi[j,i]))
			if(is.nan(rosi[j,i])) rosi[j,i] <- 0
		}
	}

	plot(z1, rosi[1,], type="o", lty=1, cex=0.5, pch=1, ylab=expression(paste("ROSI(", z[1], "+", z[2], ")")), xlab=expression(paste("SSE Investment (", z[1], ")") ), ylim=yrng )
 	abline(0, 0, col = "black")  
 	legV = bquote(z[2]==.(z2[1]))
 	ltyV = 1
 	pchV = 1
	
	for(k in 2:length(z2) ){
		lines(z1, rosi[k,], type="o", lty=1, cex=0.5, pch=k)
		legV = c(legV, bquote(z[2]==.(z2[k])) )
		ltyV = c(ltyV, 1)
		pchV = c(pchV, k)
	}

	legend("top", legend=as.expression(legV), lty=ltyV, pch=pchV, horiz=TRUE, bty="n", cex=0.65)
	
	which(rosi == max(rosi), arr.ind = TRUE)

	return(rosi)
}

####
## 
##
## Execution for Figure XX of [1]:
##    out <- S1S2_ENBIS(1000, 0.99, 1, 0.3837, 2.5, 0.1, seq(0, 800, by=20), 
##      c(0,10,25,50,100,250,370,500), c(-100, 1050) )
## 
S1S2_ENBIS <- function(loss=10, vuln=1, b=1, a1=0.5, a2=1, d=0.1, preInv, postInv, yrng){
	z1 		= preInv
	z2		= postInv
	v		= vuln
	alpha 	= a1
	beta	= b
	alpha2 	= a2
	delta 	= d
	L 		= loss
	t 		= 1
	
	enbis <- matrix( ncol=length(z1), nrow=length(z2) )
	
	for(j in 1:length(z2) ){
		for(i in 1:length(z1) ){
			enbis[j,i] = GL_SSE_S1_S2(v,z1[i],z2[j],t,L,delta,alpha,beta,alpha2)
			if(is.nan(enbis[j,i])) enbis[j,i] <- 0
		}
	}

	plot(z1, enbis[1,], type="o", lty=1, cex=0.5, pch=1, ylab=expression(paste("ENBIS(", z[1], "+", z[2], ")")), xlab=expression(paste("SSE Investment (", z[1], ")") ), ylim=yrng )
 	abline(0, 0, col = "black")  
 	legV = bquote(z[2]==.(z2[1]))
 	ltyV = 1
 	pchV = 1
	
	for(k in 2:length(z2) ){
		lines(z1, enbis[k,], type="o", lty=1, cex=0.5, pch=k)
		legV = c(legV, bquote(z[2]==.(z2[k])) )
		ltyV = c(ltyV, 1)
		pchV = c(pchV, k)
	}

	legend("top", legend=as.expression(legV), lty=ltyV, pch=pchV, horiz=TRUE, bty="n", cex=0.65)
	
	## DEBUG
	#which(enbis == max(enbis), arr.ind = TRUE)
	
	return(enbis)
}


####
## 
##
## Execution for Figure XX of [1]:
##    out <- S1S2_ROSSP(1000, 1, 1, 0.3837, 2.5, 0.1, seq(0, 600, by=5), 
##      c(10,25,50,100,250,370,500), c(-5, 70) )
##    out <- S1S2_ROSSP(1000, 0.99, 1, 0.3837, 2.5, 0.1, seq(0, 100, by=2), 
##      c(0,10,25,50,100,250,370,500), c(-20, 70) )
## 
S1S2_ROSSP <- function(loss=10, vuln=1, b=1, a1=0.5, a2=1, d=0.1, preInv, postInv, yrng){
	z1 		= preInv
	z2		= postInv
	v		= vuln
	alpha 	= a1
	beta		= b
	alpha2 	= a2
	delta 	= d
	L 		= loss
	t 		= 1
	
	rossp <- matrix( ncol=length(z1), nrow=length(z2) )
	
	for(j in 1:length(z2) ){
		for(i in 1:length(z1) ){
			rossp[j,i] = ( ( GL_SSE_S1_S2(v,z1[i],z2[j],t,L,delta,alpha,beta,alpha2) / (z1[i]+z2[j]) )- 
			               ( GL_SSE_S1_S2(v,0,z2[j],t,L,delta,alpha,beta,alpha2) / z2[j] ) )
		 	if(is.nan(rossp[j,i])) rossp[j,i] <- 0
		}
	}

	plot(z1, rossp[1,], type="o", lty=1, pch=1, cex=0.5, ylab=expression(paste("ROSSP")), xlab=expression(paste("SSE Investment (", z[1], ")") ), ylim=yrng )
 	abline(0, 0, col = "black")  
 	legV = bquote(z[2]==.(z2[1]))
 	ltyV = 1
 	pchV = 1
	
	for(k in 2:length(z2) ){
		lines(z1, rossp[k,], type="o", lty=1, cex=0.5, pch=k)
		legV = c(legV, bquote(z[2]==.(z2[k])) )
		ltyV = c(ltyV, 1)
		pchV = c(pchV, k)
	}

	legend("top", legend=as.expression(legV), lty=ltyV, pch=pchV, horiz=TRUE, bty="n", cex=0.65)
	
	## DEBUG
	#which(rossp == max(rossp), arr.ind = TRUE)
	
	return(rossp)
}


# S1S2_ENBIS <- function(){
	# investment = seq(0, 5, by=0.2)
	# startingVul = 1.0
	# alpha = 0.3837
	# beta = 1.0
	# alpha2 = 2.5
	# delta = 0.1
	# loss = 10

	# # outS1 = S1(investment, startingVul, alpha, beta)
	# # #message(outS1)
	# # for( i in 1:length(outS1) ){
		# # #message(c("i=",i, " S1[i]=", outS1[i]))
		# # outS1[i] = outS1[i] + delta
		# # if(outS1[i] > 1){outS1[i]=1} 
		# # #message(c("new S1[i]=", outS1[i]))
	# # }
	# # combined0 = ((outS1) - S2(0, outS1, alpha2))*loss-(investment)
	# # combined05 = ((outS1) - S2(0.5, outS1, alpha2))*loss-(investment+0.5)
	# # combined1 = ((outS1) - S2(1, outS1, alpha2))*loss-(investment+1)
	# # combined2 = ((outS1) - S2(2, outS1, alpha2))*loss-(investment+2)
	# # combined3 = ((outS1) - S2(3.7, outS1, alpha2))*loss-(investment+3.7)
	# # combined5 = ((outS1) - S2(5, outS1, alpha2))*loss-(investment+5)
	# # #combined5 = ((outS1) - S2(10, outS1, alpha2))*loss-(investment+10)
	
	# # #sVul=1,z1,z2,t=1,lambda,delta=0,alpha1=1,beta1=1,alpha2=1
	# combined0 = GL_SSE_S1_S2_ENBIS(startingVul,investment,0,1,loss,delta,alpha,beta,alpha2)	
	# combined05 = GL_SSE_S1_S2_ENBIS(startingVul,investment,0.5,1,loss,delta,alpha,beta,alpha2)
	# combined1 = GL_SSE_S1_S2_ENBIS(startingVul,investment,1,1,loss,delta,alpha,beta,alpha2)
	# combined2 = GL_SSE_S1_S2_ENBIS(startingVul,investment,2,1,loss,delta,alpha,beta,alpha2)
	# combined3 = GL_SSE_S1_S2_ENBIS(startingVul,investment,3.7,1,loss,delta,alpha,beta,alpha2)
	# combined5 = GL_SSE_S1_S2_ENBIS(startingVul,investment,5,1,loss,delta,alpha,beta,alpha2)
	
	# plot(investment, combined1, type="o", ylim=range(-7,5), lty=1, pch=2, ylab="ENBIS(z1, z2)", xlab="SSE Investment (z1)")
	# abline(0, 0, col = "black")
	# lines(investment, combined0, type="o", lty=1, pch=0)
	# lines(investment, combined05, type="o", lty=1, pch=1)
	# lines(investment, combined2, type="o", lty=1, pch=3)
	# lines(investment, combined3, type="o", lty=1, pch=4)
	# lines(investment, combined5, type="o", lty=1, pch=5)
	# legend("top", c("z2=0", "z2=0.5", "z2=1", "z2=2", "z2=3.7", "z2=5"), lty=c(1,1,1,1,1,1), pch=c(0,1,2,3,4,5), horiz=TRUE, bty="n", cex=0.65)
# }











### S3 followed by S2
S3HS2_ROSI <- function(){
	investment = seq(0, 5, by=0.2)
	startingVul = 1.0
	phi = 0.07637
	gamma = 4.36077
	alpha2 = 2.5
	delta = 0.1
	loss = 10

	#outS3H = S3H(investment, startingVul, phi, gamma)
	# for( i in 1:length(outS3H) ){
		# #message(c("i=",i, " S3H[i]=", outS3H[i]))
		# outS3H[i] = outS3H[i] + delta
		# if(outS3H[i] > 1){outS3H[i]=1} 
		# #message(c("new S3H[i]=", outS3H[i]))
	# }
	# combined0 = ((outS3H) - S2(0, outS3H, alpha2))*loss-(investment)
	# combined05 = ((outS3H) - S2(0.5, outS3H, alpha2))*loss-(investment+0.5)
	# combined1 = ((outS3H) - S2(1, outS3H, alpha2))*loss-(investment+1)
	# combined2 = ((outS3H) - S2(2, outS3H, alpha2))*loss-(investment+2)
	# combined3 = ((outS3H) - S2(3.7, outS3H, alpha2))*loss-(investment+3.7)
	# combined5 = ((outS3H) - S2(5, outS3H, alpha2))*loss-(investment+5)
	
	combined0 = GL_SSE_S3H_S2_ROSI(startingVul,investment,0,1,loss,delta,phi,gamma,alpha2) / (investment)	
	combined05 = GL_SSE_S3H_S2_ROSI(startingVul,investment,0.5,1,loss,delta,phi,gamma,alpha2) / (investment+0.5)
	combined1 = GL_SSE_S3H_S2_ROSI(startingVul,investment,1,1,loss,delta,phi,gamma,alpha2) / (investment+1)
	combined2 = GL_SSE_S3H_S2_ROSI(startingVul,investment,2,1,loss,delta,phi,gamma,alpha2) / (investment+2)
	combined3 = GL_SSE_S3H_S2_ROSI(startingVul,investment,3.7,1,loss,delta,phi,gamma,alpha2) / (investment+3.7)
	combined5 = GL_SSE_S3H_S2_ROSI(startingVul,investment,5,1,loss,delta,phi,gamma,alpha2) / (investment+5)
	
	plot(investment, combined1, type="o", ylim=range(-2,3), lty=1, pch=2, ylab="ROSI", xlab="SSE Investment (z1)")
	abline(0, 0, col = "black")
	lines(investment, combined0, type="o", lty=1, pch=0)
	lines(investment, combined05, type="o", lty=1, pch=1)
	lines(investment, combined2, type="o", lty=1, pch=3)
	lines(investment, combined3, type="o", lty=1, pch=4)
	lines(investment, combined5, type="o", lty=1, pch=5)
	legend("top", c("z2=0", "z2=0.5", "z2=1", "z2=2", "z2=3.7", "z2=5"), lty=c(1,1,1,1,1,1), pch=c(0,1,2,3,4,5), horiz=TRUE, bty="n", cex=0.65)	
}



### S3 followed by S2
S3HS1_ROSI <- function(){
	investment = seq(0, 5, by=0.2)
	startingVul = 1.0
	phi = 0.07637
	gamma = 4.36077
	alpha = 1
	beta = 1
	delta = 0.1
	loss = 10

	#outS3H = S3H(investment, startingVul, phi, gamma)
	# for( i in 1:length(outS3H) ){
		# #message(c("i=",i, " S3H[i]=", outS3H[i]))
		# outS3H[i] = outS3H[i] + delta
		# if(outS3H[i] > 1){outS3H[i]=1} 
		# #message(c("new S3H[i]=", outS3H[i]))
	# }
	# combined0 = ((outS3H) - S2(0, outS3H, alpha2))*loss-(investment)
	# combined05 = ((outS3H) - S2(0.5, outS3H, alpha2))*loss-(investment+0.5)
	# combined1 = ((outS3H) - S2(1, outS3H, alpha2))*loss-(investment+1)
	# combined2 = ((outS3H) - S2(2, outS3H, alpha2))*loss-(investment+2)
	# combined3 = ((outS3H) - S2(3.7, outS3H, alpha2))*loss-(investment+3.7)
	# combined5 = ((outS3H) - S2(5, outS3H, alpha2))*loss-(investment+5)
	
	combined0 = GL_SSE_S3H_S1_ROSI(startingVul,investment,0,1,loss,delta,phi,gamma,alpha,beta) / (investment)	
	combined05 = GL_SSE_S3H_S1_ROSI(startingVul,investment,0.5,1,loss,delta,phi,gamma,alpha,beta) / (investment+0.5)
	combined1 = GL_SSE_S3H_S1_ROSI(startingVul,investment,1,1,loss,delta,phi,gamma,alpha,beta) / (investment+1)
	combined2 = GL_SSE_S3H_S1_ROSI(startingVul,investment,2,1,loss,delta,phi,gamma,alpha,beta) / (investment+2)
	combined3 = GL_SSE_S3H_S1_ROSI(startingVul,investment,3.7,1,loss,delta,phi,gamma,alpha,beta) / (investment+3.7)
	combined5 = GL_SSE_S3H_S1_ROSI(startingVul,investment,5,1,loss,delta,phi,gamma,alpha,beta) / (investment+5)
	
	plot(investment, combined1, type="o", ylim=range(-2,3), lty=1, pch=2, ylab="ROSI", xlab="SSE Investment (z1)")
	abline(0, 0, col = "black")
	lines(investment, combined0, type="o", lty=1, pch=0)
	lines(investment, combined05, type="o", lty=1, pch=1)
	lines(investment, combined2, type="o", lty=1, pch=3)
	lines(investment, combined3, type="o", lty=1, pch=4)
	lines(investment, combined5, type="o", lty=1, pch=5)
	legend("top", c("z2=0", "z2=0.5", "z2=1", "z2=2", "z2=3.7", "z2=5"), lty=c(1,1,1,1,1,1), pch=c(0,1,2,3,4,5), horiz=TRUE, bty="n", cex=0.65)	
}





S1S2_ENBIS <- function(){
	investment = seq(0, 5, by=0.2)
	startingVul = 1.0
	alpha = 0.3837
	beta = 1.0
	alpha2 = 2.5
	delta = 0.1
	loss = 10

	# outS1 = S1(investment, startingVul, alpha, beta)
	# #message(outS1)
	# for( i in 1:length(outS1) ){
		# #message(c("i=",i, " S1[i]=", outS1[i]))
		# outS1[i] = outS1[i] + delta
		# if(outS1[i] > 1){outS1[i]=1} 
		# #message(c("new S1[i]=", outS1[i]))
	# }
	# combined0 = ((outS1) - S2(0, outS1, alpha2))*loss-(investment)
	# combined05 = ((outS1) - S2(0.5, outS1, alpha2))*loss-(investment+0.5)
	# combined1 = ((outS1) - S2(1, outS1, alpha2))*loss-(investment+1)
	# combined2 = ((outS1) - S2(2, outS1, alpha2))*loss-(investment+2)
	# combined3 = ((outS1) - S2(3.7, outS1, alpha2))*loss-(investment+3.7)
	# combined5 = ((outS1) - S2(5, outS1, alpha2))*loss-(investment+5)
	# #combined5 = ((outS1) - S2(10, outS1, alpha2))*loss-(investment+10)
	
	# #sVul=1,z1,z2,t=1,lambda,delta=0,alpha1=1,beta1=1,alpha2=1
	combined0 = GL_SSE_S1_S2_ENBIS(startingVul,investment,0,1,loss,delta,alpha,beta,alpha2)	
	combined05 = GL_SSE_S1_S2_ENBIS(startingVul,investment,0.5,1,loss,delta,alpha,beta,alpha2)
	combined1 = GL_SSE_S1_S2_ENBIS(startingVul,investment,1,1,loss,delta,alpha,beta,alpha2)
	combined2 = GL_SSE_S1_S2_ENBIS(startingVul,investment,2,1,loss,delta,alpha,beta,alpha2)
	combined3 = GL_SSE_S1_S2_ENBIS(startingVul,investment,3.7,1,loss,delta,alpha,beta,alpha2)
	combined5 = GL_SSE_S1_S2_ENBIS(startingVul,investment,5,1,loss,delta,alpha,beta,alpha2)
	
	plot(investment, combined1, type="o", ylim=range(-7,5), lty=1, pch=2, ylab="ENBIS(z1, z2)", xlab="SSE Investment (z1)")
	abline(0, 0, col = "black")
	lines(investment, combined0, type="o", lty=1, pch=0)
	lines(investment, combined05, type="o", lty=1, pch=1)
	lines(investment, combined2, type="o", lty=1, pch=3)
	lines(investment, combined3, type="o", lty=1, pch=4)
	lines(investment, combined5, type="o", lty=1, pch=5)
	legend("top", c("z2=0", "z2=0.5", "z2=1", "z2=2", "z2=3.7", "z2=5"), lty=c(1,1,1,1,1,1), pch=c(0,1,2,3,4,5), horiz=TRUE, bty="n", cex=0.65)
}


### S3 followed by S2
S3HS2_ENBIS <- function(){
	investment = seq(0, 5, by=0.2)
	startingVul = 1.0
	phi = 0.07637
	gamma = 4.36077
	alpha2 = 2.5
	delta = 0.1
	loss = 10

	#outS3H = S3H(investment, startingVul, phi, gamma)
	# for( i in 1:length(outS3H) ){
		# #message(c("i=",i, " S3H[i]=", outS3H[i]))
		# outS3H[i] = outS3H[i] + delta
		# if(outS3H[i] > 1){outS3H[i]=1} 
		# #message(c("new S3H[i]=", outS3H[i]))
	# }
	# combined0 = ((outS3H) - S2(0, outS3H, alpha2))*loss-(investment)
	# combined05 = ((outS3H) - S2(0.5, outS3H, alpha2))*loss-(investment+0.5)
	# combined1 = ((outS3H) - S2(1, outS3H, alpha2))*loss-(investment+1)
	# combined2 = ((outS3H) - S2(2, outS3H, alpha2))*loss-(investment+2)
	# combined3 = ((outS3H) - S2(3.7, outS3H, alpha2))*loss-(investment+3.7)
	# combined5 = ((outS3H) - S2(5, outS3H, alpha2))*loss-(investment+5)
	
	combined0 = GL_SSE_S3H_S2_ENBIS(startingVul,investment,0,1,loss,delta,phi,gamma,alpha2)	
	combined05 = GL_SSE_S3H_S2_ENBIS(startingVul,investment,0.5,1,loss,delta,phi,gamma,alpha2)
	combined1 = GL_SSE_S3H_S2_ENBIS(startingVul,investment,1,1,loss,delta,phi,gamma,alpha2)
	combined2 = GL_SSE_S3H_S2_ENBIS(startingVul,investment,2,1,loss,delta,phi,gamma,alpha2)
	combined3 = GL_SSE_S3H_S2_ENBIS(startingVul,investment,3.7,1,loss,delta,phi,gamma,alpha2)
	combined5 = GL_SSE_S3H_S2_ENBIS(startingVul,investment,5,1,loss,delta,phi,gamma,alpha2)
	
	plot(investment, combined1, type="o", ylim=range(-7,5), lty=1, pch=2, ylab="ENBIS(z1, z2)", xlab="SSE Investment (z1)")
	abline(0, 0, col = "black")
	lines(investment, combined0, type="o", lty=1, pch=0)
	lines(investment, combined05, type="o", lty=1, pch=1)
	lines(investment, combined2, type="o", lty=1, pch=3)
	lines(investment, combined3, type="o", lty=1, pch=4)
	lines(investment, combined5, type="o", lty=1, pch=5)
	legend("top", c("z2=0", "z2=0.5", "z2=1", "z2=2", "z2=3.7", "z2=5"), lty=c(1,1,1,1,1,1), pch=c(0,1,2,3,4,5), horiz=TRUE, bty="n", cex=0.65)	
}



### S3 followed by S2
S3HS1_ENBIS <- function(){
	investment = seq(0, 5, by=0.2)
	startingVul = 1.0
	phi = 0.07637
	gamma = 4.36077
	alpha = 1
	beta = 3
	delta = 0.1
	loss = 10

	#outS3H = S3H(investment, startingVul, phi, gamma)
	# for( i in 1:length(outS3H) ){
		# #message(c("i=",i, " S3H[i]=", outS3H[i]))
		# outS3H[i] = outS3H[i] + delta
		# if(outS3H[i] > 1){outS3H[i]=1} 
		# #message(c("new S3H[i]=", outS3H[i]))
	# }
	# combined0 = ((outS3H) - S2(0, outS3H, alpha2))*loss-(investment)
	# combined05 = ((outS3H) - S2(0.5, outS3H, alpha2))*loss-(investment+0.5)
	# combined1 = ((outS3H) - S2(1, outS3H, alpha2))*loss-(investment+1)
	# combined2 = ((outS3H) - S2(2, outS3H, alpha2))*loss-(investment+2)
	# combined3 = ((outS3H) - S2(3.7, outS3H, alpha2))*loss-(investment+3.7)
	# combined5 = ((outS3H) - S2(5, outS3H, alpha2))*loss-(investment+5)
	
	combined0 = GL_SSE_S3H_S1_ENBIS(startingVul,investment,0,1,loss,delta,phi,gamma,alpha,beta)	
	combined05 = GL_SSE_S3H_S1_ENBIS(startingVul,investment,0.5,1,loss,delta,phi,gamma,alpha,beta)
	combined1 = GL_SSE_S3H_S1_ENBIS(startingVul,investment,1,1,loss,delta,phi,gamma,alpha,beta)
	combined2 = GL_SSE_S3H_S1_ENBIS(startingVul,investment,2,1,loss,delta,phi,gamma,alpha,beta)
	combined3 = GL_SSE_S3H_S1_ENBIS(startingVul,investment,3.7,1,loss,delta,phi,gamma,alpha,beta)
	combined5 = GL_SSE_S3H_S1_ENBIS(startingVul,investment,5,1,loss,delta,phi,gamma,alpha,beta)
	
	plot(investment, combined1, type="o", ylim=range(-7,5), lty=1, pch=2, ylab="ENBIS(z1, z2)", xlab="SSE Investment (z1)")
	abline(0, 0, col = "black")
	lines(investment, combined0, type="o", lty=1, pch=0)
	lines(investment, combined05, type="o", lty=1, pch=1)
	lines(investment, combined2, type="o", lty=1, pch=3)
	lines(investment, combined3, type="o", lty=1, pch=4)
	lines(investment, combined5, type="o", lty=1, pch=5)
	legend("top", c("z2=0", "z2=0.5", "z2=1", "z2=2", "z2=3.7", "z2=5"), lty=c(1,1,1,1,1,1), pch=c(0,1,2,3,4,5), horiz=TRUE, bty="n", cex=0.65)	
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
	
