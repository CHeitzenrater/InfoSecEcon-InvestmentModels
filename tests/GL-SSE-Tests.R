
##
## GL-SSE Experimentation
##
##

################################################################################################
## Default params
#################

z1 = 3
z2 = 3
z1seq = seq(0,100,by=1)
z2seq = seq(0,100,by=1)

lambda = 100
lambdaSeq = seq(0,100,by=1)

t = 1
tSeq = seq(0,1,by=0.1)
v = 1
vSeq = seq(0,1,by=0.1)

delta = 0.1
deltaSeq = seq(0,1,by=0.1)

alpha1 = 0.3837
beta1 = 1
alpha2 = 1
alpha1Seq = seq(0,3,by=0.01)
beta1Seq = seq(1,3,by=0.01)
alpha2Seq = seq(0,3,by=0.01)

##################################################################################################

##################################################################################################
#### vary z1
## ENBIS
enbis = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2)
plot(z1seq,enbis, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)

## ROSI 
rosi = enbis / (z1seq+z2)
plot(z1seq,rosi, type="l", lty=1, ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rosi == max(rosi), arr.ind = TRUE)

##ROSSP
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(z1seq,rossp, type="l", lty=1, ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)

z2=30
enbis = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2)
rosi = enbis / (z1seq+z2)
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(z1seq,rossp, type="l", lty=1, ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)


##################################################################################################
#### 
## vary z2
##

z1=1
z1=5
z1=30
z1=50


## ENBIS
enbis = GL_SSE_S1_S2(v,z1,z2seq,t,lambda,delta,alpha1,beta1,alpha2)
plot(z1seq,enbis, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste("Post-deployment Investment ",'z'[2]))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)
  
## ROSI 
rosi = enbis / (z1+z2seq)
plot(z2seq,rosi, type="l", lty=1, ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste("Post-deployment Investment ",'z'[2]))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rosi == max(rosi), arr.ind = TRUE)


## ENBIS Alt
z2_1=1
enbis_1 = GL_SSE_S1_S2(v,z1seq,z2_1,t,lambda,delta,alpha1,beta1,alpha2)
#plot(z1seq,enbis_1, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
#  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
#abline(0, 0, col = "black")
z2_5=5
enbis_5 = GL_SSE_S1_S2(v,z1seq,z2_5,t,lambda,delta,alpha1,beta1,alpha2)
#plot(z1seq,enbis_5, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
#  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
#abline(0, 0, col = "black")
z2_30=30
enbis_30 = GL_SSE_S1_S2(v,z1seq,z2_30,t,lambda,delta,alpha1,beta1,alpha2)
#plot(z1seq,enbis_30, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
#  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
#abline(0, 0, col = "black")
z2_50=50
enbis_50 = GL_SSE_S1_S2(v,z1seq,z2_50,t,lambda,delta,alpha1,beta1,alpha2)
plot(z1seq,enbis_1, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  ylim=c(-40,100), xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
#lines(z1seq,enbis_1, type="l", lty=1)
lines(z1seq,enbis_5, type="l", lty=2)
lines(z1seq,enbis_30, type="l", lty=3)
lines(z1seq,enbis_50, type="l", lty=4)




##################################################################################################
#### vary \alpha_{S^{1}}

z1 = 3
z2 = 3

## ENBIS
enbis = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1Seq,beta1,alpha2)
plot(alpha1Seq,enbis, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(alpha[1]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)


## ROSI 
rosi = enbis / (z1+z2)
plot(alpha1Seq,rosi, type="l", lty=1, ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(alpha[1]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rosi == max(rosi), arr.ind = TRUE)

##ROSSP
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1Seq,beta1,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(alpha1Seq,rossp, type="l", lty=1, ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(alpha[1]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)

z1=5
z2=1
enbis2 = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1Seq,beta1,alpha2)
rosi2 = enbis2 / (z1+z2)
enbis02 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1Seq,beta1,alpha2)
rossp2 = rosi2 - (enbis02 / (z2))
z1=1
z2=5
enbis3 = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1Seq,beta1,alpha2)
rosi3 = enbis3 / (z1+z2)
enbis03 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1Seq,beta1,alpha2)
rossp3 = rosi3 - (enbis03 / (z2))
plot(alpha1Seq,rossp, type="l", lty=1, ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(alpha[1]))))
lines(alpha1Seq,rossp2,type="l",lty=2)
lines(alpha1Seq,rossp3,type="l",lty=3)
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(3))), as.expression(bquote(z[1]==.(5))), as.expression(bquote(z[1]==.(1)))), lty=c(1,2,3), y.intersp=1.3)


# ENBIS-Alt
alpha1_0001 = 0.01
enbis_0001 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1_0001,beta1,alpha2)
alpha1_001 = 0.01
enbis_001 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1_001,beta1,alpha2)
alpha1_01 = 0.1
enbis_01 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1_01,beta1,alpha2)
alpha1_1 = 1.0
enbis_1 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1_1,beta1,alpha2)
alpha1_2 = 2.0
enbis_2 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1_2,beta1,alpha2)

plot(z1seq,enbis_2, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  ylim=c(-40,100), xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
lines(z1seq,enbis_1, type="l", lty=2)
lines(z1seq,enbis_01, type="l", lty=3)
lines(z1seq,enbis_001, type="l", lty=4)
lines(z1seq,enbis_0001, type="l", lty=5)
abline(0, 0, col = "black")





##################################################################################################
#### vary \beta_{S^{1}}

z1 = 3
z2 = 3

## ENBIS
enbis = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1,beta1Seq,alpha2)
plot(beta1Seq,enbis, type="l", lty=1, ylim=c(0,100), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(beta))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha==.(alpha1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)

## ROSI 
rosi = enbis / (z1+z2)
plot(beta1Seq,rosi, type="l", lty=1, ylim=c(0,50), ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(beta))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha==.(alpha1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rosi == max(rosi), arr.ind = TRUE)

##ROSSP
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1Seq,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(beta1Seq,rossp, type="l", lty=1, ylim=c(0,50), ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(beta))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha==.(alpha1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)

z1=5
z2=1
enbis2 = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1,beta1Seq,alpha2)
rosi2 = enbis2 / (z1+z2)
enbis02 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1Seq,alpha2)
rossp2 = rosi2 - (enbis02 / (z2))
z1=1
z2=5
enbis3 = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1,beta1Seq,alpha2)
rosi3 = enbis3 / (z1+z2)
enbis03 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1Seq,alpha2)
rossp3 = rosi3 - (enbis03 / (z2))
plot(beta1Seq,rossp, type="l", lty=1, ylim=c(0,20), ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(beta))))
lines(beta1Seq,rossp2,type="l",lty=2)
lines(beta1Seq,rossp3,type="l",lty=3)
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(3))), as.expression(bquote(z[1]==.(5))), as.expression(bquote(z[1]==.(1)))), lty=c(1,2,3), y.intersp=1.3)


# ENBIS-Alt
beta_1 = 1.0
enbis_1 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta_1,alpha2)
beta_2 = 2.0
enbis_2 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta_2,alpha2)
beta_3 = 3.0
enbis_3 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta_3,alpha2)

plot(z1seq,enbis_3, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  ylim=c(-40,100), xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
lines(z1seq,enbis_2, type="l", lty=2)
lines(z1seq,enbis_1, type="l", lty=3)
abline(0, 0, col = "black")


##################################################################################################
#### vary \alpha_{S^{II}}

z1 = 3
z2 = 3

## ENBIS
enbis = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1,beta1,alpha2Seq)
plot(alpha2Seq,enbis, type="l", lty=1, ylim=c(-1, 96), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(alpha[2]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)

## ROSI 
rosi = enbis / (z1+z2)
plot(alpha2Seq,rosi, type="l", lty=1, ylim=c(-1, 16), ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(alpha[2]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))) ), y.intersp=1.3)
which(rosi == max(rosi), arr.ind = TRUE)

##ROSSP
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1,alpha2Seq)
rossp = rosi - (enbis0 / (z2))
plot(alpha2Seq,rossp, type="l", lty=1, ylim=c(-1, 17), ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(alpha[2]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)

z1=5
z2=1
enbis2 = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1,beta1,alpha2Seq)
rosi2 = enbis2 / (z1+z2)
enbis02 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1,alpha2Seq)
rossp2 = rosi2 - (enbis02 / (z2))
z1=1
z2=5
enbis3 = GL_SSE_S1_S2(v,z1,z2,t,lambda,delta,alpha1,beta1,alpha2Seq)
rosi3 = enbis3 / (z1+z2)
enbis03 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1,alpha2Seq)
rossp3 = rosi3 - (enbis03 / (z2))
plot(alpha2Seq,rossp, type="l", lty=1, ylim=c(-1, 17), ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(alpha[2]))))
lines(alpha2Seq,rossp2,type="l",lty=2)
lines(alpha2Seq,rossp3,type="l",lty=3)
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(3))), as.expression(bquote(z[1]==.(5))), as.expression(bquote(z[1]==.(1)))), lty=c(1,2,3), y.intersp=1.3)



# ENBIS-Alt
alpha2_0001 = 0.01
enbis_0001 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2_0001)
alpha2_001 = 0.01
enbis_001 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2_001)
alpha2_01 = 0.1
enbis_01 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2_01)
alpha2_1 = 1.0
enbis_1 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2_1)
alpha2_2 = 2.0
enbis_2 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2_2)

plot(z1seq,enbis_2, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  ylim=c(-40,100), xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
lines(z1seq,enbis_1, type="l", lty=2)
lines(z1seq,enbis_01, type="l", lty=3)
lines(z1seq,enbis_001, type="l", lty=4)
lines(z1seq,enbis_0001, type="l", lty=5)
abline(0, 0, col = "black")



##################################################################################################
#### vary v
z1 = 5
z2 = 5

## ENBIS
enbis = GL_SSE_S1_S2(vSeq,z1,z2,t,lambda,delta,alpha1,beta1,alpha2)
plot(vSeq,enbis,type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste('v'[start]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ))

## ROSI 
 rosi = enbis / (z1+z2)
 plot(vSeq,rosi,type="l", lty=1, ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
   xlab=c(expression(paste('v'[start]))))
 abline(0, 0, col = "black")
 legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ))

#### Vary z1 and examine v at 1, 0.99, 0.9, 0.8, 0.5, 0.2
z2 = 5

v=1
v=0.99
v=0.9
v=0.8
v=0.5
v=0.2

## ENBIS
enbis = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2)
plot(z1seq,enbis, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)
##ROSSP
rosi = enbis / (z1seq+z2)
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(z1seq,rossp, type="l", lty=1, ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)



##################################################################################################
#### vary t
z1 = 5
z2 = 5
## ENBIS
enbis = GL_SSE_S1_S2(v,z1,z2,tSeq,lambda,delta,alpha1,beta1,alpha2)
plot(tSeq,enbis,type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
   xlab=c(expression(paste('t'))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ))
## ROSI 
rosi = enbis / (z1+z2)
plot(tSeq,rosi,type="l", lty=1, ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],") ($K)"))), 
  xlab=c(expression(paste('t'))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ))

#### Vary z1 and examine t at 1, 0.99, 0.9, 0.8, 0.5, 0.2
z2 = 5

t=1
t=0.99
t=0.9
t=0.8
t=0.5
t=0.2
t=0.1

## ENBIS
enbis = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2)
plot(z1seq,enbis, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)
##ROSSP
rosi = enbis / (z1seq+z2)
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,delta,alpha1,beta1,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(z1seq,rossp, type="l", lty=1, ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
abline(0, 0, col = "black")
legend("right", c( as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)



##################################################################################################
#### vary \delta

z1 = 3
z2 = 3

## ENBIS
enbis = GL_SSE_S1_S2(v,z1,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
plot(deltaSeq,enbis, type="l", lty=1, ylim=c(-1, 96), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(delta))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)

## ROSI 
rosi = enbis / (z1+z2)
plot(deltaSeq,rosi, type="l", lty=1, ylim=c(-1, 16), ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(delta))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rosi == max(rosi), arr.ind = TRUE)

##ROSSP
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(deltaSeq,rossp, type="l", lty=1, ylim=c(-1, 17), ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(delta))))
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)

z1=5
z2=1
enbis2 = GL_SSE_S1_S2(v,z1,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
rosi2 = enbis2 / (z1+z2)
enbis02 = GL_SSE_S1_S2(v,0,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
rossp2 = rosi2 - (enbis02 / (z2))
z1=1
z2=5
enbis3 = GL_SSE_S1_S2(v,z1,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
rosi3 = enbis3 / (z1+z2)
enbis03 = GL_SSE_S1_S2(v,0,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
rossp3 = rosi3 - (enbis03 / (z2))
z1=8
z2=8
enbis4 = GL_SSE_S1_S2(v,z1,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
rosi4 = enbis4 / (z1+z2)
enbis04 = GL_SSE_S1_S2(v,0,z2,t,lambda,deltaSeq,alpha1,beta1,alpha2)
rossp4 = rosi4 - (enbis04 / (z2))
plot(deltaSeq,rossp, type="l", lty=1, ylim=c(-1, 17), ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(delta))))
lines(deltaSeq,rossp2,type="l",lty=2)
lines(deltaSeq,rossp3,type="l",lty=3)
lines(deltaSeq,rossp4,type="l",lty=4)
abline(0, 0, col = "black")
legend("topright", c( as.expression(bquote(z[1,2]==.(3))), as.expression(bquote(z[1]==.(5))), as.expression(bquote(z[1]==.(1))), as.expression(bquote(z[1,2]==.(8)))), lty=c(1,2,3,4), y.intersp=1.3)


# ENBIS-Alt
delta_0001 = 0.01
enbis_0001 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta_0001,alpha1,beta1,alpha2)
delta_001 = 0.01
enbis_001 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta_001,alpha1,beta1,alpha2)
delta_01 = 0.1
enbis_01 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta_01,alpha1,beta1,alpha2)
delta_05 = 0.5
enbis_05 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta_05,alpha1,beta1,alpha2)
delta_09 = 0.9
enbis_09 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda,delta_09,alpha1,beta1,alpha2)

plot(z1seq,enbis_0001, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  ylim=c(-40,100), xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
lines(z1seq,enbis_001, type="l", lty=2)
lines(z1seq,enbis_01, type="l", lty=3)
lines(z1seq,enbis_05, type="l", lty=4)
lines(z1seq,enbis_09, type="l", lty=5)
abline(0, 0, col = "black")




##################################################################################################
#### vary \lambda

z1 = 3
z2 = 3

## ENBIS
enbis = GL_SSE_S1_S2(v,z1,z2,t,lambdaSeq,delta,alpha1,beta1,alpha2)
plot(lambdaSeq,enbis, type="l", lty=1, ylim=c(-1, 96), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(lambda))))
abline(0, 0, col = "black")
legend("topleft", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(enbis == max(enbis), arr.ind = TRUE)

## ROSI 
rosi = enbis / (z1+z2)
plot(lambdaSeq,rosi, type="l", lty=1, ylim=c(-1, 16), ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],")"))), 
  xlab=c(expression(paste(lambda))))
abline(0, 0, col = "black")
legend("topleft", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rosi == max(rosi), arr.ind = TRUE)

##ROSSP
enbis0 = GL_SSE_S1_S2(v,0,z2,t,lambdaSeq,delta,alpha1,beta1,alpha2)
rossp = rosi - (enbis0 / (z2))
plot(lambdaSeq,rossp, type="l", lty=1, ylim=c(-1, 17), ylab=c(expression(paste("ROSSP"))), xlab=c(expression(paste(lambda))))
abline(0, 0, col = "black")
legend("topleft", c( as.expression(bquote(z[1]==.(z1))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2))) ), y.intersp=1.3)
which(rossp == max(rossp), arr.ind = TRUE)

## ENBIS-Alt
lambda_100 = 100
enbis_100 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda_100,delta,alpha1,beta1,alpha2)
lambda_50 = 50
enbis_50 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda_50,delta,alpha1,beta1,alpha2)
lambda_10 = 10
enbis_10 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda_10,delta,alpha1,beta1,alpha2)
lambda_1 = 1
enbis_1 = GL_SSE_S1_S2(v,z1seq,z2,t,lambda_1,delta,alpha1,beta1,alpha2)

plot(z1seq,enbis_100, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],")"))), 
  ylim=c(-40,100), xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]))))
lines(z1seq,enbis_50, type="l", lty=2)
lines(z1seq,enbis_10, type="l", lty=3)
lines(z1seq,enbis_1, type="l", lty=4)
abline(0, 0, col = "black")








# for( i in vSeq ){
	# enbis = GL_SSE_S1_S2(i,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2)	
	# plot(z1seq,enbis, type="l", lty=1, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
  	  # xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))))
	# abline(0, 0, col = "black")	
	# ## Insert pause here
	# Sys.sleep(1)
# }
# for( i in vSeq ){
	# enbis = GL_SSE_S1_S2(i,z1seq,z2,t,lambda,delta,alpha1,beta1,alpha2)	
	# rosi = enbis / (z1seq+z2)
	# enbis0 = GL_SSE_S1_S2(i,0,z2,t,lambda,delta,alpha1,beta1,alpha2)
	# rossp = rosi - (enbis0 / (z2))
	# plot(z1seq,rossp, type="l", lty=1, ylab=c(expression(paste("ROSSP"))), 
  	  # xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))))
	# abline(0, 0, col = "black")
	# ## Insert pause here
	# Sys.sleep(1)
# }
# for( i in z1seq ){
	# enbis = GL_SSE_S1_S2(v,i,z2seq,t,lambda,delta,alpha1,beta1,alpha2)
	# rosi = enbis / (i+z2seq)
	# plot(z2seq,rosi, type="l", lty=1, ylim=(c(-10,40)), ylab=c(expression(paste("ROSI(",'z'[1],"+",'z'[2],") ($K)  "))), xlab=c(expression(paste("Post-deployment Investment ",'z'[2]," ($K)"))),
  	 # main="ROSI varied z2, as z1 increases", sub=c(as.expression(bquote(z[1]==.(i))), as.expression(bquote(z[2]==.(z2))), as.expression(bquote(v[start]==.(v))), as.expression(bquote(t==.(t))), as.expression(bquote(alpha[1]==.(alpha1))), as.expression(bquote(beta==.(beta1))), as.expression(bquote(alpha[2]==.(alpha2)))) )
	# abline(0, 0, col = "black")	
	# ## Insert pause here
	# Sys.sleep(1)
# }






