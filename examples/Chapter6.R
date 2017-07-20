###############################################################
#
# Code to generate the analysis/graphs used in Chapter 6, 
#  "Software Security Economics in Practice"
#
###############################################################

#####
## Calculation for GL, phase 1
## Calculate the investment over various threat stances, t = 0.01, 0.05, 0.1
##
phase1Budget = 600
GLAlpha = 0.5142
GLBeta = 1
Vuln = 1

## Calculate the FOC for different values of t
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.01,phase1Budget)
# [1] 1.471166
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.05,phase1Budget)
# [1] 5.693494
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.1,phase1Budget)
# [1] 8.857366

## percentages
1.471166 / phase1Budget
# [1] 0.002451943
8.857366 / phase1Budget
# [1] 0.01476228


#####
## Calculation for GL-SSE, phase 1
## Calculation of various percentages of post-deployment security investment, t = 0.1, \alpha2 = 1
## 
pre = seq(0:30)
GLAlpha2 = 1
t = 0.1
delta = 0.1

post05 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.005,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
post1  = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.010,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
post25 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.025,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
post5  = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.050,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
post10 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.100,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)

plot(pre, post10, type="l", ylim=range(-40,50), lty=1, pch=0, 
  ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
abline(0, 0, col = "black")
lines(pre, post5, type="l", lty=2, pch=0)
lines(pre, post25, type="l", lty=3, pch=0)
lines(pre, post1, type="l", lty=4, pch=0)
lines(pre, post05, type="l", lty=5, pch=0)
legend("top", c(expression(paste('z'[2],"=0.5%")), expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=2.5%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(5,4,3,2,1), horiz=TRUE, bty="n", cex=0.65)


#####
## Calculation for GL-SSE, phase 2
## 
p2pre = seq(0:5000)
p2t = 1
p2Lambda = 20000

p2ENBIS3 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
#p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

plot(p2pre, p2ENBIS3, type="l", lty=2, ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
#line(p2pre, p2ENBIS3, type="l", lty=1)
abline(0, 0, col = "black")
legend("top", expression(paste('z'[2],"=3%")), lty=c(2), horiz=TRUE, bty="n", cex=0.65)

which(p2ENBIS3 == max(p2ENBIS3), arr.ind = TRUE)
#143


#####
## Calculations for the identifying the result of the investment
## 
v = GL_SSE_S1_S2_retVs(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

v_pre = unlist(v[[1]])
v_pst = unlist(v[[2]])
resS1 = unlist(v[[3]])
resS2 = unlist(v[[4]])

which(v_pre == max(v_pre), arr.ind = TRUE)
#5001
which(v_pst == max(v_pst), arr.ind = TRUE)
#163
which(resS1 == max(resS1), arr.ind = TRUE)
#1
which(resS2 == max(resS2), arr.ind = TRUE)
#5001

v_pre[143]
#0.9879244
v_pst[143]
#0.98725
resS1[143]
#0.01207558
resS2[143]
#0.0006744611


GL_SSE_S1_S2(Vuln,143,3396,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v_pst[3396]




