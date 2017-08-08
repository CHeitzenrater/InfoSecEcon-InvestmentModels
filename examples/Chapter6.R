################################################################################
##
## Code to generate the analysis/graphs used in dissertation Chapter 6, 
##  "Software Security Economics in Practice"
##
################################################################################

## Required source files
source("./models/GL-SSE.R", local=TRUE)
source("./models/GL.R", local=TRUE)


################################################################################
## Parameters
################################################################################

## Project budget numbers
phase1Budget = 700
phase2Budget = 800
develBudget = phase1Budget+phase2Budget
frameworkBudget = 20000
overallBudget = frameworkBudget + develBudget

## GL parameters, from chapter 4
GLAlpha = 0.5142
GLBeta = 1
Vuln = 1

## Investment of Project A already in security
securityInvestment = (2/43)*700



################################################################################
## Phase 1 calculations
################################################################################

#####
## Calculation for GL, phase 1
## Calculate the investment over various threat stances (different values of t)
##
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.01,phase1Budget)
# BUDGET = 600 --- [1] 1.471166
# BUDGET = 700 --- [1] 1.744863

GL_ZstarI(Vuln,GLAlpha,GLBeta,0.05,phase1Budget)
# BUDGET = 600 --- [1] 5.693494
# BUDGET = 700 --- [1] 6.305498

GL_ZstarI(Vuln,GLAlpha,GLBeta,0.1,phase1Budget)
# BUDGET = 600 --- [1] 8.857366
# BUDGET = 700 --- [1] 9.72287

GL_ZstarI(Vuln,GLAlpha,GLBeta,1,phase1Budget)
# BUDGET = 700 --- [1] 34.95155
GL_ZstarI(Vuln,GLAlpha,GLBeta,1,phase1Budget)
# BUDGET = 700 --- 
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.95,phase1Budget)
# BUDGET = 700 --- [1] 34.01731
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.90,phase1Budget)
# BUDGET = 700 --- [1] 33.05815

GL_ZstarI(Vuln,GLAlpha,GLBeta,1,develBudget)
# BUDGET = 1500 --- [1] 52.0659
GL_ZstarI_v(Vuln,GLAlpha,GLBeta,1,develBudget)
# BUDGET = 1500 --- [1] 0.03600712


####
## percentages
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.01,phase1Budget) / phase1Budget
# BUDGET = 600 --- [1] 0.002451943
# BUDGET = 700 --- [1] 0.002492661
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.1,phase1Budget) / phase1Budget
# BUDGET = 600 --- [1] 0.01476228
# BUDGET = 700 --- [1] 0.01388981

GL_ZstarI(Vuln,GLAlpha,GLBeta,1,develBudget) / develBudget
# BUDGET = 1500 --- [1] 0.0347106
GL_ZstarI(Vuln,GLAlpha,GLBeta,1,overallBudget) / overallBudget
# BUDGET = 21500 --- [1] 0.009420295



################################################################################
## Phase 2 Calculations
################################################################################

#####
## Graph from Chapter 6
## 
p2pre = seq(0:10000)		# z_{1} investment range
p2t = 1						# threat
p2Lambda = overallBudget	# loss

## Calculate ENBIS for various percentages of z_{2}
p2ENBIS05 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.005,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS1 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.01,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
#p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS10 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.10,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS25 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.25,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS37 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.37,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

## Draw plot
plot(p2pre, p2ENBIS3, type="l", lty=1, ylim=range(-500,22000), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
#line(p2pre, p2ENBIS3, type="l", lty=1)
abline(0, 0, col = "black")
#lines(p2pre, p2ENBIS05, type="l", lty=2, pch=0)
#lines(p2pre, p2ENBIS1, type="l", lty=2, pch=0)
lines(p2pre, p2ENBIS5, type="l", lty=3, pch=0)
lines(p2pre, p2ENBIS10, type="l", lty=4, pch=0)
lines(p2pre, p2ENBIS25, type="l", lty=2, pch=0)
lines(p2pre, p2ENBIS37, type="l", lty=5, pch=0)

## Set legend
legend("top", c(expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")), expression(paste('z'[2],"=25%")), expression(paste('z'[2],"=37%")) ), lty=c(1,3,4,2,5), horiz=TRUE, bty="n", cex=0.65)
#legend("top", c(expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(2,1,3,4), horiz=TRUE, bty="n", cex=0.65)
#expression(paste('z'[2],"=0.5%"))




################################################################################
## 
## EXPERIMENTAL CODE
##
################################################################################

################################################################################
## TESTING DELTA AND S1 PARAMS
################################################################################

p2pre = seq(0:10000)
p2t = 1
GLAlpha2 = 1
GLAlpha = 0.5142
GLBeta = 1
delta = 0.1
Vuln = 1
p2Lambda = overallBudget

p2ENBIS05 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.005,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS1 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.01,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
#p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS10 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.10,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS25 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.25,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS37 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.37,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

plot(p2pre, p2ENBIS3, type="l", lty=1, ylim=range(-500,22000), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
#line(p2pre, p2ENBIS3, type="l", lty=1)
abline(0, 0, col = "black")
#lines(p2pre, p2ENBIS05, type="l", lty=2, pch=0)
#lines(p2pre, p2ENBIS1, type="l", lty=2, pch=0)
lines(p2pre, p2ENBIS5, type="l", lty=3, pch=0)
lines(p2pre, p2ENBIS10, type="l", lty=4, pch=0)
lines(p2pre, p2ENBIS25, type="l", lty=2, pch=0)
lines(p2pre, p2ENBIS37, type="l", lty=5, pch=0)

#legend("top", c(expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(2,1,3,4), horiz=TRUE, bty="n", cex=0.65)
legend("top", c(expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")), expression(paste('z'[2],"=25%")), expression(paste('z'[2],"=37%")) ), lty=c(1,3,4,2,5), horiz=TRUE, bty="n", cex=0.65)
#expression(paste('z'[2],"=0.5%"))



################################################################################
## EXAMINING VULNERABILITY
################################################################################

v = GL_SSE_S1_S2_retVs(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v_pre = unlist(v[[1]])
v_pst = unlist(v[[2]])
resS1 = unlist(v[[3]])
resS2 = unlist(v[[4]])

which(p2ENBIS3 == max(p2ENBIS3), arr.ind = TRUE)
which(p2ENBIS3 > 0, arr.ind = TRUE)
which(v_pst == max(v_pst), arr.ind = TRUE)
which(v_pst == min(v_pst), arr.ind = TRUE)

which(v_pst < (1-((p2pre+(p2Lambda*0.03))/p2Lambda)), arr.ind = TRUE)
which((1-v_pst) > (ENBIS_SwSec_c / overallBudget), arr.ind = TRUE)

 
#### DEBUG
#GL_SSE_S1_S2_retVs(Vuln,355,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# v = GL_SSE_S1_S2_retVs(Vuln,p2pre,p2Lambda*0.1,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# v_pst = unlist(v[[2]])
# which(v_pst < (1-((p2pre+(p2Lambda*0.1))/p2Lambda)), arr.ind = FALSE)

#GL_SSE_S1_S2(Vuln,500,4000,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
 


################################################################################
## CALCULATIONS
################################################################################

##
which(v_pst < (1-((p2pre+(p2Lambda*0.03))/p2Lambda)), arr.ind = TRUE)

##
invMax = which(p2ENBIS3 == max(p2ENBIS3), arr.ind = TRUE)
#151

## ROSSP
ENBIS_SwSec = GL_SSE_S1_S2(Vuln,151,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# 20445.27
ENBIS_SwSec_c = 151+p2Lambda*0.03
# 796
ENBIS_NoSwSec = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# 1505
ENBIS_NoSwSec_c = 0 + p2Lambda*0.03
# 645
ROSSP_ProjA = ROSI_ENBIS(ENBIS_SwSec, ENBIS_SwSec_c) - ROSI_ENBIS(ENBIS_NoSwSec, ENBIS_NoSwSec_c)
# 23.35168




################################################################################
## 
## LEGACY CODE
##
################################################################################

################################################################################
## Application of GL-SSE to phase 1
################################################################################

#####
## Calculation for GL-SSE, phase 1
## Calculation of various percentages of post-deployment security investment
## 
## t = 0.1, \alpha2 = 1
##
# pre = seq(0:30)
# GLAlpha2 = 1
# t = 0.1
# delta = 0.1

# post05 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.005,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post1  = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.010,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post25 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.025,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post5  = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.050,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post10 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.100,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)

# plot(pre, post10, type="l", ylim=range(-40,50), lty=1, pch=0, 
  # ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
  # xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
# abline(0, 0, col = "black")
# lines(pre, post5, type="l", lty=2, pch=0)
# lines(pre, post25, type="l", lty=3, pch=0)
# lines(pre, post1, type="l", lty=4, pch=0)
# lines(pre, post05, type="l", lty=5, pch=0)
# legend("top", c(expression(paste('z'[2],"=0.5%")), expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=2.5%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(5,4,3,2,1), horiz=TRUE, bty="n", cex=0.65)

# ## t = 1, \alpha2 = 1
# ##
# pre = seq(0:150)
# GLAlpha2 = 1
# t = 0.5
# delta = 0.1
 
# post05 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.005,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post1  = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.010,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post25 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.025,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post5  = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.050,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
# post10 = GL_SSE_S1_S2(Vuln,pre,phase1Budget*0.100,t,phase1Budget,delta,GLAlpha,GLBeta,GLAlpha2)
 
# plot(pre, post10, type="l", ylim=range(-50,300), lty=1, pch=0, 
   # ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
   # xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
# abline(0, 0, col = "black")
# lines(pre, post5, type="l", lty=2, pch=0)
# lines(pre, post25, type="l", lty=3, pch=0)
# lines(pre, post1, type="l", lty=4, pch=0)
# lines(pre, post05, type="l", lty=5, pch=0)
# legend("top", c(expression(paste('z'[2],"=0.5%")), expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=2.5%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(5,4,3,2,1), horiz=TRUE, bty="n", cex=0.65)

################################################################################
## Code snippets
################################################################################

# which(v_pre == max(v_pre), arr.ind = TRUE)
# #6001
# which(v_pst == max(v_pst), arr.ind = TRUE)
# #173
# which(resS1 == max(resS1), arr.ind = TRUE)
# #1
# which(resS2 == max(resS2), arr.ind = TRUE)
# #6001

# v_pre[invMax]
# #0.9885561
# v_pst[invMax]
# #0.9879661
# resS1[invMax]
# #0.01144395
# resS2[invMax]
# #0.0005899834

#GL_SSE_S1_S2(Vuln,143,3396,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
#v_pst[3396]




