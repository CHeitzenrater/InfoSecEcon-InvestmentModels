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

factor = 100

################################################################################
## Phase 1 calculations
################################################################################

#####
## Calculation for GL, phase 1
## Calculate the investment over various threat stances (different values of t)
##
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.01,phase1Budget/factor)
# BUDGET = 600 --- [1] 1.471166
# BUDGET = 700 --- [1] 1.744863
# factor = 10: -0.7780047
# factor = 100: -0.829946
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.05,phase1Budget/factor)
# BUDGET = 600 --- [1] 5.693494
# BUDGET = 700 --- [1] 6.305498
# factor = 10: 0.6641948
# factor = 100: -0.2777199
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.1,phase1Budget/factor)
# BUDGET = 600 --- [1] 8.857366
# BUDGET = 700 --- [1] 9.72287
# factor = 10: 1.744863
# factor = 100: 0.03769753

GL_ZstarI(Vuln,GLAlpha,GLBeta,1,phase1Budget/factor)
# BUDGET = 700 --- [1] 34.95155
# factor = 10: 9.72287
# factor = 100: 1.58061
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.95,phase1Budget/factor)
# BUDGET = 700 --- [1] 34.01731
# factor = 10: 9.427439
# factor = 100: 1.535692
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.90,phase1Budget/factor)
# BUDGET = 700 --- [1] 33.05815
# factor = 10: 9.124126
# factor = 100: 1.488963

GL_ZstarI(Vuln,GLAlpha,GLBeta,1,develBudget/factor)
# BUDGET = 1500 --- [1] 52.0659
# factor = 10: 15.13491
# factor = 100: 2.320576
GL_ZstarI_v(Vuln,GLAlpha,GLBeta,1,develBudget/factor)
# BUDGET = 1500 --- [1] 0.03600712
# factor = 10: 0.1138645
# factor = 100: 0.09478542


####
## percentages
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.01,phase1Budget/factor) / (phase1Budget/factor)
# BUDGET = 600 --- [1] 0.002451943
# BUDGET = 700 --- [1] 0.002492661
# factor = 10: -0.01111435
# factor = 100: -0.1185637
GL_ZstarI(Vuln,GLAlpha,GLBeta,0.1,phase1Budget/factor) / (phase1Budget/factor)
# BUDGET = 600 --- [1] 0.01476228
# BUDGET = 700 --- [1] 0.01388981
# factor = 10: 0.02492661
# factor = 100: 0.005385361

GL_ZstarI(Vuln,GLAlpha,GLBeta,1,develBudget/factor) / (develBudget/factor)
# BUDGET = 1500 --- [1] 0.0347106
# factor = 10: 0.1008994
# factor = 100: 0.154705
GL_ZstarI(Vuln,GLAlpha,GLBeta,1,overallBudget/factor) / (overallBudget/factor)
# BUDGET = 21500 --- [1] 0.009420295
# factor = 10: 0.02917109
# factor = 100: 0.02955591



################################################################################
## Phase 2 Calculations
################################################################################

#####
## Graph from Chapter 6 --- varying z2 from 1% to 3%
## 
factor = 100
p2Lambda = overallBudget/factor
p2pre = seq(1,p2Lambda) #, by=0.01)
p2t = 1
GLAlpha2 = 1.0
GLAlpha = 0.5142
GLBeta = 3
delta = 0.1
Vuln = 0.95


p2ENBIS_0_175 = GL_SSE_S1_S2(Vuln,0,1.75,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_0_300 = GL_SSE_S1_S2(Vuln,0,3,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_0_645 = GL_SSE_S1_S2(Vuln,0,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

p2ENBIS_175 = GL_SSE_S1_S2(Vuln,p2pre,1.75,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_300 = GL_SSE_S1_S2(Vuln,p2pre,3,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_645 = GL_SSE_S1_S2(Vuln,p2pre,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)


ROSI_175 = p2ENBIS_175 / (p2pre+1.75)
ROSI_300 = p2ENBIS_300 / (p2pre+3)
ROSI_645 = p2ENBIS_645 / (p2pre+6.45)

ROSI_0_175 = p2ENBIS_0_175 / (1.75)
ROSI_0_300 = p2ENBIS_0_300 / (3)
ROSI_0_645 = p2ENBIS_0_645 / (6.45)

ROSSP_175 = ROSI_100 - ROSI_0_175
ROSSP_300 = ROSI_300 - ROSI_0_300
ROSSP_645 = ROSI_645 - ROSI_0_645

plot(p2pre, ROSI_645, type="l", lty=1, ylim=range(-1,75), ylab=c(expression(paste("ROSI"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($100K)"))) )
#line(p2pre, p2ENBIS3, type="l", lty=1)
abline(0, 0, col = "black")
lines(p2pre, ROSI_300, type="l", lty=2, pch=0)
lines(p2pre, ROSI_175, type="l", lty=3, pch=0)

legend("top", c(expression(paste('z'[2],"=$175K (0.8%)")), expression(paste('z'[2],"=$300K (1.4%)")), expression(paste('z'[2],"=$645K (3%)")) ), lty=c(3,2,1), horiz=TRUE, bty="n", cex=0.65)


p2ENBIS_0_645_099 = GL_SSE_S1_S2(0.99,0,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_0_645_095 = GL_SSE_S1_S2(0.95,0,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_0_645_090 = GL_SSE_S1_S2(0.90,0,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_0_645_100 = GL_SSE_S1_S2(1.00,0,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

p2ENBIS_645_099 = GL_SSE_S1_S2(0.99,p2pre,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_645_095 = GL_SSE_S1_S2(0.95,p2pre,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_645_090 = GL_SSE_S1_S2(0.90,p2pre,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS_645_100 = GL_SSE_S1_S2(1.00,p2pre,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

ROSI_645_099 = p2ENBIS_645_099 / (p2pre+6.45)
ROSI_645_095 = p2ENBIS_645_095 / (p2pre+6.45)
ROSI_645_090 = p2ENBIS_645_090 / (p2pre+6.45)
ROSI_645_100 = p2ENBIS_645_100 / (p2pre+6.45)


ROSI_0_645_099 = p2ENBIS_0_645_099 / (6.45)
ROSI_0_645_095 = p2ENBIS_0_645_095 / (6.45)
ROSI_0_645_090 = p2ENBIS_0_645_090 / (6.45)
ROSI_0_645_100 = p2ENBIS_0_645_100 / (6.45)

ROSSP_645_099 = ROSI_645_099 - ROSI_0_645_099
ROSSP_645_095 = ROSI_645_095 - ROSI_0_645_095
ROSSP_645_090 = ROSI_645_090 - ROSI_0_645_090
ROSSP_645_100 = ROSI_645_100 - ROSI_0_645_100


plot(p2pre, ROSSP_645_099, type="l", lty=1, ylim=range(-20,30), ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($100K)"))) )
#line(p2pre, p2ENBIS3, type="l", lty=1)
abline(0, 0, col = "black")
lines(p2pre, ROSSP_645_095, type="l", lty=2, pch=0)
lines(p2pre, ROSSP_645_090, type="l", lty=3, pch=0)
lines(p2pre, ROSSP_645_100, type="l", lty=4, pch=0)

legend("top", c(expression(paste("v=1.00")), expression(paste("v=0.99")), expression(paste("v=0.95")), expression(paste("v=0.90")) ), lty=c(4,1,2,3), horiz=TRUE, bty="n", cex=0.65)


#legend("top", c(expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(2,1,3,4), horiz=TRUE, bty="n", cex=0.65)
legend("top", c(expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=10%")), expression(paste('z'[2],"=37%")) ), lty=c(1,2,3,4), horiz=TRUE, bty="n", cex=0.65)
#expression(paste('z'[2],"=0.5%"))


factor = 100
p2Lambda = overallBudget/factor
p2pre = seq(1,p2Lambda) #, by=0.01)
p2t = 1
GLAlpha2 = 1.0
GLAlpha = 0.5142
GLBeta = 1.0
delta = 0.1
Vuln = 0.95
v = GL_SSE_S1_S2_retVs(Vuln,p2pre,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v_dep = unlist(v[[1]])
v_pst = unlist(v[[2]])
resS1 = unlist(v[[3]])
resS2 = unlist(v[[4]])


v = GL_SSE_S1_S2_retVs(Vuln,7.5,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v_dep = unlist(v[[1]])
v_pst = unlist(v[[2]])
resS1 = unlist(v[[3]])
resS2 = unlist(v[[4]])

v = GL_SSE_S1_S2_retVs(Vuln,0.34,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v_dep = unlist(v[[1]])
v_pst = unlist(v[[2]])
resS1 = unlist(v[[3]])
resS2 = unlist(v[[4]])

v = GL_SSE_S1_S2_retVs(Vuln,10,6.45,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v_dep = unlist(v[[1]])
v_pst = unlist(v[[2]])
resS1 = unlist(v[[3]])
resS2 = unlist(v[[4]])


# #####
# ## Graph from Chapter 6 --- varying z2 from 1% to 3%
# ## 
# p2Lambda = overallBudget/factor
# p2pre = seq(1,p2Lambda) #, by=0.01)
# p2t = 1
# GLAlpha2 = 1
# GLAlpha = 0.5142
# GLBeta = 3
# delta = 0.1
# Vuln = 1


# p2ENBIS1_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.01,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS3_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS10_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.10,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS37_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.37,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

# p2ENBIS1 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.01,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS3 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS10 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.10,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS37 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.37,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

# #p2ENBIS25 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.25,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# #p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# #p2ENBIS05 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.005,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

# ROSI1 = p2ENBIS1 / (p2pre+p2Lambda*0.01)
# ROSI3 = p2ENBIS3 / (p2pre+p2Lambda*0.03)
# ROSI10 = p2ENBIS10 / (p2pre+p2Lambda*0.10)
# ROSI37 = p2ENBIS37 / (p2pre+p2Lambda*0.37)

# ROSI1_0 = p2ENBIS1_0 / (p2Lambda*0.01)
# ROSI3_0 = p2ENBIS3_0 / (p2Lambda*0.03)
# ROSI10_0 = p2ENBIS10_0 / (p2Lambda*0.10)
# ROSI37_0 = p2ENBIS37_0 / (p2Lambda*0.37)

# ROSSP1 = ROSI1 - ROSI1_0
# ROSSP3 = ROSI3 - ROSI3_0
# ROSSP10 = ROSI10 - ROSI10_0
# ROSSP37 = ROSI37 - ROSI37_0

# plot(p2pre, ROSSP1, type="l", lty=1, ylim=range(-1,100), ylab=c(expression(paste("ROSSP"))), 
  # xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
# #line(p2pre, p2ENBIS3, type="l", lty=1)
# abline(0, 0, col = "black")
# #lines(p2pre, p2ENBIS05, type="l", lty=2, pch=0)
# #lines(p2pre, p2ENBIS1, type="l", lty=2, pch=0)
# lines(p2pre, ROSSP3, type="l", lty=2, pch=0)
# lines(p2pre, ROSSP10, type="l", lty=3, pch=0)
# lines(p2pre, ROSSP37, type="l", lty=4, pch=0)

# #legend("top", c(expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(2,1,3,4), horiz=TRUE, bty="n", cex=0.65)
# legend("top", c(expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=10%")), expression(paste('z'[2],"=37%")) ), lty=c(1,2,3,4), horiz=TRUE, bty="n", cex=0.65)
# #expression(paste('z'[2],"=0.5%"))



#####
## Graph from Chapter 6 --- varying t
## 
p2ENBIS3_t100_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.03,1.00,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_t080_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.03,0.80,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_t050_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.03,0.50,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_t020_0 = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.03,0.20,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

p2ENBIS3_t100 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,1.00,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_t080 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,0.80,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_t050 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,0.50,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_t020 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,0.20,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

ROSI3_t100 = p2ENBIS3_t100 / (p2pre+p2Lambda*0.03)
ROSI3_t080 = p2ENBIS3_t080 / (p2pre+p2Lambda*0.03)
ROSI3_t050 = p2ENBIS3_t050 / (p2pre+p2Lambda*0.03)
ROSI3_t020 = p2ENBIS3_t020 / (p2pre+p2Lambda*0.03)

ROSI3_t100_0 = p2ENBIS3_t100_0 / (p2Lambda*0.03)
ROSI3_t080_0 = p2ENBIS3_t080_0 / (p2Lambda*0.03)
ROSI3_t050_0 = p2ENBIS3_t050_0 / (p2Lambda*0.03)
ROSI3_t020_0 = p2ENBIS3_t020_0 / (p2Lambda*0.03)

ROSSP3_t100 = ROSI3_t100 - ROSI3_t100_0
ROSSP3_t080 = ROSI3_t080 - ROSI3_t080_0
ROSSP3_t050 = ROSI3_t050 - ROSI3_t050_0
ROSSP3_t020 = ROSI3_t020 - ROSI3_t020_0

plot(p2pre, ROSSP3_t100, type="l", lty=1, ylim=range(-2,40), xlim=range(0,p2Lambda), ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
abline(0, 0, col = "black")
lines(p2pre, ROSSP3_t080, type="l", lty=2, pch=0)
lines(p2pre, ROSSP3_t050, type="l", lty=3, pch=0)
lines(p2pre, ROSSP3_t020, type="l", lty=4, pch=0)

legend("top", c(expression(paste("t = 1")), expression(paste("t = 0.80")), expression(paste("t = 0.50")), expression(paste("t = 0.20")) ), lty=c(1,2,3,4), horiz=TRUE, bty="n", cex=0.65)


#####
## Graph from Chapter 6 --- varying v
## 
per = 0.03

z2Expend = p2Lambda*per
#z2Expend = 3000

p2ENBIS3_v100_0 = GL_SSE_S1_S2(1.00,0,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_v099_0 = GL_SSE_S1_S2(0.99,0,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_v095_0 = GL_SSE_S1_S2(0.95,0,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_v090_0 = GL_SSE_S1_S2(0.90,0,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)


p2ENBIS3_v100 = GL_SSE_S1_S2(1.00,p2pre,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_v099 = GL_SSE_S1_S2(0.99,p2pre,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_v095 = GL_SSE_S1_S2(0.95,p2pre,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ENBIS3_v090 = GL_SSE_S1_S2(0.90,p2pre,z2Expend,1,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)


ROSI3_v100 = p2ENBIS3_v100 / (p2pre+z2Expend)
ROSI3_v099 = p2ENBIS3_v099 / (p2pre+z2Expend)
ROSI3_v095 = p2ENBIS3_v095 / (p2pre+z2Expend)
ROSI3_v090 = p2ENBIS3_v090 / (p2pre+z2Expend)


ROSI3_v100_0 = p2ENBIS3_v100_0 / (z2Expend)
ROSI3_v099_0 = p2ENBIS3_v099_0 / (z2Expend)
ROSI3_v095_0 = p2ENBIS3_v095_0 / (z2Expend)
ROSI3_v090_0 = p2ENBIS3_v090_0 / (z2Expend)


ROSSP3_v100 = ROSI3_v100 - ROSI3_v100_0
ROSSP3_v099 = ROSI3_v099 - ROSI3_v099_0
ROSSP3_v095 = ROSI3_v095 - ROSI3_v095_0
ROSSP3_v090 = ROSI3_v090 - ROSI3_v090_0


plot(p2pre, ROSSP3_v100, type="l", lty=1, ylim=range(-30,40), ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($100K)"))) )
abline(0, 0, col = "black")
lines(p2pre, ROSSP3_v099, type="l", lty=2, pch=0)
lines(p2pre, ROSSP3_v095, type="l", lty=3, pch=0)
lines(p2pre, ROSSP3_v090, type="l", lty=4, pch=0)

legend("top", c(expression(paste("v = 1.0")), expression(paste("v = 0.99")), expression(paste("v = 0.95")), expression(paste("v = 0.90"))  ), lty=c(1,2,3,4), horiz=TRUE, bty="n", cex=0.65)


which(ROSSP3_v099 > 0, arr.ind = TRUE)






################################################################################
## 
## EXPERIMENTAL CODE
##
################################################################################

################################################################################
## TESTING DELTA AND S1 PARAMS
################################################################################

################################################################################
## EXAMINING VULNERABILITY
################################################################################
p2pre = seq(0, 10000, by=0.1)
p2t = 1
GLAlpha2 = 1
GLAlpha = 0.5142
GLBeta = 1
delta = 0.1
Vuln = 0.999
p2Lambda = overallBudget

p2ENBIS3 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
p2ROSSP = GL_SSE_S1_S2(Vuln,0,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v = GL_SSE_S1_S2_retVs(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
v_dep = unlist(v[[1]])
v_pst = unlist(v[[2]])
resS1 = unlist(v[[3]])
resS2 = unlist(v[[4]])

#plot(p2pre, p2ENBIS3, type="l", lty=1, ylim=range(-500,22000), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
#  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )

#which(p2ENBIS3 == max(p2ENBIS3), arr.ind = TRUE)


rossp = (p2ENBIS3/(p2pre+(p2Lambda*0.03)) - (p2ROSSP/(p2Lambda*0.03)))
plot(p2pre, rossp, type="l", lty=1, ylim=range(-20,20), ylab=c(expression(paste("ROSSP"))), 
  xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )


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







# p2pre = seq(0:21500)		# z_{1} investment range
# p2t = 1						# threat
# p2Lambda = overallBudget	# loss
# delta = 0.1					# vulnerability attributed to misconfiguration
# GLAlpha2 = 1				# alphas for S2()

# ## Calculate ENBIS for various percentages of z_{2}
# p2ENBIS05 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.005,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS1 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.01,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS3 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.03,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# #p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS5 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.05,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS10 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.10,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS25 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.25,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)
# p2ENBIS37 = GL_SSE_S1_S2(Vuln,p2pre,p2Lambda*0.37,p2t,p2Lambda,delta,GLAlpha,GLBeta,GLAlpha2)

# ## Draw plot
# plot(p2pre, p2ENBIS3, type="l", lty=1, ylim=range(-500,22000), ylab=c(expression(paste("ENBIS(",'z'[1],"+",'z'[2],") ($K)"))), 
  # xlab=c(expression(paste("Pre-deployment Investment ",'z'[1]," ($K)"))) )
# #line(p2pre, p2ENBIS3, type="l", lty=1)
# abline(0, 0, col = "black")
# #lines(p2pre, p2ENBIS05, type="l", lty=2, pch=0)
# #lines(p2pre, p2ENBIS1, type="l", lty=2, pch=0)
# lines(p2pre, p2ENBIS5, type="l", lty=3, pch=0)
# lines(p2pre, p2ENBIS10, type="l", lty=4, pch=0)
# lines(p2pre, p2ENBIS25, type="l", lty=2, pch=0)
# lines(p2pre, p2ENBIS37, type="l", lty=5, pch=0)

# ## Set legend
# legend("top", c(expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")), expression(paste('z'[2],"=25%")), expression(paste('z'[2],"=37%")) ), lty=c(1,3,4,2,5), horiz=TRUE, bty="n", cex=0.65)
# #legend("top", c(expression(paste('z'[2],"=1%")), expression(paste('z'[2],"=3%")), expression(paste('z'[2],"=5%")), expression(paste('z'[2],"=10%")) ), lty=c(2,1,3,4), horiz=TRUE, bty="n", cex=0.65)
# #expression(paste('z'[2],"=0.5%"))

