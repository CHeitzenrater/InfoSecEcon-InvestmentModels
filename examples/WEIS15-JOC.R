#############################################################################
#
# Analysis for WEIS 2015 and Journal of Cybersecurity appers
#
#############################################################################

source("./models/InfoEconFunctions.R", local=TRUE)
source("./data/ISBS-WEIS2015.R", local=TRUE)

#====================================================
#
# Year by year probabilities
#
#====================================================

#====================================================
# p_0 = probability of happening without security; here, assume it is the probability of getting attacked

# probability of a malicious event
p_0_15 <- p_malicious_SB_15 * (bt_SB_virus_15+bt_SB_outHack_15+worst_SB_compromiseWAccess_15) #JoC
p_0_14 <- p_malicious_SB_14 * (bt_SB_virus_14+bt_SB_outHack_14)
p_0_13 <- p_malicious_SB_13 * (bt_SB_virus_13+bt_SB_outHack_13)
p_0_12 <- p_malicious_SB_12 * (bt_SB_virus_12+bt_SB_outHack_12)
#p_0_both <- c(p_0_12,p_0_13,p_0_14,p_0_15) #JoC
p_0_both <- c(p_0_12,p_0_13,p_0_14) 

# Probability of the worse event being a virus, 2012-2015
p_0_15_worst_virus <- p_malicious_SB_15 * (worst_SB_virus_15) #JoC
p_0_14_worst_virus <- p_malicious_SB_14 * (worst_SB_virus_14)
p_0_13_worst_virus <- p_malicious_SB_13 * (worst_SB_virus_13)
p_0_12_worst_virus <- p_malicious_SB_12 * (worst_SB_virus_12)
#p_0_worst_virus <- c(p_0_12_worst_virus,p_0_13_worst_virus,p_0_14_worst_virus,p_0_15_worst_virus) #JoC
p_0_worst_virus <- c(p_0_12_worst_virus,p_0_13_worst_virus,p_0_14_worst_virus) 

# Probability of the worse event being a hacker, 2012-2015
p_0_15_worst_hacker <- p_malicious_SB_15 * (worst_SB_outsider_15+worst_SB_compromiseWAccess_15) #JoC
p_0_14_worst_hacker <- p_malicious_SB_14 * (worst_SB_outsider_14)
p_0_13_worst_hacker <- p_malicious_SB_13 * (worst_SB_outsider_13)
p_0_12_worst_hacker <- p_malicious_SB_12 * (worst_SB_outsider_12)
#p_0_worst_hacker <- c(p_0_12_worst_hacker,p_0_13_worst_hacker,p_0_14_worst_hacker,p_0_15_worst_hacker) #JoC
p_0_worst_hacker <- c(p_0_12_worst_hacker,p_0_13_worst_hacker,p_0_14_worst_hacker) 

# Probability of the worse event being a virus or a hacker, 2012-2015
p_0_15_worst_both <- p_malicious_SB_15 * (worst_SB_virus_15+worst_SB_outsider_15) #JoC
p_0_14_worst_both <- p_malicious_SB_14 * (worst_SB_virus_14+worst_SB_outsider_14)
p_0_13_worst_both <- p_malicious_SB_13 * (worst_SB_virus_13+worst_SB_outsider_13)
p_0_12_worst_both <- p_malicious_SB_12 * (worst_SB_virus_12+worst_SB_outsider_12)
#p_0_worst_both <- c(p_0_12_worst_both,p_0_13_worst_both,p_0_14_worst_both,p_0_15_worst_both) #JoC
p_0_worst_both <- c(p_0_12_worst_both,p_0_13_worst_both,p_0_14_worst_both) 

# Probability of a "serious" incident, 2012-2015
p_0_15_serious <- p_serious_SB_15 * (worst_SB_virus_15+worst_SB_outsider_15) #JoC
p_0_14_serious <- p_serious_SB_14 * (worst_SB_virus_14+worst_SB_outsider_14)
p_0_13_serious <- p_serious_SB_13 * (worst_SB_virus_13+worst_SB_outsider_13)
p_0_12_serious <- p_serious_SB_12 * (worst_SB_virus_12+worst_SB_outsider_13)
#p_0_serious <- c(p_0_12_serious,p_0_13_serious,p_0_14_serious,p_0_15_serious) #JoC
p_0_serious <- c(p_0_12_serious,p_0_13_serious,p_0_14_serious)


#====================================================
# p_s = probability of event happening with security; assume if entirely implemented, the probability of successful
# attack is reduced to just 1%

# Establish the probability of a successful defense as 1%
p_s_14 <- 0.01
p_s_13 <- 0.01
p_s_12 <- 0.01
p_s <- c(p_s_14,p_s_13,p_s_12)


#====================================================
# ALE by attack type --- virus, hackers, both
# Low (L) and High (H) loss estimates 
# For Small Business (SB)
ale_L_virus <- ALE(p_0_worst_virus, twci_L_SB)
ale_H_virus <- ALE(p_0_worst_virus, twci_H_SB)
ale_L_hackers <- ALE(p_0_worst_hacker, twci_L_SB)
ale_H_hackers <- ALE(p_0_worst_hacker, twci_H_SB)
ale_L_both <- ALE(p_0_worst_both, twci_L_SB)
ale_H_both <- ALE(p_0_worst_both, twci_H_SB)
ale_L_both_4 <- ALE(p_0_worst_both, 4*twci_L_SB)
ale_H_both_4 <- ALE(p_0_worst_both, 4*twci_H_SB)



#====================================================
#
# Plot Figures
#
#====================================================

# Set the max and min loss by year; this is calculated using the worst loss and projecting
# over the expected number of incidents by type (hacker, virus) per year
#  WorstLoss * (IncidentsPerYear*RateByType)
max_loss_2012_2014 <- c(116235, 124883, 149040)
min_loss_2012_2014 <- c(58117.50, 67244.80, 84240.00)

#====================================================
# Establish costs per type of defense
# Calculate for install year, maintenance year

# Firewall
firewall_l_install <- 790.4
firewall_s_install <- 222.46
## loss using gartner assumptions
firewall_l_install_mp <- (firewall_l_install/0.21 * 0.4) 
firewall_s_install_mp <- (firewall_s_install/0.21 * 0.4) 
	
# Software firewall
swfirewall_license <- 30.57
	
# Antivirus
antivirus_bl_license <- 39.37
antivirus_bl_update <- 24.37
	
# patching (use OS costs under maintenance assumptions)
os_license <- 45
	
#====================================================
# Set costs by machine, for install and maintenance years
#
costsFor1_install <- ((swfirewall_license+antivirus_bl_license+os_license)/.29) * 0.4 
costsFor1_yearly <- ((antivirus_bl_update)/.29) * 0.4
costsFor49_install <- ((swfirewall_license+antivirus_bl_license+os_license)*49/.29) * 0.4 
costsFor49_yearly <- ((antivirus_bl_update)*49/.29) * 0.4
costsFor249_install <- ((swfirewall_license+antivirus_bl_license+os_license)*249/.29) * 0.4 
costsFor249_yearly <- ((antivirus_bl_update)*249/.29) * 0.4	


	
	# independant variables
	#cost <- seq(0, 150000);
	#machines <- seq(1:249)
	#eff <- seq(0.5, 0.01, by=-0.01)
#licenseCosts <- 69.94
#maintenanceCosts <- 24.37

	

#====================================================
# Figure 1 --- ENBIS by year; 99% effectiveness
# Gartner manpower assumption
#
Figure1 <- function(){
	
	#====================================================
	# Costs, by number of machines, against loss for install and maintenance years
	#
	# Expected Net Benefit of Information Security
	# p_0    : Probability of the event occurring without security 
	# p_s    : Probability of the event occurring with security 
	# lambda : Bernoulli Loss
	# s      : Cost of security
	# ENBIS <- function( p_0, lambda, p_s, s) 
	
	# Maximum loss
	machines_1_max_install <- ENBIS(p_0_both,max_loss_2012_2014,p_s,(firewall_s_install+firewall_s_install_mp)+(swfirewall_license+antivirus_bl_license)*1+costsFor1_install)
	machines_1_max_yearly <- ENBIS(p_0_both,max_loss_2012_2014,p_s,(antivirus_bl_update)*1+costsFor1_yearly+firewall_s_install_mp)
	machines_49_max_install <- ENBIS(p_0_both,max_loss_2012_2014,p_s,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*49+costsFor49_install)
	machines_49_max_yearly <- ENBIS(p_0_both,max_loss_2012_2014,p_s,(antivirus_bl_update)*49+costsFor49_yearly+firewall_l_install_mp)
	machines_249_max_install <- ENBIS(p_0_both,max_loss_2012_2014,p_s,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*249+costsFor249_install)
	machines_249_max_yearly <- ENBIS(p_0_both,max_loss_2012_2014,p_s,(antivirus_bl_update)*249+costsFor249_yearly+firewall_l_install_mp)

	# Minimum loss
	machines_1_min_install <- ENBIS(p_0_both,min_loss_2012_2014,p_s,(firewall_s_install+firewall_s_install_mp)+(swfirewall_license+antivirus_bl_license)*1+costsFor1_install)
	machines_1_min_yearly <- ENBIS(p_0_both,min_loss_2012_2014,p_s,(antivirus_bl_update)*1+costsFor1_yearly+firewall_s_install_mp)
	machines_49_min_install <- ENBIS(p_0_both,min_loss_2012_2014,p_s,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*49+costsFor49_install)
	machines_49_min_yearly <- ENBIS(p_0_both,min_loss_2012_2014,p_s,(antivirus_bl_update)*49+costsFor49_yearly+firewall_l_install_mp)
	machines_249_min_install <- ENBIS(p_0_both,min_loss_2012_2014,p_s,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*249+costsFor249_install)
	machines_249_min_yearly <- ENBIS(p_0_both,min_loss_2012_2014,p_s,(antivirus_bl_update)*249+costsFor249_yearly+firewall_l_install_mp)

	#====================================================
	# Draw and write graph 
	#
	# Prepare output file, and design canvas
	tiff("ENBIS-ByYear-99-GartnerEst-HD.tiff", width = 8, height = 8, units = 'in', res = 600)
	layout(rbind(1,2), heights=c(6,1))
	
	# Independant variable --- Years covered
	#years <- c("2012", "2013", "2014", "2015")
	years <- c("2012", "2013", "2014")
	
	# scale range
	g_range <- range(0, machines_1_max_install,machines_249_max_install,machines_1_max_yearly,machines_249_max_yearly,machines_1_min_install,machines_249_min_install,machines_1_min_yearly,machines_249_min_yearly) 
	rng <- seq(-80000, 100000, by=10000)
	
	# Draw graph
	plot(years, machines_1_max_install, ylim= g_range, type="o", lty=1, pch=0, ylab="ENBIS (£)", xlab="Year", axes=FALSE)
	par(mai=c(0.92,0.96,0.82,0.42))
	axis(1, at=years, lab=years)
	axis(2, las=1, at=rng, lab=format(rng, digits=10, nsmall=0, big.mark=","))
	
	lines(years, machines_1_max_yearly, type="o", lty=1, pch=15) #col="orange")
	lines(years, machines_49_max_install, type="o", lty=1, pch=1) #col="green")
	lines(years, machines_49_max_yearly, type="o", lty=1, pch=16) #col="red")
	lines(years, machines_249_max_install, type="o", lty=1, pch=2) #col="green")
	lines(years, machines_249_max_yearly, type="o", lty=1, pch=17) #col="red")

	lines(years, machines_1_min_install, type="o", lty=2, pch=0) #col="blue")
	lines(years, machines_1_min_yearly, type="o", lty=2, pch=15) #col="orange")
	lines(years, machines_49_min_install, type="o", lty=2, pch=1) #col="green")
	lines(years, machines_49_min_yearly, type="o", lty=2, pch=16) #col="red")
	lines(years, machines_249_min_install, type="o", lty=2, pch=2) #col="green")
	lines(years, machines_249_min_yearly, type="o", lty=2, pch=17) #col="red")
	abline(0, 0, col = "black")

	# Legend
	# Draw as a separate plat to contorl form and placement
	par(mar=c(0, 0, 0, 0))
	plot.new()
	legend("top", c("1 max install", "1 max yearly", "49 max install", "49 max yearly", "249 max install", "249 max yearly"), lty=c(1,1,1,1,1,1), pch=c(0,15,1,16,2,17), horiz=TRUE, bty="n", cex=0.65)
	legend("center", c("1 min install", "1 min yearly", "49 min install", "49 min yearly", "249 min install", "249 min yearly"), lty=c(2,2,2,2,2,2), pch=c(0,15,1,16,2,17), horiz=TRUE, bty="n", cex=0.68)
	#legend("center", c("1 max inst", "249 max inst", "1 max yr", "249 max yr", "1 min inst", "249 min inst", "1 min yr", "249 min yr"), lty=c(1,1,1,1,2,2,2,2), pch=c(1,2,3,4,1,2,3,4), horiz=TRUE, bty="n", cex=0.75)
	#type=c("o","o","o","o","o","o","o","o"), 

	# End drawing and write the file
	dev.off()
}


#====================================================
# Figure 2 ---  Varied loss for 2014; 99% effectiveness
# Gartner manpower assumption
#
Figure2 <- function (){

	# Independant variable --- amount of loss
	loss <- seq(0,250000, by=10000)

	#====================================================
	# Costs, by number of machines, for 2014 against varied loss (install and yearly)
	#	
	machines_1_install_14 <- ENBIS(p_0_14,loss,p_s_14,(firewall_s_install+firewall_s_install_mp)+(swfirewall_license+antivirus_bl_license)*1+costsFor1_install)
	machines_1_yearly_14 <- ENBIS(p_0_14,loss,p_s_14,(antivirus_bl_update)*1+costsFor1_yearly+firewall_s_install_mp)
	
	machines_49_install_14 <- ENBIS(p_0_14,loss,p_s_14,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*49+costsFor49_install)
	machines_49_yearly_14 <- ENBIS(p_0_14,loss,p_s_14,(antivirus_bl_update)*49+costsFor49_yearly+firewall_l_install_mp)
	
	machines_249_install_14 <- ENBIS(p_0_14,loss,p_s_14,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*249+costsFor249_install)
	machines_249_yearly_14 <- ENBIS(p_0_14,loss,p_s_14,(antivirus_bl_update)*249+costsFor249_yearly+firewall_l_install_mp)

	#====================================================
	# Draw and write graph 
	#
	# Prepare output file
	tiff("ENBIS-VaryLoss-99-GartnerEst-HD.tiff", width = 8, height = 8, units = 'in', res = 600)
	
	# Set range
	g_range <- range(0, machines_1_install_14,machines_249_install_14,machines_1_yearly_14,machines_249_yearly_14)
	e <- seq(-70000, 130000, by=10000)
	
	# Draw graph
	plot(loss, machines_1_install_14, ylim=g_range, type="o", lty=1, pch=0, ylab="ENBIS (£)", xlab="Loss (£)", axes=FALSE)
	par(mai=c(0.92,0.99,0.82,0.42))
	axis(1, at=loss, lab=format(loss, digits=10, nsmall=0, big.mark=","))
	axis(2, las=1, at=e, lab=format(e, digits=10, nsmall=0, big.mark=","))
	
	lines(loss, machines_1_yearly_14, type="o", lty=1, pch=15) #col="orange")
	lines(loss, machines_49_install_14, type="o", lty=1, pch=1) #col="green")
	lines(loss, machines_49_yearly_14, type="o", lty=1, pch=16) #col="red")
	lines(loss, machines_249_install_14, type="o", lty=1, pch=2) #col="green")
	lines(loss, machines_249_yearly_14, type="o", lty=1, pch=17) #col="red")
	abline(0, 0, col = "black")

	# Draw legend
	legend("top", c("1 install", "1 yearly", "49 install", "49 yearly", "249 install", "249 yearly"), lty=c(1,1,1,1,1,1), pch=c(0,15,1,16,2,17), bty="n", cex=0.75)

	# End drawing and write the file
	dev.off()
}


#====================================================
# Figure 3 ---  Varied effectiveness for 2014
# Gartner manpower assumption
#
Figure3 <- function (){

	# Independant variable --- effectiveness of measure
	eff <- seq(0.5, 0, by=-0.05)

	#====================================================
	# Costs, by number of machines, for 2014 against varied effectiveness (install and yearly)
	#	
	# Maximum loss
	machines_1_max_install_14eff <- ENBIS(p_0_14,149040,eff,(firewall_s_install+firewall_s_install_mp)+(swfirewall_license+antivirus_bl_license)*1+costsFor1_install)
	machines_1_max_yearly_14eff <- ENBIS(p_0_14,149040,eff,(antivirus_bl_update)*1+costsFor1_yearly+firewall_s_install_mp)

	machines_49_max_install_14eff <- ENBIS(p_0_14,149040,eff,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*49+costsFor49_install)
	machines_49_max_yearly_14eff <- ENBIS(p_0_14,149040,eff,(antivirus_bl_update)*49+costsFor49_yearly+firewall_l_install_mp)

	machines_249_max_install_14eff <- ENBIS(p_0_14,149040,eff,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*249+costsFor249_install)
	machines_249_max_yearly_14eff <- ENBIS(p_0_14,149040,eff,(antivirus_bl_update)*249+costsFor249_yearly+firewall_l_install_mp)

	# Minimum loss
	machines_1_min_install_14eff <- ENBIS(p_0_14,84240,eff,(firewall_s_install+firewall_s_install_mp)+(swfirewall_license+antivirus_bl_license)*1+costsFor1_install)
	machines_1_min_yearly_14eff <- ENBIS(p_0_14,84240,eff,(antivirus_bl_update)*1+costsFor1_yearly+firewall_s_install_mp)

	machines_49_min_install_14eff <- ENBIS(p_0_14,84240,eff,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*49+costsFor49_install)
	machines_49_min_yearly_14eff <- ENBIS(p_0_14,84240,eff,(antivirus_bl_update)*49+costsFor49_yearly+firewall_l_install_mp)

	machines_249_min_install_14eff <- ENBIS(p_0_14,84240,eff,(firewall_l_install+firewall_l_install_mp)+(swfirewall_license+antivirus_bl_license)*249+costsFor249_install)
	machines_249_min_yearly_14eff <- ENBIS(p_0_14,84240,eff,(antivirus_bl_update)*249+costsFor249_yearly+firewall_l_install_mp)

	#====================================================
	# Draw and write graph 
	#
	# Prepare output file
	tiff("ENBIS-VaryEff-GartnerEst-HD.tiff", width = 8, height = 8, units = 'in', res = 600)

	# Set range
	g_range <- range(0, machines_1_max_install_14eff,machines_249_max_install_14eff,machines_1_max_yearly_14eff,machines_249_max_yearly_14eff,machines_1_min_install_14eff,machines_249_min_install_14eff,machines_1_min_yearly_14eff,machines_249_min_yearly_14eff) 

	# Draw graph
	plot(eff, machines_1_max_install_14eff, ylim=g_range, type="o", lty=1, pch=0, ylab="ENBIS (£)", xlab="Effectiveness (%)", axes=FALSE)
	par(mai=c(0.92,0.99,0.82,0.42))
	effInv <- (1-eff)*100
	e <- seq(-70000, 130000, by=10000)
	axis(1, at=eff, lab=effInv)
	axis(2, las=1, at=e, lab=format(e, digits=10, nsmall=0, big.mark=","))

	lines(eff, machines_1_max_yearly_14eff, type="o", lty=1, pch=15) #col="orange")
	lines(eff, machines_49_max_install_14eff, type="o", lty=1, pch=1) #col="orange")
	lines(eff, machines_49_max_yearly_14eff, type="o", lty=1, pch=16) #col="orange")
	lines(eff, machines_249_max_install_14eff, type="o", lty=1, pch=2) #col="green")
	lines(eff, machines_249_max_yearly_14eff, type="o", lty=1, pch=17) #col="red")

	lines(eff, machines_1_min_install_14eff, type="o", lty=2, pch=0) #col="blue")
	lines(eff, machines_1_min_yearly_14eff, type="o", lty=2, pch=15) #col="orange")
	lines(eff, machines_49_min_install_14eff, type="o", lty=2, pch=1) #col="blue")
	lines(eff, machines_49_min_yearly_14eff, type="o", lty=2, pch=16) #col="orange")
	lines(eff, machines_249_min_install_14eff, type="o", lty=2, pch=2) #col="green")
	lines(eff, machines_249_min_yearly_14eff, type="o", lty=2, pch=17) #col="red")
	abline(0, 0, col = "black")

	# Draw legend
	legend("topright", c("1 max install", "1 max yearly", "49 max install", "49 max yearly", "249 max install", "249 max yearly", "1 min install", "1 min yearly", "49 min install", "49 min yearly", "249 min install", "249 min yearly"), pch=c(0,15,1,16,2,17,0,15,1,16,2,17), lty=c(1,1,1,1,1,1,2,2,2,2,2,2), bty="n", cex=0.75)

	# End drawing and write the file
	dev.off()
}



#====================================================
# Figure 4 ---  Net Present Value for 2014 data
# Gartner manpower assumption
#
Figure4 <- function (){

	#====================================================
	# Costs, by number of machines, for 2014 against varied effectiveness (install and yearly)
	#	Using Gartnet assumption
	#
	# Virus and blacklisting
	costsFor1_virusbl_install <- ((antivirus_bl_license)/.29) * 0.4 
	costsFor1_virusbl_yearly <- ((antivirus_bl_update)/.29) * 0.4
	costsFor9_virusbl_install <- (antivirus_bl_license*9/.29) * 0.4 
	costsFor9_virusbl_yearly <- ((antivirus_bl_update)*9/.29) * 0.4
	costsFor49_virusbl_install <- (antivirus_bl_license*49/.29) * 0.4 
	costsFor49_virusbl_yearly <- ((antivirus_bl_update)*49/.29) * 0.4
	costsFor249_virusbl_install <- (antivirus_bl_license*249/.29) * 0.4 
	costsFor249_virusbl_yearly <- ((antivirus_bl_update)*249/.29) * 0.4
	# patching
	costsFor1_patching_install <- (os_license/.29) * 0.4 
	costsFor249_patching_install <- (os_license*249/.29) * 0.4 
	costsFor1_swfirewall_install <- (swfirewall_license/.29) * 0.4 
	costsFor249_swfirewall_install <- (swfirewall_license*249/.29) * 0.4 

	#====================================================
	# Establish congomerate probability values by breach type (bt) for small business (SB)
	bt_virus_SB <- c(bt_SB_virus_12,bt_SB_virus_13,bt_SB_virus_14)
	bt_hack_SB <- c(bt_SB_outHack_12,bt_SB_outHack_13,bt_SB_outHack_14)

	#====================================================
	# Establish effectiveness for different mitigations
	# virus: 75%
	# blacklist: 73.5%
	# firewall: 80%
	# out-of-the-box firewall: 60%
	p_virus <- c((1-0.75),(1-0.75),(1-0.75)) 
	p_bl <- c((1-0.735),(1-0.735),(1-0.735))
	p_firewall <- c((1-0.80),(1-0.80),(1-0.80)) 
	p_ifirewall <- c((1-0.60),(1-0.60),(1-0.60))

	#====================================================
	# Calculate ALE for various configurations
	#	
	ALE_virus_max <- ALE(bt_virus_SB*p_malicious_SB, max_loss_2012_2014)
	ALE_virus_min <- ALE(bt_virus_SB*p_malicious_SB, min_loss_2012_2014)
	ALE_virusSecurity_max <- ALE(p_virus*p_malicious_SB, max_loss_2012_2014)
	ALE_virusSecurity_min <- ALE(p_virus*p_malicious_SB, min_loss_2012_2014)
	ALE_blSecurity_max <- ALE(p_bl*p_malicious_SB, max_loss_2012_2014)
	ALE_blSecurity_min <- ALE(p_bl*p_malicious_SB, min_loss_2012_2014)
	ALE_hack_max <- ALE(bt_hack_SB*p_malicious_SB, max_loss_2012_2014)
	ALE_hack_min <- ALE(bt_hack_SB*p_malicious_SB, min_loss_2012_2014)
	ALE_fwSecurity_max <- ALE(p_firewall*p_malicious_SB, max_loss_2012_2014)
	ALE_fwSecurity_min <- ALE(p_firewall*p_malicious_SB, min_loss_2012_2014)
	ALE_ifwSecurity_max <- ALE(p_ifirewall*p_malicious_SB, max_loss_2012_2014)
	ALE_ifwSecurity_min <- ALE(p_ifirewall*p_malicious_SB, min_loss_2012_2014)

	#====================================================
	# Calculate Net Present Value (NPV) for High (H) and low (L), across various numbers 
	#  of machines
	#	
	npv_antivirus_H249 <- NPV(0.05, antivirus_bl_license*249+costsFor249_virusbl_install, antivirus_bl_update*249+costsFor249_virusbl_yearly, ALE_virus_max, ALE_virusSecurity_max, 3)
	npv_antivirus_L249 <- NPV(0.05, antivirus_bl_license*249+costsFor249_virusbl_install, antivirus_bl_update*249+costsFor249_virusbl_yearly, ALE_virus_min, ALE_virusSecurity_min, 3)

	npv_antivirus_H49 <- NPV(0.05, antivirus_bl_license*49+costsFor49_virusbl_install, antivirus_bl_update*49+costsFor49_virusbl_yearly, ALE_virus_max, ALE_virusSecurity_max, 3)
	npv_antivirus_L49 <- NPV(0.05, antivirus_bl_license*49+costsFor49_virusbl_install, antivirus_bl_update*49+costsFor49_virusbl_yearly, ALE_virus_min, ALE_virusSecurity_min, 3)

	npv_antivirus_H9 <- NPV(0.05, antivirus_bl_license*9+costsFor9_virusbl_install, antivirus_bl_update*9+costsFor9_virusbl_yearly, ALE_virus_max, ALE_virusSecurity_max, 3)
	npv_antivirus_L9 <- NPV(0.05, antivirus_bl_license*9+costsFor9_virusbl_install, antivirus_bl_update*9+costsFor9_virusbl_yearly, ALE_virus_min, ALE_virusSecurity_min, 3)

	npv_antivirus_H1 <- NPV(0.05, antivirus_bl_license*1+costsFor1_virusbl_install, antivirus_bl_update*1+costsFor1_virusbl_yearly, ALE_virus_max, ALE_virusSecurity_max, 3)
	npv_antivirus_L1 <- NPV(0.05, antivirus_bl_license*1+costsFor1_virusbl_install, antivirus_bl_update*1+costsFor1_virusbl_yearly, ALE_virus_min, ALE_virusSecurity_min, 3)

	npv_bl_H249 <- NPV(0.05, antivirus_bl_license*249+costsFor249_virusbl_install, antivirus_bl_update*249+costsFor249_virusbl_yearly, ALE_virus_max, ALE_blSecurity_max, 3)
	npv_bl_L249 <- NPV(0.05, antivirus_bl_license*249+costsFor249_virusbl_install, antivirus_bl_update*249+costsFor249_virusbl_yearly, ALE_virus_min, ALE_blSecurity_min, 3)
	npv_bl_H1 <- NPV(0.05, antivirus_bl_license*1+costsFor1_virusbl_install, antivirus_bl_update*1+costsFor1_virusbl_yearly, ALE_virus_max, ALE_blSecurity_max, 3)
	npv_bl_L1 <- NPV(0.05, antivirus_bl_license*1+costsFor1_virusbl_install, antivirus_bl_update*1+costsFor1_virusbl_yearly, ALE_virus_min, ALE_blSecurity_min, 3)

	npv_firewall_H_s <- NPV(0.05, firewall_s_install+firewall_s_install_mp, firewall_s_install_mp, ALE_hack_max, ALE_fwSecurity_max, 3)
	npv_firewall_L_s <- NPV(0.05, firewall_s_install+firewall_s_install_mp, firewall_s_install_mp, ALE_hack_min, ALE_fwSecurity_min, 3)
	npv_firewall_H_l <- NPV(0.05, firewall_l_install+firewall_l_install_mp, firewall_l_install_mp, ALE_hack_max, ALE_fwSecurity_max, 3)
	npv_firewall_L_l <- NPV(0.05, firewall_l_install+firewall_l_install_mp, firewall_l_install_mp, ALE_hack_min, ALE_fwSecurity_min, 3)

	npv_ifirewall_H_s <- NPV(0.05, firewall_s_install, 0, ALE_hack_max, ALE_ifwSecurity_max, 3)
	npv_ifirewall_L_s <- NPV(0.05, firewall_s_install, 0, ALE_hack_min, ALE_ifwSecurity_min, 3)
	npv_ifirewall_H_l <- NPV(0.05, firewall_l_install, 0, ALE_hack_max, ALE_ifwSecurity_max, 3)
	npv_ifirewall_L_l <- NPV(0.05, firewall_l_install, 0, ALE_hack_min, ALE_ifwSecurity_min, 3)


	# collate NPV values into labeled lists
	vals <- c("AV-249","AV-49","AV-9","AV-1","BL-1","FW-S","FW-L","iFW-S","iFW-L")
	v <- seq(1,9)
	#names <- c("AV-H-249","AV-L-249","AV-H-1","AV-L-1","BL-H-249","BL-L-249","BL-H-1","BL-L-1","FW-H-S","FW-L-S","FW-H-L","FW-L-L","iFW-H-S","iFW-L-S","iFW-H-L","iFW-L-L");

	npvs <- c(npv_antivirus_H249,npv_antivirus_L249,npv_antivirus_H49,npv_antivirus_L49,npv_antivirus_H9,npv_antivirus_L9,npv_antivirus_H1,npv_antivirus_L1,npv_bl_H1,npv_bl_L1,npv_firewall_H_s,npv_firewall_L_s,npv_firewall_H_l,npv_firewall_L_l,npv_ifirewall_H_s,npv_ifirewall_L_s,npv_ifirewall_H_l,npv_ifirewall_L_l)
#npv_bl_H249,npv_bl_L249,

	npvs_h <- c(npv_antivirus_H249,npv_antivirus_H49,npv_antivirus_H9,npv_antivirus_H1,npv_bl_H1,npv_firewall_H_s,npv_firewall_H_l,npv_ifirewall_H_s,npv_ifirewall_H_l)
#npv_bl_H249,

	npvs_l <- c(npv_antivirus_L249,npv_antivirus_L49,npv_antivirus_L9,npv_antivirus_L1,npv_bl_L1,npv_firewall_L_s,npv_firewall_L_l,npv_ifirewall_L_s,npv_ifirewall_L_l)
#npv_bl_L249,

	#====================================================
	# Draw and write graph 
	#
	# Prepare output file
	tiff("NPV-Controls-HD.tiff", width = 8, height = 8, units = 'in', res = 600)

	# Set range
	g_range <- range(0, npvs)

	# Draw graph
	plot(npvs_h, 
    	 #main= "Net Present Value of Security Controls",
     	xlab= "Control", ylab= "NPV (£)", col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2, ylim=g_range, axes=FALSE)
	#g_range <- range(0, npvs)
	abline(0, 0, col = "black")
	axis(1, at=v, lab=vals)
	axis(2, las=1, at=5000*-8000:g_range[2])
	points(npvs_l)
	text(npvs_h, vals, cex= 0.7, pos=3)

	# End drawing and write the file
	dev.off()
#npv_ifw_H249 <- NPV(0.05, swfirewall_license*249+costsFor249_swfirewall_install, antivirus_bl_update*249+costsFor249_virusbl_yearly, ALE_virus_max, ALE_blSecurity_max, 3)
#npv_ifw_L249 <- NPV(0.05, swfirewall_license*249+costsFor249_swfirewall_install, antivirus_bl_update*249+costsFor249_virusbl_yearly, ALE_virus_min, ALE_blSecurity_min, 3)
#npv_ifw_H1 <- NPV(0.05, swfirewall_license*1+costsFor1_swfirewall_install, antivirus_bl_update*1+costsFor1_virusbl_yearly, ALE_virus_max, ALE_blSecurity_max, 3)
#npv_ifw_L1 <- NPV(0.05, swfirewall_license*1+costsFor1_swfirewall_install, antivirus_bl_update*1+costsFor1_virusbl_yearly, ALE_virus_min, ALE_blSecurity_min, 3)
}


