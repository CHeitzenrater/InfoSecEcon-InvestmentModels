#############################################################################
#
# Data used for the analysis in the WEIS 2015 and Journal of Cybersecurity
#
#############################################################################
source("./models/InfoEconFunctions.R", local=TRUE)

#====================================================
#
# UK cost of time
#
#====================================================
min_wage <- 6.50
c_hour_minw <- min_wage
c_minute_minw <- min_wage/60
c_sec_minw <- c_minute_minw/60

# number of working days in a year, UK
# 253 in 2015, according to http://www.work-day.co.uk/#showcalendar
UK_working_days <- 253



#====================================================
#
# 2014 BIS Information Security Breaches Report
#
#====================================================

#Respondants by size
total_resp <- 1098 
bus_14_500plus <- 0.43
bus_14_250_499 <- 0.09
bus_14_50_249 <- 0.18
bus_14_10_49 <- 0.13
bus_14_less_10 <- 0.17

#respondants categorized into large (LB) and small businesses (SB)
bus_14_SB <- bus_14_50_249 + bus_14_10_49 + bus_14_less_10
bus_14_LB <- bus_14_250_499 + bus_14_500plus
num_bus_14_SB <- bus_14_SB * total_resp
num_bus_14_LB <- bus_14_LB * total_resp

#Upper and lower bounds of size for large and small business
num_emp_14_SB_ub <- bus_14_less_10*9 + bus_14_10_49*49 + bus_14_50_249*249
num_emp_14_SB_lb <- bus_14_less_10*1 + bus_14_10_49*10 + bus_14_50_249*50
num_emp_14_LB_ub <- bus_14_250_499*499 + bus_14_500plus*5000
num_emp_14_LB_lb <- bus_14_250_499*250 + bus_14_500plus*500


#====================================================
# probability of incident, from page 10 - Security Breaches

# Malicious incident
# Large Business (LB)
p_malicious_LB_14 <- 0.81
p_malicious_LB_13 <- 0.86
# Small Business (SB)
p_malicious_SB_15 <- 0.75 #JoC -- from 2015 report
p_malicious_SB_14 <- 0.60 # from 2014 report
p_malicious_SB_13 <- 0.76 # from 2013 report; 0.64 in the 2014 report
p_malicious_SB_12 <- 0.70 # from 2012 report; same in 2014 report
p_malicious_SB_10 <- 0.74
p_malicious_SB <- c(p_malicious_SB_12,p_malicious_SB_13,p_malicious_SB_14)
#p_malicious_SB <- c(p_malicious_SB_12,p_malicious_SB_13,p_malicious_SB_14,p_malicious_SB_15) #JoC

# Serious incident
# Large Business (LB)
p_serious_LB_14 <- 0.66
p_serious_LB_13 <- 0.62
# Small Business (SB)
p_serious_SB_15 <- 0.25 # JoC -- from 2015 report
p_serious_SB_14 <- 0.50 # from 2014 report
p_serious_SB_13 <- 0.32 # from 2013 report; 0.23 in the 2014 report
p_serious_SB_12 <- 0.38 # from 2012 report


#====================================================
# median events per year (ISBS page 11) *for the affected companies*
mepy_SB_virus_15 <- 2		#JoC
mepy_SB_theft_15 <- 2		#JoC
mepy_SB_staff_15 <- 2		#JoC
mepy_SB_outsider_15 <- 3	#JoC
mepy_SB_any_15 <- 4			#JoC
mepy_LB_virus_14 <- 5
mepy_LB_virus_13 <- 3
mepy_SB_virus_14 <- 3
mepy_SB_virus_13 <- 3
mepy_LB_theft_14 <- 3
mepy_LB_theft_13 <- 5
mepy_SB_theft_14 <- 1
mepy_SB_theft_13 <- 2
mepy_LB_staff_14 <- 6
mepy_LB_staff_13 <- 10
mepy_SB_staff_14 <- 3
mepy_SB_staff_13 <- 6
mepy_LB_outsider_14 <- 11
mepy_LB_outsider_13 <- 10
mepy_SB_outsider_14 <- 5
mepy_SB_outsider_13 <- 5
mepy_LB_any_14 <- 16
mepy_LB_any_13 <- 21
mepy_SB_any_14 <- 6
mepy_SB_any_13 <- 10


#====================================================
# Breach type data (from ISBS page 11)

# Breaches by virus
# Large Business (LB)
bt_LB_virus_14 <- 0.73
bt_LB_virus_13 <- 0.59
# Small Business (SB)
bt_SB_virus_15 <- 0.63	#JoC
bt_SB_virus_14 <- 0.45
bt_SB_virus_13 <- 0.41
bt_SB_virus_12 <- 0.40

# Breaches by theft
# Large Business (LB)
bt_LB_theft_14 <- 0.44
bt_LB_theft_13 <- 0.47
# Small Business (SB)
bt_SB_theft_15 <- 0.06	#JoC
bt_SB_theft_14 <- 0.10
bt_SB_theft_13 <- 0.16
bt_SB_theft_12 <- 0.12

# Breaches by staff action
# Large Business (LB)
bt_LB_staff_14 <- 0.58
bt_LB_staff_13 <- 0.73
# Small Business (SB)
bt_SB_staff_15 <- 0.27	#JoC
bt_SB_staff_14 <- 0.22
bt_SB_staff_13 <- 0.41
bt_SB_staff_12 <- 0.45

# Breaches by an outside hacker
# Large Business (LB)
bt_LB_outHack_14 <- 0.55
bt_LB_outHack_13 <- 0.66
# Small Business (SB)
bt_SB_outHack_15 <- 0.35	#JoC
bt_SB_outHack_14 <- 0.33
bt_SB_outHack_13 <- 0.43
bt_SB_outHack_12 <- 0.41



#====================================================
#
# Worst Breach Information
#
#====================================================

#====================================================
# detection method of worst breach (ISBS page 10)
# Routine internal security monitoring detected 29% of the worst breaches
worst_breach_detect_security_14 <- 0.29
worst_breach_detect_impact_14 <- 0.21
worst_breach_detect_accident_14 <- 0.10
worst_breach_detect_accident_13 <- 0.06

#====================================================
# time that the worst incident was found (ISBS page 10)
worst_breach_detect_day_14 <- 0.29
worst_breach_detect_weeks_14 <- 0.06
worst_breach_detect_weeks_13 <- 0.05
worst_breach_detect_month_14 <- 0.14
worst_breach_detect_month_13 <- 0.09

#====================================================
# type of worst incident

# 2015, Small Business (ISBS page 19 of 2015 report)
worst_SB_fraud_15 <- 0.00
worst_SB_virus_15 <- 0.10
worst_SB_legal_15 <- 0.20
worst_SB_theft_15 <- 0.00
worst_SB_misuse_15 <- 0.00
worst_SB_failure_15 <- 0.10
worst_SB_disclosure_15 <- 0.00
worst_SB_outsider_15 <- 0.40
worst_SB_compromiseWAccess_15 <- 0.20

# 2014, Small Business (ISBS page 12 of 2014 report)
worst_SB_fraud_14 <- 0.04
worst_SB_virus_14 <- 0.31
worst_SB_legal_14 <- 0.04
worst_SB_theft_14 <- 0.00
worst_SB_misuse_14 <- 0.12
worst_SB_failure_14 <- 0.07
worst_SB_disclosure_14 <- 0.19
worst_SB_outsider_14 <- 0.23

# 2013, Small Business (ISBS page 12 of 2013 report)
worst_SB_fraud_13 <- 0.03
worst_SB_theft_13 <- 0.04
worst_SB_legal_13 <- 0.04
worst_SB_failure_13 <- 0.23
worst_SB_outsider_13 <- 0.18
worst_SB_virus_13 <- 0.14
worst_SB_misuse_13 <- 0.12
worst_SB_disclosure_13 <- 0.10
worst_SB_other_13 <- 0.12

# 2012, Small Business (ISBS page 12 of 2012 report)
worst_SB_failure_12 <- 0.34
worst_SB_virus_12 <- 0.33
worst_SB_fraud_12 <- 0.01
worst_SB_theft_12 <- 0.05
worst_SB_disclosure_12 <- 0.02
worst_SB_misuse_12 <- 0.15
worst_SB_legal_12 <- 0.01
worst_SB_outsider_12 <- 0.9

#====================================================
# Total cost of worst incident (ISBS page 18)
# broken into high (H) and low (L) by year
tcwi_H_SB_15 <- 310800	#JoC
tcwi_L_SB_15 <- 75200	#JoC

tcwi_H_SB_14 <- 115000
tcwi_L_SB_14 <- 65000

tcwi_H_SB_13 <- 65000
tcwi_L_SB_13 <- 35000

tcwi_H_SB_12 <- 30000
tcwi_L_SB_12 <- 15000

tcwi_H_SB_10 <- 55000
tcwi_L_SB_10 <- 27500

tcwi_H_LB_14 <- 1150000
tcwi_L_LB_14 <- 600000

tcwi_H_LB_13 <- 850000
tcwi_L_LB_13 <- 450000

tcwi_H_LB_12 <- 250000
tcwi_L_LB_12 <- 110000

tcwi_H_LB_10 <- 690000
tcwi_L_LB_10 <- 280000

# Calculate the total cost of worst incident for high (H) and low (L) 
#twci_H_SB <- c(tcwi_H_SB_12,tcwi_H_SB_13,tcwi_H_SB_14,tcwi_H_SB_15) #JoC
#twci_L_SB <- c(tcwi_L_SB_12,tcwi_L_SB_13,tcwi_L_SB_14,tcwi_L_SB_15) #JoC

twci_H_SB <- c(tcwi_H_SB_12,tcwi_H_SB_13,tcwi_H_SB_14)
twci_L_SB <- c(tcwi_L_SB_12,tcwi_L_SB_13,tcwi_L_SB_14)

twci_H_SB_13_14 <- c(tcwi_H_SB_13,tcwi_H_SB_14)
twci_L_SB_13_14 <- c(tcwi_L_SB_13,tcwi_L_SB_14)



#====================================================
#
# Calculations from BIS ISBS
#
#====================================================

#====================================================
# Annual loss expectancy - ALE
# p      : Probability of the event occurring
# lambda : Fixed loss amount incurred with probability p_s = L_s(lambda).  
#          With probability 1-p_s = L_s(0), no loss is suffered
# ALE <- function( p, lambda ){ (p * lambda + (1 - p) * 0); }

# Overall ALE by year, overall malicious (m)
# Small Business (SB)
Ale_SBm_H_15 = ALE(p_malicious_SB_15,tcwi_H_SB_15)	#JoC
Ale_SBm_L_15 = ALE(p_malicious_SB_15,tcwi_L_SB_15) 	#JoC
Ale_SBm_H_14 = ALE(p_malicious_SB_14,tcwi_H_SB_14)
Ale_SBm_L_14 = ALE(p_malicious_SB_14,tcwi_L_SB_14)
Ale_SBm_H_13 = ALE(p_malicious_SB_13,tcwi_H_SB_13)
Ale_SBm_L_13 = ALE(p_malicious_SB_13,tcwi_L_SB_13)
Ale_SBm_H_12 = ALE(p_malicious_SB_12,tcwi_H_SB_12)
Ale_SBm_L_12 = ALE(p_malicious_SB_12,tcwi_L_SB_12)
Ale_SBm_H_10 = ALE(p_malicious_SB_10,tcwi_H_SB_10)
Ale_SBm_L_10 = ALE(p_malicious_SB_10,tcwi_L_SB_10)
# Large Business (LB)
Ale_LBm_H_14 = ALE(p_malicious_LB_14,tcwi_H_LB_14)
Ale_LBm_L_14 = ALE(p_malicious_LB_14,tcwi_L_LB_14)
Ale_LBm_H_13 = ALE(p_malicious_LB_13,tcwi_H_LB_13)
Ale_LBm_L_13 = ALE(p_malicious_LB_13,tcwi_L_LB_13)



#====================================================
# Net Present Value - NPV --- UNUSED
#npv_SB_L_14 <- NPV(0.05, 3000, 300, Ale_SB_L_14, Ale_SB_L_14/10, 5 )
#npv_SB_H_14 <- NPV(0.05, 3000, 300, Ale_SB_H_14, Ale_SB_H_14/10, 5 )
#npv_SB_L_13 <- NPV(0.05, 3000, 300, Ale_SB_L_13, Ale_SB_L_13/10, 5 )
#npv_SB_H_13 <- NPV(0.05, 3000, 300, Ale_SB_H_13, Ale_SB_H_13/10, 5 )
#npv_SB_H_12 <- NPV(0.05, 3000, 300, Ale_SB_H_12, Ale_SB_H_12/10, 5 )
#npv_SB_L_12 <- NPV(0.05, 3000, 300, Ale_SB_L_12, Ale_SB_L_12/10, 5 )
#npv_SB_H_10 <- NPV(0.05, 3000, 300, Ale_SB_H_10, Ale_SB_H_10/10, 5 )
#npv_SB_L_10 <- NPV(0.05, 3000, 300, Ale_SB_L_10, Ale_SB_H_10/10, 5 )
#high <- c(npv_SB_H_10, npv_SB_H_12, npv_SB_H_13, npv_SB_H_14)
#low  <- c(npv_SB_L_10, npv_SB_L_12, npv_SB_L_13, npv_SB_L_14)




