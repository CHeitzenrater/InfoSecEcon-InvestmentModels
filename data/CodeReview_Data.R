#############################################################################
##
## Cost-effectiveness analysis of code review data.
## Sources are listed in context. 
##
#############################################################################


################################################################################
## Labour rate data
################################################################################
## Staff costs for software in \pounds
## Source: www.payscale.com, accessed 28 January 2017.
##  Hourly rates based on an average UK work year of 1,674 hours: 
##  https://stats.oecd.org/Index.aspx?DataSetCode=ANHRS, accessed 8 Feb 2017.  
## IT Security Architect: 
##		Yearly: \pounds59,820 (\pounds40,508 -- \pounds87,332)
##		Hourly: \pounds24.20--\pounds52.17, average \pounds35.73
## Software Architect: 
##		Yearly: \pounds53,246 (\pounds39,444 -- \pounds84,885)
##		Hourly: \pounds23.56--\pounds50.71, average \pounds31.81
## Security Consultant (Computing / Networking / Information Technology): 
##		Yearly: \pounds48,557 (\pounds27,790 -- \pounds76,819)
##		Hourly: \pounds16.60--\pounds45.89, average \pounds29.01
## Software Developer: 
##		Yearly: \pounds30,727 (\pounds21,372 -- \pounds49,696) 
##		Hourly: \pounds12.77--\pounds29.69, average \pounds18.36
## Test/Quality Assurance (QA) Engineer (Computer Software):
##		Yearly: \pounds26,974 (\pounds17,937 -- \pounds41,044)
##		Hourly: \pounds10.72--\pounds24.52, average \pounds16.11
##
ITSecArch_CostPerH = 35.73
SoftArch_CostPerH = 31.81
SecConsult_CostPerH = 29.01
SoftDevel_CostPerH = 18.36
SoftDevel_CostPerH = 16.11


################################################################################
## General Code Review Data
################################################################################
## "Software Defect Top 10"
## - 100x increase in cost between reqt/design and ops
## - Reviews, analysis tools, and testing catch different classes of defect
## - All else being equal, high-dependabiity SW costs 53% more

## "Building Real Software"
## - 85% of bugs in design/devel caught before release
## - 7-20% of fixes result in new bugs


################################################################################
## Manual Code Review Data
################################################################################

#############################################################################
## [MAN 1]: Kemerer, C.~F. and Paulk, M.~C. "The impact of design and code 
## reviews on software quality: An empirical study" (self published??)
##
## Notes:
##  - 52-110 defects/KLOC
##  - Optimal of 4 reviewers
##  - Inspections should not be > 2h
##
## 200 LOC per hour is deemed effective
##
MAN1_Tool = c("Manual Review")
MAN1_Subjects = c("")
MAN1_LOC = c(200) #range of 100-200 LOC/hour prep, 125-150 review
MAN1_TP = c()
MAN1_FP = c()
MAN1_FN = c()
MAN1_KnownVuln = c()
MAN1_time = c()
MAN1_lang = c()
# MAN1_vulnHour = c()
MAN1_eff_High = c(0.90)
MAN1_eff_Low = c(0.30)
MAN1_eff_Median = c(0.60) 

#############################################################################
## [MAN 2]: Edumndson, A., Holtkamp, B., Rivera, E., Finifter, M., Mettler, A. 
##  and Wagner, D.  "An empirical study on the effectiveness of security code 
##  review" 
##
## Notes:
##  - 30 participants total in study
##  - used oDesk to farm out work
##  - None of the reviewers found > 5 of the 7 known vuln; 20% found 0
##  - 12 hours were calculated using OWASP recommendation of 250 LOC
##  - LOOK AT PLOT
##
MAN2_Tool = c("Manual Review")
MAN2_Subjects = c("Anchor CMS, v0.6")
MAN2_LOC = c(3500)
MAN2_TotalReports = c(6.29) #average, std dev of 5.87
MAN2_TotalSecReports = c(6.29) #from above
MAN2_KnownVuln = c(7) #p.8
MAN2_TP = c(2.33) #average, std 1.67
MAN2_FP = c(3.96) 
MAN2_FP_rate = c(0.630) # FP/TotalSecReports
MAN2_FN = c()
MAN2_time = c(12)
MAN2_lang = c("PHP+JavaScript")
# MAN2_vulnHour = c()
MAN2_eff_High = c(0.73) # This is from table 1, but is not quite accurate
MAN2_eff_Low = c(0.17) # This is from table 1, but is not quite accurate
MAN2_eff_Median = c(0.30) # This is from table 1, but is not quite accurate
MAN2_eff_Ave = 0.334


################################################################################
## Static Analysis Data
##
## "Static code analysis to detect software security vulnerabilities..."
## - people with security and SAT background successfully classify 67%; 
##   no one does much better
################################################################################

#############################################################################
## [SAT 1]: Austin, A., Holmgreen, C. and Williams, L. "A comparison of the
##  efficiency and effectiveness of vulnerability discovery techniques"  
##  Information and Software Technology, Dec 2012
## NOTES:
## - SAT tool employed is Fortify 360 v. 2.6
## - All studied items were medical health record systems
##
SAT1_Subjects = c("Tolven eCHR","OpenEMR","PatientOS")
SAT1_LOC = c(466538,277702,487437)
#SAT1_KnownVuln = c()
SAT1_lang = c("Java","PHP","Java")

SAT1_Tool = "Fortify 360" #HP
SAT1_TotalReports = c(3765,5036,12333)
SAT1_TotalSecReports = c(2315,5036,1643)
SAT1_TP = c(50,1321,145)
SAT1_FP = c(2265,3715,1644)
SAT1_FP_rate = c(0.98,0.74,0.92)
#SAT1_FN = c()
SAT1_time = c(18,40,13) #just in classification
SAT1_vulnHour = c(2.78,32.40,11.15)
# SAT1_eff_High = c()
# SAT1_eff_Low = c()
# SAT1_eff_Median = c()
#SAT1_eff_Ave =

#############################################################################
## [SAT 2]: Baca, D., Carlsson, B. and Lundberg, L.  "Evaluating the Cost 
## Reduction of Static Code Analysis for Software Security", PLAS June 2008
##
## Notes:
##  - 2.6 more vuln by SAT (total of 59)
##  - lowered maintenance costs 18%
##  - effectiveness: E = b / (b+c), where b = TR found by SAT, 
##    c = TR not found by SAT
##  - for all tools, 34 warnings can be examined per hour
##  - found new vul per: A) 12,500 LOC, b) 42,850 LOC, c) 16,700
##
SAT2_Subjects = c("A","B","C")
SAT2_LOC = c(600000,300000,50000)
SAT2_KnownVuln = c(8,7,8)
SAT2_lang = c("C++","C++","C++")

SAT2_Tool = "Coverity Prevent"
SAT2_TotalReports = c(1680,132,33) 
SAT2_TotalSecReports = c(419,14,5) #With code quality: 456, 23, 11
SAT2_TP = c(48,7,3) #With code quality: 85,16,9
SAT2_FP = c(371,7,2)
SAT2_FP_rate = c(0.221,0.053,0.06)
SAT2_FN = c(5,5,6) #
SAT2_time = c()

# SAT2_vulnHour = c(3.58,10.29,9.38)
# SAT2_eff_High = c()
# SAT2_eff_Low = c()
SAT2_eff_Median = c(0.375, 0.286, 0.25)
SAT2_eff_Ave = 0.304

#############################################################################
## [SAT 3]: 
##
## Notes:
##  - 
##
# SAT3_Subjects = c()
# SAT3_LOC = c()
# SAT3_KnownVuln = c()
# SAT3_lang = c()

# SAT3_Tool = ""
# SAT3_TotalReports = c() 
# SAT3_TotalSecReports = c() 
# SAT3_TP = c() 
# SAT3_FP = c()
# SAT3_FP_rate = c()
# SAT3_FN = c() #
# SAT3_time = c()

# SAT3_vulnHour = c()
# SAT3_eff_High = c()
# SAT3_eff_Low = c()
# SAT3_eff_Median = c()
# SAT3_eff_Ave = 

#############################################################################
## [SAT 4]: Goseva-Popstojanova, K. and Perhinschi, A. "On the capability of 
##  static code analysis to detect security vulnerabilities", Journal of
##  Information and Software Technology, Aug 2015
##
## Notes:
##  - 
##
SAT4_Subjects = c("GZIP 1.3.5","Dovecoat 1.2.0","Tomcat 5.5.13")
SAT4_LOC = c(8500,280000,4800000)
SAT4_KnownVuln = c(4,8,32)
SAT4_lang = c("C", "C", "Java")

## Data primarily from table 7 of paper
SAT4_Tool_A = "Tool A"
SAT4_A_TotalReports = c(112,8263,12399) 
SAT4_A_TotalSecReports = c(112,8263,12399) 
SAT4_A_TP = c(1,0,7)
SAT4_A_FP = c()
SAT4_A FP_rate = c()
SAT4_A_FN = c(3,8,25)
SAT4_A_time = c()

SAT4_Tool_B = "Tool B"
SAT4_B_TotalReports = c(36,538,12904) 
SAT4_B_TotalSecReports = c(36,538,12904) 
SAT4_B_TP = c(0,0,3) 
SAT4_B_FP = c()
SAT4_B FP_rate = c()
SAT4_B_FN = c(4,8,29) 
SAT4_B_time = c()

SAT4_Tool_C = "Tool C"
SAT4_C_TotalReports = c(119,1356,20608) 
SAT4_C_TotalSecReports = c(119,1356,20608) 
SAT4_C_TP = c(1,0,5) 
SAT4_C_FP = c()
SAT4_C FP_rate = c()
SAT4_C_FN = c(3,8,27) 
SAT4_C_time = c()

# SAT4_vulnHour = c()
SAT4_eff_High = c(0.25,0.21)
SAT4_eff_Low = c(0,0.09)
SAT4_eff_Median = c(0.125,0.15) # calculated


