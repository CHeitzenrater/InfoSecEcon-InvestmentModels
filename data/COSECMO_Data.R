#############################################################################
##
## COSECMO cost data 
## Sources for each listed in context
##
#############################################################################

####
## COSECMO costs from GAO Report
## [1] Yang, Y. and Du, J. and Wang, Q.  "Shaping the effort of developing secure 
##  software" in the 2015 Conference on Systems Engineering Research.  Elsevier 
##  Open Access Article
##
## EAL					1	 2	3			4		 5		  6			7
## Effort Relative		N/A	 1	1.2-3.0		1.7-4.0	 N/A		  N/A		N/A
## COSECMO				1	 1	1.2-1.8		1.5-3.0	 2.25-6.0 4.13-13.5 8.8-32.25

## Not including "0" (one to typically use)
COSECMO_High = c(1,1,1.8,3.0,6.0,13.5,32.25)
COSECMO_Low = c(1,1,1.2,1.5,2.25,4.13,8.81)

## Incuding "0" (included for calculations that must start at origin)
COSECMO_High_0 = c(0,1,1,1.8,3.0,6.0,13.5,32.25)
COSECMO_Low_0 = c(0,1,1,1.2,1.5,2.25,4.13,8.81)


######################## 
## Cost factors calculated for given code size (from report)
## [2] Colbert, E. and Boehm, B.  "Cost estimation for secure software \& 
##  systems" in ISPA / SECA 2008 Joint International Conference, Huis ter Duin, 
##  Noordwijk, The Netherlands, 12-14 May 2008
##

## Data as presented directly
COSECMO_5KLOC = c(1,1,1.2,1.5,2.25,4.13,8.81)
COSECMO_20KLOC = c(1,1,1.4,2.0,3.5,7.25,16.63)
COSECMO_100KLOC = c(1,1,1.6,2.5,4.75,10.38,24.44)
COSECMO_1MLOC = c(1,1,1.8,3.0,6.0,13.5,32.25)

## "_0" version (included for calculations that must start at origin)
COSECMO_5KLOC_0 = c(0,1,1,1.2,1.5,2.25,4.13,8.81)
COSECMO_20KLOC_0 = c(0,1,1,1.4,2.0,3.5,7.25,16.63)
COSECMO_100KLOC_0 = c(0,1,1,1.6,2.5,4.75,10.38,24.44)
COSECMO_1MLOC_0 = c(0,1,1,1.8,3.0,6.0,13.5,32.25)

