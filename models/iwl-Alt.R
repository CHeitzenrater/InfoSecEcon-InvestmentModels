################################################################################
## 
## Version of the IWL code augmented for use in the IWL-SSE model.
## 
## This code was provided by Rainer Böhme, Feb 2016.
## Used with permission. Inclusion in this repository by permission of the 
##  author (email to C. Heitzenrater, 27 April 2017)
##
################################################################################


################################################################
#	ORIGINAL IWL CODE --- presented as supplied
################################################################

# --------------- Static analysis ---------------  
static.exp.q <- function(n=20,a=1000,z=.02,x0=0,dx=3,sigma=0) {
	1-rev(cumprod(1-c(pnorm(z*a,n:0*dx+x0,sigma/dx))))
}

static.revenue <- function(n=20,a=1000,r=.05,z=.02,x0=0,dx=3,sigma=0,rho=.2,cp=0) {
	k <- 0:n
	rev <- a*(r-z*static.exp.q(n=n,a=a,z=z,x0=x0,dx=dx,sigma=sigma))-k-rho/2*(k^2-k)-cp
	list(revenue=rev,k=which.max(rev)-1)
}

examineCpStatic <- function( sigma=0 ){
	for( i in 0:50 ){
		vals <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=sigma,cp=i)
		plot(NA,NA,las=1, 
			xlab="number of defenses in place (k)",
			ylab=paste("expected return, cp = ",i),
			ylim=range(-10:50),xlim=c(0,25))	
		rect(-80,-80,30,0,col=gray(.85),lty=3)
	
		points(vals$k,vals$rev[vals$k+1],pch=16,cex=2,col="yellow")
		lines(0:25,vals$rev,type="b",col="black",pch=16)
		Sys.sleep(1)
	}
}


# --------------- Dynamic analysis ---------------  
prob.tatt <- function(n=1:25,x0=15,dx=1,sigma=1,a=1000,z=.025)
{
	# calculate distribution of t.att	
	p <- pnorm(z*a,n*dx+x0,sigma/dx)
	mu <- sum(p)
	sigma <- sqrt(sum(p*(1-p)))
	
	res <- pnorm((0:length(n))+.5,mu,sigma)-pnorm((0:length(n))-.5,mu,sigma)
	names(res) <- 0:length(n)
	res 
}

def.cost <- function(k,rho=.1){
	rho/2*k^2 + (1-rho/2)*k
}

################################################################
#	UPDATES TO IWL CODE
################################################################

####
## Dynamic analysis code
## Added cp (cost of software process) as an argument; added the reduction by 
##  cp to the outcome (CDH, Feb 2016)
##
dyn.outcome <- function(tatt,tmax,k1,rho=.1,a=1000,z=.025,r=.05,lambda=0, cp=0){
	a*lambda+tatt*a*(r-z-lambda) + (tmax-tatt)*(a*r-def.cost(k1+tatt,rho=rho)) - sum(def.cost(k1:(k1+tatt),rho=rho))-cp
}


####
## Added cp as an argument (CDH, Feb 2016)
##
dyn.expected.outcome <- function(k1=0,tmax=25,a=1000,r=.05,z=.025,rho=.1,x0=15,dx=1,sigma=1,n=25,lambda=0,cp=0){
	p <- prob.tatt(n=k1:n,x0=x0,dx=dx,sigma=sigma,a=a,z=z)
	o <- sapply(as.numeric(names(p)),dyn.outcome,tmax=tmax,k1=k1,rho=rho,a=a,z=z,r=r,lambda=lambda,cp=cp)
	sum(p*o)
}


####
## Added cp as an argument (CDH, Feb 2016)
##
dynamic.revenue <- function(tmax=25,a=1000,r=.05,z=.025,rho=.1,x0=15,dx=1,sigma=1,n=25,lambda=0,cp=0){
	res.dyn <- sapply(0:n,dyn.expected.outcome,tmax=tmax,a=a,r=r,z=z,rho=rho,x0=x0,dx=dx,sigma=sigma,n=n,lambda=lambda,cp=cp)
	res.stat <- static.revenue(n=n,a=a,z=z,r=r,rho=rho,dx=dx,x0=x0,sigma=sigma,cp=cp)$rev

	# dynamic or static solution, whatever is higher
	res <- pmax(res.stat,res.dyn/tmax)
	which <- apply(cbind(res.stat,res.dyn/tmax),1,which.max)

	return(list(rev=res,k=which.max(res)-1,which=which)) 
	
	### DEBUG
	#message("res=",res," k=",k=which.max(res)-1," which=",which=which)
}


