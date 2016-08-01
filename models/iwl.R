# Sep 28, 2008 - Rainer Böhme

### Commented out for general usability --- CDH 2017.08.01
#setwd("/Users/rb21/Documents/Projekte/WEIS2009/IWL/R")
#setwd("/home/tmoore/pub/fc2009/R")

plot.attack.cost <- function(sigma,n=8)
{
	set.seed(123)
	xs <- 1:n
	x <- pmax(0,xs+rnorm(n,0,sigma))
	plot(NA,NA,axes=FALSE,xlim=c(.5,n+.5),ylim=c(.5,n+.5),
			ylab="true cost of attack",
			xlab="expected cost of attack")
	points(xs,xs,pch=16,col="gray",cex=.6)
	
	for (i in (1:n)[abs(xs-x)>.2]) 
			lines(c(i,i),c(xs[i],x[i]),col="gray",lty=2)

	points(xs,x)
	text(xs,x,1:n,pos=c(1,3)[(xs<=x)+1],cex=.8,xpd=NA)
	
	arrows(.25,-1,n+.5,-1,xpd=NA)
	arrows(-1,0.25,-1,n+.5,xpd=NA)
}


pdf("../fig/fig-attack-cost-0.pdf",4,4,pointsize=12)
par(mar=c(4,4,1,1))
plot.attack.cost(0)
dev.off()

pdf("../fig/fig-attack-cost-1.pdf",4,4,pointsize=12)
par(mar=c(4,4,1,1))
plot.attack.cost(1)
dev.off()

plot.emv.cost <- function(n=8)
{

	xs <- 1:n
	x <- c(1,1.8,3,4.8,3.6,3.8,6.5,5.4)
	plot(NA,NA,axes=FALSE,xlim=c(.5,n+.5),ylim=c(.5,n+.5),
			ylab="true cost of attack",
			xlab="expected cost of attack")
	points(xs,xs,pch=16,col="gray",cex=.6)
	
	for (i in (1:n)[abs(xs-x)>.2]) 
			lines(c(i,i),c(xs[i],x[i]),col="gray",lty=2)

	points(xs,x)
	text(xs,x,c('F2F\nretail','L&S\nUK','ATM\nUK','L&S\nabroad','ATM\nabroad','CNP','PED\ntap','PED\nrelay'),pos=c(1,3)[(xs<=x)+1],cex=.8,xpd=NA)
	
	arrows(.25,-1,n+.5,-1,xpd=NA)
	arrows(-1,0.25,-1,n+.5,xpd=NA)
}


pdf("../fig/fig-emv.pdf",4,4,pointsize=12)
par(mar=c(4,4,1,1))
plot.emv.cost()
dev.off()

plot.phish.cost <- function(n=8)
{

	xs <- 1:n
	x <- c(1,1.8,5,3.5,3.2,6,7,8)
	plot(NA,NA,axes=FALSE,xlim=c(.5,8.5),ylim=c(.5,8.5),
			ylab="true cost of attack",
			xlab="expected cost of attack")
	points(xs,xs,pch=16,col="gray",cex=.6)
	
	for (i in (1:n)[abs(xs-x)>.2]) 
			lines(c(i,i),c(xs[i],x[i]),col="gray",lty=2)

	points(xs,x)
	text(xs,x,c('.cn','.hk','.py','.tk','.at','.se','.edu','.gov'),
			pos=c(1,3)[(xs<=x)+1],cex=.8,xpd=NA)
	
	arrows(.25,-1,n+.5,-1,xpd=NA)
	arrows(-1,0.25,-1,n+.5,xpd=NA)
}


pdf("../fig/fig-phish.pdf",4,4,pointsize=12)
par(mar=c(4,4,1,1))
plot.phish.cost()
dev.off()


# ------------------------------------------


stay.in.business <- function(a=1000,r=.05,z=.02,rho=.2,x0=0,dx=3)
{
	cat("profit of successful attack =",z*a,"\n")

	k <- ceiling((z*a-x0-dx)/dx)
	cat("level of defense k =",k,"\n")
	
	c <- k+rho/2*(k^2-k)
	cat("cost of defense c =",c,"\n")

	cat("return w/o cost =",a*r,"\n")
	
	rev <- a*r-c
	cat("net return w/ defense =",rev,"\n")

	cat("net return w/o defense =",a*(r-z),"\n")

}

stay.in.business()
stay.in.business(rho=0)

# --------------- Static analysis ---------------  

static.exp.q <- function(n=20,a=1000,z=.02,x0=0,dx=3,sigma=0)
{
	1-rev(cumprod(1-c(pnorm(z*a,n:0*dx+x0,sigma/dx))))
}

static.revenue <- function(n=20,a=1000,r=.05,z=.02,x0=0,dx=3,sigma=0,rho=.2)
{
	k <- 0:n
	rev <- a*(r-z*static.exp.q(n=n,a=a,z=z,x0=x0,dx=dx,sigma=sigma))-k-rho/2*(k^2-k)
	list(revenue=rev,k=which.max(rev)-1)
}

plot(static.revenue(n=20,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=0)$rev)


# --------------- Figure for static analysis part  --------------------

pdf("../fig/fig-static-analysis.pdf",6,4,pointsize=12)
par(mar=c(4,4,1,1))

s0 <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=0)
s1 <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=1)
s2 <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=2)
s3 <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=4)
s4 <- static.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=8)

plot(NA,NA,las=1,
	ylim=range(c(s0$rev,s1$rev,s2$rev,s3$rev,s4$rev)),xlim=c(0,25),
	xlab="number of defenses in place (k)",
	ylab="expected return")
rect(-10,-10,30,0,col=gray(.85),lty=3)

points(s0$k,s0$rev[s0$k+1],pch=16,cex=2,col="yellow")
lines(0:25,s0$rev,type="b",col=gray(.8),pch=16)

points(s1$k,s1$rev[s1$k+1],pch=16,cex=2,col="yellow")
lines(0:25,s1$rev,type="b",col=gray(.6),pch=2)

points(s2$k,s2$rev[s2$k+1],pch=16,cex=2,col="yellow")
lines(0:25,s2$rev,type="b",col=gray(.4),pch=1)

points(s3$k,s3$rev[s3$k+1],pch=16,cex=2,col="yellow")
lines(0:25,s3$rev,type="b",col=gray(.2),pch=8)

points(s4$k,s4$rev[s4$k+1],pch=16,cex=2,col="yellow")
lines(0:25,s4$rev,type="b",pch=3)

legend("topright",pch=c(16,2,1,8,3),col=gray(c(.8,.6,.4,.2,0)),bty="n",
		leg=expression(sigma==0,sigma==1,sigma==2,sigma==4,sigma==8))

dev.off()

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

def.cost <- function(k,rho=.1)
	rho/2*k^2 + (1-rho/2)*k

dyn.outcome <- function(tatt,tmax,k1,rho=.1,a=1000,z=.025,r=.05,lambda=0)
	a*lambda+tatt*a*(r-z-lambda) + (tmax-tatt)*(a*r-def.cost(k1+tatt,rho=rho)) - sum(def.cost(k1:(k1+tatt),rho=rho))

dyn.expected.outcome <- function(k1=0,tmax=25,a=1000,r=.05,z=.025,rho=.1,x0=15,dx=1,sigma=1,n=25,lambda=0)
{
	p <- prob.tatt(n=k1:n,x0=x0,dx=dx,sigma=sigma,a=a,z=z)
	o <- sapply(as.numeric(names(p)),dyn.outcome,tmax=tmax,k1=k1,rho=rho,a=a,z=z,r=r,lambda=lambda)
#	print(cbind(o,p))
	sum(p*o)
}

dynamic.revenue <- function(tmax=25,a=1000,r=.05,z=.025,rho=.1,x0=15,dx=1,sigma=1,n=25,lambda=0)
{
	res.dyn <- sapply(0:n,dyn.expected.outcome,tmax=tmax,a=a,r=r,z=z,rho=rho,x0=x0,dx=dx,sigma=sigma,n=n,lambda=lambda)
	res.stat <- static.revenue(n=n,a=a,z=z,r=r,rho=rho,dx=dx,x0=x0,sigma=sigma)$rev

	# dynamic or static solution, whatever is higher
	res <- pmax(res.stat,res.dyn/tmax)
	which <- apply(cbind(res.stat,res.dyn/tmax),1,which.max)

	list(rev=res,k=which.max(res)-1,which=which)
}

# --------------- Figure for dynamic analysis part  --------------------

# --- w/o sunk cost ---

pdf("../fig/fig-dynamic-analysis.pdf",6,4,pointsize=12)
par(mar=c(4,4,1,1))

d0 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=0)
d1 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=2)
d2 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=4)
d3 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=8)
d4 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=16)

R <- range(c(d0$rev,d1$rev,d2$rev,d3$rev,d4$rev))

plot(NA,NA,las=1,
	ylim=R,xlim=c(0,25),
	xlab=expression(list("number of proactive defenses "(k[1]))),
	ylab="expected return per period")
rect(-10,-10,30,0,col=gray(.85),lty=3)

points(d0$k,d0$rev[d0$k+1],pch=16,cex=2,col="yellow")
lines(0:25,d0$rev,type="b",col=gray(.8),pch=16)

points(d1$k,d1$rev[d1$k+1],pch=16,cex=2,col="yellow")
lines(0:25,d1$rev,type="b",col=gray(.6),pch=2)

points(d2$k,d2$rev[d2$k+1],pch=16,cex=2,col="yellow")
lines(0:25,d2$rev,type="b",col=gray(.4),pch=1)

points(d3$k,d3$rev[d3$k+1],pch=16,cex=2,col="yellow")
lines(0:25,d3$rev,type="b",col=gray(.2),pch=8)

points(d4$k,d4$rev[d4$k+1],pch=16,cex=2,col="yellow")
lines(0:25,d4$rev,type="b",pch=3)

legend("topright",pch=c(16,2,1,8,3),col=gray(c(.8,.6,.4,.2,0)),bty="n",
		leg=expression(sigma==0,sigma==2,sigma==4,sigma==8,sigma==16))

dev.off()

# --- with sunk cost ---

for (SIGMA in c(4,7)) {

pdf(paste("../fig/fig-sunk-analysis-",SIGMA,".pdf",sep=""),6,4,pointsize=12)
par(mar=c(4,4,1,1))

ds0 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=SIGMA,lambda=0)
ds1 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=SIGMA,lambda=.01)
ds2 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=SIGMA,lambda=.025)
ds3 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=SIGMA,lambda=.05)
ds4 <- dynamic.revenue(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=SIGMA,lambda=.1)

plot(NA,NA,las=1,
	ylim=R,xlim=c(0,25),
	xlab=expression(list("number of proactive defenses "(k[1]))),
	ylab="expected return per period")
rect(-10,-10,30,0,col=gray(.85),lty=3)

points(ds0$k,ds0$rev[ds0$k+1],pch=16,cex=2,col="yellow")
lines(0:25,ds0$rev,type="b",col=gray(.8),pch=16)

points(ds1$k,ds1$rev[ds1$k+1],pch=16,cex=2,col="yellow")
lines(0:25,ds1$rev,type="b",col=gray(.6),pch=2)

points(ds2$k,ds2$rev[ds2$k+1],pch=16,cex=2,col="yellow")
lines(0:25,ds2$rev,type="b",col=gray(.4),pch=1)

points(ds3$k,ds3$rev[ds3$k+1],pch=16,cex=2,col="yellow")
lines(0:25,ds3$rev,type="b",col=gray(.2),pch=8)

points(ds4$k,ds4$rev[ds4$k+1],pch=16,cex=2,col="yellow")
lines(0:25,ds4$rev,type="b",pch=3)

legend("topright",pch=c(16,2,1,8,3),col=gray(c(.8,.6,.4,.2,0)),bty="n",
		leg=expression(lambda==0,lambda==0.01,lambda==0.025,lambda==0.05,lambda==0.1))

legend("bottomleft",bty="n",leg=substitute(sigma==x,list(x=SIGMA)))

dev.off()

}

# --------------- ROSI Table ---------------  

table.row <- function(n=25,a=1000,z=.025,r=.05,rho=.1,dx=1,x0=15,sigma=0,lambda=0)
{
	# static analysis
	# ---------------
	
	S <- static.revenue(n=n,a=a,r=r,z=z,x0=x0,dx=dx,sigma=sigma,rho=rho)
	
	k <- S$k
	q <- static.exp.q(n=n,a=a,z=z,x0=x0,dx=dx,sigma=sigma)

	avg.attack 	 	 <- q[k+1]
	avg.spending 	 <- def.cost(k,rho=rho)
	avg.return.sec 	 <- S$rev[k+1]
	avg.return.insec <- S$rev[1]
	ale0 			 <- q[1]*a*z
	ale1			 <- avg.attack*a*z
	rosi			 <- (ale0-ale1-avg.spending)/avg.spending
	
	ST <- list(
		k					= k,
		avg.attack 			= 100*avg.attack,
		avg.spending		= 100*avg.spending/a,
		avg.return.sec  	= 100*avg.return.sec/a,
		avg.return.insec	= 100*avg.return.insec/a,
		ale0				= ale0,
		ale1				= ale1,
		rosi				= 100*rosi
		)

	# dynamic analysis w/o sunk costs
	# -------------------------------

	S <- dynamic.revenue(n=n,a=a,z=z,r=r,rho=rho,dx=dx,x0=x0,sigma=sigma,lambda=0)
	
	k <- S$k
	
	p <- prob.tatt(n=k:n,x0=x0,dx=dx,sigma=sigma,a=a,z=z)
	att <- as.numeric(names(p))
	
	
	avg.attack		<- sum(att*p)/n
	avg.spending    <- sum(apply(sapply(att, 
								function(i) c(k:(k+i),rep(k+i,n-i-1))),2,
								function(j) sum(def.cost(j,rho=rho)))*p)/n
	avg.return.sec	<- S$rev[k+1]
	ale0			<- q[1]*a*z
	ale1			<- avg.attack*a*z
	rosi			<- (ale0-ale1-avg.spending)/avg.spending

	DY <- list(
		k					= k,
		avg.attack 			= 100*avg.attack,
		avg.spending		= 100*avg.spending/a,
		avg.return.sec  	= 100*avg.return.sec/a,
		avg.return.insec	= 100*avg.return.insec/a,
		ale0				= ale0,
		ale1				= ale1,
		rosi				= 100*rosi
		)


	# dynamic analysis w/ sunk costs
	# ------------------------------

	S <- dynamic.revenue(n=n,a=a,z=z,r=r,rho=rho,dx=dx,x0=x0,sigma=sigma,lambda=lambda)
	
	k <- S$k
	
	if (S$which[k+1]==2)
	{

	p <- prob.tatt(n=k:n,x0=x0,dx=dx,sigma=sigma,a=a,z=z)
	att <- as.numeric(names(p))
	
	avg.attack		<- sum(att*p)/n
	avg.spending    <- sum((apply(sapply(att, 
								function(i) c(k:(k+i),rep(k+i,n-i-1))),2,
								function(j) sum(def.cost(j,rho=rho)))+att*lambda*a)*p)/n
	avg.return.sec	<- S$rev[k+1]
	ale0			<- q[1]*a*z
	ale1			<- avg.attack*a*z
	rosi			<- (ale0-ale1-avg.spending)/avg.spending

	SC <- list(
		k					= k,
		avg.attack 			= 100*avg.attack,
		avg.spending		= 100*avg.spending/a,
		avg.return.sec  	= 100*avg.return.sec/a,
		avg.return.insec	= 100*avg.return.insec/a,
		ale0				= ale0,
		ale1				= ale1,
		rosi				= 100*rosi
		)
	}
	else SC <- ST	
		
	rbind(ST,DY,SC)

	}
	

Sweave("quant-table.Rnw")

# ---------------------------------------------------------------------
