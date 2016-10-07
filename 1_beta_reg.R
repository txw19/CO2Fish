# rm(list=ls())
library(R2jags)
# library(arm)
library(car)
dat <- read.csv('pp_data_tw.csv')
head(dat)
summary(dat)
dim(dat)

# levels(dat$ACC_CO2)
levels(dat$EXP_CO2)

# Convert factors to numeric vectors
dat$acc <- as.numeric(dat$ACC_CO2) 
dat$exp <- as.numeric(dat$EXP_CO2) 


# RESPONSE VARIABLE
dat$y <- dat$T_TO_CONSUME/1200
range(dat$y,na.rm=T)

# hist(dat$y)


# Betareg 
library(betareg)

# t1beta <- betareg(y ~ ACC_CO2 + EXP_CO2 + ACC_CO2*EXP_CO2, data=dat)
# summary(t1beta)
# Means parameteraized
breg1 <- betareg(y ~ ACC_CO2 + EXP_CO2 + ACC_CO2*EXP_CO2 -1 -ACC_CO2 - EXP_CO2, data=dat)
summary(breg1)



#################################################################
########## BUGS CODE ############################################
#################################################################

####### Define the model in the BUGS language and write a text file
sink("BregModel.txt")
cat("
model {
    
for(i in 1:n){
    y[i] ~ dbeta(a[i],b[i])
    a[i] <- mu[i] * phi
    b[i] <- ((1-mu[i]) * phi)
    logit(mu[i]) <- group.mean[trt1[i], trt2[i]]
}
    
    # Priors
for(i in 1:ntrt1){
	for(j in 1:ntrt2){
  group.mean[i,j]~dnorm(0,0.001)
  }
}


    phi ~ dgamma(0.1, 0.1)  

    
} # end model

",fill = TRUE)
sink()


# Load data
data <- list(y = dat$y, n = nrow(dat), trt1 = dat$acc, trt2 = dat$exp, 
             ntrt1 = length(unique(dat$acc)), ntrt2 = length(unique(dat$exp))  )


# Initial values
inits <- function (){
  list (phi=runif(1))
}

# Parameters monitored
parameters <- c("group.mean")


# MCMC settings
ni <- 12000
nt <- 2
nb <- 8000
nc <- 3

out <- jags(data, inits, parameters, "BregModel.txt", n.chains = nc, 
             n.thin = nt, n.iter = ni, n.burnin = nb)

#out.sum2 <- out$BUGSoutput$summary
#write.csv(out.sum2, 'output2.csv')

# Summarize posteriors
print(out, dig = 3) 
# str(out)

# # Ouptut order
# ACC_CO2control:EXP_CO2control  [1,1]
# ACC_CO2high:EXP_CO2control     [2,1]
# ACC_CO2low:EXP_CO2control      [3,1]
# ACC_CO2control:EXP_CO2high     [1,2]
# ACC_CO2high:EXP_CO2high        [2,2]
# ACC_CO2low:EXP_CO2high         [3,2]
# ACC_CO2control:EXP_CO2low      [1,3]
# ACC_CO2high:EXP_CO2low         [2,3]
# ACC_CO2low:EXP_CO2low          [3,3]

# Put estimates on natural scale
nout <- plogis(out$BUGSoutput$sims.list$group.mean[,,])
dim(nout)

# hist(out$BUGSoutput$sims.list$group.mean[,1,1])

# Get group means and CIs
mean(nout[,1,1])

# Obtain posterior means
groupmeans <- apply(nout,c(2,3),mean)

# Obtain credible intervals for means
cis <- apply(nout,c(2,3),quantile, probs=c(0.025,0.975))
# dim(cis)



######## Quick Plot
#######################################

## NOTE: you will need to reorder these the way you want to show them
# Using the transpose function (t) here just to make the order of the vector the same as what is above - you can change
groupmeans2 <- as.vector(t(groupmeans))
lowerci <- as.vector(t(cis[1,,]))
upperci <- as.vector(t(cis[2,,]))

# Ordering of groups to plot
ord <- c(1,2,3,4,5,6,7,8,9)

cex.xlab <- 0.8

res<-6
name_figure <- "co2Plot1.png"
png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.3
y.label <- 'Proportion of time until prey consumed'
x.label = ''

par(mfrow = c(1,1), mar=c(4,4,1,1))

# Plot means with CRIs - for MS
plot(ord,groupmeans2,cex=4, ylim=c(0,1),axes=F,ylab='',xlab='',pch=16)
segments(ord,lowerci,ord,upperci,lty=1,lwd=3)
axis(side=1,at=ord,labels=FALSE, tck=0,cex.axis=size.text)
axis(side=2,cex.axis=size.text,font=1,las=1)
mtext(y.label, line = 2.9, side = 2, cex = size.text)

## NOTE: you can add your group labels once you know the order you are plotting them in
mtext('ACC_C\n EXP_C', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.15)
mtext('ACC_H-\n EXP_C', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.25)
mtext('ACC_L-\n EXP_C', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.35)

mtext('ACC_C\n EXP_H', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.45)
mtext('ACC_H-\n EXP_H', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.55)
mtext('ACC_L-\n EXP_H', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.65)

mtext('ACC_C\n EXP_L', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.75)
mtext('ACC_H-\n EXP_L', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.85)
mtext('ACC_L-\n EXP_L', side=1, line=-2.7, cex=cex.xlab , outer=T, at=0.95)


box()

par(def.par)
dev.off()


# ACC_CO2control:EXP_CO2control  [1,1]
# ACC_CO2high:EXP_CO2control     [2,1]
# ACC_CO2low:EXP_CO2control      [3,2]
# ACC_CO2control:EXP_CO2high     [1,2]
# ACC_CO2high:EXP_CO2high        [2,2]
# ACC_CO2low:EXP_CO2high         [3,2]
# ACC_CO2control:EXP_CO2low      [1,3]
# ACC_CO2high:EXP_CO2low         [2,3]
# ACC_CO2low:EXP_CO2low          [3,3]






