setwd(getwd())

# total biomass

input.data <- read.csv('glo_totbio.csv', header=F)

# input.data <- input.data[,1]

# For one year
input.data[,2] <- seq(1,365,1)
# For two years
input.data[,2] <- seq(1,730,1)

par( mar=c(3,3,1.5,1.5) )

plot(x=input.data[,2], y=input.data[,1], type='l', col='cadetblue', xlab='time', ylab='biomass', lwd=3, axes=FALSE, frame.plot=TRUE, line=1)
lines(x=input.data[,2], y=input.data[,1], col='chocolate', lwd=3)
lines(x=input.data[,2], y=input.data[,1], col='burlywood', lwd=3, lty=2)
lines(x=input.data[,2], y=input.data[,1], col='coral', lwd=3)
lines(x=input.data[,2], y=input.data[,1], col='cyan', lwd=3, lty=2)
lines(x=input.data[,2], y=input.data[,1], col='darkgoldenrod', lwd=3)

plot(x=input.data[,2], y=input.data[,1], type='l', xlab='time', ylab='biomass', lwd=3, axes=FALSE, frame.plot=TRUE, line=1)

lines(x=input.data[,2], y=input.data[,1], lwd=3, lty=2)

lines(x=input.data[,2], y=input.data[,1], lwd=3, lty=4)

lines(x=input.data[,2], y=input.data[,1], lwd=3, lty=3)

legend(230,2700, c("No effects", "Defoliation", "Detachment", "Combined"), lty=c(1,2,3,4))

# biomass for each species

pla.1 <- read.csv('glo_totbio_pla_1.csv', header=F)
pla.2 <- read.csv('glo_totbio_pla_2.csv', header=F)

pla.1[,2] <- seq(1,365,1)
pla.2[,2] <- seq(1,365,1)

par(mfrow=c(1,2))

plot(x=pla.1[,2], y=pla.1[,1], type='l', col='blue')
lines(x=pla.1[,2], y=pla.1[,1], col='red')

plot(x=pla.2[,2], y=pla.2[,1], type='l', col='blue')
lines(x=pla.2[,2], y=pla.2[,1], col='red')

par(mfrow=c(1,1))
