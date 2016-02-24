setwd(getwd())

# total biomass

input.data <- read.csv('glo_totbio.csv', header=F)

# input.data <- input.data[,1]

input.data[,2] <- seq(1,365,1)
input.data[,2] <- seq(1,730,1)

plot(x=input.data[,2], y=input.data[,1], type='l', col='blue', xlab='time', ylab='biomass', lwd=3)
lines(x=input.data[,2], y=input.data[,1], col='red', lwd=3)
lines(x=input.data[,2], y=input.data[,1], col='red', lwd=3, lty=2)
lines(x=input.data[,2], y=input.data[,1], col=81, lwd=3)
lines(x=input.data[,2], y=input.data[,1], col=81, lwd=3, lty=2)
lines(x=input.data[,2], y=input.data[,1], col='coral', lwd=3)

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
