setwd(getwd())

# total biomass

input.data <- read.csv('glo_totbio.csv', header=F)

# input.data <- input.data[,1]

# For one year
input.data[,2] <- seq(1,365*20,1)
# For two years
input.data[,2] <- seq(1,730,1)

par( mar=c(3,3,1.5,1.5) )

frame()
plot(x=input.data[,2], y=input.data[,1], type='l', col='cadetblue', xlab='time', ylab='biomass', lwd=3, axes=FALSE, frame.plot=TRUE, line=1)
plot(x=input.data[,2], y=input.data[,1], type='l', col='cadetblue', xlab='time', ylab='biomass', lwd=3)
lines(x=input.data[,2], y=input.data[,1], col='chocolate', lwd=3)
lines(x=input.data[,2], y=input.data[,1], col='burlywood', lwd=3, lty=2)
lines(x=input.data[,2], y=input.data[,1], col='coral', lwd=3)
lines(x=input.data[,2], y=input.data[,1], col='cyan', lwd=3, lty=2)
lines(x=input.data[,2], y=input.data[,1], col='darkgoldenrod', lwd=3)

plot(x=input.data[,2], y=input.data[,1], type='l', xlab='time', ylab='biomass', lwd=3, axes=FALSE, frame.plot=TRUE, line=1)

lines(x=input.data[,2], y=input.data[,1], lwd=3, lty=2)

lines(x=input.data[,2], y=input.data[,1], lwd=3, lty=4)

lines(x=input.data[,2], y=input.data[,1], lwd=3, lty=3)

legend(230,2700, c("No effects", "Defoliation", "Detachment", "Combined"), lty=c(1,2,4,3))
legend(0.3,0.5, c("No effects", "Defoliation", "Detachment", "Combined"), lty=c(1,2,4,3))


# ----------------------------------
# ----------------------------------
# ----------------------------------


input.data.y <- read.csv('yearly_glo.csv', header=F)

input.data.y[,2] <- seq(1,10,1)

png(filename="fig_15.png")

plot(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=0, xlab='years', ylim=c(0,1300000), ylab='biomass', cex.lab=2, lwd=3, axes=FALSE, frame.plot=TRUE, line=1)

lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=1, lwd=2, lty=2)

lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=2, lwd=2, lty=2)

# Soil compactness
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=3, lwd=2, lty=3, col="blue")

# Respiration
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=3, lwd=2, lty=3, col="aquamarine3")

# N return
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=3, lwd=2, lty=3, col="bisque4")

# litter pool
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=3, lwd=2, lty=3, col="brown4")

# photosynthesis
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=3, lwd=2, lty=3, col="coral1")

# rainfall interception
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=3, lwd=2, lty=3, col="blueviolet")

# evaportranspiration
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=2, lwd=3, lty=3, col="white")

# evaporation or transpiration
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=2, lwd=2, lty=3, col="gold2")

# N uptake or C conversion
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=3, lwd=2, lty=3, col="firebrick1")

# Grow days
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=4, lwd=2, lty=3, col="purple1")

# K
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=4, lwd=2, lty=3, col="royalblue1")

# R
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=4, lwd=2, lty=3, col="palegreen2")

# D
lines(x=input.data.y[,2], y=input.data.y[,1], type='b', pch=4, lwd=2, lty=3, col="orangered3")

legend(5, 500000, c("Soil compactness", "Respiration", "N return", "Litter pool", 
                       "Photosysnthesis", "Rainfall interception", "Evaporation or transpiration", 
                       "N uptake or C conversion", "No effects", "Only first stage effects", "All effects")
       , pch=c(3,3,3,3,3,3,3,3,0,1,2), lty=c(3,3,3,3,3,3,3,3,1,2,2), 
       col=c("blue","aquamarine3","bisque4","brown4","coral1","blueviolet","gold2","firebrick1","black","black","black"))

dev.off()

legend(5, 500000, c("Effects from grow days", "Effects from carrying capacity", "Effects from growth rate", "Effects from decrease rate", 
                        "No effects", "Only first stage effects", "All effects")
       , pch=c(4,4,4,4,0,1,2), lty=c(3,3,3,3,1,2,2), 
       col=c("purple1","royalblue1","palegreen2","orangered3","black","black","black"))

# ----------------------------------
# ----------------------------------
# ----------------------------------

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
