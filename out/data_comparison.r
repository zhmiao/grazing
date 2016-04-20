setwd(getwd())

origin.data <- read.csv('data_origin.csv')

# par(mfrow=c(2,2), mar=c(5,5,5,1.5))

layout(matrix(c(1,1,2,2,3,3,4,4,0,5,5,0), 3, 4, byrow=TRUE))

# DM 

plot(x=c(1:4), y=origin.data[1:4,3], type='b', pch=0, lty=3, lwd=1,
     ylim=c(0,8), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,3], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(a)', xlab='Grazing intensity', ylab=expression('Plant biomass ' (tDM/ha)),
     cex.lab=1)

# SLA

plot(x=c(1:4), y=origin.data[1:4,4], type='b', pch=0, lty=3, lwd=1,
     ylim=c(150,190), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,4], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(b)', xlab='Grazing intensity', ylab=expression('Specific leaf area ' (cm^2/g)),
     cex.lab=1)
# LAI 

plot(x=c(1:4), y=origin.data[1:4,5], type='b', pch=0, lty=3, lwd=1,
     ylim=c(0,9), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,5], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(c)', xlab='Grazing intensity', ylab='LAI',
     cex.lab=1)

# PS 

plot(x=c(1:4), y=origin.data[1:4,6], type='b', pch=0, lty=3, lwd=1,
     ylim=c(0,7), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,6], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(d)', xlab='Grazing intensity', ylab=expression('Photosynthesis rate ' (paste(mu, mol,CO[2])/m^2%.% s)),cex.lab=1)

# TI 

plot(x=c(1:4), y=origin.data[1:4,7], type='b', pch=0, lty=3, lwd=1,  
     ylim=c(450,1100), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,7], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(e)', xlab='Grazing intensity', ylab=expression('Tiller density ' (tillers/m^2)),
     cex.lab=1)



layout(matrix(c(1), 1, 1, byrow=TRUE))

plot.new()

legend('bottom', c("Data from late-July", "Data from mid-September"), lty=3, lwd=1, pch=c(0,3), col=c('blue', 'red'))

# par(mfrow=c(1,1))

# -++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
# -++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
# -++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
# -++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
# -++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-


setwd(getwd())

origin.data <- read.csv('data_origin.csv')

simulate.data <- read.csv('compare.csv', header=FALSE)

par(oma=c(4,0,0,0), mar = c(4, 5, 5, 1.5))

layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))

# DM 

plot(x=c(1:4), y=origin.data[1:4,3], type='b', pch=0, lty=3, lwd=1,
     ylim=c(0,8), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,3], type='b', pch=3, lty=3, lwd=1, col='blue')

lines(x=c(1:4), y=simulate.data[seq(1,7,2),3], type='b', pch=0, lty=3, lwd=1, col='red')

lines(x=c(1:4), y=simulate.data[seq(2,8,2),3], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(a)', xlab='Grazing intensity', ylab=expression('Plant biomass ' (tDM/ha)),
     cex.lab=1)

# LAI 

plot(x=c(1:4), y=origin.data[1:4,5], type='b', pch=0, lty=3, lwd=1,
     ylim=c(0,15), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,5], type='b', pch=3, lty=3, lwd=1, col='blue')

lines(x=c(1:4), y=simulate.data[seq(1,7,2),4], type='b', pch=0, lty=3, lwd=1, col='red')

lines(x=c(1:4), y=simulate.data[seq(2,8,2),4], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(b)', xlab='Grazing intensity', ylab='LAI',
     cex.lab=1)

# PS 

plot(x=c(1:4), y=origin.data[1:4,6], type='b', pch=0, lty=3, lwd=1,
     ylim=c(0,7), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='blue')

lines(x=c(1:4), y=origin.data[5:8,6], type='b', pch=3, lty=3, lwd=1, col='blue')

lines(x=c(1:4), y=simulate.data[seq(1,7,2),5], type='b', pch=0, lty=3, lwd=1, col='red')

lines(x=c(1:4), y=simulate.data[seq(2,8,2),5], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(c)', xlab='Grazing intensity', ylab=expression('Photosynthesis rate ' (paste(mu, mol,CO[2])/m^2%.% s)),cex.lab=1)

# Growth ratio
plot(x=c(1:4), y=simulate.data[seq(1,7,2),7], type='b', pch=0, lty=3, lwd=1,  
     ylim=c(0,10), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n', col='red')

lines(x=c(1:4), y=simulate.data[seq(2,8,2),7], type='b', pch=3, lty=3, lwd=1, col='red')

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(d)', xlab='Grazing intensity', ylab='Growth ratio',
     cex.lab=1)


layout(matrix(c(1), 1, 1, byrow=TRUE))

plot.new()

legend('bottom', c("Observed data from late-July", "Observed data from mid-September", "Simulated data for late-July", "Simulated data for mid-September"),
       lty=3, lwd=1, pch=c(0,3,0,3), col=c('blue', 'blue', 'red', 'red'))

