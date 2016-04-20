setwd(getwd())

origin.data <- read.csv('data_origin.csv')

# par(mfrow=c(2,2), mar=c(5,5,5,1.5))

layout(matrix(c(1,1,2,2,3,3,4,4,0,5,5,0), 3, 4, byrow=TRUE))

# DM 

plot(x=c(1:4), y=origin.data[1:4,3], type='b', pch=0, lty=1, lwd=2,
     ylim=c(0,8), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n')

lines(x=c(1:4), y=origin.data[5:8,3], type='b', pch=3, lty=2, lwd=2)

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(a)', xlab='Grazing intensity', ylab=expression('Plant biomass ' (tDM/ha)),
     cex.lab=1)

# SLA

plot(x=c(1:4), y=origin.data[1:4,4], type='b', pch=0, lty=1, lwd=2,
     ylim=c(150,190), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n')

lines(x=c(1:4), y=origin.data[5:8,4], type='b', pch=3, lty=2, lwd=2)

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(b)', xlab='Grazing intensity', ylab=expression('Specific leaf area ' (cm^2/g)),
     cex.lab=1)
# LAI 

plot(x=c(1:4), y=origin.data[1:4,5], type='b', pch=0, lty=1, lwd=2,
     ylim=c(0,9), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n')

lines(x=c(1:4), y=origin.data[5:8,5], type='b', pch=3, lty=2, lwd=2)

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(c)', xlab='Grazing intensity', ylab='LAI',
     cex.lab=1)

# PS 

plot(x=c(1:4), y=origin.data[1:4,6], type='b', pch=0, lty=1, lwd=2,
     ylim=c(0,7), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n')

lines(x=c(1:4), y=origin.data[5:8,6], type='b', pch=3, lty=2, lwd=2)

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(d)', xlab='Grazing intensity', ylab=expression('Photosynthesis rate ' (paste(mu, mol,CO[2])/m^2%.% s)),cex.lab=1)

# TI 

plot(x=c(1:4), y=origin.data[1:4,7], type='b', pch=0, lty=1, lwd=2,  
     ylim=c(450,1100), xlim=c(0.8,4.2), xlab='', ylab='', xaxt='n')

lines(x=c(1:4), y=origin.data[5:8,7], type='b', pch=3, lty=2, lwd=2)

axis(1, at=c(1:4), labels=c('NG', 'LG', 'MG', 'HG') )

title('(e)', xlab='Grazing intensity', ylab=expression('Tiller density ' (tillers/m^2)),
     cex.lab=1)

# par(mfrow=c(1,1))

