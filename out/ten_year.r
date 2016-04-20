setwd(getwd())

# total biomass

input.data <- read.csv('glo_tot.csv', header=F)

par(mar=c(5,5,3,1.5))

plot(1, type='n', xlab='Days', ylab='Plant biomass (tDM/ha)', xlim=c(0,(365*10+1)), ylim=c(0,9.5))

  # lines(x=seq(1,(365*10),1), y=input.data[(1+10*365*1):(10*365+10*365*1),1], type='l', lwd=1)

for (i in 0:3) {

  lines(x=seq(1,(365*10),1), y=input.data[(1+10*365*i):(10*365+10*365*i),1], type='l', lwd=1, col=i+1)

}


plot.new()

legend('bottom', c('NG', 'LG', 'MG', 'HG'), lty=1, lwd=1, col=c(1,2,3,4))
