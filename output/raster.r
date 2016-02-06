# X11(type='dbcairo')
setwd(getwd())
out.dir <- '/output_raster/'

library(raster)
library(RColorBrewer)
library(classInt)

max.x <- 10 
max.y <- 10 

temp.1 <- read.csv('output.csv', header=F)

temp.2 <- temp.1[,1]

temp.3 <- matrix(temp.2, ncol=365)

output <- temp.3[3:nrow(temp.3),]

color <- colorRampPalette(brewer.pal(9,'YlGn'))(100)

# interval <- c(0,40,80,120,160,200,240,280,320,360)
# interval <- classIntervals(output, n=100, style='equal')
interval <- seq(1,350,3.5)

# colcode <- findColours(interval, color)

r <- raster(xmn = 0, xmx = max.x, ymn = 0, ymx = max.y, nrows = max.y, ncols = max.x)

for(i in 1:365){ 

    outfile <- paste(getwd(),out.dir,'day_',i,'.jpg',sep='')

    r[] <- output[,i]

    jpeg(file=outfile)#,width=10,height=10

    # plot(r, col=colcode)
    plot(r, col=color, breaks=interval)
    # plot(r, col=topo.colors(20), breaks=interval)

    #savePlot(filename=outfile,type='png',device=dev.cur())

	dev.off()

}


