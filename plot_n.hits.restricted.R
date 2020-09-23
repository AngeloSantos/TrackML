###############################################
input.names = "figures/application_2020_09_19_v2/application_objects.RData"

output.folder = "figures/application_2020_09_19_v2"

output.names = "hist_n.hits.restricted"

draw.layers <- c(1:6) # indeed 5 to 10
xmin = 0
xmax = 45
x.step = 1
ymin = 0
ymax = 2000
break.hists = seq(xmin, xmax, x.step)
h.scale = 0.7
rgb.color <- list( rgb(255,   0,   0, maxColorValue=255), # red
		   rgb(  0, 100,   0, maxColorValue=255), # dark green
		   rgb(  0,   0, 255, maxColorValue=255), # blue
		   rgb(255, 255,   0, maxColorValue=255), # yellow
		   rgb(  0, 128, 128, maxColorValue=255), # teal
		   rgb(255,   0, 255, maxColorValue=255), # magenta
		   rgb(128,   0,   0, maxColorValue=255), # maroon
		   rgb(128, 128,   0, maxColorValue=255), # olive
		   rgb(119, 135, 153, maxColorValue=255) )# light state gray
###############################################

library(latex2exp)
library(scales)

load(input.names)

png(paste0(output.folder, "/", output.names, ".png"),
    units="px", width=1600, height=1600, res=250)

hist( n.hits.restricted[, 6][n.hits.restricted[, 6] > xmin &
                             n.hits.restricted[, 6] <= xmax],
      breaks=break.hists,
      ylim=c(ymin, ymax),
      xlab="Number of Searching Hits",
      ylab=sprintf("Frequency", x.step),
      main="Number of Hits around Predicted Hit",
      col=alpha(rgb.color[[6]], h.scale),
      #labels=TRUE,
      border=NA )

for( l in (length(draw.layers)-1):1 ){
	hist( n.hits.restricted[, l][n.hits.restricted[, l] > xmin &
	                             n.hits.restricted[, l] <= xmax],
	      breaks=break.hists,
	      col=alpha(rgb.color[[l]], h.scale),
	      border=NA,
	      #labels=TRUE,
	      add=TRUE )
}

number.hits.restricted <- c()
for(l in draw.layers){
	number.hits.restricted <- c(number.hits.restricted,
				    sum(n.hits.restricted[, l]) )
}

legend( "topright",
        #title = ,
        legend=TeX(sprintf( "Layers %d (%d)", draw.layers+4, number.hits.restricted )),
        fill=c( alpha(rgb.color[draw.layers], h.scale) ),
       	border=NA, bty="n" )
dev.off()


#########################################
