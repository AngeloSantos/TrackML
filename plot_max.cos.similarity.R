###############################################
input.names = "figures/application_2020_09_19_v2/application_objects.RData"

output.folder = "figures/application_2020_09_19_v2"

output.names = "hist_Max_Cosine_Similarity.png"

draw.layers <- c(1:6) # indeed 5 to 10
xmin = 0.999
xmax = 1
x.step = 0.00005
ymin = 0
ymax = 3000
break.hists = seq(xmin, xmax, x.step)
h.scale = 1
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

png(paste0(output.folder, "/", output.names),
    units="px", width=1600, height=1600, res=250)

hist( max.cos.similarity[,1][max.cos.similarity[,1] > xmin],
      breaks=break.hists,
      ylim=c(ymin, ymax),
      xlab=TeX("$cos(\\theta) \\; \\[rad\\]$"),
      ylab=sprintf("Frequency / %0.5f [rad]", x.step),
      main="Maximum Cosine Similarity between Predicted and Near Hits",
      #col=alpha(rgb.color[[1]], h.scale),
      border=alpha(rgb.color[[1]], h.scale) )

for( l in 2:length(draw.layers) ){
	hist( max.cos.similarity[,l][max.cos.similarity[,l] > xmin],
	      breaks=break.hists,
	      #col=alpha(rgb.color[[l]], h.scale),
	      border=alpha(rgb.color[[l]], h.scale),
	      add=TRUE )
}

#number.cos.sim <- c()
#for(l in draw.layers)
#	number.cos.sim <- c(number.cos.sim, length(max.cos.similarity[,l]))

legend( "topleft",
        legend=TeX(sprintf( "Layers %d", draw.layers+4 )),# number.cos.sim )),
        fill=c( alpha(rgb.color[draw.layers], h.scale) ),
       	border=NA, bty="n" )
dev.off()


