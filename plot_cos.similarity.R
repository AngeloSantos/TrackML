###############################################
input.names = "figures/application_2020_09_19_v2/application_objects.RData"

output.folder = "figures/application_2020_09_19_v2"

output.names = "hist_Cosine_Similarity"

draw.layers <- c(1:6) # indeed 5 to 10
xmin = 0.975
xmax = 1
x.step = 0.0005
ymin = 0
ymax = 8000
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

plot.cos.hist <- function(cos.object, output_names, legend.title){

	png(paste0(output.folder, "/", output_names, ".png"),
	    units="px", width=1600, height=1600, res=250)
	
	hist( cos.object[[1]][cos.object[[1]] > xmin & cos.object[[1]] <= xmax],
	      breaks=break.hists,
	      ylim=c(ymin, ymax),
	      xlab=TeX("$cos(\\theta) \\; \\[rad\\]$"),
	      ylab=sprintf("Frequency / %0.4f [rad]", x.step),
	      main="Cosine Similarity between Predicted and Near Hits",
	      col=alpha(rgb.color[[1]], h.scale),
	      border=NA )
	
	for( l in 2:length(draw.layers) ){
		hist( cos.object[[l]][cos.object[[l]] > xmin & cos.object[[l]] <= xmax],
		      breaks=break.hists,
		      col=alpha(rgb.color[[l]], h.scale),
		      border=NA,
		      add=TRUE )
	}
	
	number.cos.sim <- c()
	for(l in draw.layers){
		number.cos.sim <- c(number.cos.sim,
				    length(cos.object[[l]][cos.object[[l]] <= xmax]))
	}
	
	legend( "topleft",
	        title = legend.title,
	        legend=TeX(sprintf( "Layers %d (%d)", draw.layers+4, number.cos.sim )),
	        fill=c( alpha(rgb.color[draw.layers], h.scale) ),
	       	border=NA, bty="n" )
	dev.off()
}

plot.cos.hist( cosine.similarity,
	       "hist_Cosine_Similarity_S.F",
	       "Success + Fail Cases")

plot.cos.hist( cosine.similarity.s,
               "hist_Cosine_Similarity_S",
               "Success Cases")

plot.cos.hist( cosine.similarity.f,
               "hist_Cosine_Similarity_F",
               "Fail Cases")


#########################################
