###
#input <- read.csv("figures/application_2020_05_19/leastDist.pred_near.csv")
#input <- read.csv("figures/application_2020_05_19/leastDist.near_real.csv")
input <- read.csv("figures/application_2020_05_19/leastDist.pred_real.csv")

output_folder    = "figures/application_2020_05_19/leastDist_20200520"

#output_name      = "leastDist.pred_near_veryShortRange"
#output_name      = "leastDist.near_real_veryShortRange"
output_name      = "leastDist.pred_real_veryShortRange"

#main_title = "Least Distance between Predicted and Nearest Hits"
#main_title = "Least Distance between Nearest and Real Hits"
main_title = "Least Distance between Predicted and Real Hits"

xMin = 0
xMax = 20
yMin = 1
yMax = 10000

nrow.pred_near <- nrow(input)
ncol.pred_near <- ncol(input)

hist.colors <- c("red", "blue", "forestgreen", "darkorchid", "darkorange4", "gold")
hist.legend <- c("Layer 5", "Layer 6", "Layer 7", "Layer 8", "Layer 9", "Layer 10")

leastDist <- list()
xRange <- c()
yRange <- c()
for( c in 1:6 ){
	leastDist[[c]] <- hist(input[,c], nclass = 1000)
	xRange <- c( range(xRange, leastDist[[c]]$mids) )
	yRange <- c( range(yRange, leastDist[[c]]$counts) )
}

message("* Minimu and maximum value in x: ", min(xRange), " - ", max(xRange))
message("* Minimu and maximum value in y: ", min(yRange), " - ", max(yRange))

output_file_name <- paste( output_folder, "/", output_name, ".png", sep="" )
png(output_file_name, units="px", width=1600, height=1600, res=250)
for( l in 5:10 ){
	if( l == 5 ){
		plot( leastDist[[l-4]]$breaks[-length(leastDist[[l-4]]$counts+1)],
		      leastDist[[l-4]]$counts,
		      log = "y", type = "b",
		      #xlim = c(min(xRange), max(xRange)),
		      xlim = c(xMin, xMax),
		      #ylim = c(1, max(yRange)),
		      ylim = c(yMin, yMax),
		      xlab = "Distance (mm)",
	     	      ylab = "Frequency",
		      col = hist.colors[l-4], pch = 19, lty = 1, lwd = 2,
	      	      main = main_title )
	}
	else{
		points( leastDist[[l-4]]$breaks[-length(leastDist[[l-4]]$counts+1)],
		        leastDist[[l-4]]$counts,
		        log = "y", type = "b", col = hist.colors[l-4], pch = 19,
		        lty = 1, lwd = 2 )
	}
}
legend( 0.75*xMax, yMax, c(hist.legend),
        col = c(hist.colors), pch = c(19, 19, 19, 19, 19, 19),
        lty = c(1, 1, 1, 1, 1, 1), lwd = c(2, 2, 2, 2, 2, 2) )
dev.off()


