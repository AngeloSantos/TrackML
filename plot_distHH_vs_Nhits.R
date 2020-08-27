#################

output_folder = "figures/application_2020_05_17/mean_dist_HH_20200519"
output_name   = "mean_dist_HH"

area_per_layer <- c(811035.3, 1350445.6, 1809037.9,
		    2888430.6, 4415044.8, 6808761.2)

rmse <- c(3.20, 2.85, 3.53,
	  13.02, 6.69, 8.54)

n_hits <- seq(1000, 200000, 1000)

dist.colors <- c("red", "blue", "forestgreen",
		 "darkorchid", "darkorange4", "gold")
dist.legend <- c("Layer 5", "Layer 6", "Layer 7",
		 "Layer 8", "Layer 9", "Layer 10")

rmse.colors <- c("red", "blue", "forestgreen",
		 "darkorchid", "darkorange4", "gold")
rmse.legend <- c("RMSE(L5) = 3.2", "RMSE(L6) = 2.9", "RMSE(L7) = 3.5",
		 "RMSE(L8) = 13.0", "RMSE(L9) = 6.7", "RMSE(L10) = 8.5")

dist_hh <- data.frame()

cat("* Layer: ")
for( a in 1:length(area_per_layer) ){
	cat(a+4, "...")
	for( n in 1:length(n_hits) ){
		dist_hh[n,a] <- sqrt( area_per_layer[a] / n_hits[n] )
	}
}
cat("\n")

output_file_name <- paste( output_folder, "/", output_name, ".png", sep="" )
png(output_file_name, units="px", width=1600, height=1600, res=250)
for( a in 1:length(area_per_layer) ){
	if( a == 1 ){
		plot( n_hits, dist_hh[,1],
	       	      log = "y", type = "l",
		      xlim = c(min(n_hits), max(n_hits)),
		      ylim = c(min(dist_hh), max(dist_hh)),
		      xlab = "Number of Hits", 
		      ylab = "Mean Distance (mm)",
		      main = "Mean Distance between Two Near Hits",
		      col = dist.colors[a], lty = 1, lwd = 2 )
		abline( h=rmse[1], col = rmse.colors[1], lty = 2, lwd = 2 )
	}
	else{
		points( n_hits, dist_hh[,a],
		        log = "y", type = "l", col = dist.colors[a], lwd = 2 )
		abline( h=rmse[a], col = rmse.colors[a], lty = 2, lwd = 2 )
	}
} # for( a in 1:length(area_per_layer) )
legend( 0.2*max(n_hits), max(dist_hh), c(dist.legend),
        col = c(dist.colors), lty = c(1, 1, 1, 1, 1, 1), lwd = c(2, 2, 2, 2, 2, 2) )
legend( 0.5*max(n_hits), max(dist_hh), c(rmse.legend),
        col = c(rmse.colors), lty = c(2, 2, 2, 2, 2, 2), lwd = c(2, 2, 2, 2, 2, 2) )
dev.off()



