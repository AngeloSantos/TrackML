###############################################
input.names = "data/eta_n0.5-0.5_phi_ninf-pinf.csv"

output.folder = "data/figures/histograms_2020_06_13"

output.names = c( "hist_z1.png",
		  "hist_z2.png",
		  "hist_z3.png",
                  "hist_z4.png",
                  "hist_z5.png",
                  "hist_z6.png",
		  "hist_z7.png",
                  "hist_z8.png",
		  "hist_z9.png",
		  "hist_z10.png" )

draw.layers <- c(0:9) # indeed 1 to 10
break.hists <- c(1000, 1000, 1000, 1000, 2000, 2000, 2000, 2000, 1000, 1000)
xmin <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
xmax <- c(35, 50, 75, 100, 150, 200, 275, 350, 450, 550)
###############################################

inputs <- list()
z <- list()

input <- read.csv(input.names)

for( o in 1:length(output.names) ){
	column.name <- paste0("z_", draw.layers[o])
	z[[o]] <- input[, column.name]

	png(paste0(output.folder, "/", output.names[o]),
	    units="px", width=1600, height=1600, res=250)
	hist( abs(z[[o]]), breaks=break.hists[o], xlim=c(xmin[o],xmax[o]),
	      xlab="|Z| (mm)", main=paste0("30k hits from 30k tracks (Layer ",
					  draw.layers[o] + 1, ")") )
	dev.off()
}

