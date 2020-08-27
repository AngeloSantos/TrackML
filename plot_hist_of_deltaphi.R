###############################################
input.names = "data/eta_n0.5-0.5_phi_ninf-pinf.csv"

output.folder = "data/figures/histograms_2020_06_16"

output.names = "hist_DeltaPhi.png"

draw.layers <- c(0:9) # indeed 1 to 10
deltaphi <- c(1:9)
break.hists = 5000
xmin = -0.08
xmax =  0.08
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

inputs <- list()
phi.i  <- list()
phi.f  <- list()

input <- read.csv(input.names)


png(paste0(output.folder, "/", output.names),
    units="px", width=1600, height=1600, res=250)

for( o in 1:9 ){
	column.name <- paste0("phi_", draw.layers[o])
	phi.i[[o]] <- input[, column.name]
	column.name <- paste0("phi_", draw.layers[o]+1)
	phi.f[[o]] <- input[, column.name]

	if( o == 1 ){
		hist( (phi.f[[o]] - phi.i[[o]]),
		      breaks=break.hists,
		      xlim=c(xmin,xmax),
		      col=alpha(rgb.color[[o]], h.scale),
		      border=NA,
		      xlab=TeX("$\\Delta\\Phi \\; \\[rad\\]$"),
		      main=TeX("$\\Delta\\Phi$ between Two Subsequent Layers") )
	}
	else{
        	hist( (phi.f[[o]] - phi.i[[o]]),
        	      breaks=break.hists,
		      col=alpha(rgb.color[[o]], h.scale),
		      border=NA,
        	      add=TRUE )
	}
#	dev.off()
}

legend( "topright", legend=TeX(sprintf("Layers %d and %d",
				       deltaphi, deltaphi+1)),
        fill=c( alpha(rgb.color, h.scale) ), border=NA, bty="n" )
dev.off()


