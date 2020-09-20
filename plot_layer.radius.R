################

input.name    = "data/eta_n0.5-0.5_phi_ninf-pinf.csv"
output.folder = "data/figures/histograms_2020_09_19"
output.names  = "radius_per_layer.png"

xmin = 0
xmax = 1050
x.step = 2
break.hist = seq(xmin, xmax, x.step)

input <- read.csv(input.name)
var.names <- paste0("rho_", c(0:9))

library(latex2exp)



png(paste0(output.folder, "/", output.names),
    units="px", width=2500, height=800, res=200)

hist( unlist(input[, var.names], use.names=F),
      breaks = break.hist,
      xlab = "Radius [mm]",
      ylab = sprintf("Frequency / %d [mm]", x.step),
      main = "Radius from All 10 Layers (30k Tracks)",
      col = "blue",
      border=NA )

dev.off()

