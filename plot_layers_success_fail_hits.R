#######################################

input.least.name = "figures/application_2020_05_17/leastDist.near_real.csv"
input.near.name  = "figures/application_2020_05_17/nearest.hits.csv"

output.folder = "figures/application_2020_05_17/layers_success_fail_20200617"
output.name   = "layer"

chosen_layers = c(5:10)

# Layers     5    6    7    8    9   10
xmin <- c(-150,-200,-300,-350,-450,-600)
xmax <- c( 150, 200, 300, 350, 450, 600)

########################################

library(latex2exp)

message("**********************************")
message("* Reading input files ...")
input.least <- read.csv(input.least.name)
input.near  <- read.csv(input.near.name)

message("  * File: ", input.least.name)
message("    * Dimensions: ",
        nrow(input.least), " rows and ",
        ncol(input.least), " columns")

message("  * File: ", input.near.name)
message("    * Dimensions: ",
        nrow(input.near), " rows and ",
        ncol(input.near), " columns")

# s -> success
x.s <- list() # x
y.s <- list() # y
z.s <- list() # z
p.s <- list() # phi
# f -> fail
x.f <- list() # x
y.f <- list() # y
z.f <- list() # z
p.f <- list() # phi

message("**********************************")
message("* Reading values of(x,y,z) coordinates ...")
cat("  * Layer ")
for( l in 1:length(chosen_layers) ){
	cat(chosen_layers[l], " ... ")

	# Check tracks which were reconstructed successfully or not
	success <- which( input.least[,l] == 0 )
	fail    <- which( input.least[,l] >  0 )

	# Read coordinate values for cases of success
	coord.name <- paste0("x_",   chosen_layers[l])
	x.s[[l]] <- input.near[success, coord.name]
	coord.name <- paste0("y_",   chosen_layers[l])
        y.s[[l]] <- input.near[success, coord.name]
	coord.name <- paste0("z_",   chosen_layers[l])
        z.s[[l]] <- input.near[success, coord.name]
        coord.name <- paste0("phi_", chosen_layers[l])
        p.s[[l]] <- input.near[success, coord.name]

	# Read coordinate values for cases of success
        coord.name <- paste0("x_",   chosen_layers[l])
        x.f[[l]] <- input.near[fail, coord.name]
        coord.name <- paste0("y_",   chosen_layers[l])
        y.f[[l]] <- input.near[fail, coord.name]
        coord.name <- paste0("z_",   chosen_layers[l])
        z.f[[l]] <- input.near[fail, coord.name]
	coord.name <- paste0("phi_", chosen_layers[l])
        p.f[[l]] <- input.near[fail, coord.name]
}
cat("\n")

message("**********************************")
message("* Create figures with 3pi/2 < phi < pi/2 ...")
for( l in 1:length(chosen_layers) ){
	output.file <- paste0(output.folder, "/", output.name, "_",
			      chosen_layers[l], "_3pi2_phi_pi2.png")
	message("  * File: ", output.file)

	png(output.file, units="px", width=1600, height=1600, res=250)
	plot( z.s[[l]][(p.s[[l]] >= -pi/2) & (p.s[[l]] < pi/2)],
	      y.s[[l]][(p.s[[l]] >= -pi/2) & (p.s[[l]] < pi/2)],
	      xlab="z (mm)", ylab="y (mm)",
	      xlim=c(xmin[l],xmax[l]),
	      main=TeX(sprintf("30k Hits from 30k Tracks (Layer %d , $3\\pi/2 < \\Phi < \\pi/2$)", chosen_layers[l])),
	      pch=".", col="green" )
	points( z.f[[l]][(p.f[[l]] >= -pi/2) & (p.f[[l]] < pi/2)],
	        y.f[[l]][(p.f[[l]] >= -pi/2) & (p.f[[l]] < pi/2)],
	        pch=".", col="red" )
}
dev.off()

message("**********************************")
message("* Create figures with pi/2 < phi < 3pi/2 ...")
for( l in 1:length(chosen_layers) ){
        output.file <- paste0(output.folder, "/", output.name, "_",
                              chosen_layers[l], "_pi2_phi_3pi2.png")
        message("  * File: ", output.file)

        png(output.file, units="px", width=1600, height=1600, res=250)
        plot( z.s[[l]][(p.s[[l]] >= pi/2) | (p.s[[l]] < -pi/2)],
              y.s[[l]][(p.s[[l]] >= pi/2) | (p.s[[l]] < -pi/2)],
              xlab="z (mm)", ylab="y (mm)",
              xlim=c(xmin[l],xmax[l]),
	      main=TeX(sprintf("30k Hits from 30k Tracks (Layer %d , $\\pi/2 < \\Phi < 3\\pi/2$)", chosen_layers[l])),
              pch=".", col="green" )
        points( z.f[[l]][(p.f[[l]] >= pi/2) | (p.f[[l]] < -pi/2)],
	        y.f[[l]][(p.f[[l]] >= pi/2) | (p.f[[l]] < -pi/2)],
                pch=".", col="red" )
}
dev.off()

message("**********************************")
message("* Done!")
message("**********************************")


