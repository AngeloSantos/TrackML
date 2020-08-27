#################################################

input_all <- read.csv("data/eta_n0.5-0.5_phi_ninf-pinf.csv")
input_train <- read.csv("data/training_sample-05_05.csv")

x.columns <- c("x_0", "x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9")
y.columns <- c("y_0", "y_1", "y_2", "y_3", "y_4", "y_5", "y_6", "y_7", "y_8", "y_9")

q.b <- c(0.5, 1, 2, 3, 4)

pt_R.colors <- c("blue", "gold", "red", "green", "purple")
###############################################

px <- input_all[, "px"]
py <- input_all[, "py"]
pt <- sqrt( px^2 + py^2 )

sample_id <- input_train[, "X"]
pt.train <- pt[sample_id]

library(latex2exp)

expected.radius <- list()

message("* Starting to compute radius...")
for( q in 1:length(q.b) ){
	expected.radius[[q]] <- ( pt.train / q.b[q] )
}

png("momentum/fit_2020_06_22/pt_vs_R_expected_result.png",
    units="px", width=1600, height=1600, res=250)
plot( expected.radius[[1]], pt.train,
      xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
      main="21k Tracks",
      xlim=c(0.2, 265),
      col=pt_R.colors[1], pch=20, log="xy" )
grid( col="gray48" )

for( q in 2:length(q.b) ){
	points( expected.radius[[q]], pt.train,
	        col=pt_R.colors[q], pch=20, log="xy" )
}

legend( "topleft", legend=c( sprintf("Constant = %.1f GeV/c/m", q.b) ),
	pch=18, col=c(pt_R.colors), bg="white" )
dev.off()




