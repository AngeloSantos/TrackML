#########################333

input_all <- read.csv("data/eta_n0.5-0.5_phi_ninf-pinf.csv")
input_train <- read.csv("data/training_sample-05_05.csv")

chosen_sample_id <- c(3177, 12115, 12612, 14010, 28518, 29804)

x.columns <- c("x_0", "x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9")
y.columns <- c("y_0", "y_1", "y_2", "y_3", "y_4", "y_5", "y_6", "y_7", "y_8", "y_9")

x <- unlist( input_all[, x.columns], use.names=FALSE )
y <- unlist( input_all[, y.columns], use.names=FALSE )


chosen_tracks <- c()
for( c in 1:length(chosen_sample_id) ){
       chosen_tracks <- c( chosen_tracks, 
			   which(input_train[,"X"] == chosen_sample_id[c]) )
}

x.chosen <- input_train[chosen_tracks, x.columns]
y.chosen <- input_train[chosen_tracks, y.columns]

library(circular)

fit.circle <- list()
x.c        <- list()
y.c        <- list()
x.fit      <- list()
y.fit      <- list()

for( c in 1:length(chosen_sample_id) ){
	x.c[[c]] <- unlist( x.chosen[c,], use.names=FALSE)
	y.c[[c]] <- unlist( y.chosen[c,], use.names=FALSE)

	fit.circle[[c]] <- lsfit.circle(x.c[[c]], y.c[[c]])

	x.fit[[c]] <- as.numeric(fit.circle[[c]]$coefficients[2] +
		    		 fit.circle[[c]]$coefficients[1]*cos(fit.circle[[c]]$angles))
	y.fit[[c]] <- as.numeric(fit.circle[[c]]$coefficients[3] +
	    			 fit.circle[[c]]$coefficients[1]*sin(fit.circle[[c]]$angles))
}

png("fit.circle2.png", units="px", width=1600, height=1600, res=250)
plot(x, y, xlab="x (mm)", ylab="y (mm)",
     main="Fitting Tracks",
     col="grey", pch=".")

for( c in 1:length(chosen_sample_id) ){
	points(x.c[[c]], y.c[[c]], col="red", pch=20)
	lines( x.fit[[c]], y.fit[[c]], col="blue", lty=2, type="l" )
}

legend( min(x), max(y),
        c("Chosen Tracks", "Circular Fit"),
	lwd = c(NA, 1), lty = c(NA, 2),
	col = c("red", "blue"), pch = c(20, NA) )
dev.off()




