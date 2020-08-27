#################################################

input_all <- read.csv("data/eta_n0.5-0.5_phi_ninf-pinf.csv")
input_train <- read.csv("data/training_sample-05_05.csv")

#chosen_sample_id <- c(3177, 12115, 12612, 14010, 28518, 29804)

layers <- c(1:10)

break.hists = 100

x.columns <- c("x_0", "x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9")
y.columns <- c("y_0", "y_1", "y_2", "y_3", "y_4", "y_5", "y_6", "y_7", "y_8", "y_9")

rgb.color <- list( rgb(255,   0,   0, maxColorValue=255), # red
                   rgb(  0, 100,   0, maxColorValue=255), # dark green
                   rgb(  0,   0, 255, maxColorValue=255), # blue
                   rgb(255, 255,   0, maxColorValue=255), # yellow
                   rgb(  0, 128, 128, maxColorValue=255), # teal
                   rgb(255,   0, 255, maxColorValue=255), # magenta
                   rgb(128,   0,   0, maxColorValue=255), # maroon
                   rgb(128, 128,   0, maxColorValue=255), # olive
                   rgb(119, 135, 153, maxColorValue=255), # light state gray
		   rgb(  0, 255, 255, maxColorValue=255) )# aqua

h.scale = 0.7
###############################################

px <- input_all[, "px"]
py <- input_all[, "py"]
pt <- sqrt( px^2 + py^2 )

sample_id <- input_train[, "X"]

x <- input_train[, x.columns]
y <- input_train[, y.columns]

library(circular)
library(latex2exp)
library(scales)

x.c         <- list()
y.c         <- list()
x.fit       <- list()
y.fit       <- list()
x0.fit      <- list()
y0.fit      <- list()
fit.cos     <- list()
fit.sin     <- list()
fit.xy.dist <- list()
fit.circle  <- list()

r.fit    <- c()

message("* Starting fits...")
for( r in 1:nrow(input_train) ){
	if( r %% 100 == 0 ) cat(".")

	x.c[[r]] <- unlist( x[r,], use.names=FALSE)
	y.c[[r]] <- unlist( y[r,], use.names=FALSE)

	fit.circle[[r]] <- lsfit.circle(x.c[[r]], y.c[[r]])

	x0.fit[[r]] <- fit.circle[[r]]$coefficients[2]
	y0.fit[[r]] <- fit.circle[[r]]$coefficients[3]

	fit.xy.dist[[r]] <- sqrt( (x.c[[r]] - x0.fit[[r]])^2 +
	 			  (y.c[[r]] - y0.fit[[r]])^2 ) 
	fit.cos[[r]] <- (x.c[[r]] - x0.fit[[r]]) / fit.xy.dist[[r]]
	fit.sin[[r]] <- (y.c[[r]] - y0.fit[[r]]) / fit.xy.dist[[r]]

	r.fit <- c(r.fit, fit.circle[[r]]$coefficients[1])

	x.fit[[r]] <- x0.fit[[r]] + r.fit[r]*fit.cos[[r]]
	y.fit[[r]] <- y0.fit[[r]] + r.fit[r]*fit.sin[[r]]
}
cat("\n")

pt.train <- pt[sample_id]

message("* Radius results:")
print(summary(r.fit))

#####################################################
r.fit1000 <- (r.fit/1000)
png("momentum/fit_2020_06_10/pt_vs_R.png",
    units="px", width=1600, height=1600, res=250)
plot(r.fit1000, pt.train,
     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
     main="21k Tracks",
     xlim=c(0.5,5.5),
     col="blue", pch=1)
pt_R.fit <- lm( pt.train ~ r.fit1000 )
abline( pt_R.fit, col="red", lwd=2 )
grid(col="gray48")
legend( "topleft", legend=c("Training Sample",
			    sprintf("pT(R) = %.1f + %.1f.R",
				       	pt_R.fit$coefficients[1],
				       	pt_R.fit$coefficients[2])),
        lwd=c(NA, 2), pch=c(1, NA), col=c("blue", "red"), border=NA, bg="white" )
dev.off()

#####################################################

png("momentum/fit_2020_06_10/hist_R.png",
    units="px", width=1600, height=1600, res=250)
hist(r.fit, breaks=200,
     xlim=c(1500,3500),
     ylim=c(0,600),
     xlab="Radius of Track Curvature [mm]",
     main="21k Tracks",
     col="red")
dev.off()

#####################################################

png("momentum/fit_2020_06_10/hist_pt_1-4.png",
    units="px", width=1600, height=1600, res=250)
pt.hist <- hist(pt.train, breaks=1000)
plot(pt.hist$mids[pt.hist$counts > 0], pt.hist$counts[pt.hist$counts > 0],
     xlim=c(1,4),
     #ylim=c(1, max(pt.hist$counts)),
     xlab="Transverse Momentum [GeV/c]", ylab="Frequency",
     main="21k Tracks",
     col="blue", type="h", lwd=2, log="")
dev.off()

#####################################################
x.diff <- list()
x.diff.hist <- list()
x.diff.min = -0.4
x.diff.max = 0.8
x.diff.breaks = c(seq(x.diff.min,x.diff.max,0.02))
png("momentum/fit_2020_06_10/hist_x_difference.png",
    units="px", width=1600, height=1600, res=250)
for( l in layers ){
        x.diff[[l]] <- unlist(x.fit[1:nrow(input_train)],
			      use.names=F)[seq(l,nrow(input_train)*length(layers),
					   length(layers))] -
                       unlist(x.c[1:nrow(input_train)],
			      use.names=F)[seq(l,nrow(input_train)*length(layers),
					   length(layers))]
	x.diff.hist[[l]] <- hist( x.diff[[l]][(x.diff[[l]] < x.diff.max) &
				              (x.diff[[l]] > x.diff.min)],
				  breaks=x.diff.breaks, plot=F )
	if( l == 1 ){
		plot( x.diff.hist[[l]]$mids, x.diff.hist[[l]]$counts,
		      xlab=TeX("$(x_{Fitted} - x_{Real}) \\; \\[mm\\]$"),
		      ylab="Frequency",
                      main="21k Tracks",
		      ylim=c(0,2800), lwd=2, col=rgb.color[[l]], type="l")
	}
	else{
		lines( x.diff.hist[[l]]$mids, x.diff.hist[[l]]$counts,
		       lwd=2, col=rgb.color[[l]], type="l")
	}
}
sd.x.diff <- c()
for( l in layers ) { sd.x.diff <- c(sd.x.diff, sd(x.diff[[l]])) }
bw.x.diff <- c()
for( l in layers ) { bw.x.diff <- c(bw.x.diff, bw.nrd(x.diff[[l]])) }
legend( "topright", legend=sprintf( "Layer %2d, (sd, BW) = (%1.1f, %1.2f) mm",
				       layers, sd.x.diff, bw.x.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bty="n" )
dev.off()
#####################################################
y.diff <- list()
y.diff.hist <- list()
y.diff.min = -0.4
y.diff.max = 0.8
y.diff.breaks = c(seq(y.diff.min,y.diff.max,0.02))
png("momentum/fit_2020_06_10/hist_y_difference.png",
    units="px", width=1600, height=1600, res=250)
for( l in layers ){
        y.diff[[l]] <- unlist(y.fit[1:nrow(input_train)],
                              use.names=F)[seq(l,nrow(input_train)*length(layers),
                                           length(layers))] -
                       unlist(y.c[1:nrow(input_train)],
                              use.names=F)[seq(l,nrow(input_train)*length(layers),
                                           length(layers))]
        y.diff.hist[[l]] <- hist( y.diff[[l]][(y.diff[[l]] < y.diff.max) &
                                              (y.diff[[l]] > y.diff.min)],
                                  breaks=y.diff.breaks, plot=F )
        if( l == 1 ){
                plot( y.diff.hist[[l]]$mids, y.diff.hist[[l]]$counts,
                      xlab=TeX("$(y_{Fitted} - y_{Real}) \\; \\[mm\\]$"),
                      ylab="Frequency",
                      main="21k Tracks",
                      ylim=c(0,2800), lwd=2, col=rgb.color[[l]], type="l")
        }
        else{
                lines( y.diff.hist[[l]]$mids, y.diff.hist[[l]]$counts,
                       lwd=2, col=rgb.color[[l]], type="l")
        }
}
sd.y.diff <- c()
for( l in layers ) { sd.y.diff <- c(sd.y.diff, sd(y.diff[[l]])) }
bw.y.diff <- c()
for( l in layers ) { bw.y.diff <- c(bw.y.diff, bw.nrd(y.diff[[l]])) }
legend( "topright", legend=sprintf( "Layer %2d, (sd, BW) = (%1.1f, %1.2f) mm",
                                       layers, sd.y.diff, bw.y.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bty="n" )
dev.off()
#####################################################
xy.diff <- list()
xy.diff.hist <- list()
xy.diff.xmax = 1
xy.diff.breaks = c(seq(0,xy.diff.xmax,0.04))
png("momentum/fit_2020_06_10/hist_xy_difference.png",
    units="px", width=1600, height=1600, res=250)
for( l in layers ){
        xy.diff[[l]] <- sqrt( x.diff[[l]]^2 + y.diff[[l]]^2 )
	xy.diff.hist[[l]] <- hist( xy.diff[[l]][xy.diff[[l]] < xy.diff.xmax],
				   breaks=xy.diff.breaks, plot=F )
        if( l == 1 ){
		plot( xy.diff.hist[[l]]$mids,
		      xy.diff.hist[[l]]$counts,
		      xlab=TeX("$(Hit_{Fitted} - Hit_{Real}) \\; \\[mm\\]$"),
                      ylab="Frequency",
                      xlim=c(min(xy.diff.breaks),max(xy.diff.breaks)),
                      ylim=c(0,6000),
                      main=TeX("Distance between $Hit_{Fitted}$ and $Hit_{Real}$ in XY Plane"),
                      col=alpha(rgb.color[[l]], 1), lwd=2, type="l" )
        }
        else{
		lines(xy.diff.hist[[l]]$mids,
                      xy.diff.hist[[l]]$counts,
                      col=alpha(rgb.color[[l]], 1),
                      lwd=2, type="l")
        }
}

sd.xy.diff <- c()
for( l in layers ) sd.xy.diff <- c(sd.xy.diff, sd(xy.diff[[l]]))
bw.xy.diff <- c()
for( l in layers ) bw.xy.diff <- c(bw.xy.diff, bw.nrd(xy.diff[[l]]))

legend( "topright", legend=sprintf( "Layer %2d, (sd, BW) = (%1.1f, %1.2f) mm",
				    layers, sd.xy.diff, bw.xy.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bty="n" )
dev.off()

#####################################################
count.diff <- data.frame()
hit.diff <- seq(0.05,60,0.05)
png("momentum/fit_2020_06_10/count_fits_outside_diff.png",
    units="px", width=1600, height=1600, res=250)
for( l in 1:length(layers) ){
	for( h.d in 1:length(hit.diff) ){
		count.diff[l,h.d] <- length(which(xy.diff[[layers[l]]] > hit.diff[h.d]))
	}
	if( l == 1 ){
                plot( hit.diff[count.diff[l,] > 0],
		      unlist(count.diff[l,][count.diff[l,] > 0],use.names=F),
                      xlab=TeX("$(Hit_{Fitted} - Hit_{Real}) \\; \\[mm\\]$"),
                      ylab="Frequency",
                      xlim=c(min(hit.diff),max(hit.diff)),
                      ylim=c(0,21000),
                      main=TeX("Number of Fits Out of Difference $(Hit_{Fitted} - Hit_{Real})$"),
                      col=alpha(rgb.color[[l]], 1), lwd=2, type="l", log="x" )
        }
        else{
                lines(hit.diff[count.diff[l,] > 0],
                      unlist(count.diff[l,][count.diff[l,] > 0],use.names=F),
                      col=alpha(rgb.color[[l]], 1), lwd=2, type="l", log="x")
        }
}

grid(col="gray48")

legend( "topright", legend=sprintf( "Layer %2d", layers, sd.xy.diff, bw.xy.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bg="white")
dev.off()
#####################################################

message("******************************************")
dist.2d <- c(0.05,0.1,0.2,0.5,1,2,5,10,20,30)
cat("D \t"); for( d in dist.2d ){ cat(d, "\t")}; cat("\n")
cat("\n")
for( l in layers ){
	cat("L ", l, "\t")
	for( d in dist.2d ){
		cat(length(which(xy.diff[[layers[l]]] > d)), "\t")
	}
	cat("\n")
}
message("******************************************")
#####################################################
#### (i.e, > 3 cm)
#> which(xy.diff[[layers[1]]] > 30)
#[1]  8697  9954 12088 20367
#> which(xy.diff[[layers[6]]] > 30)
#[1] 12088 16472 20367
#> which(xy.diff[[layers[7]]] > 30)
#[1]  7689 15209
#> which(xy.diff[[layers[8]]] > 30)
#[1]  1548  1641  8697  9954 12088 13339 20367 20632
#> which(xy.diff[[layers[10]]] > 30)
#[1]  8697  9954 12088 13339 20367 20632
#
# train ID
# 1548 1641 7689 8697 9954 12088 13339 15209 16472 20367 20632
# 8    8    7    1    1    1     8     7     6     1     8
#                8    8    6     10                6     10
#                10   10   8                       8
#                          10                      10
# pt
# 8.215296  5.849100  9.994519  1.156115  1.254458  1.080766  9.994519 9.994519  9.994519  1.094807 13.632988
####

high.diff <- c(1548, 1641, 7689, 8697, 9954, 12088, 13339, 15209, 16472, 20367, 20632)
png("momentum/fit_2020_06_10/fit.circle_high.diff.png",
    units="px", width=1600, height=1600, res=250)
plot(unlist(x.c, use.names=F), unlist(y.c, use.names=F),
     xlab="x (mm)", ylab="y (mm)",
     main=TeX("Fitting Tracks with ($Hit_{Fitted} - Hit_{Real}$) > 30 mm"),
     col="grey", pch=".")

for( c in 1:length(high.diff) ){
        points(  x.c[[high.diff[c]]],   y.c[[high.diff[c]]],
	         col="red", lty=1, pch=20, type="b" )
        points(x.fit[[high.diff[c]]], y.fit[[high.diff[c]]],
	       col="blue",pch=1)
        lines( x.fit[[high.diff[c]]], y.fit[[high.diff[c]]],
	       col="blue",lty=2,type="l")
}

legend( "topleft", legend=c("Chosen Tracks", "Hits from Fit", "Circular Fit"),
        lty = c(1, NA, 2), col = c("red", "blue", "blue"), pch = c(20, 1, NA) )
dev.off()
#####################################################
# sample(which( pt.train > 10 ), 10)
high.pt <- c(1547, 4490, 5523, 7679, 11366, 13316, 14366, 15184, 16079, 17866, 20627)

png("momentum/fit_2020_06_10/fit.circle_high.pt.png",
    units="px", width=1600, height=1600, res=250)
plot(unlist(x.c, use.names=F), unlist(y.c, use.names=F),
     xlab="x (mm)", ylab="y (mm)",
     main=TeX("Fitting Tracks ($p_{T}$ > 10 GeV/c)"),
     col="grey", pch=".")

for( c in 1:length(high.pt) ){
        points(  x.c[[high.pt[c]]],   y.c[[high.pt[c]]],
	         col="red",  pch=20, lty=1, type="b" )
        points(x.fit[[high.pt[c]]], y.fit[[high.pt[c]]],
	       col="blue", pch=1)
        lines( x.fit[[high.pt[c]]], y.fit[[high.pt[c]]],
	       col="blue", lty=2, type="l" )
}

legend( "bottomleft", legend=c("Chosen Tracks", "Hits from Fit", "Circular Fit"),
        lty = c(1, NA, 2), col = c("red", "blue", "blue"), pch = c(20, 1, NA) )
dev.off()
#####################################################
message("***********************************")
cat("Layer\t") ; for( l in layers ){ cat(l, "\t") } ; cat("\n")
cat("Track ID\n")
for( i in 1:length(high.pt) ){
	cat(high.pt[i], "\t")
	for( l in layers ) cat(sprintf("%.1f", xy.diff[[l]][high.pt[i]]), "\t")
	cat("\n")
}
message("***********************************")
#####################################################
sum.diff.all <- c()
xy.diff.all  <- unlist(xy.diff,use.names=F)
for( r in 1:nrow(input_train) ){
	if( (r %% 100) == 0 ) cat(".")
	sum.diff = sum( xy.diff.all[seq(r,length(xy.diff.all),nrow(input_train))] )
#	lower.pt <- c(lower.pt, which( xy.diff[[layers[l]]] < 10 ))
	sum.diff.all <- c(sum.diff.all, sum.diff)
}
cat("\n")

png("momentum/fit_2020_06_10/sum.all.diff_0-20_mm.png",
    units="px", width=1600, height=1600, res=250)
hist( sum.diff.all, breaks=2000,
      xlim=c(0,20),
      ylim=c(0,2000),
      xlab=TeX("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real}) \\; \\[mm\\]$"),
      ylab="Frequency",
      main="21k Tracks", col="blue", border=NA )
dev.off()
#####################################################
sum.diff.cut = 3
lower.pt <- c(which(sum.diff.all < sum.diff.cut))

png("momentum/fit_2020_06_10/pt_vs_R_all.pT_and_lower.pT.png",
    units="px", width=1600, height=1600, res=250)
plot(r.fit1000, pt.train,
     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
     main=TeX(sprintf("Before and After $\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
     xlim=c(0.5,5.5),
     #ylim=c(0.5,3),
     col="blue", pch=1)
points(r.fit1000[lower.pt], pt.train[lower.pt], col="green", pch=1)
pt_R.fit.lower.pt <- lm( pt.train[lower.pt] ~ (r.fit1000[lower.pt]) )
abline( pt_R.fit.lower.pt, col="red", lwd=2 )
grid(col="gray48")
legend( "topleft", legend=c("Training Sample", TeX(sprintf("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
                            sprintf("pT(R) = %.1f + %.1f.R",
                                        pt_R.fit.lower.pt$coefficients[1], 
                                        pt_R.fit.lower.pt$coefficients[2])),
        lwd=c(NA,NA,2), pch=c(1,1,NA), col=c("blue","green","red") )
dev.off()

#####################################################
png("momentum/fit_2020_06_10/pt_vs_R_lower.pT.png",
    units="px", width=1600, height=1600, res=250)
plot(r.fit1000[lower.pt], pt.train[lower.pt],
     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
     main="10k Tracks",
     xlim=c(0.5,5.5),
     ylim=c(0.5,3),
     col="forestgreen", pch=1)
pt_R.fit.lower.pt <- lm( pt.train[lower.pt] ~ (r.fit1000[lower.pt]) )
abline( pt_R.fit.lower.pt, col="red", lwd=2 )
grid(col="gray48")
legend( "topleft", legend=c(TeX(sprintf("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
                            sprintf("pT(R) = %.1f + %.1f.R",
                                        pt_R.fit.lower.pt$coefficients[1],
                                        pt_R.fit.lower.pt$coefficients[2])),
        lwd=c(NA,2), pch=c(1,NA), col=c("forestgreen","red"), border=NA, bg="white" )
dev.off()

#####################################################
r.breaks <- seq(1500, 3500, 20)
png("momentum/fit_2020_06_10/hist_R_lower.pT.png",
    units="px", width=1600, height=1600, res=250)
#r.fit.hist <- hist(r.fit, plot=F)
hist(r.fit[(r.fit > min(r.breaks)) & (r.fit < max(r.breaks))],
     breaks=r.breaks,
     xlab="Radius of Track Curvature [mm]",
     xlim=c(1500,3500),
     ylim=c(0,600),
     main=TeX("Before and After Cut in $\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$"),
     col=alpha(rgb.color[[1]], 1))
#r.fit.low.pt.hist <- hist(r.fit[lower.pt], plot=F)
hist(r.fit[(r.fit > min(r.breaks)) & (r.fit < max(r.breaks))][lower.pt],
      breaks=r.breaks,
      col=alpha(rgb.color[[3]], 0.7), add=TRUE)
legend( "topright", legend=c("21k Tracks", "10k Tracks"),
        fill=c(alpha(rgb.color[[1]], 1), alpha(rgb.color[[3]], 0.7)) )
dev.off()

#####################################################
png("momentum/fit_2020_06_10/hist_all.pt_and_low.pt.png",
    units="px", width=1600, height=1600, res=250)
pt.hist <- hist(pt.train, breaks=1000, plot=F)
pt.breaks <- pt.hist$breaks
pt.hist <- hist(pt.train, breaks=pt.breaks, plot=F)
plot(pt.hist$mids[pt.hist$counts > 0], pt.hist$counts[pt.hist$counts > 0],
     xlab="Transverse Momentum [GeV/c]", ylab="Frequency",
     main=TeX("Before and After Cut in $\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$"),
     col="red", type="h", lwd=5, log="xy")
pt.hist.low.pt <- hist(pt.train[lower.pt], breaks=pt.breaks, plot=F)
points(pt.hist.low.pt$mids[pt.hist.low.pt$counts >0],
       pt.hist.low.pt$counts[pt.hist.low.pt$counts >0],
       col="blue", type="h", lwd=5, lty=5)
legend( "topright", legend=c("21k Tracks",
			    TeX(sprintf("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut))), col=c("red","blue"), lwd=5, lty=c(1,5) )
dev.off()
#####################################################

####
####
####
