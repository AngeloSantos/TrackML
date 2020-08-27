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
q.b <- seq(0.0002, 0.004, 0.0002)
###############################################

px <- input_all[, "px"]
py <- input_all[, "py"]
pt <- sqrt( px^2 + py^2 )

sample_id <- input_train[, "X"]

pt.train <- pt[sample_id]

x <- input_train[, x.columns]
y <- input_train[, y.columns]

library(circular)
library(latex2exp)
library(scales)

total.sum     <- c()
total.sum.avg <- c()

message("* Starting fits...")
for( q in 1:length(q.b) ) {
	message("  * q.B = ", q.b[q])
	
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
	r.fit       <- c()

	for( r in 1:nrow(input_train) ){
		if( r %% 100 == 0 ) cat(".")
	
		x.c[[r]] <- unlist( x[r,], use.names=FALSE)
		y.c[[r]] <- unlist( y[r,], use.names=FALSE)
	
		init.x0 = 0
		init.y0 = 0
		init.r  = pt.train[r] / q.b[q]
	
		init.alpha <- atan( (x.c[[r]][1]  - x.c[[r]][10])/
				    (y.c[[r]][10] - y.c[[r]][1]) )
	
		if( x.c[[r]][7] <= (x.c[[r]][1] + x.c[[r]][10])/2 )
			init.x0 <- x.c[[r]][7] + init.r*cos(init.alpha)
		else
			init.x0 <- x.c[[r]][7] - init.r*cos(init.alpha)
	
		if( ((y.c[[r]][10] + y.c[[r]][7])/2 - y.c[[r]][7]) *
		    (tan(init.alpha)) > 0 )
			init.y0 <- y.c[[r]][7] + init.r*sin(init.x0)
		else
			init.y0 <- y.c[[r]][7] - init.r*sin(init.x0)
	
		fit.circle[[r]] <- lsfit.circle( x.c[[r]], y.c[[r]],
					       	 init=c(init.r, init.x0, init.y0) )
	
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
	cat("---\n")
	
	x.diff  <- list()
	y.diff  <- list()
	xy.diff <- list()

	for( l in layers ){
	
	        x.diff[[l]] <- ( unlist(x.fit[1:nrow(input_train)],
	      				use.names=F)[seq(l,
							 nrow(input_train) *
						         length(layers),
							 length(layers))] -
				 unlist(x.c[1:nrow(input_train)],
					use.names=F)[seq(l,
						         nrow(input_train) *
							 length(layers),
						         length(layers))] )
		y.diff[[l]] <- ( unlist(y.fit[1:nrow(input_train)],
	      				use.names=F)[seq(l,
							 nrow(input_train) *
							 length(layers),
							 length(layers))] -
	       			 unlist(y.c[1:nrow(input_train)],
		  			use.names=F)[seq(l,
						         nrow(input_train) *
							 length(layers),
						         length(layers))] )
	        xy.diff[[l]] <- sqrt( x.diff[[l]]^2 + y.diff[[l]]^2 )
	}
	
	sum.diff.all <- c()
	xy.diff.all  <- unlist(xy.diff, use.names=F)
	for( r in 1:nrow(input_train) ){
	        if( (r %% 100) == 0 ) cat(".")
	        sum.diff = sum( xy.diff.all[seq(r, length(xy.diff.all),
						nrow(input_train))] )
	        sum.diff.all <- c(sum.diff.all, sum.diff)
	}
	cat("\n")
	cat("---\n")
	total.sum     <- c(total.sum,     sum(sum.diff.all))
	total.sum.avg <- c(total.sum.avg, sum(sum.diff.all)/nrow(input_train))
}

message("************************************************")
message("* Results:")
message("  * Minimum sum avg = ", min(total.sum.avg), " mm")
message("  * Best q.B value  = ", q.b[which( total.sum.avg == min(total.sum.avg) )])
message("************************************************")


####################################

png("momentum/fit_2020_06_22/min_q.B.png",
    units="px", width=1600, height=1600, res=250)
plot( q.b, total.sum.avg,
      xlab="Contant [GeV/c/mm]",
      ylab="Average Sum [mm]",
      main=sprintf("%dk Tracks", nrow(input_train)/1000),
      col="blue", pch=18, type="p" )
grid(col="gray48")
abline( v=q.b[which( total.sum.avg == min(total.sum.avg) )],
        col="forestgreen", lty=5, lwd=2 )
legend( "topright", legend=c(TeX(sprintf("$Avg Sum = \\frac{\\sum_{Tracks}\\sum_{Hits} (Hit_{Fitted} - Hit_{Real})}{%d}$", nrow(input_train))),
			     TeX(sprintf("Best Fit Constant = %.4f $\\frac{GeV/c}{mm}$",
				         q.b[which(total.sum.avg ==
						   min(total.sum.avg))]))),
        col=c("blue", "forestgreen"), pch=c(18, NA),
       	lty=c(NA, 5), lwd=c(NA, 2), bty="n" )
dev.off()

stop()

message("* Radius results:")
print(summary(r.fit))

#####################################################
r.fit1000 <- (r.fit/1000)
png("momentum/fit_2020_06_21/pt_vs_R.png",
    units="px", width=1600, height=1600, res=250)
plot(r.fit1000, pt.train,
     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
     main="21k Tracks",
     #xlim=c(0.5,5.5),
     col="blue", pch=1)
pt_R.fit <- lm( pt.train ~ r.fit1000 )
abline( pt_R.fit, col="red", lwd=2 )
grid(col="gray48")
legend( "topleft", legend=c("Training Sample",
			    sprintf("pT(R) = %.2f + %.2f.R",
				       	pt_R.fit$coefficients[1],
				       	pt_R.fit$coefficients[2])),
        lwd=c(NA, 2), pch=c(1, NA), col=c("blue", "red"), border=NA, bg="white" )
dev.off()

#####################################################

png("momentum/fit_2020_06_21/hist_R.png",
    units="px", width=1600, height=1600, res=250)
hist(r.fit, breaks=2000,
     xlim=c(500,10000),
     #ylim=c(0,600),
     xlab="Radius of Track Curvature [mm]",
     main="21k Tracks",
     col="red")
dev.off()

#####################################################

#png("momentum/fit_2020_06_21/hist_pt_1-4.png",
#    units="px", width=1600, height=1600, res=250)
#pt.hist <- hist(pt.train, breaks=1000)
#plot(pt.hist$mids[pt.hist$counts > 0], pt.hist$counts[pt.hist$counts > 0],
#     xlim=c(1,4),
#     #ylim=c(1, max(pt.hist$counts)),
#     xlab="Transverse Momentum [GeV/c]", ylab="Frequency",
#     main="21k Tracks",
#     col="blue", type="h", lwd=2, log="")
#dev.off()

#####################################################
x.diff <- list()
x.diff.hist <- list()
x.diff.min = -0.4
x.diff.max = 0.8
x.diff.breaks = c(seq(x.diff.min,x.diff.max,0.02))
png("momentum/fit_2020_06_21/hist_x_difference.png",
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
png("momentum/fit_2020_06_21/hist_y_difference.png",
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
png("momentum/fit_2020_06_21/hist_xy_difference.png",
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

legend( "topright", legend=sprintf( "Layer %2d, (sd, BW) = (%2.1f, %1.2f) mm",
				    layers, sd.xy.diff, bw.xy.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bty="n" )
dev.off()

#####################################################
count.diff <- data.frame()
hit.diff <- c(seq(0.05,60,0.05), seq(70, 100, 5))
png("momentum/fit_2020_06_21/count_fits_outside_diff.png",
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
#> which(xy.diff[[layers[1]]] > 200)
# 106  3218  4489 10814 14090 20627
#> which(xy.diff[[layers[2]]] > 200)
# 106  3218  4489 10814 14090 20627
#> which(xy.diff[[layers[3]]] > 200)
# 3218 10814 14090
#> which(xy.diff[[layers[4]]] > 200)
# 10814 14090
#> which(xy.diff[[layers[8]]] > 200)
# 12088
#> which(xy.diff[[layers[9]]] > 200)
# 106  3218  4489 10814 14090 20627
#> which(xy.diff[[layers[10]]] > 200)
# 106  1731  3218  4489  5148  9567 10814 14090 20057 20627
#
# train ID
# 106  1731  3218  4489  5148  9567 10814 12088 14090 20057 20627
# pt
# 
####

#high.diff <- c(1548, 1641, 7689, 8697, 9954, 12088, 13339, 15209, 16472, 20367, 20632)
high.diff <- c(106, 1731, 3218, 4489, 5148, 9567, 10814, 12088, 14090, 20057, 20627)
png("momentum/fit_2020_06_21/fit.circle_high.diff.2.png",
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

png("momentum/fit_2020_06_21/fit.circle_high.pt.png",
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
	sum.diff.all <- c(sum.diff.all, sum.diff)
}
cat("\n")

#png("momentum/fit_2020_06_21/sum.all.diff.png",
png("momentum/fit_2020_06_21/sum.all.diff_0-20_mm.png",
    units="px", width=1600, height=1600, res=250)
hist( sum.diff.all, breaks=10000,
      xlim=c(0,20),
      #ylim=c(0,2000),
      xlab=TeX("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real}) \\; \\[mm\\]$"),
      ylab="Frequency",
      main="21k Tracks", col="blue", border=NA )
dev.off()
#####################################################
sum.diff.cut = 25
lower.pt <- c(which(sum.diff.all < sum.diff.cut))

png("momentum/fit_2020_06_21/pt_vs_R_all.pT_and_lower.pT.png",
    units="px", width=1600, height=1600, res=250)
plot(r.fit1000, pt.train,
     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
     main=TeX(sprintf("Before and After $\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
     #xlim=c(0.5,5.5),
     #ylim=c(0.5,3),
     col="blue", pch=1)
points(r.fit1000[lower.pt], pt.train[lower.pt], col="green", pch=1)
pt_R.fit.lower.pt <- lm( pt.train[lower.pt] ~ (r.fit1000[lower.pt]) )
abline( pt_R.fit.lower.pt, col="red", lwd=2 )
grid(col="gray48")
legend( "topleft", legend=c("Training Sample", TeX(sprintf("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
                            sprintf("pT(R) = %.2f + %.2f.R",
                                        pt_R.fit.lower.pt$coefficients[1], 
                                        pt_R.fit.lower.pt$coefficients[2])),
        lwd=c(NA,NA,2), pch=c(1,1,NA), col=c("blue","green","red") )
dev.off()

#####################################################
#png("momentum/fit_2020_06_21/pt_vs_R_lower.pT.png",
#    units="px", width=1600, height=1600, res=250)
#plot(r.fit1000[lower.pt], pt.train[lower.pt],
#     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
#     main="10k Tracks",
#     xlim=c(0.5,8),
#     ylim=c(0.5,5),
#     col="forestgreen", pch=1)
#pt_R.fit.lower.pt <- lm( pt.train[lower.pt] ~ (r.fit1000[lower.pt]) )
#abline( pt_R.fit.lower.pt, col="red", lwd=2 )
#grid(col="gray48")
#legend( "topleft", legend=c(TeX(sprintf("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
#                            sprintf("pT(R) = %.1f + %.1f.R",
#                                        pt_R.fit.lower.pt$coefficients[1],
#                                        pt_R.fit.lower.pt$coefficients[2])),
#        lwd=c(NA,2), pch=c(1,NA), col=c("forestgreen","red"), border=NA, bg="white" )
#dev.off()
#
#####################################################
#r.breaks <- seq(1500, 3500, 20)
r.breaks <- seq(500, 10000, 100)
png("momentum/fit_2020_06_21/hist_R_lower.pT.png",
    units="px", width=1600, height=1600, res=250)
hist(r.fit[(r.fit > min(r.breaks)) & (r.fit < max(r.breaks))],
     breaks=r.breaks,
     xlab="Radius of Track Curvature [mm]",
     xlim=c(1500,10000),
     #ylim=c(0,600),
     main=TeX("Before and After Cut in $\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$"),
     col=alpha(rgb.color[[1]], 1))
hist(r.fit[(r.fit > min(r.breaks)) & (r.fit < max(r.breaks))][lower.pt],
      breaks=r.breaks,
      col=alpha(rgb.color[[3]], 0.7), add=TRUE)
legend( "topright", legend=c("21k Tracks", "13k Tracks"),
        fill=c(alpha(rgb.color[[1]], 1), alpha(rgb.color[[3]], 0.7)) )
dev.off()

#####################################################
png("momentum/fit_2020_06_21/hist_all.pt_and_low.pt.png",
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
       col="blue", type="h", lwd=5, lty=1)
legend( "topright", legend=c("21k Tracks",
			    TeX(sprintf("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut))), col=c("red","blue"), lwd=5, lty=c(1,1) )
dev.off()
#####################################################

####
####
####
