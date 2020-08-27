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
pt.train <- pt[sample_id]

x <- input_train[, x.columns]
y <- input_train[, y.columns]

#library(pracma)
library(circular)
library(latex2exp)
library(scales)

combinations <- factorial(length(layers)) / ( factorial(3)*factorial(7) )

i.low.R    <- c(1, 1, 1, 1, 1, 1, 1, 1)
j.low.R    <- c(2, 2, 2, 3, 3, 3, 4, 4)
k.low.R    <- c(4, 5, 6, 4, 5, 6, 5, 6)

i.medium.R <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
j.medium.R <- c(3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 9)
k.medium.R <- c(9, 9,10, 9,10, 9,10, 9,10,10,10)

i.high.R   <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
j.high.R   <- c(3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)
k.high.R   <- c(9,10, 9,10, 9,10, 9,10, 9,10, 9,10)

i.index <- list( i.low.R, i.medium.R, i.high.R )
j.index <- list( j.low.R, j.medium.R, j.high.R )
k.index <- list( k.low.R, k.medium.R, k.high.R )

x.c         <- list()
y.c         <- list()
x.fit       <- list()
y.fit       <- list()
#x0.fit      <- list()
#y0.fit      <- list()
fit.cos     <- list()
fit.sin     <- list()
fit.xy.dist <- list()

x0.fit  <- c()
y0.fit  <- c()
r.fit   <- c()
x0.mean <- c()
y0.mean <- c()
r.mean  <- c()

sum.diff.xy.all <- c()

count = 0

message("* Starting fits...")
for( r in 1:nrow(input_train) ){
	if( r %% 100 == 0 ) cat(".")

	x.c[[r]] <- unlist( x[r,], use.names=FALSE)
	y.c[[r]] <- unlist( y[r,], use.names=FALSE)

	###################
	#d2_i.j    <- ( (x.c[[r]][j.high.R] - x.c[[r]][i.high.R])^2 +
	#               (y.c[[r]][j.high.R] - y.c[[r]][i.high.R])^2 )

        #d2_i.k    <- ( (x.c[[r]][k.high.R] - x.c[[r]][i.high.R])^2 +
	#	       (y.c[[r]][k.high.R] - y.c[[r]][i.high.R])^2 )

	#delta.x_i.j <- (x.c[[r]][j.high.R] - x.c[[r]][i.high.R])
	#delta.x_i.k <- (x.c[[r]][k.high.R] - x.c[[r]][i.high.R])

        #delta.y_i.j <- (y.c[[r]][j.high.R] - y.c[[r]][i.high.R])
        #delta.y_i.k <- (y.c[[r]][k.high.R] - y.c[[r]][i.high.R])

	#x0.c <- 0.5*( ((d2_i.k      * delta.y_i.j) - (d2_i.j      * delta.y_i.k)) /
	#	      ((delta.x_i.k * delta.y_i.j) - (delta.x_i.j * delta.y_i.k)) )

	#y0.c <- 0.5*( ((d2_i.k      * delta.x_i.j) - (d2_i.j      * delta.x_i.k)) /
	#	      ((delta.y_i.k * delta.x_i.j) - (delta.y_i.j * delta.x_i.k)) )

	#r.c <- sqrt( (x.c[[r]][i.high.R] - x0.c)^2 + (y.c[[r]][i.high.R] - y0.c)^2 )
	###########################

	min.sum.xy.diff.all <- c()
	for( ijk in 1:length(i.index) ){
		count = count + 1

		i.R <- i.index[[ijk]]
		j.R <- j.index[[ijk]]
		k.R <- k.index[[ijk]]

	        d2_i.j    <- ( (x.c[[r]][j.R] - x.c[[r]][i.R])^2 +
	                       (y.c[[r]][j.R] - y.c[[r]][i.R])^2 )
	
	        d2_i.k    <- ( (x.c[[r]][k.R] - x.c[[r]][i.R])^2 +
	                       (y.c[[r]][k.R] - y.c[[r]][i.R])^2 )
	
	        delta.x_i.j <- (x.c[[r]][j.R] - x.c[[r]][i.R])
	        delta.x_i.k <- (x.c[[r]][k.R] - x.c[[r]][i.R])
	
	        delta.y_i.j <- (y.c[[r]][j.R] - y.c[[r]][i.R])
	        delta.y_i.k <- (y.c[[r]][k.R] - y.c[[r]][i.R])
	
	        x0.c <- 0.5*( ((d2_i.k     *delta.y_i.j) - (d2_i.j     *delta.y_i.k)) /
	                      ((delta.x_i.k*delta.y_i.j) - (delta.x_i.j*delta.y_i.k)) )
	
	        y0.c <- 0.5*( ((d2_i.k     *delta.x_i.j) - (d2_i.j     *delta.x_i.k)) /
	                      ((delta.y_i.k*delta.x_i.j) - (delta.y_i.j*delta.x_i.k)) )
	
	        r.c <- sqrt( (x.c[[r]][i.R] - x0.c)^2 +
			     (y.c[[r]][i.R] - y0.c)^2 )
		
		x0.mean <- c(x0.mean, mean(x0.c))
		y0.mean <- c(y0.mean, mean(y0.c))
		 r.mean <- c( r.mean, mean( r.c))

                fit.xy <- sqrt( (x.c[[r]] - x0.mean[count])^2 +
		      	        (y.c[[r]] - y0.mean[count])^2 )
                fit.cosine <- (x.c[[r]] - x0.mean[count]) / fit.xy
                fit.sine   <- (y.c[[r]] - y0.mean[count]) / fit.xy
                x.all <- x0.mean[count] + r.mean[count]*fit.cosine
                y.all <- y0.mean[count] + r.mean[count]*fit.sine

                diff.xy.all <- sqrt( (x.c[[r]] - x.all)^2 + (y.c[[r]] - y.all)^2 )
		min.sum.xy.diff.all <- c(min.sum.xy.diff.all, sum(diff.xy.all))
                sum.diff.xy.all     <- c(sum.diff.xy.all,     sum(diff.xy.all))
	}

	ijk <- which( min.sum.xy.diff.all == min(min.sum.xy.diff.all) )[1]

  	#x0.fit <- c(x0.fit, mean(x0.mean[count - 3 + ijk]))
   	#y0.fit <- c(y0.fit, mean(y0.mean[count - 3 + ijk]))
 	#r.fit  <- c( r.fit, mean( r.mean[count - 3 + ijk]))

	fit.circle <- lsfit.circle( x.c[[r]], y.c[[r]],
				    init=c(mean( r.mean[count - 3 + ijk]),
					   mean(x0.mean[count - 3 + ijk]),
					   mean(y0.mean[count - 3 + ijk])) )

        r.fit  <- c( r.fit, fit.circle$coefficients[1])
        x0.fit <- c(x0.fit, fit.circle$coefficients[2])
        y0.fit <- c(y0.fit, fit.circle$coefficients[3])
	
		#fit.circle <- circlefit(x.c[[r]], y.c[[r]])
		#x0.fit <- c(x0.fit, fit.circle[1])
		#y0.fit <- c(y0.fit, fit.circle[2])
		#r.fit  <- c(r.fit , fit.circle[3])

		#fit.circle <- lsfit.circle( x.c[[r]],
		#			    y.c[[r]],
		#			    init=c(mean(r.c),
		#				   mean(x0.c),
		#				   mean(y0.c)) )
                #r.fit  <- c( r.fit, fit.circle$coefficients[1])
                #x0.fit <- c(x0.fit, fit.circle$coefficients[2])
                #y0.fit <- c(y0.fit, fit.circle$coefficients[3])
	#}
	#else{
	#	r.fit  <- c( r.fit, mean(r.c))
	#	x0.fit <- c(x0.fit, mean(x0.c))
	#	y0.fit <- c(y0.fit, mean(y0.c))
	#}

	fit.xy.dist[[r]] <- sqrt( (x.c[[r]] - x0.fit[r])^2 +
	 			  (y.c[[r]] - y0.fit[r])^2 ) 
	fit.cos[[r]] <- (x.c[[r]] - x0.fit[r]) / fit.xy.dist[[r]]
	fit.sin[[r]] <- (y.c[[r]] - y0.fit[r]) / fit.xy.dist[[r]]


	x.fit[[r]] <- x0.fit[r] + r.fit[r]*fit.cos[[r]]
	y.fit[[r]] <- y0.fit[r] + r.fit[r]*fit.sin[[r]]
}
cat("\n")

# best.combinations.1 <- c(2, 3, 4, 9, 10, 11, 16, 17)
# best.combinations.2 <- c(14, 20, 21, 25, 26, 29, 30, 32, 33, 35, 36)
# #best.combinations.2.3 <- c(8, 15, 21, 26, 30, 33, 35, 36)
# best.combinations.3 <- c(14, 15, 20, 21, 25, 26, 29, 30, 32, 33, 34, 35)
# x.combination <- c(1:120)
# #png("momentum/fit_2020_06_24_v2/sum.xy.diff_vs_combinations.png",
# png("momentum/fit_2020_06_24_v2/sum.xy.diff_vs_combinations_normalized.png",
# #png("momentum/fit_2020_06_24_v2/sum.xy.diff_vs_combinations_R.lt.5m.png",
# #png("momentum/fit_2020_06_24_v2/sum.xy.diff_vs_combinations_R.gt.5m.lt.20m.png",
# #png("momentum/fit_2020_06_24_v2/sum.xy.diff_vs_combinations_R.gt.20m.png",
#     units="px", width=1600, height=1600, res=250)
# #plot(  x.combination, sum.xxyy.diff.all,
# plot(  x.combination, sum.xxyy.diff.all/nrow(input_train),
#        xlab="Triplet Combination",
#        #ylab="(Fit - Real) [mm]",
#        ylab="(Fit - Real) / Number of Tracks [mm]",
#        #xlim=c(0, 40),
#        #ylim=c(1000, 20000000),
#        ylim=c(10, 1000),
#        #ylim=c(925000, 1060000),
#        #ylim=c(20000, 50000),
#        #ylim=c(1500, 18000),
#        #main=TeX("$\\sum (Hit_{Fitted} - Hit_{Real})$ vs. Triplet Combination"),
#        main=TeX("Avg. of $\\sum (Hit_{Fitted} - Hit_{Real})$ vs. Triplet Combination"),
#        col="red", type="b", pch=17, log="y" )
# grid( col="gray48" )
# #points(c(1:120), sum.xxyy.diff.all.1,
# points(c(1:120), sum.xxyy.diff.all.1/length(which((pt.train/0.0006) <= 5000)),
#        col="green", type="b", pch=20 )
# #abline(v=x.combination[best.combinations.1], lwd=2, lty=5, col="gold")
# #points(c(1:120), sum.xxyy.diff.all.2,
# points(c(1:120), sum.xxyy.diff.all.2/length(which((pt.train/0.0006) > 5000 &
# 						  (pt.train/0.0006) <= 20000)),
#        col="blue", type="b", pch=20 )
# #abline(v=x.combination[best.combinations.2], lwd=2, lty=5, col="gold")
# #points(c(1:120), sum.xxyy.diff.all.3,
# points(c(1:120), sum.xxyy.diff.all.3/length(which((pt.train/0.0006) > 20000)),
#        col="magenta", type="b", pch=20 )
# #abline(v=x.combination[best.combinations.3], lwd=2, lty=5, col="gold")
# #legend( "topleft", legend=c("No Radius Cut",
# #			    "Radius < 5 m",
# #			    "5 < Radius < 20 m",
# #			    "Radius > 20 m"),
# #			    #"Best Hit Combinations"),
# legend( "topleft", legend=c(sprintf("%5dk Tracks (No Radius Cut)",
# 				    nrow(input_train)),
#                             sprintf("%5dk Tracks (Radius < 5 m)",
# 				    length(which((pt.train/0.0006) <= 5000))),
#                             sprintf(" %5dk Tracks (5 < Radius < 20 m)",
# 				    length(which((pt.train/0.0006) > 5000 &
# 						 (pt.train/0.0006) <= 20000))),
#                             sprintf("  %5dk Tracks (Radius > 20 m)",
# 				    length(which((pt.train/0.0006) > 20000)))),
#         lwd=c(1, 1, 1, 1), pch=c(17, 20, 20, 20), lty=c(1, 1, 1, 1),
#         #lwd=c(1, 1, 2), pch=c(20, 20, NA), lty=c(1, 1, 5),
#         #lwd=c(1, 2), pch=c(20, NA), lty=c(1, 5),
# 	col=c("red",
# 	      "green",
# 	      "blue",
#  	      "magenta") )
# 	      #"gold") )
# dev.off()

####### best.combinations.1 (8)
# which(sum.xxyy.diff.all.1 < 940000)
#             x1      x2      x3
#  [2,]       1       2       4
#  [3,]       1       2       5
#  [4,]       1       2       6
#  [9,]       1       3       4
# [10,]       1       3       5
# [11,]       1       3       6
# [16,]       1       4       5
# [17,]       1       4       6
#######

####### best.combinations.2 (11)
# which(sum.xxyy.diff.all.2 < 29000)
#             x1      x2      x3
# [14,]       1       3       9
# [20,]       1       4       9
# [21,]       1       4      10
# [25,]       1       5       9
# [26,]       1       5      10
# [29,]       1       6       9
# [30,]       1       6      10
# [32,]       1       7       9
# [33,]       1       7      10
# [35,]       1       8      10
# [36,]       1       9      10
#######

####### best.combinations.3 (12)
# which(sum.xxyy.diff.all.3 < 2000)
#             x1      x2      x3
# [14,]       1       3       9
# [15,]       1       3      10
# [20,]       1       4       9
# [21,]       1       4      10
# [25,]       1       5       9
# [26,]       1       5      10
# [29,]       1       6       9
# [30,]       1       6      10
# [32,]       1       7       9
# [33,]       1       7      10
# [34,]       1       8       9
# [35,]       1       8      10
#######

stop("******** Stop ************")

message("* Radius results:")
print(summary(r.fit))

#####################################################
r.fit1000 <- (r.fit/1000)
png("momentum/fit_2020_06_24_v2/pt_vs_R_high.R.png",
    units="px", width=1600, height=1600, res=250)
plot(r.fit1000,
     pt.train,
#plot(r.fit1000[r.fit1000 > 0.1 & r.fit1000 < 1000],
#     pt.train[r.fit1000  > 0.1 & r.fit1000 < 1000],
     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
     main="21k Tracks",
     #xlim=c(0.1,300),
     #ylim=c(0,20),
     col="blue", pch=1)
pt_R.fit <- lm( pt.train ~ r.fit1000 )
#pt_R.fit <- lm( pt.train[r.fit1000  > 0.1 & r.fit1000 < 1000] ~
#	       (r.fit1000[r.fit1000 > 0.1 & r.fit1000 < 1000]) )
abline( pt_R.fit, col="red", lwd=2 )
grid(col="gray48")
legend( "topleft", legend=c("Training Sample",
			    sprintf("pT(R) = %.2f + %.2f.R",
				       	pt_R.fit$coefficients[1],
				       	pt_R.fit$coefficients[2])),
        lwd=c(NA, 2), pch=c(1, NA), col=c("blue", "red"), border=NA, bg="white" )
dev.off()

#####################################################

#png("momentum/fit_2020_06_24_v2/hist_R.png",
png("momentum/fit_2020_06_24_v2/hist_R.lowRange.png",
    units="px", width=1600, height=1600, res=250)
#hist(r.fit,
#     breaks=200,
hist(r.fit[r.fit < 10000 & r.fit > 500],
     breaks=c(seq(500, 10000, 100)),
     xlab="Radius of Track Curvature [mm]",
     main="21k Tracks",
     col="red")
dev.off()

message("*****************************")
#####################################################

#png("momentum/fit_2020_06_24_v2/hist_pt_1-4.png",
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
x.diff.min = -0.2
x.diff.max =  0.6
x.diff.breaks = c(seq(x.diff.min,x.diff.max,0.02))
png("momentum/fit_2020_06_24_v2/hist_x_difference.png",
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
		      ylab="Frequency", main="21k Tracks",
		      ylim=c(0,7000),
		      lwd=2, col=rgb.color[[l]], type="l" )
	}
	else{
		lines( x.diff.hist[[l]]$mids, x.diff.hist[[l]]$counts,
		       lwd=2, col=rgb.color[[l]], type="l" )
	}
}
sd.x.diff <- c()
for( l in layers ) { sd.x.diff <- c(sd.x.diff, sd(x.diff[[l]])) }
bw.x.diff <- c()
for( l in layers ) { bw.x.diff <- c(bw.x.diff, bw.nrd(x.diff[[l]])) }
legend( "topright", legend=sprintf( "Layer %2d, (sd, BW) = (%.2f, %.4f) mm",
				       layers, sd.x.diff, bw.x.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bty="n" )
dev.off()
#####################################################
y.diff <- list()
y.diff.hist <- list()
y.diff.min = -0.2
y.diff.max =  0.6
y.diff.breaks = c(seq(y.diff.min,y.diff.max,0.02))
png("momentum/fit_2020_06_24_v2/hist_y_difference.png",
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
                      ylab="Frequency", main="21k Tracks",
                      ylim=c(0,7000), lwd=2, col=rgb.color[[l]], type="l")
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
legend( "topright", legend=sprintf( "Layer %2d, (sd, BW) = (%.2f, %.4f) mm",
                                       layers, sd.y.diff, bw.y.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bty="n" )
dev.off()
#####################################################
xy.diff <- list()
xy.diff.hist <- list()
xy.diff.xmax = 0.4
xy.diff.breaks = c(seq(0,xy.diff.xmax,0.02))
png("momentum/fit_2020_06_24_v2/hist_xy_difference.png",
    units="px", width=1600, height=1600, res=250)
for( l in layers ){
        xy.diff[[l]] <- sqrt( x.diff[[l]]^2 + y.diff[[l]]^2 )
	xy.diff.hist[[l]] <- hist( xy.diff[[l]][xy.diff[[l]] < xy.diff.xmax],
				   breaks=xy.diff.breaks, plot=F )
        if( l == 1 ){
		plot( xy.diff.hist[[l]]$mids, xy.diff.hist[[l]]$counts,
		      xlab=TeX("$(Hit_{Fitted} - Hit_{Real}) \\; \\[mm\\]$"),
                      ylab="Frequency",
                      xlim=c(min(xy.diff.breaks),max(xy.diff.breaks)),
                      ylim=c(0,8000),
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

legend( "topright", legend=sprintf( "Layer %2d, (sd, BW) = (%.2f, %.4f) mm",
				    layers, sd.xy.diff, bw.xy.diff ),
        fill=c( alpha(rgb.color, 1) ), border=NA, bty="n" )
dev.off()

#####################################################
count.diff <- data.frame()
#hit.diff <- seq(0.05,60,0.05)
hit.diff <- c(seq(0.001, 0.04, 0.001), seq(0.05,10,0.05))#, seq(70, 650, 10))
png("momentum/fit_2020_06_24_v2/count_fits_outside_diff.png",
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
                      ylim=c(0,21000),#15000),
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
#### (i.e, > 30 cm)
# which(xy.diff[[layers[1]]] > 370)
# for(l in 1:10) print(which(xy.diff[[layers[l]]] > 4000))
# 590  8697  9954 11350 12088 13339 16561 16845 20367 20632
# pt.train[high.diff]
# 10.799837  1.156115  1.254458 50.638954  1.080766  9.994519 29.971027
# 3.484532  1.094807 13.632988
####

#high.diff <- c( 1548,  1641,  7689,  8697,  9954, 12088,
#	       13339, 15209, 16472, 20367, 20632)
high.diff <- c(590,  8697,  9954, 11350, 12088, 13339, 16561, 16845, 20367, 20632)
png("momentum/fit_2020_06_24_v2/fit.circle_high.diff.2.png",
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

png("momentum/fit_2020_06_24_v2/fit.circle_high.pt.png",
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

#png("momentum/fit_2020_06_24_v2/sum.all.diff.png",
png("momentum/fit_2020_06_24_v2/sum.all.diff_0-5_mm.png",
    units="px", width=1600, height=1600, res=250)
#hist( sum.diff.all, breaks=100,
hist( sum.diff.all[sum.diff.all < 5], breaks=100,
#      xlim=c(0,300),
#      ylim=c(0,4000),
      xlab=TeX("$\\sum_{Layers}(Hit_{Fitted} - Hit_{Real}) \\; \\[mm\\]$"),
      ylab="Frequency",
      main="21k Tracks", col="blue", border=NA )
dev.off()
#####################################################
sum.diff.cut = 3
lower.pt <- c(which(sum.diff.all < sum.diff.cut))

png("momentum/fit_2020_06_24_v2/pt_vs_R_all.pT_and_lower.pT.sum.diff.cut.3.png",
    units="px", width=1600, height=1600, res=250)
plot(r.fit1000, pt.train,
     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
     main=TeX(sprintf("Before and After $\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
     #xlim=c(0,100),
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
#png("momentum/fit_2020_06_24_v2/pt_vs_R_lower.pT.png",
#    units="px", width=1600, height=1600, res=250)
#plot(r.fit1000[lower.pt], pt.train[lower.pt],
#     xlab="Radius of Track Curvature [m]", ylab="Transverse Momentum [GeV/c]",
#     main="10k Tracks",
#     xlim=c(0.5,5.5),
#     ylim=c(0.5,3),
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
r.breaks <- seq(100, 10000, 100)
png("momentum/fit_2020_06_24_v2/hist_R_lower.pT.sum.diff.cut.3.png",
    units="px", width=1600, height=1600, res=250)
hist(r.fit[(r.fit > min(r.breaks)) & (r.fit < max(r.breaks))],
     breaks=r.breaks,
     xlab="Radius of Track Curvature [mm]",
     xlim=c(100,10000),
     #ylim=c(0,1500),
     main=TeX(sprintf("Before and After Cut in $\\sum_{Layers}(Hit_{Fitted} - Hit_{Real})$ < %d mm", sum.diff.cut)),
     col=alpha(rgb.color[[1]], 1))
#r.fit.low.pt.hist <- hist(r.fit[lower.pt], plot=F)
hist(r.fit[(r.fit > min(r.breaks)) & (r.fit < max(r.breaks))][lower.pt],
      breaks=r.breaks,
      col=alpha(rgb.color[[3]], 0.7), add=TRUE)
legend( "topright", legend=c("21.0k Tracks", "20.7k Tracks"),
        fill=c(alpha(rgb.color[[1]], 1), alpha(rgb.color[[3]], 0.7)) )
dev.off()

#####################################################
png("momentum/fit_2020_06_24_v2/hist_all.pt_and_low.pt.sum.diff.cut.3.png",
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
