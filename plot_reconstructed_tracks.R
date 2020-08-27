#####################################

all_hits_input_name  = "data/eta_n0.5-0.5_phi_ninf-pinf.csv"
real_input_name      = "data/application_whole_track.csv"
predict_input_name   = "figures/application_2020_06_13/predicted.hits.mean.csv"
reconst_input_name   = "figures/application_2020_06_13/nearest.hits.csv"
output_folder        = "figures/application_2020_06_13/reconstructed_tracks_20200613"
#output_image_name.yx = "reconst_tracks_success_yx.png"
#output_image_name.yz = "reconst_tracks_success_yz.png"
output_image_name.yx = "reconst_tracks_failure_yx.png"
output_image_name.yz = "reconst_tracks_failure_yz.png"

#tracks_to_draw <- c(  495,  1782,  4652,  8853,  8970, 17460,
#                   18027, 21576, 22580, 25035, 26069, 26843)
tracks_to_draw <- c( 2018,  2282,  3475,  5802, 21216, 21938,
                    23682, 23854, 24233, 26837, 27693, 29485)

#tracks_to_draw <- c(7740, 11586, 17964)
#tracks_to_draw <- c(8, 25, 35)
#tracks_to_draw <- c(1937,   2048,  3475,  5028,  6357, 11319,
#                   12358, 12401, 15040, 18486, 20051, 25772)
#tracks_to_draw <- c( 1103,  2050,  5321,  8204, 20467, 22249,
#                   22682, 25148, 26144, 27691, 28472, 28982)

# Setup of Plots
#main_title = "Success in the Reconstructed Tracks"
main_title   = "Failure in the Reconstructed Tracks"
#x.min_legend1 = -1000
x.min_legend1 = 300
y.max_legend1 = 1050
#x.min_legend2 = -500
x.min_legend2 = 200
y.max_legend2 = 1050

# Column numbers for x, y and z
# - Input file with all hits
x.all.cols  <- seq(11, 120, 11)
y.all.cols  <- seq(12, 120, 11)
z.all.cols  <- seq(13, 120, 11)
# - Input file with real tracks
x.real.cols <- seq(3, 111, 11)
y.real.cols <- seq(4, 111, 11)
z.real.cols <- seq(5, 111, 11)
# - Input file with predicted hits
x.pred.cols <- c(seq(3, 45, 11), seq(46, 63, 3))
y.pred.cols <- c(seq(4, 45, 11), seq(47, 63, 3))
z.pred.cols <- c(seq(5, 45, 11), seq(48, 63, 3))
# - Input file with reconstructed hits
x.rt.cols   <- seq(3, 111, 11)
y.rt.cols   <- seq(4, 111, 11)
z.rt.cols   <- seq(5, 111, 11)

#####################################
# Read input file
message("**********************************")
message("* Reading input file with all hits ...")
input.all  <- read.csv(all_hits_input_name)
message("* Reading input file with real tracks ...")
input.real  <- read.csv(real_input_name)
message("* Reading input file with predicted hits ...")
input.pred <- read.csv(predict_input_name)
message("* Reading input file with reconstructed tracks ...")
input.rt   <- read.csv(reconst_input_name)

nrow.all  = nrow(input.all)
nrow.real = nrow(input.real)
nrow.pred = nrow(input.pred)
nrow.rt   = nrow(input.rt)

ncol.all  = ncol(input.all)
ncol.real = ncol(input.real)
ncol.pred = ncol(input.pred)
ncol.rt   = ncol(input.rt)

message("  * Dimensions of dataset with all hits: ")
message("    * number of rows: ", nrow.all)
message("    * number of cols: ", ncol.all)
message("  * Dimensions of dataset with real tracks: ")
message("    * number of rows: ", nrow.real)
message("    * number of cols: ", ncol.real)
message("  * Dimensions of dataset with predicted hits: ")
message("    * number of rows: ", nrow.pred)
message("    * number of cols: ", ncol.pred)
message("  * Dimensions of dataset with reconstructed tracks: ")
message("    * number of rows: ", nrow.rt)
message("    * number of cols: ", ncol.rt)

# # # Get sample of reconstructed track ids which succeeded and failed
# rtm <- read.csv("figures/application_2020_05_17/rtm.csv")
# sum_of_rtm_cols <- c()
# for( r in 1:nrow(rtm) ){
# 	adding = 0
# 	for( c in 2:ncol(rtm) ){
# 		adding = adding + abs( rtm[r,c] - rtm[r,1] )
# 	}
# 	sum_of_rtm_cols <- c(sum_of_rtm_cols, adding)
# }
# # head( which( sum_of_rtm_cols == 0 ) )
# # rtm[ head( which( sum_of_rtm_cols == 0 ) ), 1]
# # set.seed(123)
# # head( sample(rtm[which( sum_of_rtm_cols == 0 ), 1], length(which( sum_of_rtm_cols == 0 )))
# 
# # set.seed(123)
# # tail( sample(rtm[which( sum_of_rtm_cols == 0 ), 1], length(which( sum_of_rtm_cols == 0 )))
# 
# set.seed(123)
# sort( c(head( sample(rtm[which( sum_of_rtm_cols == 0 ), 1], length(which( sum_of_rtm_cols == 0 ))) ), tail( sample(rtm[which( sum_of_rtm_cols == 0 ), 1], length(which( sum_of_rtm_cols == 0 ))) )) )
# set.seed(123)
# sort( c(head( sample(rtm[which( sum_of_rtm_cols != 0 ), 1], length(which( sum_of_rtm_cols != 0 ))) ), tail( sample(rtm[which( sum_of_rtm_cols != 0 ), 1], length(which( sum_of_rtm_cols != 0 ))) )) )

# Read x, y and z values of all hits
x.all <- unlist( input.all[, seq(11, 120, 11)], use.names=FALSE )
y.all <- unlist( input.all[, seq(12, 120, 11)], use.names=FALSE )
z.all <- unlist( input.all[, seq(13, 120, 11)], use.names=FALSE )

# Read (x, y, z) of the selected predicted and selected reconstruced tracks
message("**********************************")
message("* Reading (x, y, z) of the selected predicted and selected reconstruced tracks...")
x.real <- list()
y.real <- list()
z.real <- list()
x.pred <- list()
y.pred <- list()
z.pred <- list()
x.rt   <- list()
y.rt   <- list()
z.rt   <- list()
for( i in 1:length(tracks_to_draw) ){
	# Check which row corresponds to the chosen track id
	row.id <- which( input.pred[, "sample_id"] == tracks_to_draw[i] )
	# Real tracks
	x.real[[i]] <- unlist( input.real[row.id, x.real.cols], use.names=FALSE )
	y.real[[i]] <- unlist( input.real[row.id, y.real.cols], use.names=FALSE )
	z.real[[i]] <- unlist( input.real[row.id, z.real.cols], use.names=FALSE )
	# Predicted tracks
	x.pred[[i]] <- unlist( input.pred[row.id, x.pred.cols], use.names=FALSE )
	y.pred[[i]] <- unlist( input.pred[row.id, y.pred.cols], use.names=FALSE )
	z.pred[[i]] <- unlist( input.pred[row.id, z.pred.cols], use.names=FALSE )
	# Reconstruted tracks   
	x.rt[[i]]   <- unlist( input.rt[row.id,   x.rt.cols],   use.names=FALSE )
	y.rt[[i]]   <- unlist( input.rt[row.id,   y.rt.cols],   use.names=FALSE )
	z.rt[[i]]   <- unlist( input.rt[row.id,   z.rt.cols],   use.names=FALSE )
}

# Drawing tracks
message("**********************************")
message("* Drawing tracks in xy plane ...")
output_file_name <- paste( output_folder, "/", output_image_name.yx, sep="")
png(output_file_name, units="px", width=1600, height=1600, res=250)
plot( x.all, y.all,
      xlab="x (mm)", ylab="y (mm)",
      main=main_title,
      col="darkgray", pch=".",
      xlim=range(x.all), ylim=range(y.all) )
for( i in 1:length(tracks_to_draw) ){
	lines( x.real[[i]], y.real[[i]],
               type="b", col="blue", lty=1 , lwd=2, pch=19 )
	lines( x.pred[[i]], y.pred[[i]],
               type="b", col="red", lty=5 , lwd=2, pch=0 )
	lines( x.rt[[i]], y.rt[[i]],
               type="b", col="forestgreen",  lty=3 , lwd=2, pch=2 )
}
legend( x.min_legend1, y.max_legend1,
        c("Real", "Predicted", "Reconstructed"),
	lwd = c(2, 2, 2), lty = c(1, 5, 3),
	col = c("blue", "red", "forestgreen"), pch = c(19, 0, 2) )
dev.off()

message("**********************************")
message("* Drawing tracks in yz plane ...")
output_file_name <- paste( output_folder, "/", output_image_name.yz, sep="")
png(output_file_name, units="px", width=1600, height=1600, res=250)
plot( z.all[abs(x.all) < 30], y.all[abs(x.all) < 30],
      xlab="z (mm)", ylab="y (mm)",
      main=main_title,
      col="darkgray", pch=".",
      #xlim=range(z.all), ylim=range(y.all)
      xlim=c(-600, 600), ylim=range(y.all)
      )
for( i in 1:length(tracks_to_draw) ){
	lines( z.real[[i]], y.real[[i]],
               type="b", col="blue", lty=1 , lwd=2, pch=19 )
	lines( z.pred[[i]], y.pred[[i]],
	       type="b", col="red", lty=5 , lwd=2, pch=0 )
	lines( z.rt[[i]], y.rt[[i]],
       	       type="b", col="forestgreen",  lty=3 , lwd=2, pch=2 )
}
legend( x.min_legend2, y.max_legend2,
        c("Real", "Predicted", "Reconstructed"),
        lwd = c(2, 2, 2), lty = c(1, 5, 3),
       	col = c("blue", "red", "forestgreen"), pch = c(19, 0, 2) )
dev.off()


message("* Done!")
message("**********************************")


