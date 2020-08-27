########################################
########################################
########################################
folders_models              = "models/2020_05_16"
csv_application_sequence    = "data/application_sequences-05_05.csv"
csv_application_p5          = "data/application_p5-05_05.csv"
output_folder               = "figures/application_2020_06_09_v2"

update_dataset = TRUE

########################################
# Optimum number of trees
#            Layers    L5    L6    L7    L8    L9   L10
num.trees <- list( c( 360,  940, 1000,   320, 	1000, 	1000), # x
                   c(1000, 1000, 1000, 	1000, 	1000, 	1000), # y
                   c(1000,  980, 1000, 	1000, 	1000, 	1000)  # z
                   ) # num.trees <- list( list (), list(), list() )

########################################
########################################
########################################

########################################
# Reading input files
message("*******************************************")
message("* Reading input files...")
application_sequence <- read.csv(csv_application_sequence)
application_p5       <- read.csv(csv_application_p5)

#seed = 124
#set.seed(seed)
#samp <- sample(1:200, 200)
#application_sequence <- application_sequence[samp,]
#
#set.seed(seed)
#samp1 <- sample(1:200,       200)
#set.seed(seed)
#samp2 <- sample(6001:6200,   200)
#set.seed(seed)
#samp3 <- sample(12001:12200, 200)
#set.seed(seed)
#samp4 <- sample(18001:18200, 200)
#set.seed(seed)
#samp5 <- sample(24001:24200, 200)
#set.seed(seed)
#samp6 <- sample(30001:30200, 200)
#application_p5       <- application_p5[c( samp1, samp2, samp3, samp4, samp5, samp6),]

########################################
# Data frames to be updated
application_4hits  <- application_sequence
application_source <- application_p5

########################################
# Counting number of rows and columns
nrows_sequence = nrow(application_sequence)
ncols_sequence = ncol(application_sequence)

nrows_p5 = nrow(application_p5)
ncols_p5 = ncol(application_p5)

message("  * Application (sequence of 4 hits):")
message("    * ", nrows_sequence, " rows")
message("    * ", ncols_sequence, " cols")
message("  * Application (5th hit):")
message("    * ", nrows_p5, " rows")
message("    * ", ncols_p5, " cols")

########################################
layers <- c(5:10)

########################################
xyz_names <- c("x", "y", "z")
xyz_coord <- c( 1 ,  2 ,  3 )

########################################
column_names <- c("sample_id", "hit_id", "x", "y", "z", "rho", "eta", "phi",
		  "volume_id", "layer_id", "module_id", "value")

########################################
# List of models
## Each sublist will get values correspondent to each one of the 6 layers
gbm_model <- list( list(),  # x
		   list(),  # y
		   list() ) # z

########################################
# Predicted hits (x, y, z) for each layer
predicted.hits <- application_sequence

########################################
# Nearest Hits (x, y, z) for each layer
nearest.hits   <- application_sequence

########################################
# Least distance between predicted and nearest hits
leastDist.pred_near <- c()
# Least distance between predicted and real hits
leastDist.pred_real <- c()
# Least distance between nearest and real hits
leastDist.near_real <- c()

########################################
# Reconstructed Tracks Map (RTM)
rtm <- application_sequence[, "sample_id"]

########################################
# Start Machine Learning Model
# BDT - GBM
require(gbm)

########################################
# Reading GBM models
message("*******************************************")
message("* Reading GBM models:")
for( l in layers ){
	l = l-4
	for( c in xyz_coord ){
		gbm_model_name <- paste( folders_models, "/gbm_model_",
				    	 xyz_names[c], l+4, ".rda", sep="" )
		message("  * ", gbm_model_name)
		gbm_model[[c]][[l]] <- readRDS(gbm_model_name)
	} # for( c in xyz_coord )
} # for( l in layers )

########################################
# Starting track reconstrucion
# Application Step
message("*******************************************")
message("* Starting track reconstrucion (Application Step)...")
for( l in layers ){
	message("  * Predicting hits of layer ", l)
	l = l-4

	########################################
	prediction <- list()

	########################################
	# Running over (x, y, z) to predict correspondent coordinates
	for( c in xyz_coord ){
		prediction[[c]] <- predict( gbm_model[[c]][[l]],
					    newdata = application_4hits,
					    n.trees = num.trees[[c]][l] )
	} # for( c in xyz_coord )

	########################################
	leastDist.col.pred_near <- c()
	leastDist.col.pred_real <- c()
	leastDist.col.near_real <- c()
	nearest.col  <- c()
	track.col.id <- c()

	########################################
	# Running over all tracks to find nearest hits
	cat("    * Predicting tracks ...")
	for( p in 1:nrows_sequence ){
		if( (p %% 10) == 0 ) cat(".")

		########################################
		# (x, y, z) coordinates for the predicted hit
		pred.x = prediction[[1]][p] # x
		pred.y = prediction[[2]][p] # y
		pred.z = prediction[[3]][p] # z

		########################################
		near.index     <- c()
		nearest.row    <- c()
		track.index    <- c()
		#leastDistance  <- c()
		#dist.pred_near <- 999999
		#nTracks.p5     <- nrow(application_source)

		#########################################
		#for( n in 1:nTracks.p5 ){
		#	near.x <- application_source[n, "x_5"]
		#	near.y <- application_source[n, "y_5"]
		#	near.z <- application_source[n, "z_5"]

		#	########################################
		#	leastDistance = sqrt( (pred.x - near.x)^2 +
		#			      (pred.y - near.y)^2 +
		#			      (pred.z - near.z)^2 )

		#	########################################
		#	if( leastDistance < dist.pred_near ){
		#		near.index     <- n
		#		dist.pred_near <- leastDistance
		#		track.index    <- application_source[n, "sample_id"]
		#	}
		#} # for( n in 1:nTracks.p5 )	

                leastDistance <- sqrt( (pred.x - application_source[, "x_5"])^2 +
                                       (pred.y - application_source[, "y_5"])^2 +
                                       (pred.z - application_source[, "z_5"])^2 )

                dist.pred_near = min(leastDistance)
                near.index     = which( leastDistance == min(leastDistance) )[1]
                track.index    = application_source[near.index, "sample_id"]

		########################################
		# (x, y, z) coordinates for the real hit
		real.index = p + (l-1)*nrows_sequence
		real.x = application_p5[real.index, "x_5"]
		real.y = application_p5[real.index, "y_5"]
		real.z = application_p5[real.index, "z_5"]

		dist.pred_real <- sqrt( (pred.x - real.x)^2 +
				        (pred.y - real.y)^2 +
					(pred.z - real.z)^2 )

		########################################
		# (x, y, z) coordinates for the nearest hit
		nearest.x = application_source[near.index, "x_5"]
		nearest.y = application_source[near.index, "y_5"]
		nearest.z = application_source[near.index, "z_5"]

		dist.near_real <- sqrt( (nearest.x - real.x)^2 +
				        (nearest.y - real.y)^2 +
					(nearest.z - real.z)^2 )

		########################################
		leastDist.col.pred_near <- c(leastDist.col.pred_near, dist.pred_near)
		leastDist.col.pred_real <- c(leastDist.col.pred_real, dist.pred_real)
		leastDist.col.near_real <- c(leastDist.col.near_real, dist.near_real)

		########################################
		track.col.id <- c(track.col.id, track.index)

		########################################
		# Fill columns of the nearest hits for the current layer
		for(i in 2:ncols_p5){
			var_name <- paste( column_names[i], "_5", sep="")
			nearest.row <- c( nearest.row,
					  application_source[near.index, var_name] )
		}
		nearest.col <- rbind(nearest.col, nearest.row)

		########################################
		# Update dataset removing the hit which was just found
		if( update_dataset == TRUE ){
			application_source <- application_source[-near.index,]
		}
		
	} # for( p in 1:nrows_sequence )
	cat("\n")

	########################################
	predicted.hits <- cbind( predicted.hits,
			       	 prediction[[1]],
				 prediction[[2]],
				 prediction[[3]] )


	########################################
	leastDist.pred_near <- cbind(leastDist.pred_near, leastDist.col.pred_near)
	leastDist.pred_real <- cbind(leastDist.pred_real, leastDist.col.pred_real)
	leastDist.near_real <- cbind(leastDist.near_real, leastDist.col.near_real)

	########################################
	rtm <- cbind(rtm, track.col.id)

	########################################
	nearest.hits <- cbind( nearest.hits, nearest.col )

	########################################
	# Update sequence of 4 hits
	# - Remove columns related to the first hit in the sequence
	application_4hits <- application_4hits[,-c( seq(2, ncols_p5, 1) )]
	# - Add columns related to newest hit in the sequence
	application_4hits <- cbind( application_4hits, nearest.col )
	# - Rename columns
	count.cols = 2
	for( l4 in 1:4 ){
		for( cn in 2:length(column_names) ){
			var_name <- paste( column_names[cn], "_", l4, sep="" )
			colnames(application_4hits)[count.cols] <- var_name
			count.cols = count.cols + 1
		} # for( cn in 2:length(column_names) )
	} # for( l in 1:4 )

} # for( l in layers )

########################################
# Set column names for the dataset of minimum distance between predicted and nearest hits
message("*******************************************")
message("* Setting column names for the dataset of minimum")
message("  distance between predicted and nearest hits...")
for( l in layers ) colnames(leastDist.pred_near)[l-4] <- l

########################################
# Set column names for the dataset of minimum distance between predicted and real hits
message("*******************************************")
message("* Setting column names for the dataset of minimum")
message("  distance between predicted and real hits...")
for( l in layers ) colnames(leastDist.pred_real)[l-4] <- l

########################################
# Set column names for the dataset of minimum distance between nearest and real hits
message("*******************************************")
message("* Setting column names for the dataset of minimum")
message("  distance between nearest and real hits...")
for( l in layers ) colnames(leastDist.near_real)[l-4] <- l

########################################
# Set column names for the dataset of predicted hits
message("*******************************************")
message("* Setting column names for the dataset of predicted hits...")
count.cols = 1
for( l in layers ){
	for( c in xyz_coord ){
		var_name <- paste( xyz_names[c], "_", l, sep="" )
		colnames(predicted.hits)[ncols_sequence + count.cols] <- var_name
		count.cols = count.cols + 1
	} #for( c in xyz_coord )
} # for( l in layers )

########################################
# Set column names for the dataset of nearest hits
message("*******************************************")
message("* Setting column names for the dataset of nearest hits...")
for( l in layers ){
	for( i in 2:ncols_p5 ){
		# 2 + (5 - 1)*(12 - 1) = 2 + 4*11 = 46
		column = i + (l - 1)*(ncols_p5 - 1)
		colnames(nearest.hits)[column] <- paste( column_names[i], "_", l, sep="" )
	} # for( i in 2:ncols_p5 )
} # for( l in layers )

########################################
# Set column names for the RTM dataset
message("*******************************************")
message("* Set column names for the RTM dataset...")
colnames(rtm)[1] <- "4"
for( l in layers ) colnames(rtm)[l-3] <- l

########################################
# Save all necessary data.frames as csv for further analysis
message("*******************************************")
message("* Saving all necessary data.frames as csv for further analysis...")
write.csv( leastDist.pred_near,
	   file=paste(output_folder, "/leastDist.pred_near.csv", sep=""),
	   row.names=FALSE )
write.csv( leastDist.pred_real,
           file=paste(output_folder, "/leastDist.pred_real.csv", sep=""),
           row.names=FALSE ) 
write.csv( leastDist.near_real,
           file=paste(output_folder, "/leastDist.near_real.csv", sep=""),
           row.names=FALSE )
write.csv( predicted.hits,
           file=paste(output_folder, "/predicted.hits.csv", sep=""),
           row.names=FALSE )
write.csv( nearest.hits,
           file=paste(output_folder, "/nearest.hits.csv", sep=""),
           row.names=FALSE )
write.csv( rtm,
           file=paste(output_folder, "/rtm.csv", sep=""),
           row.names=FALSE )

########################################
# For safety, it is better to save this workspace so far
message("*******************************************")
message("* Saving workspace...")
save.image( file=paste(output_folder, "/application_workspace.RData", sep="") )

########################################
# Save all necessary objects for further analysis
message("*******************************************")
message("* Saving all necessary objects for further analysis...")
save( leastDist.pred_near,
      leastDist.pred_real,
      leastDist.near_real,
      predicted.hits,
      nearest.hits,
      rtm,
      file=paste(output_folder, "/application_objects.RData", sep="") )

message("* Done!")
message("*********************************************")



