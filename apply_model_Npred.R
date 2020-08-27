########################################
########################################
########################################
folders_models              = "models/2020_06_13"
csv_application_sequence    = "data/application_sequences-05_05.csv"
csv_application_p5          = "data/application_p5-05_05.csv"
output_folder               = "figures/application_2020_06_13_v4"

trained_cases = 3

update_dataset = FALSE

########################################
# Optimum number of trees
#                 Layers    L5    L6    L7    L8    L9   L10
num.trees <- list( ### Trained case 1
		   list( c(1000, 1000, 1000, 1000, 1000, 1000),   # x
  			 c(1000, 1000, 1000, 1000, 1000, 1000),   # y 
      			 c( 980,  740, 1000, 1000,  960,  260) ), # z
		   ### Trained case 2
		   list( c( 940,  480, 1000,  360,  620, 1000),   # x
                         c(1000,  980,  980,  700,  900, 1000),   # y 
                         c(1000,  720,  980,  420,  300, 1000) ), # z
		   ### Trained case 3
		   list( c( 920, 1000, 1000,  960,  160, 1000),   # x
                         c(1000, 1000, 1000, 1000, 1000, 1000),   # y
                         c(1000,  920, 1000, 1000,  800,  740) )  # z
		   ) # num.trees <- list( list (), list(), list() )

discrete.z <- list( # Layer  9
		    c(-428.5, -417.5, -406.5, -395.5, -385.5,
		      -374.5, -363.5, -357.5, -346.5, -336.5,
		      -325.5, -314.5, -303.5, -292.5, -282.5,
		      -271.5, -260.5, -254.5, -243.5, -233.5,
		      -222.5, -211.5, -200.5, -189.5, -179.5,
		      -168.5, -157.5, -151.5, -140.5, -130.5,
		      -119.5, -108.5,  -97.5,  -86.5,  -76.5,
		       -65.5,  -54.5,  -48.5,  -37.5,  -27.5,
		       -16.5,   -5.5,    5.5,   16.5,   26.5,
		        37.5,   48.5,   54.5,   65.5,   75.5,
		        86.5,   97.5,  108.5,  119.5,  129.5,
		       140.5,  151.5,  157.5,  168.5,  178.5,
		       189.5,  200.5,  211.5,  222.5,  232.5,
		       243.5,  254.5,  260.5,  271.5,  281.5,
		       292.5,  303.5,  314.5,  325.5,  335.5,
		       346.5,  357.5,  363.5,  374.5,  384.5,
		       395.5,  406.5,  417.5,  428.5),
         	    # Layer 10
         	    c(-531.5, -520.5, -509.5, -498.5, -488.5,
		      -477.5, -466.5, -460.5, -449.5, -439.5,
		      -428.5, -417.5, -406.5, -395.5, -385.5,
		      -374.5, -363.5, -357.5, -346.5, -336.5,
		      -325.5, -314.5, -303.5, -292.5, -282.5,
		      -271.5, -260.5, -254.5, -243.5, -233.5,
		      -222.5, -211.5, -200.5, -189.5, -179.5,
		      -168.5, -157.5, -151.5, -140.5, -130.5,
		      -119.5, -108.5,  -97.5,  -86.5,  -76.5,
		       -65.5,  -54.5,  -48.5,  -37.5,  -27.5,
		       -16.5,   -5.5,    5.5,   16.5,   26.5,
		        37.5,   48.5,   54.5,   65.5,   75.5,
		        86.5,   97.5,  108.5,  119.5,  129.5,
		       140.5,  151.5,  157.5,  168.5,  178.5,
		       189.5,  200.5,  211.5,  222.5,  232.5,
		       243.5,  254.5,  260.5,  271.5,  281.5,
		       292.5,  303.5,  314.5,  325.5,  335.5,
		       346.5,  357.5,  363.5,  374.5,  384.5,
		       395.5,  406.5,  417.5,  428.5,  438.5,
		       449.5,  460.5,  466.5,  477.5,  487.5,
		       498.5,  509.5,  520.5,  531.5) )

########################################
########################################
########################################

########################################
# Reading input files
message("*******************************************")
message("* Reading input files...")
application_sequence <- read.csv(csv_application_sequence)
application_p5       <- read.csv(csv_application_p5)

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
gbm_model <- list( list(list(), list(), list(), list(), list(), list()),  # x
		   list(list(), list(), list(), list(), list(), list()),  # y
		   list(list(), list(), list(), list(), list(), list()) ) # z

########################################
# Predicted hits (x, y, z) for each layer
predicted.hits <- list( application_sequence,  # Trained case 1
		        application_sequence,  # Trained case 2
			application_sequence ) # Trained case 3

########################################
# Mean values (x, y, z) between predicted hits for each layer
predicted.hits.mean <- application_sequence

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
		for( t in 1:trained_cases ){
			gbm_model_name <- paste0( folders_models, "/gbm_model_",
					    	  xyz_names[c], l+4, ".", t, ".rda" )
			message("  * ", gbm_model_name)
			gbm_model[[c]][[l]][[t]] <- readRDS(gbm_model_name)
		} # for( t in 1:trained_cases )
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
	# Coordinates         x       y       z
	prediction <- list( list(), list(), list() )

	########################################
	# Running over (x, y, z) to predict correspondent coordinates
	for( c in xyz_coord ){
		for( t in 1:trained_cases ){
			prediction[[c]][[t]] <- predict( gbm_model[[c]][[l]][[t]],
						         newdata = application_4hits,
					     		 n.trees = num.trees[[t]][[c]][l] )
		} # for( t in 1:trained_cases )
	} # for( c in xyz_coord )

	########################################
	leastDist.col.pred_near <- c()
	leastDist.col.pred_real <- c()
	leastDist.col.near_real <- c()
	nearest.col  <- c()
	track.col.id <- c()

        ########################################
        # Get (x, y, z) coordinates for the predicted hit
	# as mean value from all trained cases
        pred.x.mean <- apply(as.data.frame(prediction[[1]]), 1, mean) # x
        pred.y.mean <- apply(as.data.frame(prediction[[2]]), 1, mean) # y
        pred.z.mean <- apply(as.data.frame(prediction[[3]]), 1, mean) # z

	########################################
	# Running over all tracks to find nearest hits
	cat("    * Predicting tracks ...")
	for( p in 1:nrows_sequence ){
		if( (p %% 10) == 0 ) cat(".")

		########################################
		# Get (x, y, z) coordinates for the predicted hit
		pred.x = pred.x.mean[p] # x
		pred.y = pred.y.mean[p] # y
		pred.z = pred.z.mean[p] # z

		########################################
		# Values are discrete along z coordinate in layers 9 and 10.
		# So let's take it into account and update z coordinate.
		if( (l+4 == 9) || (l+4 == 10) ){
			# In case of z is exactly in the middle of 2 discrete
			# values, take the first ([1])
			p.z = which(     abs(discrete.z[[l+4-8]] - pred.z) ==
				     min(abs(discrete.z[[l+4-8]] - pred.z)) )[1]
			# l = 5 -> layer  9 -> discrete.z[[1]]
			# l = 6 -> layer 10 -> discrete.z[[2]]
			pred.z = discrete.z[[l+4-8]][p.z]
		}

		########################################
		#near.index     <- c()
		nearest.row    <- c()
		track.index    <- c()
		#leastDistance  <- c()
		#dist.pred_near <- 999999
		#nTracks.p5     <- nrow(application_source)
		#stop("stop")
		########################################
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
			var_name <- paste0( column_names[i], "_5")
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
	# Run over all trained cases to get predicted hits
	for( t in 1:trained_cases ){
		predicted.hits[[t]] <- cbind( predicted.hits[[t]],
		     			      prediction[[1]][[t]],
	     				      prediction[[2]][[t]],
	     				      prediction[[3]][[t]] )
	} # for( t in 1:trained_cases )

	########################################
	# Get mean values from predicted hits
	predicted.hits.mean <- cbind(predicted.hits.mean,
				     pred.x.mean,
				     pred.y.mean,
				     pred.z.mean)

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
			var_name <- paste0( column_names[cn], "_", l4)
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
for( t in 1:trained_cases ){
	count.cols = 1
	for( l in layers ){
		for( c in xyz_coord ){
			var_name <- paste0( xyz_names[c], "_", l)
			colnames(predicted.hits[[t]])[ncols_sequence + count.cols] <- var_name
			count.cols = count.cols + 1
		} #for( c in xyz_coord )
	} # for( l in layers )
} # for( t in 1:trained_cases )

########################################
# Set column names for the dataset of mean values from predicted hits
count.cols = 1
for( l in layers ){
	for( c in xyz_coord ){
		var_name <- paste0( xyz_names[c], "_", l)
		colnames(predicted.hits.mean)[ncols_sequence + count.cols] <- var_name
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
		colnames(nearest.hits)[column] <- paste0( column_names[i], "_", l)
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
	   file=paste0(output_folder, "/leastDist.pred_near.csv"),
	   row.names=FALSE )
write.csv( leastDist.pred_real,
           file=paste0(output_folder, "/leastDist.pred_real.csv"),
           row.names=FALSE ) 
write.csv( leastDist.near_real,
           file=paste0(output_folder, "/leastDist.near_real.csv"),
           row.names=FALSE )
for( t in 1:trained_cases ){
	write.csv( predicted.hits[[t]],
	           file=paste0(output_folder, "/predicted.hits.", t, ".csv"),
	           row.names=FALSE )
} # for( t in 1:trained_cases )
write.csv( predicted.hits.mean,
	   file=paste0(output_folder, "/predicted.hits.mean.csv"),
	   row.names=FALSE )
write.csv( nearest.hits,
           file=paste0(output_folder, "/nearest.hits.csv"),
           row.names=FALSE )
write.csv( rtm,
           file=paste0(output_folder, "/rtm.csv"),
           row.names=FALSE )

########################################
# For safety, it is better to save this workspace so far
message("*******************************************")
message("* Saving workspace...")
save.image( file=paste0(output_folder, "/application_workspace.RData") )

########################################
# Save all necessary objects for further analysis
message("*******************************************")
message("* Saving all necessary objects for further analysis...")
save( leastDist.pred_near,
      leastDist.pred_real,
      leastDist.near_real,
      predicted.hits,
      predicted.hits.mean,
      nearest.hits,
      rtm,
      file=paste0(output_folder, "/application_objects.RData") )

message("* Done!")
message("*********************************************")



