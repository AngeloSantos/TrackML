#

###################################################
### Modify the lines bellow #######################
###

training_file_name        = "data/training_sequences-05_05.csv"
optimization_file_name    = "data/optimization_sequences-05_05.csv"
optimization_p5_file_name = "data/optimization_p5-05_05.csv"

heatmap_output_folder = "figures/optimization_2020_06_14/heatmaps"
output_file_name      = "optimization_workspace.RData"

training_nrows        = 21000
optimization_nrows    = 3000
optimization_p5_nrows = 3000

layers <- c(1:6)

xyz_coordinates <- c(  1,   2,   3)
xyz_names       <- c("x", "y", "z")

#groupvars <- c("x_1", "x_2", "x_3", "x_4",
#	       "y_1", "y_2", "y_3", "y_4",
#	       "z_1", "z_2", "z_3", "z_4")

groupvars <- c(   "x_1",   "x_2",   "x_3",   "x_4",
                  "y_1",   "y_2",   "y_3",   "y_4",
                  "z_1",   "z_2",   "z_3",   "z_4",
	        "rho_1", "rho_2", "rho_3", "rho_4",
	        "eta_1", "eta_2", "eta_3", "eta_4",
	        "phi_1", "phi_2", "phi_3", "phi_4" )


model_expression <- c( paste("x_5", paste(groupvars, collapse=" + "), sep=" ~ "),  # x
		       paste("y_5", paste(groupvars, collapse=" + "), sep=" ~ "),  # y
		       paste("z_5", paste(groupvars, collapse=" + "), sep=" ~ ") ) # z

num.trees.xyz <- list( seq(20, 1000, 20),  # x
		       seq(20, 1000, 20),  # y
		       seq(20, 1000, 20) ) # z

shrinkage.xyz <- list( seq(0.02, 0.4, 0.02),  # x
		       seq(0.02, 0.4, 0.02),  # y
		       seq(0.02, 0.4, 0.02) ) # z

interaction.depth.xyz <- list( seq(2, 20, 2),  # x
			       seq(2, 20, 2),  # y
			       seq(2, 20, 2) ) # z

minMSE <- list()
minNTrees <- list()
minShrinkage <- list()
minTreePosition <- list()
minInteractionDepth <- list()

mapNtreeShrinkage <- list()

###
###################################################
###################################################

# Read input files
message("*********************************************")
message("* Reading training and optimization input files...")
input_training        = read.csv(training_file_name)
input_optimization    = read.csv(optimization_file_name)
input_optimization_p5 = read.csv(optimization_p5_file_name)

# Start Machine Learning Model
# BDT - GBM
require(gbm)

message("*********************************************")
message("* Starting optimization of parameters...")
# Loop over layers 5, 6, 7, 8, 9 and 10
for( l in layers ) {
	message("-------------------------------------")
	message("* Layer ", l+4)

	first_training_row = 1 + (l-1)*training_nrows
        last_training_row  = l*training_nrows
        training_sequence  = c(first_training_row:last_training_row)

        first_optimization_row = 1 + (l-1)*optimization_nrows
        last_optimization_row  = l*optimization_nrows
        optimization_sequence  = c(first_optimization_row:last_optimization_row)

        first_optimization_p5_row = 1 + (l-1)*optimization_p5_nrows
        last_optimization_p5_row  = l*optimization_nrows
        optimization_p5           = c(first_optimization_p5_row:last_optimization_p5_row)

	for( c in xyz_coordinates ){
		message("  * Training over ", xyz_names[c], " ...")

       		min_MSE              = 9999999
        	min_NTrees           = 9999999
        	min_Shrinkage        = 9999999
        	min_TreePosition     = 9999999
        	min_InteractionDepth = 9999999

		create_map <- c()

		for( j in 1:length(shrinkage.xyz[[c]]) ){
			cat("    * Shrinkage = ", shrinkage.xyz[[c]][j], "\n")
			cat("      * intDepth = ")
	
			minError <- c()
	
			for( k in 1:length(interaction.depth.xyz[[c]]) ){
				cat(interaction.depth.xyz[[c]][k], "...")

				set.seed(123)		
				gbm_model <- gbm(as.formula(model_expression[c]),
					     data = input_training[training_sequence,],
					     distribution = "gaussian",
					     n.trees = max(num.trees.xyz[[c]]),
					     shrinkage = shrinkage.xyz[[c]][j],
					     interaction.depth = interaction.depth.xyz[[c]][k])
	
				prediction <- predict(gbm_model,
				     		      newdata = input_optimization[optimization_sequence,],
						      n.trees = num.trees.xyz[[c]])

				var_name <- paste0( xyz_names[c], "_5" )

                                error <- apply( (prediction -
                                                 input_optimization_p5[optimization_p5,
                                                                       var_name])^2,
                                                 2, mean )
	
#				if ( xyz_names[c] == "x" )
#					error <- with(input_optimization_p5[optimization_p5,],
#						      apply( (prediction - x_5)^2, 2, mean))
#				else if ( xyz_names[c] == "y" )
#                                        error <- with(input_optimization_p5[optimization_p5,],
#                                                      apply( (prediction - y_5)^2, 2, mean))
#				else
#                                        error <- with(input_optimization_p5[optimization_p5,],
#                                                      apply( (prediction - z_5)^2, 2, mean))
#
				minError <- c (minError, min(error) )
	
				if( min(error) < min_MSE ){
					min_MSE              = min(error)
					min_NTrees           = names(which(error==min(error)))
					min_Shrinkage        = shrinkage.xyz[[c]][j]
					min_TreePosition     = which(error == min(error))
					min_InteractionDepth = interaction.depth.xyz[[c]][k]
				}

			} # for( k in 1:length(interaction.depth.xyz[[c]]) )

			create_map <- rbind(create_map, minError)
			cat("\n")

		} # for( j in 1:length(shrinkage.xyz[[c]]) )
		cat("\n")

		# Creating Matrix
		create_map <- as.matrix(create_map)

		# Renaming columns and rows of the matrix
		colnames(create_map) <- c(interaction.depth.xyz[[c]])
		rownames(create_map) <- c(shrinkage.xyz[[c]])

		if( l > 1 ){
			minMSE[[c]]              <- append(minMSE[[c]],
							   min_MSE)
			minNTrees[[c]]           <- append(minNTrees[[c]],
							   min_NTrees)
			minShrinkage[[c]]        <- append(minShrinkage[[c]],
							   min_Shrinkage)
			minTreePosition[[c]]     <- append(minTreePosition[[c]],
							   min_TreePosition)
			minInteractionDepth[[c]] <- append(minInteractionDepth[[c]],
							   min_InteractionDepth)
			if( l > 2)
				mapNtreeShrinkage[[c]][[l]] <- create_map
			else # (l == 2)
				mapNtreeShrinkage[[c]] <- list(mapNtreeShrinkage[[c]],
							       create_map)
		}
		else { # (l == 1)
			minMSE[[c]]              <- min_MSE
			minNTrees[[c]]           <- min_NTrees
			minShrinkage[[c]]        <- min_Shrinkage
			minTreePosition[[c]]     <- min_TreePosition
			minInteractionDepth[[c]] <- min_InteractionDepth
			mapNtreeShrinkage[[c]]   <- create_map
		} # else
	} # for( c in xyz_coordinates )
} # for( l in layers ) # Loop over layers 5, 6, 7, 8, 9 and 10

# For safety, it is better to save this workspace so far
save.image( file=paste(heatmap_output_folder, "/", output_file_name, sep="") )

# Plot Heat Maps
library(gplots)
message("*********************************************")
message("* Plotting Heat Map")
for( l in layers ){
	message("  * Layer", l+4)
	cat("    * Coordinate ")
	for( c in xyz_coordinates ){
		cat(xyz_names[c], " ... ")
		
		save_figure <- paste(heatmap_output_folder,
				     "/map_Layer", l+4, "_",
				     xyz_names[c], ".png", sep="")
		png(save_figure, units="px", width=1600, height=1600, res=200, pointsize=15)

		heatmap.2(mapNtreeShrinkage[[c]][[l]],
			  cellnote=round(mapNtreeShrinkage[[c]][[l]], 1),
			  xlab="Tree Interaction Depth",
			  ylab="Learning Rate",
			  main=paste("Minimu MSE for ", xyz_names[c],
				     " (Layer ", l+4, ")", sep=""),
			  col = greenred(100), trace="none",
			  dendrogram="none", Colv="NA", Rowv="NA",
			  key.xlab="Minimum MSE", key.title="Color Key",
			  cexRow=1.5, cexCol=1.5, srtCol=0)
		dev.off()
	}
	cat("\n")
}

# Showing results
message("*********************************************")
for( c in xyz_coordinates ){
        message("* Results for ", xyz_names[c], ":")
        cat("* Layer               = ")
        for( l in layers ) cat(l+4, "\t")                         ; cat("\n")
        cat("* minMSE              = ")
        for( l in layers ) cat(round(minMSE[[c]][l], 3), "\t")    ; cat("\n")
        cat("* minNTrees           = ")
        for( l in layers ) cat(minNTrees[[c]][l], "\t")           ; cat("\n")
        cat("* minShrinkage        = ")
        for( l in layers ) cat(minShrinkage[[c]][l], "\t")        ; cat("\n")
        cat("* minTreePosition     = ")
        for( l in layers ) cat(minTreePosition[[c]][l], "\t")     ; cat("\n")
        cat("* minInteractionDepth = ")
        for( l in layers ) cat(minInteractionDepth[[c]][l], "\t") ; cat("\n")
        message("---------------------------------------------")
}

iii = 1
while(iii >0){
	message("*********************************************")
	message("* Total MSE (x, y and z together) per layer:")
	cat("  * Layer = \t\t")
	for( l in layers ) cat(l+4, "\t")                         ; cat("\n")
	cat("  * MSE   = \t\t")
	for( l in layers ){
	        total_MSE = 0
	        for( c in xyz_coordinates ){ total_MSE = total_MSE + minMSE[[c]][l] }
	        cat( round(total_MSE, 2) , "\t")
	} # for( l in layers )
	cat("\n")
	iii = 0
}
message("* Done!")
message("*********************************************")

