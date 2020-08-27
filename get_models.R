###################################################
###################################################
### Modify the lines bellow #######################
###

training_file_name        = "data/data_2020_05_28/training_sequences.csv"
optimization_file_name    = "data/data_2020_05_28/optimization_sequences.csv"
optimization_p5_file_name = "data/data_2020_05_28/optimization_p5.csv"
models_output_dir         = "models/2020_06_09"
folder_figures            = "figures/optimization_2020_06_07/mse_vs_ntrees_2020_06_09"

training_nrows    = 21000
optimization_nrows = 3000

num.trees.xyz <- list( seq(20, 1000, 20),  # x
                       seq(20, 1000, 20),  # y
                       seq(20, 1000, 20) ) # z

#num.trees.xyz <- list( c( 360,  940, 1000,  320, 1000, 1000),  # x
#                       c(1000, 1000, 1000, 1000, 1000, 1000),  # y
#                       c(1000,  980, 1000, 1000, 1000, 1000) ) # z

shrinkage.xyz <- list( c(0.02,	0.14,	0.02,	0.08,	0.06,	0.18),  # x
                       c(0.14,	0.1,	0.08,	0.06,	0.08,	0.12),  # y
                       c(0.02,	0.12,	0.14,	0.38,	0.02,	0.28) ) # z

interaction.depth.xyz <- list( c(20,	20,	20,	20,	20,	18),  # x
                               c(20,	20,	20,	20,	20,	18),  # y
                               c(18,	20,	20,	6,	2,	20) ) # z

###
### DO NOT modify lines from hear to bellow
###################################################
###################################################

layers <- c(5:10)

xyz_coordinates <- c(  1,   2,   3)
xyz_names       <- c("x", "y", "z")
#xyz_names       <- c("rho", "phi", "z")

groupvars <- c("x_1", "x_2", "x_3", "x_4", # x
               "y_1", "y_2", "y_3", "y_4", # y
               "z_1", "z_2", "z_3", "z_4") # z

#groupvars <- c("rho_1", "rho_2", "rho_3", "rho_4", # rho
#               "phi_1", "phi_2", "phi_3", "phi_4", # phi
#                 "z_1",   "z_2",   "z_3",   "z_4") # z

model_expression <- c( paste("x_5", paste(groupvars, collapse=" + "), sep=" ~ "),  # x
                       paste("y_5", paste(groupvars, collapse=" + "), sep=" ~ "),  # y
                       paste("z_5", paste(groupvars, collapse=" + "), sep=" ~ ") ) # z


#model_expression <- c( paste("rho_5", paste(groupvars, collapse=" + "), sep=" ~ "),  # rho
#                       paste("phi_5", paste(groupvars, collapse=" + "), sep=" ~ "),  # phi
#                       paste(  "z_5", paste(groupvars, collapse=" + "), sep=" ~ ") ) # z

###
###################################################
###################################################

###################################################
# Read input files
message("*********************************************")
message("* Reading training and optimization input files ...")
input_training        <- read.csv(training_file_name)
input_optimization    <- read.csv(optimization_file_name)
input_optimization_p5 <- read.csv(optimization_p5_file_name)

###################################################
# Load Machine Learning Model
# BDT - GBM
require(gbm)

###################################################
# Loop over layers 5, 6, 7, 8, 9 and 10
for( l in layers ){
	message("*********************************************")
	message("* Training model for layer ", l)

        first_training_row = 1 + (l-5)*training_nrows
        last_training_row  = (l-4)*training_nrows
        training_sequence  = c(first_training_row:last_training_row)

        first_optimization_row = 1 + (l-5)*optimization_nrows
        last_optimization_row  = (l-4)*optimization_nrows
        optimization_sequence  = c(first_optimization_row:last_optimization_row)

        first_optimization_p5_row = 1 + (l-5)*optimization_nrows
        last_optimization_p5_row  = (l-4)*optimization_nrows
        optimization_p5           = c(first_optimization_p5_row:last_optimization_p5_row)

	###################################################
        # Training model for variables x_5, y_5 and z_5
	for( c in xyz_coordinates ){
                message("  ---------------------------------------------")
                message("  ---------------------------------------------")
		message("  * Training over ", xyz_names[c], " ...")

		set.seed(123)
                gbm_model <- gbm(as.formula(model_expression[c]),
                                            data = input_training[training_sequence,],
                                            distribution = "gaussian",
                                            n.trees = max(num.trees.xyz[[c]]),
                                            shrinkage = shrinkage.xyz[[c]][l-4],
                                            interaction.depth=interaction.depth.xyz[[c]][l-4])
	
		###################################################
		# For each x, y and z coordinates,
		# get relative importance of each input variable
                message("    ---------------------------------------------")
		message("    * Relative importance of each input variable:")
		print(summary(gbm_model))
		
		###################################################	
		# Saving model
                output_model <- paste(models_output_dir,
                                      "/gbm_model_", xyz_names[c], l, ".rda", sep="")
		saveRDS(gbm_model, file = output_model)

		###################################################
		# Predict 5th hit from GBM model
                message("    ---------------------------------------------")
                message("    * Predicting 5th hit ...")
		predict.xyz <- predict( gbm_model,
		   		        newdata = input_optimization[optimization_sequence,],
		   			n.trees = num.trees.xyz[[c]] )

		###################################################
		# Computing MSE (error)
		var_name <- paste( xyz_names[c], "_5", sep="" )
		error <- apply( (predict.xyz -
				 input_optimization_p5[optimization_p5, var_name])^2,
			        2, mean )
		cat( "    * Minimum MSE(", xyz_names[c], ") =", min(error), " mm^2 |",
		     names(which(error == min(error))), "trees |",
		     "tree position =", which(error == min(error)), "\n" )

		###################################################
		# Save image of MSE vs. Number of Trees
                message("    ---------------------------------------------")
                message("    * Saving image of MSE vs. Number of Trees ...")
		save_image <- paste( folder_figures, "/c", l,
				     "/mse_vs_trees_", xyz_names[c], ".png", sep="" )
		png(save_image, units="px", width=1600, height=1600, res=250)
		plot( num.trees.xyz[[c]], error,
		      pch = 18, col="blue", log="y",
		      ylab = paste("Mean Squared Error(", xyz_names[c], ")", sep=""),
		      xlab = "Number of Trees",
		      main = "Optimization Step" )
		abline( h=min(error), col="red", lty=2 )
		abline( v=names(which( error == min(error) )), col="green4", lty=2 )
		legend( min(num.trees.xyz[[c]])  +
		        0.4*( max(num.trees.xyz[[c]]) - min(num.trees.xyz[[c]]) ),
		        max(error),
		        legend = c(paste("MSE(", xyz_names[c], ")", sep=""),
                		   paste("Minimum MSE = ", round(min(error),2), " mm^2"),
		                   paste("Number of trees =",
					 names(which(error == min(error))))),
		        pch = c(20,3,3),
		        col = c("blue", "red", "green4") )
		dev.off()

		###################################################
                # Save image of Real vs. Predicted Values
                message("    ---------------------------------------------")
                message("    * Saving image of Real vs. Predicted Values ...")
                save_image <- paste( folder_figures, "/c", l,
                                     "/real_vs_pred_", xyz_names[c], ".png", sep="" )
                png(save_image, units="px", width=1600, height=1600, res=250)
		plot( predict.xyz[, as.numeric(which(error == min(error)))],
		      input_optimization_p5[optimization_p5, var_name],
		      xlab = paste("Predicted ", xyz_names[c], " (mm)", sep=""),
		      ylab = paste("Real ", xyz_names[c], " (mm)", sep=""),
		      main = "Optimization Step" )
		fit.xyz <- lm( input_optimization_p5[optimization_p5, var_name] ~
		   	       predict.xyz[, as.numeric(which(error == min(error)))] )
		abline( fit.xyz, col="red" )
		legend( min(predict.xyz[, as.numeric(which(error == min(error)))]),
	       	        max(input_optimization_p5[optimization_p5, var_name]),
			legend = c( "Data",
				    paste("f(", xyz_names[c], ") = ",
					  round(fit.xyz$coefficients[1], 3), " + ",
		 			  round(fit.xyz$coefficients[2], 3), ".",
					  xyz_names[c], sep="") ),
		        pch = c(1, 3), col = c("black", "red") )
		dev.off()

	} # for( c in xyz_coordinates ) -> Loop over x, y and z
} # for( l in layers ) -> Loop over layers 5, 6, 7, 8, 9 and 10

message("* Done!")
message("*********************************************")


