#########################################################################
message("************************************************************")
message("* Get optimized parameters to predict the transverse momentum (pT)...")

###################################################
### Modify the lines bellow #######################
###

training_file_name        = "data/training_sample-05_05.csv"
optimization_file_name    = "data/optimization_sample-05_05.csv"

#optimization_p5_file_name = "data/optimization_p5-05_05.csv"

heatmap_output_folder = "momentum/optimization_2020_07_05_v2/heatmaps"
output_file_name      = "optimization_workspace.RData"
models_output_dir     = "momentum/optimization_2020_07_05_v2/models"

#training_nrows        = 21000
#optimization_nrows    = 3000
#optimization_p5_nrows = 3000

#q.B = 0.6 # pT/Radius (GeV/c/m)

layers <- c(1:10)

#xyz_coordinates = 1
#xyz_names = "R"

input.vars <- c()
for( l in (layers-1) ) input.vars <- c(input.vars, paste0("x_", l))
for( l in (layers-1) ) input.vars <- c(input.vars, paste0("y_", l))

all.vars <- c(input.vars, "pt")

model_expression <- paste("pt", paste(input.vars, collapse=" + "), sep=" ~ ") # pT

num.trees.xyz <- seq(20, 1000, 20)
shrinkage.xyz <- seq(0.02, 0.4, 0.02)
interaction.depth.xyz <- seq(2, 40, 2)

#minMSE <- list()
#minNTrees <- list()
#minShrinkage <- list()
#minTreePosition <- list()
#minInteractionDepth <- list()
#
#mapNtreeShrinkage <- list()

###
###################################################
###################################################

# Read input files
message("*********************************************")
message("* Reading training and optimization input files...")
input_training        = read.csv(training_file_name)
input_optimization    = read.csv(optimization_file_name)
#input_optimization_p5 = read.csv(optimization_p5_file_name)

# Compute Radius for training sample
px.train     <- input_training[,"px"]
py.train     <- input_training[,"py"]
pt.train     <- sqrt( px.train^2 + py.train^2 )
#radius.train <- pt.train / q.B

# Compute Radius for optimization sample
px.optim     <- input_optimization[,"px"]
py.optim     <- input_optimization[,"py"]
pt.optim     <- sqrt( px.optim^2 + py.optim^2 )
#radius.optim <- pt.optim / q.B

# Update training input files
input_training <- cbind(input_training, pt.train)
colnames(input_training)[ncol(input_training)] <- "pt"

# Start Machine Learning Model
# BDT - GBM
require(gbm)

message("*********************************************")
message("* Starting optimization of parameters...")

min_MSE              = 99999999999999
min_NTrees           = 99999999999999
min_Shrinkage        = 99999999999999
min_TreePosition     = 99999999999999
min_InteractionDepth = 99999999999999

create_map <- c()

for( j in 1:length(shrinkage.xyz) ){
	cat("    * Shrinkage = ", shrinkage.xyz[j], "\n")
	cat("      * intDepth = ")

	minError <- c()

	for( k in 1:length(interaction.depth.xyz) ){
		cat(interaction.depth.xyz[k], "...")

		set.seed(123)		
		gbm_model <- gbm( as.formula(model_expression),
				  data              = input_training[,all.vars],
	     			  distribution      = "gaussian",
     				  n.trees           = max(num.trees.xyz),
     				  shrinkage         = shrinkage.xyz[j],
     				  interaction.depth = interaction.depth.xyz[k] )

		prediction <- predict( gbm_model,
	  			       newdata = input_optimization[,input.vars],
		   		       n.trees = num.trees.xyz )

                error <- apply( (prediction - pt.optim)^2, 2, mean )

		minError <- c( minError, min(error) )

		if( min(error) < min_MSE ){
			min_MSE              = min(error) # Given in meters (m)
			min_NTrees           = names(which(error==min(error)))
			min_Shrinkage        = shrinkage.xyz[j]
			min_TreePosition     = which(error == min(error))
			min_InteractionDepth = interaction.depth.xyz[k]
		}
	} # for( k in 1:length(interaction.depth.xyz[[c]]) )

	create_map <- rbind(create_map, minError)
	cat("\n")

} # for( j in 1:length(shrinkage.xyz[[c]]) )
cat("\n")

# Creating Matrix
create_map <- as.matrix(create_map)

# Renaming columns and rows of the matrix
colnames(create_map) <- c(interaction.depth.xyz)
rownames(create_map) <- c(shrinkage.xyz)

minMSE              <- min_MSE
minNTrees           <- min_NTrees
minShrinkage        <- min_Shrinkage
minTreePosition     <- min_TreePosition
minInteractionDepth <- min_InteractionDepth
mapNtreeShrinkage   <- create_map

# For safety, it is better to save this workspace so far
save.image( file=paste0(heatmap_output_folder, "/", output_file_name) )

# Plot Heat Maps
library(gplots)
message("*********************************************")
message("* Plotting Heat Map")
save_figure <- paste0( heatmap_output_folder, "/map_radius.png", sep="")
png(save_figure, units="px", width=1600, height=1600, res=200, pointsize=15)

heatmap.2(mapNtreeShrinkage,
	  #cellnote=mapNtreeShrinkage,
	  xlab="Tree Interaction Depth",
	  ylab="Learning Rate",
	  main="Minimum MSE",
	  col = greenred(100), trace="none",
	  dendrogram="none", Colv="NA", Rowv="NA",
	  key.xlab="Minimum MSE [GeV/c]", key.title="Color Key",
	  cexRow=1.2, cexCol=1.0, srtCol=0, adjCol=c(0.5,1))
dev.off()

# Showing results
message("*********************************************")
cat("* minMSE              = ", minMSE,              "\n")
cat("* minNTrees           = ", minNTrees,           "\n")
cat("* minShrinkage        = ", minShrinkage,        "\n")
cat("* minTreePosition     = ", minTreePosition,     "\n")
cat("* minInteractionDepth = ", minInteractionDepth, "\n")
message("---------------------------------------------")

###################################################
# Training model with optimized parameters
message("*********************************************")
message("* Training model with optimized parameters")
require(gbm)
set.seed(123)
gbm_model <- gbm( as.formula(model_expression),
                  data              = input_training[,all.vars],
                  distribution      = "gaussian",
                  n.trees           = max(num.trees.xyz),
                  shrinkage         = minShrinkage,
                  interaction.depth = minInteractionDepth )
# Saving model
output_model <- paste0(models_output_dir, "/gbm_model_pT.rda")
saveRDS(gbm_model, file = output_model)

###################################################
# Prediction from GBM model
message("    ---------------------------------------------")
message("    * Predicting ...")
prediction <- predict( gbm_model,
                       newdata = input_optimization[,input.vars],
                       n.trees = num.trees.xyz )

###################################################
# Computing MSE (error)
error <- apply( (prediction - pt.optim)^2, 2, mean )
cat( "    * Minimum MSE(pT) =", min(error), " GeV/c |",
     names(which(error == min(error))), "trees |",
     "tree position =", which(error == min(error)), "\n" )

###################################################
# Save image of MSE vs. Number of Trees
message("    ---------------------------------------------")
message("    * Saving image of MSE vs. Number of Trees ...")
save_image <- paste0( models_output_dir, "/mse_vs_trees_pT.png")
png(save_image, units="px", width=1600, height=1600, res=250)
plot( num.trees.xyz,
      error,
      pch = 18, col="blue", log="y",
      ylab = paste0("Mean Squared Error(pT)"),
      xlab = "Number of Trees",
      main = "Optimization Step" )
grid( col="gray48" )
abline( h=min(error), col="red", lty=2 )
abline( v=names(which( error == min(error) )), col="green4", lty=2 )
legend( "topright",
        legend = c(paste0("MSE(pT)"),
                   paste0("Minimum MSE = ", round(min(error),2), " GeV/c"),
                   paste0("Number of trees =",
                          names(which(error == min(error))))),
        pch = c(20,3,3),
        col = c("blue", "red", "green4") )
dev.off()

###################################################
# Save image of Real vs. Predicted Values
message("    ---------------------------------------------")
message("    * Saving image of Real vs. Predicted Values ...")
save_image <- paste0( models_output_dir, "/real_vs_pred_pT.png")
png(save_image, units="px", width=1600, height=1600, res=250)
plot( prediction[, as.numeric(which(error == min(error)))],
      pt.optim,
      xlab = paste0("Predicted pT (GeV/c)"),
      ylab = paste0("Real pT (GeV/c)"),
      main = "Optimization Step" )
grid( col="gray48" )
fit.xyz <- lm( pt.optim ~ prediction[, as.numeric(which(error == min(error)))] )
abline( fit.xyz, col="red" )
legend( "topleft",
        legend = c( "Data",
                    paste0("f(pT) = ",
                           round(fit.xyz$coefficients[1], 3), " + ",
                           round(fit.xyz$coefficients[2], 3), ".",
                           "pT" )),
        pch = c(1, NA), lty = c(NA, 1), col = c("black", "red") )
dev.off()

message("* Done!")
message("*********************************************")

