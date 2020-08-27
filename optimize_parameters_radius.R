#########################################################################
message("************************************************************")
message("* Get optimized parameters to predict the track curvature...")

###################################################
### Modify the lines bellow #######################
###

training_file_name        = "data/training_sample-05_05.csv"
optimization_file_name    = "data/optimization_sample-05_05.csv"
#optimization_p5_file_name = "data/optimization_p5-05_05.csv"

heatmap_output_folder = "momentum/optimization_2020_07_05/heatmaps"
output_file_name      = "optimization_workspace.RData"

#training_nrows        = 21000
#optimization_nrows    = 3000
#optimization_p5_nrows = 3000

q.B = 0.6 # pT/Radiu (GeV/c/m)

layers <- c(1:10)

#xyz_coordinates = 1
#xyz_names = "R"

input.vars <- c()
for( l in (layers-1) ) input.vars <- c(input.vars, paste0("x_", l))
for( l in (layers-1) ) input.vars <- c(input.vars, paste0("y_", l))

all.vars <- c(input.vars, "R")

model_expression <- paste("R", paste(input.vars, collapse=" + "), sep=" ~ ") # Radius

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
radius.train <- pt.train / q.B

# Compute Radius for optimization sample
px.optim     <- input_optimization[,"px"]
py.optim     <- input_optimization[,"py"]
pt.optim     <- sqrt( px.optim^2 + py.optim^2 )
radius.optim <- pt.optim / q.B

# Update training input files
input_training <- cbind(input_training, radius.train)
colnames(input_training)[ncol(input_training)] <- "R"

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
				  data = input_training[,all.vars],
	     			  distribution = "gaussian",
     				  n.trees = max(num.trees.xyz),
     				  shrinkage = shrinkage.xyz[j],
     				  interaction.depth = interaction.depth.xyz[k] )

		prediction <- predict( gbm_model,
	  			       newdata = input_optimization[,input.vars],
		   		       n.trees = num.trees.xyz )

                error <- apply( (prediction - radius.optim)^2, 2, mean )

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
	  key.xlab="Minimum MSE [m]", key.title="Color Key",
	  cexRow=1.5, cexCol=1.0, srtCol=0, adjCol=c(0.5,1))
dev.off()

# Showing results
message("*********************************************")
cat("* minMSE              = ", minMSE,              "\n")
cat("* minNTrees           = ", minNTrees,           "\n")
cat("* minShrinkage        = ", minShrinkage,        "\n")
cat("* minTreePosition     = ", minTreePosition,     "\n")
cat("* minInteractionDepth = ", minInteractionDepth, "\n")
message("---------------------------------------------")

message("* Done!")
message("*********************************************")

