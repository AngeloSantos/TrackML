#

###################################################
### Modify the lines bellow #######################
###

training_file_name        = "data/data_2020_06_12/training_sequences_3.csv"
optimization_file_name    = "data/optimization_sequences-05_05.csv"
optimization_p5_file_name = "data/optimization_p5-05_05.csv"

#training_sequence        = c(seq(1, 126000, 6))
training_sequence        = c(35001:42000)
optimization_sequence    = c(15001:18000)
#optimization_sequence    = c(1:18000)
optimization_sequence_p5 = c(15001:18000)
#optimization_sequence_p5 = c(1:18000)

num.trees.x = c(seq(20, 1000, 20))
num.trees.y = c(seq(20, 1000, 20))
num.trees.z = c(seq(20, 1000, 20))

test_vs_pred.x="test_vs_pred_x.png"
test_vs_pred.y="test_vs_pred_y.png"
test_vs_pred.z="test_vs_pred_z.png"

mse_vs_trees.x="mse_vs_trees_x.png"
mse_vs_trees.y="mse_vs_trees_y.png"
mse_vs_trees.z="mse_vs_trees_z.png"

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

# Training model for variables x_5, y_5 and z_5
message("*********************************************")
message("* Training model for variables x_5, y_5 and z_5:")
message("*** x_5...")
set.seed(123)
gbm.x = gbm(x_5 ~ x_1 + x_2 + x_3 + x_4 +
	    	  y_1 + y_2 + y_3 + y_4 +
		  z_1 + z_2 + z_3 + z_4,
	    data = input_training[training_sequence,],
	    distribution = "gaussian",
	    n.trees = max(num.trees.x),
	    shrinkage = 0.04,
	    interaction.depth = 18)

message("*** y_5...")
set.seed(123)
gbm.y = gbm(y_5 ~ x_1 + x_2 + x_3 + x_4 +
	          y_1 + y_2 + y_3 + y_4 +
		  z_1 + z_2 + z_3 + z_4,
            data = input_training[training_sequence,],
            distribution = "gaussian",
            n.trees = max(num.trees.y),
            shrinkage = 0.06,
            interaction.depth = 18)

message("*** z_5...")
set.seed(123)
gbm.z = gbm(z_5 ~ x_1 + x_2 + x_3 + x_4 +
	          y_1 + y_2 + y_3 + y_4 +
		  z_1 + z_2 + z_3 + z_4,
            data = input_training[training_sequence,],
            distribution = "gaussian",
            n.trees = max(num.trees.z),
            shrinkage = 0.12,
            interaction.depth = 14)

# For each x_5, y_5 and z_5,
# get relative importance of each input variable
message("*********************************************")
message("* Relative importance of each input variable:")
message("*** for x_5:")
print(summary(gbm.x))
message("*** for y_5:")
print(summary(gbm.y))
message("*** for z_5:")
print(summary(gbm.z))

# Start prediction from trained models
message("*********************************************")
message("* Starting prediction")
message("*** for x_5...")
predict.x = predict( gbm.x,
		     newdata = input_optimization[optimization_sequence,],
		     n.trees = num.trees.x )
message("*** for y_5...")
predict.y = predict( gbm.y,
		     newdata = input_optimization[optimization_sequence,],
		     n.trees = num.trees.y )
message("*** for x_5...")
predict.z = predict( gbm.z,
		     newdata = input_optimization[optimization_sequence,],
		     n.trees = num.trees.z )

message("*********************************************")
cat("* Dimension x: ", dim(predict.x), "\n")
cat("* Dimension y: ", dim(predict.y), "\n")
cat("* Dimension z: ", dim(predict.z), "\n")

# Compute errors
message("*********************************************")
message("* Computing errors")
message("---------------------------------------------")
message("*** for x_5...")
error.x = with( input_optimization_p5[optimization_sequence_p5,],
                apply( (predict.x - x_5)^2, 2, mean) )
cat("*** Minimum MSE(x) =", min(error.x), " mm^2 |",
    names(which(error.x == min(error.x))), "trees |",
    "tree position =", which(error.x == min(error.x)), "\n")
png(mse_vs_trees.x, units="px", width=1600, height=1600, res=250)
plot(num.trees.x, error.x,
     pch = 18, col="blue", log="y",
     ylab = "Mean Squared Error(X)",
     xlab = "Number of Trees",
     main = "Optimization Step")
abline( h=min(error.x), col="red", lty=2 )
abline( v=names(which( error.x == min(error.x) )), col="green4", lty=2 )
legend( min(num.trees.x) + (max(num.trees.x) - min(num.trees.x))/2,
       	max(error.x),
	legend = c("MSE(X)",
		   paste("Minimum MSE = ", round(min(error.x),1), " mm^2"),
		   paste("# of trees =", names(which(error.x == min(error.x))))),
        pch = c(20,3,3),
	col = c("blue", "red", "green4"))
dev.off()

message("---------------------------------------------")
message("*** for y_5...")
error.y = with( input_optimization_p5[optimization_sequence_p5,],
	        apply( (predict.y - y_5)^2, 2, mean) )
cat("*** Minimum MSE(y) =", min(error.y), " mm^2 |",
    names(which(error.y == min(error.y))), "trees |",
    "tree position =", which(error.y == min(error.y)), "\n")
png(mse_vs_trees.y, units="px", width=1600, height=1600, res=250)
plot(num.trees.y, error.y,
     pch = 18, col="blue", log="y",
     ylab = "Mean Squared Error(Y)",
     xlab = "Number of Trees",
     main = "Optimization Step")
abline( h=min(error.y), col="red", lty=2 )
abline( v=names(which( error.y == min(error.y) )), col="green4", lty=2 )
legend( min(num.trees.y) + (max(num.trees.y) - min(num.trees.y))/2,
        max(error.y),
        legend = c("MSE(Y)",
                   paste("Minimum MSE = ", round(min(error.y),1), " mm^2"),
                   paste("# of trees =", names(which(error.y == min(error.y))))),
        pch = c(20,3,3),
        col = c("blue", "red", "green4"))
dev.off()

message("---------------------------------------------")
message("*** for z_5...")
error.z = with( input_optimization_p5[optimization_sequence_p5,],
	        apply( (predict.z - z_5)^2, 2, mean) )
cat("*** Minimum MSE(z) =", min(error.z), " mm^2 |",
    names(which(error.z == min(error.z))), "trees |",
    "tree position =", which(error.z == min(error.z)), "\n")
png(mse_vs_trees.z, units="px", width=1600, height=1600, res=250)
plot(num.trees.z, error.z,
     pch = 18, col="blue", log="y",
     ylab = "Mean Squared Error(Z)",
     xlab = "Number of Trees",
     main = "Optimization Step")
abline( h=min(error.z), col="red", lty=2 )
abline( v=names(which( error.z == min(error.z) )), col="green4", lty=2 )
legend( min(num.trees.z) + (max(num.trees.z) - min(num.trees.z))/2,
        max(error.z),
        legend = c("MSE(Z)",
                   paste("Minimum MSE = ", round(min(error.z),1), " mm^2"),
                   paste("# of trees =", names(which(error.z == min(error.z))))),
        pch = c(20,3,3),
        col = c("blue", "red", "green4"))
dev.off()

# Drawing Real vs Predicted values
message("*********************************************")
message("* Drawing Real vs Predicted values")
message("---------------------------------------------")
message("*** for x_5...")
png(test_vs_pred.x, units="px", width=1600, height=1600, res=250)
plot( predict.x[, as.numeric(which(error.x == min(error.x)))],
     input_optimization_p5$x_5[optimization_sequence_p5],
     xlab="Predicted X (mm)",
     ylab="Real X (mm)",
     main="Optimization Step" )
fit.x = lm( input_optimization_p5$x_5[optimization_sequence_p5] ~
	    predict.x[, as.numeric(which(error.x == min(error.x)))] )
abline( fit.x, col="red" )
legend( min(predict.x[, as.numeric(which(error.x == min(error.x)))]),
        max(input_optimization_p5$x_5[optimization_sequence_p5]),
        legend = c( "Data",
		    paste("f(x) = ",
			  round(fit.x$coefficients[1], 3), " + ",
			  round(fit.x$coefficients[2], 3), ".x") ),
        pch = c(1, 3), col = c("black", "red") )
dev.off()

message("---------------------------------------------")
message("*** for y_5...")
png(test_vs_pred.y, units="px", width=1600, height=1600, res=250)
plot( predict.y[, as.numeric(which(error.y == min(error.y)))],
     input_optimization_p5$y_5[optimization_sequence_p5],
     xlab="Predicted Y (mm)",
     ylab="Real Y (mm)",
     main="Optimization Step" )
fit.y = lm( input_optimization_p5$y_5[optimization_sequence_p5] ~
            predict.y[, as.numeric(which(error.y == min(error.y)))] )
abline( fit.y, col="red" )
legend( min(predict.y[, as.numeric(which(error.y == min(error.y)))]),
        max(input_optimization_p5$y_5[optimization_sequence_p5]),
        legend = c( "Data",
                    paste("f(y) = ",
                          round(fit.y$coefficients[1], 3), " + ",
                          round(fit.y$coefficients[2], 3), ".y") ),
        pch = c(1, 3), col = c("black", "red") )

dev.off()

message("---------------------------------------------")
message("*** for z_5...")
png(test_vs_pred.z, units="px", width=1600, height=1600, res=250)
plot( predict.z[, as.numeric(which(error.z == min(error.z)))],
     input_optimization_p5$z_5[optimization_sequence_p5],
     xlab="Predicted Z (mm)",
     ylab="Real Z (mm)",
     main="Optimization Step" )
fit.z = lm( input_optimization_p5$z_5[optimization_sequence_p5] ~
            predict.z[, as.numeric(which(error.z == min(error.z)))] )
abline( fit.z, col="red" )
legend( min(predict.z[, as.numeric(which(error.z == min(error.z)))]),
        max(input_optimization_p5$z_5[optimization_sequence_p5]),
        legend = c( "Data",
                    paste("f(z) = ",
                          round(fit.z$coefficients[1], 3), " + ",
                          round(fit.z$coefficients[2], 3), ".z") ),
        pch = c(1, 3), col = c("black", "red") )
dev.off()






