#########################

input_file_name <- c( "data/data_2020_05_28/training_sequences_1.csv",
		      "data/data_2020_05_28/training_sequences_2.csv",
		      "data/data_2020_05_28/training_sequences_3.csv",
		      "data/data_2020_05_28/training_sequences_4.csv",
		      "data/data_2020_05_28/training_sequences_5.csv",
		      "data/data_2020_05_28/training_sequences_6.csv",
		      "data/data_2020_05_28/training_sequences_7.csv",
		      "data/data_2020_05_28/training_sequences_8.csv",
		      "data/data_2020_05_28/training_sequences_9.csv",
		      "data/data_2020_05_28/training_sequences_10.csv" )


output_file_name = "figures/optimization_2020_06_05/training_hits_10pred_zy.png"

train.colors <- c("red", "blue", "forestgreen", "darkorchid", "darkorange4",
		  "gold", "deepskyblue", "hotpink", "mediumspringgreen", "gold4")

x.col.names   <- c("x_1", "x_2", "x_3", "x_4", "x_5")
y.col.names   <- c("y_1", "y_2", "y_3", "y_4", "y_5")
z.col.names   <- c("z_1", "z_2", "z_3", "z_4", "z_5")
rho.col.names <- c("rho_1", "rho_2", "rho_3", "rho_4", "rho_5")

message("**********************************")
message("* Reading input files ...")
input_file <- list()
for( i in 1:length(input_file_name) ){
	input_file[[i]] <- read.csv(input_file_name[i])
	message("  * File: ", input_file_name[i])
	message("  * Dimensions: ",
       		nrow(input_file[i]), " rows | ",
       		ncol(input_file[i]), " columns")
}

xx  <- list()
yy  <- list()
zz  <- list()
rho <- list()

message("**********************************")
message("* Reading x, y and z...")
for( i in 1:length(input_file_name) ){
	xx[[i]] <- unlist(input_file[[i]][, x.col.names], use.names=FALSE)
	yy[[i]] <- unlist(input_file[[i]][, y.col.names], use.names=FALSE)
	zz[[i]] <- unlist(input_file[[i]][, z.col.names], use.names=FALSE)
}

message("**********************************")
message("* Create figures...")
png(output_file_name,
    units="px", width=1600, height=1600, res=250)
plot(zz[[5]], yy[[5]],
     xlab="z (mm)", ylab="y (mm)",
     main="",
     pch=20, col=train.colors[[5]])
#for( i in 2:length(input_file_name) ){
#	points(zz[[i]], yy[[i]],
#	       pch=20, col=train.colors[[i]])
#}
dev.off()


message("* Done!")
message("**********************************")
