###

input_file_name  = "data/training_sequences-05_05.csv"
output_folder    = "data/data_2020_06_12"
output_name      = "training_sequences"

input_row_range     = 21000
divide_row_range_by = 3
layers              = c(5:10)

# Copying input file
message("************************************************")
message("* Reading input file: ", input_file_name)
input_file = read.csv(input_file_name)
nrows_input = nrow(input_file)
ncols_input = ncol(input_file)
message("* Dimensions: ", nrows_input, " rows | ", ncols_input, " columns")

training_dataset <- list()

# Divide input file
message("************************************************")
message("* Starting to divide input file by ", divide_row_range_by)
for( d in 1:divide_row_range_by ){
	cat(".")

	training_sequence <- c()

	# Run over layers
	for( l in (layers-4) ){

		row_range = (input_row_range / divide_row_range_by)

		first_row = 1 + ((l-1)*input_row_range) + ((d-1)*row_range)
		last_row  = first_row + row_range - 1

		training_sequence <- rbind( training_sequence,
					    input_file[first_row:last_row,] )
	} # for( l in (layers-4) )

	# Copy dataset
	training_dataset[[d]] <- training_sequence
} # for( d in 1:divide_row_range_by )
cat("\n")

# Create output files
message("************************************************")
message("* Writing training sequence into output file ...")
for( d in 1:divide_row_range_by ){
	output_file_name = paste0(output_folder, "/", output_name, "_", d, ".csv")
	message("  * File: ", output_file_name)
	write.csv( training_dataset[[d]], file = output_file_name, row.names=FALSE )
	message("  * Dimensions: ",
		nrow(training_sequence), " rows | ",
		ncol(training_sequence), " columns")
} # for( d in 1:divide_row_range_by )

message("* Done!")
message("************************************************")

