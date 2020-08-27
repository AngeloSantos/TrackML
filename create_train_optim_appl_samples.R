#############################


input_file_name               = "data/data_2020_05_28/eta_n0.5-0.5_phi_ninf-pinf_20200528124042_tracks.csv"
output_file_name_training     = "data/data_2020_05_28/training_sample-05_05.csv"
output_file_name_optimization = "data/data_2020_05_28/optimization_sample-05_05.csv"
output_file_name_application  = "data/data_2020_05_28/application_sample-05_05.csv"

training_rate     = 0.7
optimization_rate = 0.1

first_column = 1
last_column  = 119

message("************************************************")
message("* Reading input file: ", input_file_name)
input_file = read.csv(input_file_name)
nrows_input = nrow(input_file)
ncols_input = ncol(input_file)
message("* Dimensions: ",
	nrows_input, " rows and ",
       	ncols_input, " columns")
message("************************************************")

message("* Creating training sample from input file...")
set.seed(123)
training_tracks      = sample( 1:nrows_input, (training_rate * nrows_input) )
output_file_training = input_file[ sort(training_tracks), first_column:last_column ]
message("* Dimensions: ",
	nrow(output_file_training), " rows and ",
	ncol(output_file_training), " columns")
message("************************************************")

message("* Creating optimization sample from input file...")
remaining_tracks         = ( seq(1, nrows_input, 1) )[-training_tracks]
set.seed(123)
optimization_tracks      = sample( remaining_tracks, optimization_rate*nrows_input )
output_file_optimization = input_file[ sort(optimization_tracks), first_column:last_column ]
message("* Dimensions: ",
	nrow(output_file_optimization), " rows and ",
	ncol(output_file_optimization), " columns")
message("************************************************")

message("* Creating application sample from input file...")
application_tracks      = setdiff(remaining_tracks, optimization_tracks)
output_file_application = input_file[ application_tracks, first_column:last_column ]
message("* Dimensions: ",
	nrow(output_file_application), " rows and ",
	ncol(output_file_application), " columns")
message("************************************************")

message("* Writing output file: ",  output_file_name_training)
write.csv(output_file_training,     file = output_file_name_training)
message("* Writing output file: ",  output_file_name_optimization)
write.csv(output_file_optimization, file = output_file_name_optimization)
message("* Writing output file: ",  output_file_name_application)
write.csv(output_file_application,  file = output_file_name_application)
message("* Done!")
message("************************************************")

