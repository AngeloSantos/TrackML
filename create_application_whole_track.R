########################################

input_file_name  = "data/data_2020_05_28/application_sample.csv"
output_file_name = "data/data_2020_05_28/application_whole_track.csv"

layers <- c(1:10)

input <- read.csv(input_file_name)

application <- input[, c(1, c(11:120)) ]

column_names <- c("hit_id", "x", "y", "z", "rho", "eta", "phi",
                  "volume_id", "layer_id", "module_id", "value")

colnames(application)[1] <- "sample_id"

count.column = 2
for( l in layers ){
	for( c in 1:length(column_names) ){
		column.name <- paste0(column_names[c], "_", l)
		colnames(application)[count.column] <- column.name
		count.column = count.column + 1
	}
}

write.csv( application, file=output_file_name, row.names=FALSE )

