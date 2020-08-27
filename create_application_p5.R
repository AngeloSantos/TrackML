#################################################################################
#                                                                               #
#  This script reads an optimization input file of 10 subsequent hits per       #
#  track and split each track into 6 sequences of 5 subsequent hits. However,   #
#  the next sequence of 5 hits only starts after completing running over all    #
#  tracks.                                                                      #
#                                                                               #
#  Example:              1,2,3,4,5,6,7,8,9,10                                   #
#                        ====================                                   #
#                       / * * * * * * * * * *  (track 1)                        #
#    Input (3 tracks) ->| * * * * * * * * * *  (track 2)                        #
#         ||            \ * * * * * * * * * *  (track 3)                        #
#         ||             ====================                    5              #
#         ||             ====================                   ===             #
#         ||           /         *           \ (track 1)       / * \            #
#         ||           |         *           | (track 2)       | * |            #
#         ||           |         *           | (track 3)       | * |            #
#         ||           |           *         | (track 1)       | * |            #
#         ||           |           *         | (track 2)       | * |            #
#         ||           |           *         | (track 3)       | * |            #
#         ||           |             *       |        .        | * |            #
#         ||           |             *       |        .        | * |            #
#         \/           |             *       |        .        | * |            #
#    6 sequences ----> |               *     |---> Output ---> | * |            #
#     of 5 hits        |               *     |                 | * |            #
#                      |               *     |                 | * |            #
#                      |                 *   |                 | * |            #
#                      |                 *   |                 | * |            #
#                      |                 *   |                 | * |            #
#                      |                   * |                 | * |            #
#                      |                   * |                 | * |            #
#                      \                   * /                 \ * /            #
#                       =====================                   ===             #
#                                                                               #
#################################################################################

#############################################
### Modify the 3 variables bellow ###########
input_file_name  = "data/data_2020_05_28/application_sample-05_05.csv"
output_file_name = "data/data_2020_05_28/application_p5-05_05.csv"
all_column_names <- c("sample_id", "hit_id_5", "x_5", "y_5", "z_5", "rho_5", "eta_5", "phi_5", "volume_id_5", "layer_id_5", "module_id_5", "value_5")

simple_column_names <- c("hit_id", "x", "y", "z", "rho", "eta", "phi",
                         "volume_id", "layer_id", "module_id", "value")

layers <- c(5:10)
#############################################

# Copying input file
message("************************************************")
message("* Reading input file: ", input_file_name)
input_file  = read.csv(input_file_name)
nrows_input = nrow(input_file)
ncols_input = ncol(input_file)
message("* Dimensions: ",
	nrows_input, " rows and ",
	ncols_input, " columns")
message("************************************************")

# Create sequences
message("* Reading columns for the 5th hit from optimization input file...")
application_sequence <- c()
#hit <- c()
#for( i in 5:10 ){
#        message("* Camada: ", i)
#        for( j in 1:nrows_input ){
#                cat(j, "\b\b\b\b\b\b\b\b\b\b\b")
#                hit <- c()
#                hit <- c(hit, input_file[j,1])
#                sequences = seq(i*11, (i*11 + 10), 1)
#                for( k in sequences ){
#                        hit <- c(hit, input_file[j, k])
#                }
#                application_sequence <- rbind(application_sequence, hit)
#        }
#}
#application_sequence <- data.frame(application_sequence)

# Run over layers 5 to 10
sample.id <- c()
count = 1
for( l in (layers-1) ){
        column_names <- sapply(simple_column_names, paste0, "_", l, USE.NAMES=FALSE)
        sequences <- input_file[, column_names]
        if( count > 1 ) names(application_sequence) <- names(sequences)
        application_sequence <- rbind(application_sequence, sequences)

        sample.id <- c(sample.id, input_file[,1])
        count = count + 1
}
application_sequence <- cbind(sample.id, application_sequence)

# Rename columns of data frame
message("************************************************")
#message("* Renaming columns of data frame")
for( i in 1:length(all_column_names) ){
	colnames(application_sequence)[i] <- paste(all_column_names[i])
}
#message("* Column names: ", col_names)

#colnames(application_sequence)[1] <- paste(col_names[1])

# Create ouput file
message("************************************************")
message("* Writing application sequence into output file...")
write.csv(application_sequence, file = output_file_name, row.names=FALSE)
message("* Dimensions: ",
	nrow(application_sequence), " rows and ",
	ncol(application_sequence), " columns")
message("* Done!")
message("************************************************")

