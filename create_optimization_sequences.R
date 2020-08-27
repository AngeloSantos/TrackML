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
#         ||             ====================                    1,2,3,4,5      #
#         ||             ====================                   ===========     #
#         ||           / * * * * *           \ (track 1)       / * * * * * \    #
#         ||           | * * * * *           | (track 2)       | * * * * * |    #
#         ||           | * * * * *           | (track 3)       | * * * * * |    #
#         ||           |   * * * * *         | (track 1)       | * * * * * |    #
#         ||           |   * * * * *         | (track 2)       | * * * * * |    #
#         ||           |   * * * * *         | (track 3)       | * * * * * |    #
#         ||           |     * * * * *       |        .        | * * * * * |    #
#         ||           |     * * * * *       |        .        | * * * * * |    #
#         \/           |     * * * * *       |        .        | * * * * * |    #
#    6 sequences ----> |       * * * * *     |---> Output ---> | * * * * * |    #
#     of 5 hits        |       * * * * *     |                 | * * * * * |    #
#                      |       * * * * *     |                 | * * * * * |    #
#                      |         * * * * *   |                 | * * * * * |    #
#                      |         * * * * *   |                 | * * * * * |    #
#                      |         * * * * *   |                 | * * * * * |    #
#                      |           * * * * * |                 | * * * * * |    #
#                      |           * * * * * |                 | * * * * * |    #
#                      \           * * * * * /                 \ * * * * * /    #
#                       =====================                   ===========     #
#                                                                               #
#################################################################################

#############################################
### Modify the 3 variables bellow ###########
input_file_name  = "data/optimization_sample-05_05.csv"
output_file_name = "optimization_sequences-05_05.csv"
all_column_names <- c("sample_id", "hit_id_1", "x_1", "y_1", "z_1", "rho_1", "eta_1", "phi_1", "volume_id_1", "layer_id_1", "module_id_1", "value_1", "hit_id_2", "x_2", "y_2", "z_2", "rho_2", "eta_2", "phi_2", "volume_id_2", "layer_id_2", "module_id_2", "value_2", "hit_id_3", "x_3", "y_3", "z_3", "rho_3", "eta_3", "phi_3", "volume_id_3", "layer_id_3", "module_id_3", "value_3", "hit_id_4", "x_4", "y_4", "z_4", "rho_4", "eta_4", "phi_4", "volume_id_4", "layer_id_4", "module_id_4", "value_4")

simple_column_names <- c("hit_id", "x", "y", "z", "rho", "eta", "phi",
                         "volume_id", "layer_id", "module_id", "value")

layers <- c(5:10)

#############################################

# Copying input file
message("************************************************")
message("* Reading input file: ", input_file_name)
input_file = read.csv(input_file_name)
nrows_input = nrow(input_file)
ncols_input = ncol(input_file)
message("* Dimensions: ",
	nrows_input, " rows and ",
	ncols_input, " columns")
message("************************************************")

#11-54
#22-65
#33-76
#44-87
#55-98
#66-109

# Create sequences
message("* Creating optimization sequence from input file...")
optimization_sequence <- c()
#hit <- c()
#for( i in 1:6 ){
#	message("* Camada: ", i+4)
#	for( j in 1:nrows_input ){
#		cat(j, "\b\b\b\b\b\b\b\b\b\b\b")
#		hit <- c()
#		hit <- c(hit, input_file[j, 1])
#		sequences = seq(i*11, (i*11 + 43), 1)
#		for( k in sequences ){
#			hit <- c(hit, input_file[j, k])
#		}
#		optimization_sequence <- rbind(optimization_sequence, hit)
#	}
#}
#optimization_sequence <- data.frame(optimization_sequence)

# Run over layers 5 to 10
sample.id <- c()
count = 1
for( l in (layers-1) ){
        column_names <- c()
        for( c in (l-4):(l-1) ){
                column_names <- c( column_names,
                                   sapply(simple_column_names, paste0, "_", c,
                                          USE.NAMES=FALSE) )
        }
        sequences <- input_file[, column_names]
        if( count > 1 ) names(optimization_sequence) <- names(sequences)
        optimization_sequence <- rbind(optimization_sequence, sequences)

        sample.id <- c(sample.id, input_file[,1])
        count = count + 1
}
optimization_sequence <- cbind(sample.id, optimization_sequence)

# Rename columns of data frame
message("************************************************")
message("* Renaming columns of data frame")
for( i in 1:length(all_column_names) ){
	colnames(optimization_sequence)[i] <- paste(all_column_names[i])
}
#message("* Column names: ", col_names)

#colnames(optimization_sequence)[1] <- paste(col_names[1])

# Create ouput file
message("************************************************")
message("* Writing optimization sequence into output file...")
write.csv(optimization_sequence, file = output_file_name, row.names=FALSE)
message("* Dimensions: ",
	nrow(optimization_sequence), " rows and ",
	ncol(optimization_sequence), " columns")
message("* Done!")
message("************************************************")

