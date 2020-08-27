#################################################################################
#                                                                               #
#  This script reads a test input file of 10 subsequent hits per track and      #
#  split each track into 6 sequences of 5 subsequent hits. However, the next    #
#  sequence of 5 hits only starts after completing running over all tracks.     #
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
input_file_name  = "data/data_2020_05_28/application_sample-05_05.csv"
output_file_name = "data/data_2020_05_28/application_sequences-05_05.csv"
col_names <- c("sample_id", "hit_id_1", "x_1", "y_1", "z_1", "rho_1", "eta_1", "phi_1", "volume_id_1", "layer_id_1", "module_id_1", "value_1", "hit_id_2", "x_2", "y_2", "z_2", "rho_2", "eta_2", "phi_2", "volume_id_2", "layer_id_2", "module_id_2", "value_2", "hit_id_3", "x_3", "y_3", "z_3", "rho_3", "eta_3", "phi_3", "volume_id_3", "layer_id_3", "module_id_3", "value_3", "hit_id_4", "x_4", "y_4", "z_4", "rho_4", "eta_4", "phi_4", "volume_id_4", "layer_id_4", "module_id_4", "value_4")
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

# Create sequences
message("* Creating application sequence from input file...")
#application_sequence <- c()
#hit <- c()
##for( i in 1:6 ){
##	message("* Camada: ", i+4)
##	for( j in 1:nrows_input ){
##		cat(j, "\b\b\b\b\b\b\b\b\b\b\b")
##		hit <- c()
##		hit <- c(hit, input_file[j,1])
##		sequences = seq(i*11, (i*11 + 43), 1)
##		for( k in sequences ){
##			hit <- c(hit, input_file[j, k])
##		}
##		application_sequence <- rbind(application_sequence, hit)
##	}
##}
#for( i in 1:nrows_input ){
#	cat(i, "\b\b\b\b\b\b\b\b\b\b\b")
#        hit <- c()
#        hit <- c(hit, input_file[i,1])
#        sequences = seq(11, (11 + 43), 1)
#        for( j in sequences ){
#                hit <- c(hit, input_file[i, j])
#        }
#        application_sequence <- rbind(application_sequence, hit)
#}
#application_sequence <- data.frame(application_sequence)

application_sequence <- input_file[, col_names[-1]]
application_sequence <- cbind(input_file[,1], application_sequence)

# Rename columns of data frame
message("************************************************")
message("* Renaming columns of data frame")
#for( i in 1:length(col_names) ){
#	colnames(application_sequence)[i] <- paste(col_names[i])
#}
#message("* Column names: ", col_names)

colnames(application_sequence)[1] <- paste(col_names[1])

# Create ouput file
message("************************************************")
message("* Writing application sequence into output file...")
write.csv(application_sequence, file = output_file_name, row.names=FALSE)
message("* Dimensions: ",
	nrow(application_sequence), " rows and ",
       	ncol(application_sequence), " columns")
message("* Done!")
message("************************************************")

