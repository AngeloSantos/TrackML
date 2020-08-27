#############################

# Read input file names
input_original_name = "data/eta_n0.5-0.5_phi_ninf-pinf.csv"
input_near_name     = "figures/application_2020_05_17/nearest.hits.csv"
#input_score_name    = "/data/track-ml/bracis/datasets/eta_n0.5-0.5_phi_n0.5-0.5/eta_n0.5-0.5_phi_n0.5-0.5_20200518171238_truth.csv"
input_score_name    = "/data/track-ml/bracis/datasets/eta_n0.5-0.5_phi_ninf-pinf/eta_n0.5-0.5_phi_ninf-pinf_20200528124042_truth.csv"

# Read output file name
output_folder    = "figures/application_2020_05_17"
output_file_name = "score.RData"

#############################

# Read input files
message(" * Reading input files:")
message("   * Information of particle ID -> ", input_original_name)
input_original <- read.csv(input_original_name)
message("   * Dataset with ID of nearest hits -> ", input_near_name)
input_near     <- read.csv(input_near_name)
message("   * Table with weights of each hit ID -> ", input_score_name)
input_score    <- read.csv(input_score_name)

# Read dimensions of input files
near.nrows <- nrow(input_near) # rows from file with nearest hits
near.ncols <- ncol(input_near) # cols from file with nearest hits

message(" * Reading dimensions of input files...")
message("   * Particle ID -> ",
       	nrow(input_original), " rows | ", ncol(input_original), " columns")
message("   * Nearest  ID -> ",
        nrow(input_near),     " rows | ", ncol(input_near),     " columns")
message("   * Weights     -> ",
        nrow(input_score),    " rows | ", ncol(input_score),    " columns")

# Read ID of each track from input file with nearest hits
near.id <- input_near[, "sample_id"]

# Read particle IDs of original file correspondent to the
# tracks of nearest hits
near.particle_id <- input_original[near.id, "particle_id"]

# Read hit ID of all nearest hits
near.hit_id <- input_near[, seq(2, 111, 11)]

# Find number of layers
n.layers = ncol(near.hit_id)

message("******************************")
message("* Starting sum of scores ...")
sum.scores = 0 # sum weights of all hits
near.weight <- data.frame() # weights of each nearest hit

# Run over layers
for( l in 1:n.layers ){
	message("  * Layer ", l)

	# Run over tracks in the file with nearest hits
	for( r in 1:near.nrows ){
		cat(r, "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
	
		# Find line (from input with weights) containing
		# candidates for the correspondent nearest hit
		candidate = which((input_score[,"hit_id"]      == near.hit_id[r, l]) &
				  (input_score[,"particle_id"] == near.particle_id[r]) )

		# Read correspondent weight
		if( length(candidate) == 0 )
			hit.weight = 0
		else
			hit.weight = input_score[candidate, "weight"]

		# Record weight
		sum.scores = sum.scores + hit.weight
		near.weight[r,l] = hit.weight

	} # for( r in 1:near.nrows )
	cat("\n")
} # for( l in 1:n.layers )

# Information about generated data frame
# with weights of each nearest hit
message("******************************")
message(" * Generated data frame with weights of each nearest hit:")
message("   * ", nrow(near.weight), " rows | ",
          	 ncol(near.weight), " columns")

# Write output on screen
message("******************************")
message("* Score for each layer:")
    cat("  * Layer\t", sep="")
for( l in 1:(n.layers/2) ) cat(l, "\t\t", sep="")
cat("\n")
    cat("  * Score\t", sep="")
for( l in 1:(n.layers/2) ) cat(sum(near.weight[,l]), "\t", sep="")
cat("\n")
    cat("  * Layer\t", sep="")
for( l in (1 + n.layers/2):n.layers ) cat(l, "\t\t", sep="")
cat("\n")
    cat("  * Score\t", sep="")
for( l in (1 + n.layers/2):n.layers ) cat(sum(near.weight[,l]), "\t", sep="")
cat("\n")

message("******************************")
message(" * Total Score = ", sum.scores)
message("******************************")

########################################
# For safety, it is better to save this workspace
message("*******************************************")
message("* Saving workspace...")
save.image( file=paste(output_folder, "/", output_file_name, sep="") )

