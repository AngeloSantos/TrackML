#####################

nr <- read.csv("figures/application_2020_09_19/leastDist.near_real.csv")
pr <- read.csv("figures/application_2020_09_19/leastDist.pred_real.csv")
pn <- read.csv("figures/application_2020_09_19/leastDist.pred_near.csv")

nr.rec <- c()
pr.rec <- c()
pn.rec <- c()
for( r in 1:nrow(nr) ){
	nr.summ = 0
	pr.summ = 0
	pn.summ = 0
	for( c in 1:ncol(nr) ){
		nr.summ = nr.summ + nr[r,c]
		pr.summ = pr.summ + pr[r,c]
		pn.summ = pn.summ + pn[r,c]
	}
	nr.rec <- c(nr.rec, nr.summ)
	pr.rec <- c(pr.rec, pr.summ)
	pn.rec <- c(pn.rec, pn.summ)
}

#####################################
# Count Successes and Failures
message("*********************************")
message("* Count successes")
message("  * Layer 5\t\t6\t\t7\t\t8\t\t9\t\t10\t\tTrack")
    cat("  *       ")
for( c in 1:ncol(nr) ){
	cat( length(which( nr[,c] == 0 )) )
	cat( " (", round(length(which( nr[,c] == 0 ))*100/nrow(nr), 0), "%)\t", sep="" )
}
cat( length(which( nr.rec == 0 )) )
cat( " (", round(length(which( nr.rec == 0 ))*100/nrow(nr), 0), "%)\t", sep="" )
cat("\n")

message("*********************************")
message("* Count failures")
message("  * Layer 5\t\t6\t\t7\t\t8\t\t9\t\t10\t\tWhole Track")
    cat("  *       ")
for( c in 1:ncol(nr) ){
        cat( length(which( nr[,c] > 0 )) )
        cat( " (", round(length(which( nr[,c] > 0 ))*100/nrow(nr), 0), "%)\t", sep="" )
}
cat( length(which( nr.rec > 0 )) )
cat( " (", round(length(which( nr.rec > 0 ))*100/nrow(nr), 0), "%)\t", sep="" )
cat("\n")

#####################################
# Mean distance over failing hits
message("*********************************")
message("* Mean distance over failing hits")
message("  * Layer     5\t6\t7\t8\t9\t10\tWhole Track")
    cat("  * Pred-Real ")
pr.summ = 0
for( c in 1:ncol(pr) ){
        cat( round(sum( pr[,c] )/length(which( nr[,c] > 0 )), 2), "\t" )
        pr.summ = pr.summ + (sum( pr[,c] )/length(which( nr[,c] > 0 )))
}
cat( round(pr.summ, 2) )
cat("\n")

message("*********************************")
    cat("  * Pred-Near ")
pn.summ = 0
for( c in 1:ncol(pn) ){
        cat( round(sum( pn[,c] )/length(which( nr[,c] > 0 )), 2), "\t" )
        pn.summ = pn.summ + (sum( pn[,c] )/length(which( nr[,c] > 0 )))
}
cat( round(pn.summ, 2) )
cat("\n")

message("*********************************")
    cat("  * Near-Real ")
nr.summ = 0
for( c in 1:ncol(nr) ){
        cat( round(sum( nr[,c] )/length(which( nr[,c] > 0 )), 2), "\t" )
        nr.summ = nr.summ + (sum( nr[,c] )/length(which( nr[,c] > 0 )))
}
cat( round(nr.summ, 2) )
cat("\n")

#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
# Mean distance over all hits
message("*********************************")
message("* Mean distance over all hits")
message("  * Layer     5\t6\t7\t8\t9\t10\tWhole Track")
    cat("  * Pred-Real ")
pr.summ = 0
for( c in 1:ncol(pr) ){
        cat( round(mean(pr[,c]), 2), "\t" )
        pr.summ = pr.summ + mean(pr[,c])
}
cat( round(pr.summ, 2) )
cat("\n")

message("*********************************")
    cat("  * Pred-Near ")
pn.summ = 0
for( c in 1:ncol(pn) ){
        cat( round(mean(pn[,c]), 2), "\t" )
        pn.summ = pn.summ + mean(pn[,c])
}
cat( round(pn.summ, 2) )
cat("\n")

message("*********************************")
    cat("  * Near-Real ")
nr.summ = 0
for( c in 1:ncol(nr) ){
        cat( round(mean(nr[,c]), 2), "\t" )
        nr.summ = nr.summ + mean(nr[,c])
}
cat( round(nr.summ, 2) )
cat("\n")

