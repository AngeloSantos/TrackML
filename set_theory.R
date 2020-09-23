#####################################
nr.ed.input.name  = "figures/application_2020_05_19/leastDist.near_real.csv"
nr.cs.input.name  = "figures/application_2020_09_19_v2/leastDist.near_real.csv"

rtm.ed.input.name = "figures/application_2020_05_19/rtm.csv"
rtm.cs.input.name = "figures/application_2020_09_19_v2/rtm.csv"

layers <- c(5:10)

#####################################
nr.ed <- read.csv(nr.ed.input.name)
nr.cs <- read.csv(nr.cs.input.name)

rtm.ed <- read.csv(rtm.ed.input.name)
rtm.cs <- read.csv(rtm.cs.input.name)

n.ed.U.cs.success     <- c()
n.ed.int.cs.success   <- c()
n.ed.minus.cs.success <- c()
n.cs.minus.ed.success <- c()

n.ed.U.cs.failure     <- c()
n.ed.int.cs.failure   <- c()
n.ed.minus.cs.failure <- c()
n.cs.minus.ed.failure <- c()

#####################################
for( l in (layers-4) ){
	nr.ed.success <- which(nr.ed[,l] == 0)
	nr.ed.failure <- which(nr.ed[,l] >  0)
	
	nr.cs.success <- which(nr.cs[,l] == 0)
	nr.cs.failure <- which(nr.cs[,l] >  0)
	
	rtm.ed.success <- rtm.ed[nr.ed.success, l+1]
	rtm.ed.failure <- rtm.ed[nr.ed.failure, l+1]
	
	rtm.cs.success <- rtm.cs[nr.cs.success, l+1]
	rtm.cs.failure <- rtm.cs[nr.cs.failure, l+1]
	#####################################
	ed.int.cs.success   <- intersect( unique(rtm.ed[nr.ed.success, l+1]),
				          unique(rtm.cs[nr.cs.success, l+1]) )
	ed.int.cs.failure   <- intersect( unique(rtm.ed[nr.ed.failure, l+1]),
	                                  unique(rtm.cs[nr.cs.failure, l+1]) )

	ed.minus.cs.success <- setdiff(rtm.ed.success, ed.int.cs.success)
	cs.minus.ed.success <- setdiff(rtm.cs.success, ed.int.cs.success)
	ed.minus.cs.failure <- setdiff(rtm.ed.failure, ed.int.cs.failure)
	cs.minus.ed.failure <- setdiff(rtm.cs.failure, ed.int.cs.failure)
	
	ed.U.cs.success     <- unique( c(rtm.ed[nr.ed.success, l+1],
		  			 rtm.cs[nr.cs.success, l+1]) )
	ed.U.cs.failure     <- unique( c(rtm.ed[nr.ed.failure, l+1],
					 rtm.cs[nr.cs.failure, l+1]) )
	#####################################
	n.ed.int.cs.success <- c(n.ed.int.cs.success, length(ed.int.cs.success))
	n.ed.int.cs.failure <- c(n.ed.int.cs.failure, length(ed.int.cs.failure))

	n.ed.minus.cs.success <- c(n.ed.minus.cs.success, length(ed.minus.cs.success))
	n.ed.minus.cs.failure <- c(n.ed.minus.cs.failure, length(ed.minus.cs.failure))

	n.cs.minus.ed.success <- c(n.cs.minus.ed.success, length(cs.minus.ed.success))
	n.cs.minus.ed.failure <- c(n.cs.minus.ed.failure, length(cs.minus.ed.failure))

	n.ed.U.cs.success <- c(n.ed.U.cs.success, length(ed.U.cs.success))
	n.ed.U.cs.failure <- c(n.ed.U.cs.failure, length(ed.U.cs.failure))
} # for( l in (layers-4) )

#####################################
message("****************************************")

write.output <- function(set.case, numerator, denominator){
	cat(set.case, "\t\t", sep="" )
	for( l in (layers-4) ){
		cat(paste0(numerator[l], "(",
			   round(100*numerator[l]/denominator[l], 0),
			   "%)"), "\t")
	}; cat("\n")

}
#####################################

message("****************************************")
message("* Cases of Success")
cat("Layer\t\t")
for( l in layers ){cat(l, "         \t")}; cat("\n")
write.output("ED^CS",   n.ed.int.cs.success,   n.ed.U.cs.success)
write.output("ED-CS",   n.ed.minus.cs.success, n.ed.U.cs.success)
write.output("CS-ED",   n.cs.minus.ed.success, n.ed.U.cs.success)
write.output("ED U CS", n.ed.U.cs.success,     n.ed.U.cs.success)

message("****************************************")
message("* Cases of Failure")
cat("Layer\t\t")
for( l in layers ){cat(l, "         \t")}; cat("\n")

write.output("ED^CS",   n.ed.int.cs.failure,   n.ed.U.cs.failure)
write.output("ED-CS",   n.ed.minus.cs.failure, n.ed.U.cs.failure)
write.output("CS-ED",   n.cs.minus.ed.failure, n.ed.U.cs.failure)
write.output("ED U CS", n.ed.U.cs.failure,     n.ed.U.cs.failure)

message("****************************************")
message(" * Done!")

#####################################
