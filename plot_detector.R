#########################

input_file_name = "data/eta_n0.5-0.5_phi_ninf-pinf.csv"

output_folder   = "data/figures/detector_2020_06_16"

output_file_name = c( "x_y_hits_from_30k_tracks.png", # 1
                      "z_y_hits_from_30k_tracks.png", # 2
                      "z_x_hits_from_30k_tracks.png", # 3
                      "z_y_xCut20_hits_from_30k_tracks.png", # 4
                      "z_x_yCut20_hits_from_30k_tracks.png", # 5
		      "z_y_Layer-5_hits_from_30k_tracks.png", # 6
		      "z_y_Layer-6_hits_from_30k_tracks.png", # 7
		      "z_y_Layer-7_hits_from_30k_tracks.png", # 8
                      "z_y_Layer-8_hits_from_30k_tracks.png", # 9
                      "z_y_Layer-9_hits_from_30k_tracks.png", # 10
                      "z_y_Layer-10_hits_from_30k_tracks.png", # 11
		      "z_y_Layer-5_phi_3pi.2-pi.2_30k_tracks.png", # 12
		      "z_y_Layer-6_phi_3pi.2-pi.2_30k_tracks.png", # 13
		      "z_y_Layer-7_phi_3pi.2-pi.2_30k_tracks.png", # 14
		      "z_y_Layer-8_phi_3pi.2-pi.2_30k_tracks.png", # 15
		      "z_y_Layer-5_phi_pi.2-3pi.2_30k_tracks.png", # 16
		      "z_y_Layer-6_phi_pi.2-3pi.2_30k_tracks.png", # 17
		      "z_y_Layer-7_phi_pi.2-3pi.2_30k_tracks.png", # 18
		      "z_y_Layer-8_phi_pi.2-3pi.2_30k_tracks.png" ) # 19

# Figures     1    2    3    4    5    6    7    8    9   10   11
xmin <- c(-1050,-600,-600,-600,-600,-150,-200,-300,-350,-450,-600,
#	     12   13   14   15   16   19   18   19
	   -150,-200,-300,-350,-150,-200,-300,-350)
xmax <- c( 1050, 600, 600, 600, 600, 150, 200, 300, 350, 450, 600,
#	     12   13   14   15   16   19   18   19
	    150, 200, 300, 350, 150, 200, 300, 350)

#strange_track     <- c(   11,   516, 26038,
#		        2254,  2465,  3061,  3313,  3418,  3847,  4144,  4332,
#		        4580,  5191,  5567,  5777,  6230,  6265,  6941,  6646,
#		        6905,  6941,  7823,  7954,  8581,  9768, 10038, 10628,
#		       12862, 13135, 13401, 14845, 15372, 15974, 16100, 16150,
#		       16250, 17281, 17379, 17553, 18466, 18923, 18990, 19896,
#		       20710, 20951, 21090, 21966, 22373, 22971, 23134, 24976,
#		       25237, 26038, 26296, 27896, 27899, 29893 )

message("**********************************")
message("* Reading input file")
input_file = read.csv(input_file_name)
message("* Dimensions: ",
       	nrow(input_file), " rows and ",
       	ncol(input_file), " columns")

#num.rows = nrow(input_file)
chosen_layers = c(0:9)

xx  <- c()
yy  <- c()
zz  <- c()
rho <- c()
phi <- c()

message("**********************************")
message("* Reading x, y and z...")
#for( i in chosen_layers ){
#	x.name   <- paste("x_",   i, sep="")
#	y.name   <- paste("y_",   i, sep="")
#	z.name   <- paste("z_",   i, sep="")
#	rho.name <- paste("rho_", i, sep="")
#
#	xx  <- c(xx,  input_file[, x.name])
#	yy  <- c(yy,  input_file[, y.name])
#	zz  <- c(zz,  input_file[, z.name])
#       rho <- c(rho, input_file[, rho.name])
#}

x.columns <- c()
y.columns <- c()
z.columns <- c()
r.columns <- c()
p.columns <- c()
for( i in chosen_layers ){
	x.columns <- c(x.columns, paste0("x_",   i))
	y.columns <- c(y.columns, paste0("y_",   i))
	z.columns <- c(z.columns, paste0("z_",   i))
	r.columns <- c(r.columns, paste0("rho_", i))
	p.columns <- c(p.columns, paste0("phi_", i))
}

xx  <- unlist(input_file[, x.columns], use.names=FALSE)
yy  <- unlist(input_file[, y.columns], use.names=FALSE)
zz  <- unlist(input_file[, z.columns], use.names=FALSE)
rho <- unlist(input_file[, r.columns], use.names=FALSE)
phi <- unlist(input_file[, p.columns], use.names=FALSE)

#########################################
# Whole detector

message("**********************************")
message("* Create figures...")
output_file <- paste0(output_folder, "/", output_file_name[1])
message("*** for y vs x -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(xx, yy,
     xlab="x (mm)", ylab="y (mm)",
     xlim=c(xmin[1],xmax[1]),
     main="300k Hits from 30k Tracks",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[2])
message("*** for y vs z -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz, yy,
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[2],xmax[2]),
     main="300k Hits from 30k Tracks",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[3])
message("*** for x vs z -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz, xx,
     xlab="z (mm)", ylab="x (mm)",
     xlim=c(xmin[3],xmax[3]),
     main="300k Hits from 30k Tracks",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[4])
message("*** for y vs z (|x| < 20 mm) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[abs(xx) < 20], yy[abs(xx) < 20],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[4],xmax[4]),
     main="300k Hits from 30k Tracks (Transversal Section)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[5])
message("*** for x vs z (|y| < 20 mm) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[abs(yy) < 20], xx[abs(yy) < 20],
     xlab="z (mm)", ylab="x (mm)",
     xlim=c(xmin[5],xmax[5]),
     main="300k Hits from 30k Tracks (Transversal Section)",
     pch=".", col="red")
dev.off()

#########################################3
# Layers 5, 6, 7, 8, 9, 10

output_file <- paste0(output_folder, "/", output_file_name[6])
message("*** for y vs z (layer 5) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 200) & (rho < 300)], yy[(rho > 200) & (rho < 300)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[6],xmax[6]),
     main="30k Hits from 30k Tracks (Layer 5)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[7])
message("*** for y vs z (layer 6) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 300) & (rho < 400)], yy[(rho > 300) & (rho < 400)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[7],xmax[7]),
     main="30k Hits from 30k Tracks (Layer 6)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[8])
message("*** for y vs z (layer 7) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 400) & (rho < 600)], yy[(rho > 400) & (rho < 600)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[8],xmax[8]),
     main="30k Hits from 30k Tracks (Layer 7)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[9])
message("*** for y vs z (layer 8) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 600) & (rho < 700)], yy[(rho > 600) & (rho < 700)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[9],xmax[9]),
     main="30k Hits from 30k Tracks (Layer 8)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[10])
message("*** for y vs z (layer 9) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 800) & (rho < 900)], yy[(rho > 800) & (rho < 900)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[10],xmax[10]),
     main="30k Hits from 30k Tracks (Layer 9)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[11])
message("*** for y vs z (layer 10) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[rho > 1000], yy[rho > 1000],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[11],xmax[11]),
     main="30k Hits from 30k Tracks (Layer 10)",
     pch=".", col="red")
dev.off()

#######################################
# 3pi/2 < phi < pi/2

output_file <- paste0(output_folder, "/", output_file_name[12])
message("*** for y vs z (layer 5) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 200) & (rho < 300) & (phi >= -pi/2) & (phi < pi/2)],
     yy[(rho > 200) & (rho < 300) & (phi >= -pi/2) & (phi < pi/2)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[12],xmax[12]),
     main="15k Hits from 30k Tracks (Layer 5 - |phi| < pi/2)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[13])
message("*** for y vs z (layer 6) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 300) & (rho < 400) & (phi >= -pi/2) & (phi < pi/2)],
     yy[(rho > 300) & (rho < 400) & (phi >= -pi/2) & (phi < pi/2)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[13],xmax[13]),
     main="15k Hits from 30k Tracks (Layer 6 - |phi| < pi/2)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[14])
message("*** for y vs z (layer 7) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 400) & (rho < 600) & (phi >= -pi/2) & (phi < pi/2)],
     yy[(rho > 400) & (rho < 600) & (phi >= -pi/2) & (phi < pi/2)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[14],xmax[14]),
     main="15k Hits from 30k Tracks (Layer 7 - |phi| < pi/2)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[15])
message("*** for y vs z (layer 8) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 600) & (rho < 700) & (phi >= -pi/2) & (phi < pi/2)],
     yy[(rho > 600) & (rho < 700) & (phi >= -pi/2) & (phi < pi/2)],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[15],xmax[15]),
     main="15k Hits from 30k Tracks (Layer 8 - |phi| < pi/2)",
     pch=".", col="red")
dev.off()

########################################
# pi/2 < phi < 3pi/2

output_file <- paste0(output_folder, "/", output_file_name[16])
message("*** for y vs z (layer 5) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 200) & (rho < 300) & ((phi >= pi/2) | (phi < -pi/2))],
     yy[(rho > 200) & (rho < 300) & ((phi >= pi/2) | (phi < -pi/2))],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[16],xmax[16]),
     main="15k Hits from 30k Tracks (Layer 5 - |phi| > pi/2)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[17])
message("*** for y vs z (layer 6) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 300) & (rho < 400) & ((phi >= pi/2) | (phi < -pi/2))],
     yy[(rho > 300) & (rho < 400) & ((phi >= pi/2) | (phi < -pi/2))],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[17],xmax[17]),
     main="15k Hits from 30k Tracks (Layer 6 - |phi| > pi/2)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[18])
message("*** for y vs z (layer 7) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 400) & (rho < 600) & ((phi >= pi/2) | (phi < -pi/2))],
     yy[(rho > 400) & (rho < 600) & ((phi >= pi/2) | (phi < -pi/2))],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[18],xmax[18]),
     main="15k Hits from 30k Tracks (Layer 7 - |phi| > pi/2)",
     pch=".", col="red")
dev.off()

output_file <- paste0(output_folder, "/", output_file_name[19])
message("*** for y vs z (layer 8) -> ", output_file)
png(output_file,
    units="px", width=1600, height=1600, res=250)
plot(zz[(rho > 600) & (rho < 700) & ((phi >= pi/2) | (phi < -pi/2))],
     yy[(rho > 600) & (rho < 700) & ((phi >= pi/2) | (phi < -pi/2))],
     xlab="z (mm)", ylab="y (mm)",
     xlim=c(xmin[19],xmax[19]),
     main="15k Hits from 30k Tracks (Layer 8 - |phi| > pi/2)",
     pch=".", col="red")
dev.off()


#message("**********************************")
#library(scatterplot3d)
#for( i in strange_track ){
#	message("* Drawing strange track no.", i)
#	output_strange_track <- paste("figures/phi_cut_2020_04_17/",
#				      "strange_tracks/check_strange_track_",
#				      i, ".png", sep="")
#	png(output_strange_track, units="px", width=1600, height=1600, res=250)
#	scatterplot3d( xx[seq(i, 300000, 30000)],
#		       yy[seq(i, 300000, 30000)],
#		       zz[seq(i, 300000, 30000)],
#		       highlight.3d=TRUE, col.axis="blue", col.grid="lightblue",
#		       pch=20, angle=60, scale.y=1,
#		       xlab="x (mm)", ylab="y (mm)", zlab="z (mm)",
#		       main=paste("Strange Track no. ", i, sep="") )
#	dev.off()
#}


#strange_track = 26278 
#output_strange_track <- paste("figures/nophiCut_2020_05_04/",
#                                     "check_strange_track_",
#                                     strange_track, "_xy.png", sep="")
#png(output_strange_track, units="px", width=1600, height=1600, res=250)
#plot(xx[seq(strange_track, 300000, 30000)], yy[seq(strange_track, 300000, 30000)],
#     xlab="x (mm)", ylab="y (mm)",
#     main=paste("Strange Track no. ", strange_track, sep=""),
#     pch=20, col="red")
#dev.off()
#
#output_strange_track <- paste("figures/nophiCut_2020_05_04/",
#                                     "check_strange_track_",
#                                     strange_track, "_zy.png", sep="")
#png(output_strange_track, units="px", width=1600, height=1600, res=250)
#plot(zz[seq(strange_track, 300000, 30000)], yy[seq(strange_track, 300000, 30000)],
#     xlab="z (mm)", ylab="y (mm)",
#     main=paste("Strange Track no. ", strange_track, sep=""),
#     pch=20, col="red")
#dev.off()

message("* Done!")
message("**********************************")
