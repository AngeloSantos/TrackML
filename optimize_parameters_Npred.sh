#!/bin/bash

##################################################
#

# Name of R script (without ".R")
r_script="optimize_parameters_Npred"

# Output txt file
output_file="figures/optimization_2020_06_14_v2/optParams_20200614_v2"

# Number of predictions
n_predictions=3

#
##################################################

echo "******************************************"
echo "* Starting optimization processes ..."
echo "*"
# Run over number of possible predictions
for i in $(seq 1 $n_predictions); do

	cp  ${r_script}.R  ${r_script}_${i}.R
	sed -i "s/1111/${i}/g" ${r_script}_${i}.R

	echo "Rscript ${r_script}_${i}.R &> ${output_file}.${i}.txt &"
	Rscript ${r_script}_${i}.R &> ${output_file}.${i}.txt &

done # for i in $(seq 1 $possible_predictions)
echo "*"

echo "* Done!"
echo "******************************************"

