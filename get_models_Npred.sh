#!/bin/bash

##################################################
#

# Name of R script (without ".R")
r_script="get_models_Npred"

# Input txt with optimized parameters
input_opt_params="figures/optimization_2020_06_12/optParams_20200612"

# Output txt file
output_file="figures/optimization_2020_06_12/get_GBMmodel_202006013"

# Number of predictions
n_predictions=3

#
##################################################

echo "******************************************"
echo "* Starting to get models ..."
echo "*"
# Run over number of possible predictions
for i in $(seq 1 $n_predictions); do

	cp  ${r_script}.R  ${r_script}_${i}.R
	sed -i "s/1111/${i}/g" ${r_script}_${i}.R

	shrinkage_values=$(grep minShrinkage ${input_opt_params}.${i}.txt | \
		           cut -f2- -d'=' | cut -c 2- | sed 's/^/c\(/g' | rev | \
			   cut -c 2- | sed 's/^/\)/g' | rev | sed 's/ )/\)/g' | \
			   sed 's/ /,/g' | tr "\n" "," | sed 's/,$//g')
	sed -i "s/shrinkage\.xyz\_predictions/shrinkage\.xyz \<\- list\( ${shrinkage_values} \)/g" ${r_script}_${i}.R


	interaction_depth=$(grep minInteractionDepth ${input_opt_params}.${i}.txt | \
		            cut -f2- -d'=' | cut -c 2- | sed 's/^/c\(/g' | rev | \
			    cut -c 2- | sed 's/^/\)/g' | rev | sed 's/ )/\)/g' | \
			    sed 's/ /,/g' | tr "\n" "," | sed 's/,$//g')
	sed -i "s/interaction\.depth\.xyz\_predictions/interaction\.depth\.xyz \<\- list\( ${interaction_depth} \)/g" ${r_script}_${i}.R

	echo "  * Rscript ${r_script}_${i}.R &> ${output_file}.${i}.txt &"
	Rscript ${r_script}_${i}.R &> ${output_file}.${i}.txt &

done # for i in $(seq 1 $possible_predictions)
echo "*"

echo "* Done!"
echo "******************************************"

