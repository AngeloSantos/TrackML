# TrackML
Machine Learning applied to particle tracks

Diary
=====

Details about what have been produced so far can be found on directory _Diary/_.

Almost all scripts were created based on **R** language.

There is NO information about functionality of all scripts yet.

Summary about R and Bash Scripts
================================

* apply_model.R - application step based on optimized *GMB* models
* apply_model_Npred.R - application step based on optimized *GMB* models considering discrete values in the Z axis
* compute_Kaggle_score.R - compute Kaggle score comparing results between Kaggle simulations and results from GBM
* create_application_p5.R - create dataset with real data (from 5th to 10th layer) for application step
* create_application_sequences.R - create dataset with real data (from 1st to 4th layer) for application step
* create_application_whole_track.R - create dataset with real data (from 1st to 10th layer) for application step
* create_optimization_p5.R - create dataset with real data (from 5th to 10th layer) for optimization step
* create_optimization_sequences.R - create dataset with real data (from 1st to 9th layer) for optimization step
* create_train_optim_appl_samples.R - create samples (from original CSV) for training, optimization and application step
* create_training_sequences.R - create dataset with real data (from 1st to 10th layer) for training step
* divide_training_sequences_by_N.R - divide samples for training step based on original training sample
* gbm.R - train model for the 5th layer with specific *GBM* parameter values
* get_best_triplet_combination.R - find best triplet combinations for circle fit
* get_circular_constant.R - get *q.B* constant value correspondent to the Kaggle simulations
* get_models.R - train models with optimized parameters only
* get_models_Npred.R - train models with optimized parameters based on splitted training and optimized samples
* get_models_Npred.sh - create *R* scripts based on *get_models_Npred.R* for each sample after splitting
* get_radius_angelos.lib.R - apply circle fit based on Angelo's model with best triplet combinations
* get_radius_circular.lib.R - apply circle fit based on *circular* lib
* get_radius_pracma.lib.R - apply circle fit based on *pracma* lib
* optimize_parameters.R - optimize *GBM* parameters for x, y and z coordinates
* optimize_parameters_Npred.R - optimize *GBM* parameters for splitted training and optimization samples
* optimize_parameters_Npred.sh - create *R* scripts based on *optimize_parameters_Npred.R* for each sample after splitting
* optimize_parameters_pt.R - optimize *GBM* parameters to predict pT
* optimize_parameters_radius.R - optimize *GBM* parameters to predict tracking radius
* plot_detector.R - create figures of the detector based on the hits from Kaggle
* plot_distHH_vs_Nhits.R - create figure of distance between two expected nearest hits versus number of hits per layer
* plot_fit_circunference.R - create figure of circle fit on tracking hits
* plot_hist_of_deltaphi.R - create figure of deltaphi

