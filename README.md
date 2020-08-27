# TrackML
Machine Learning applied to particle tracks

Diary
=====

Details about what have been produced so far can be found on directory _Diary/_.

Almost all scripts were created based on **R** language.

There is NO information about functionality of all scripts yet.

Understanding R and Bash Scripts
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


