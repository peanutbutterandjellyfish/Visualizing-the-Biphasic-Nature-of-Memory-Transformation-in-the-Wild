Overview
This repository contains the R code used to implement various visualizations for the paper "Visualizing the Biphasic Nature of Memory Transformation in the Wild". The scripts generate the main plots and supplementary materials as described in the paper.

Repository Contents
1. Data Preparation
FinalMerge.R: This script creates the dataset that is subsequently analyzed using the Linguistic Inquirer and Word Count (LIWC) software, applying the German Dictionary (Meier et al., 2018) to the review texts.
2. Main Visualization Scripts
Main_Plots_Memory_Consolidation.R: Generates the primary visualizations, including Prediction Error graphs and Memory Consolidation graphs. The dependent variables (Word Count, Positive Emotions, Big Words, Negative Emotions) are aggregated by prediction error and the number of days since the return from vacation. A simple Generalized Additive Model (GAM) is overlaid for additional robustness.
3. Robustness Checks
Robustness_GAMs.R: Compares the simple GAM model used in Main_Plots_Memory_Consolidation.R with the best models identified by function_find_best_GAM.R, based on the lowest AIC. This script generates model outputs and additional graphs showcasing the contribution of extra predictors in the supplementary materials.
function_find_best_GAM.R: Contains functions to find the best GAM based on the lowest AIC.
4. Supplementary Data and Analyses
ratings_lrec16_koeper_ssiw.txt: Contains a collection of 350,000 German lemmatized words rated on linguistic abstractness, arousal, imageability, and valence. This dataset, sourced from Köper, M., & Im Walde, S. S. (2016), is used to verify that the Big Words and Word Count metrics accurately measure linguistic abstraction and detail.
POS_HC_Data.R: Lemmatizes words in each review, preparing the dataset for merging with the abstractness ratings from Köper and Im Walde (2016).
ConcreteAbstractLinguistics.R: Merges the abstractness ratings by Köper and Im Walde (2016) with the words in our dataset to measure linguistic abstraction and generates the graphical outputs included in the supplementary materials.
Data Availability
Please note that, due to a non-disclosure agreement (NDA) with the data-providing company, we are unable to make the dataset publicly accessible.
