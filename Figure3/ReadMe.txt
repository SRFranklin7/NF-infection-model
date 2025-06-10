Code provided in this directory was used to create the figures presented in Figure 3 of the paper. This figure describes the machine learning model and its results. The first step to creating this model was hyperparameter optimization, where as described in the methods, I used a coarse-to-fine optimization approach, where I manually optimized the hyperparameter search space. First beginning with the default hyperparameter range, and gradually shrinking this space based on visual interpretation. As described in the methods section, four models were created to validate variable pre-processing. The code to handle each of these datasets are the same, it was only the input data used for the hyperparameter optimization and the model itself that would change for each of these four models. 

Files present within this directory are as follows:

XGBoost_CoarsetoFineLoop.R: This file contains code utilized to perform coarse to fine optimization of the ML model. This was performed in an interative approach. Where all the code was run and a resulting figure produced to show the distribution of ACC, AUC, and accuracy of the test mean across the range of each of 8 hyperparameters. Based on the distribution of AUC, ACC and accuracy of the test mean this search space (x-axis) could be shrunk. Once a new hyperparameter search space was defined, the tuning strategy (line 60) could be edited and the code rerun. Once a suitable search space was defined, this optimized search space was used in the final model. 

XGBoost.R: This file contains code to run the model, as well as create the SHAP plot on Figure 3D and the ROC curve on Figure 3B. 

Figure3A_ComparisonROCplots.R: This file contains code to create the comparison ROC plots. From the model code, the 

Figure3C_BubblePlot: This file contains code used to create Figure 3C which depicts a bubble plot showing the average variable importances of the top variables contributing to model performance. From each of the four models, I saved and extracted the average variable importance scores to be able to create this figure. 
