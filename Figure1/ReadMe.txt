Input data for each file held within the Figure1 directory is described below. 

"Aim1_StackedBarplot.R": This file was used to create Figure 1D. Relative abundance from the class level was used to where the rarified abundance was averaged across all patients within a given class. These averages were then used to make this plot. Input data can be formatted in the long format which will have four columns 1) Group (InfectiousNF, NonInfectiousNF or NoFever), 2) Abundance (continuous numerical value), 3) Taxa (Class Name), and 4) Time (Onset or Baseline). Each row is the average abundance of a class for a group at onset or baseline. 

"Aim1_AlphaDiversity_KruskalWallis.R": This file was used to create Figure 1A. The data used to create this is in the long data format, and had three columns 1) Time (Baseline or Onset), 2) Group (InfectiousNF, NonInfectiousNF, or NoFever), and 3) Level (continuous numerical value, alpha diversity value). Each row is the alpha diversity value for a patient within a particular group at either baseline or onset. 

"Aim1_BetaDiversity.R": This file was used to create Figure 1B and C. The data was an adjusted version of the original beta diversity data matrices which are the output of qiime2. The first column (patient/sample ID) was removed, and a column at the end was added with group ID. 
