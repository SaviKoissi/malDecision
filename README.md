# malDecision

 Data-driven decision-making to manage infectious diseases throughout the country with different degrees of urbanization. This tool is versatile and interactive. 

 # How to use the app? 

 Please insert a CSV file formatted as follows 

 |country|Region|District|long|lat|cluster| classifiers| Output

 Clusters represent the degree of urbanization of the area and classifiers represent the factors that determine access. The output variable represents the variable to be predicted, such as prevalence or incidence

 # Example 

 1. Load the 'example.csv' file 

 2. Choose a variable cluster that represents the degree of urbanization

 3- Select the classifiers (topo, precipitation, etc.) that represent the determinants

 4. Choose the output variable (PC1) 

 5. With a single click, you can run the analysis

 6. A cluster-specific Random Forest or a customized report can be withdrawn as an Excel sheet showing the variable of importance. In the custom pdf report  containing the descriptive statistics of the dataset, the degree of urbanization is represented spatially. And the variable of importance for each group

 You can also download the pdf report. 
