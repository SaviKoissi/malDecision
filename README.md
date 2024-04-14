# Welcome to malDecision!
malDecision is an R-based application developed to translate research findings into actionable insights and enhance preparedness for malaria and other emerging infectious diseases. Our tool replicates the analyses undertaken in our study, empowering a broader community of infectious disease researchers, public health officials, and policymakers.

# Key Features:

## Streamlined Analysis: 
malDecision provides a user-friendly interface for conducting analysis, visualization, and reporting, all within the R environment.
## Configurable Reports:
Users can generate customizable reports using a single file that includes predictors, target variables (incidence or prevalence), and the administrative classification of targeted locations.
## Regionally Adaptable: 
malDecision is designed to be regionally adaptable, supporting a wide range of geographies and circumstances. Whether you're focusing on urban, peri-urban, or rural areas, malDecision can accommodate your specific needs.
How to Use:

# Installation
Simply clone or download the malDecision repository to your local machine.
Setup: Follow the setup instructions provided in the README.md file to install any necessary dependencies and configure your environment.
or
Execute the following from your R console:

```R
## From an R console
shiny::runGitHub("malDecision", "SaviKoissi")
```
# Analysis: 
Input your data and specify your analysis parameters using the provided scripts.
 Please insert a CSV file formatted as follows 

 `|country|Region|District|long|lat|cluster| classifiers| Output`

 Clusters represent the degree of urbanization of the area and classifiers represent the factors that determine access. The output variable represents the variable to be predicted, such as prevalence or incidence

# Visualization: 
Visualize your results using built-in plotting functions or customize your visualizations as needed.

# Reporting: 
Generate comprehensive reports summarizing your findings, which can be easily shared with stakeholders or incorporated into research publications.

## Example 

 1. Load the 'example.csv' file 

 2. Choose a variable cluster that represents the degree of urbanization

 3. Select the classifiers (topo, precipitation, etc.) that represent the determinants

 4. Choose the output variable (PC1) 

 5. With a single click, you can run the analysis

 6. A cluster-specific Random Forest or a customized report can be withdrawn as an Excel sheet showing the variable of importance. In the custom pdf report  containing the descriptive statistics of the dataset, the degree of urbanization is represented spatially. And the variable of importance for each group

# Who Should Use malDecision:

## Researchers: 
Conduct sophisticated analyses and visualize results to deepen understanding of infectious disease dynamics.
## Public Health Officials: 
Inform decision-making and resource allocation by analyzing trends and identifying high-risk areas.
## Policymakers: 
Develop evidence-based policies and interventions to mitigate the spread of malaria and other infectious diseases.

# Get Involved:
We welcome contributions from the community to further enhance the functionality and usability of malDecision. Whether you're interested in adding new features, improving documentation, or reporting bugs, we encourage you to get involved and help us make malDecision even better!
