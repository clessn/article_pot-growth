# Replicable code: Forecasting Election Swings in Multiparty Systems Using Underlying Potential for Growth of Parties

This README follows the guidelines for data replication of The Journal of Politics of the University of Chicago Press. These guidelines can be found here: https://www.journals.uchicago.edu/journals/jop/data-replication

This article can be replicated using R version 4.1.3. We consider opening the `article_pot-growth.Rproj` to work in this repo.

# Packages to install
`devtools::install_github("ellipse-science/sondr")`

# Ethical constraints
Some of the surveys used in this article were exclusive, and access to the data was conditional upon signing an ethics form. As a result, the datasets presented are a shortened version of the full dataset used in this study. If you are interested in accessing additional variables or data, please contact us at <hubert.cadieux.1@ulaval.ca> to discuss the possibility of obtaining access.

CODE TO SIMULATE DATA

# Datasets
This repository implements the Extract-Transform-Load (ETL) process for efficient data management. The `data` folder contains three main folders:
- **lake**: all the raw datasets used in the article (simulated when necessary upon ethical constraints).
- **warehouse**: the tables obtained after wrangling and cleaning the raw datasets and selecting only relevant variables for the article. These tables are organized along the conceptual framwework of the article.
- **marts**: refined tables obtained by merging, joining, agregating, etc. the tables in the **warehouse**. These tables are the ones used to generate the main plots of the article.

# Code files

## data_wrangling
This folder contains the code files used to transform datasets in the **data/lake** folder into clean tables containing the relevant variables used in the article. These clean tables are located in the **data/warehouse** folder.

The **code/data_wrangling** folder is organized along three distinct steps in the article.
- **step1_people_predict**: 
- **step2_electoral_swings**:
- **step3_agregated_rci**: