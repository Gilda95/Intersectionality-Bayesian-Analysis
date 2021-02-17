# Intersectionality-Bayesian-Analysis
Women participation to labour market


## Introduction 

In the last years, we have seen a strong growth of female participation in labour market. However, we do not know who are the women that have accessed to the labour market and strong disparities remain across countries in terms of the capacity of the labour market to offer an occupation to women.
Intersectionality theory argues that social aspects as gender, race, class, education, … might combine to generate systematic social inequalities in labour market. 


## Goal of the project

Our goal is to investigate this phenomenon through an analysis of data of European Social Survey collected in 2018: is there any difference in the capacity of women to access labour market if we take into account the intersectionality of their characteristics? Which are the other discriminant factors that combined with gender enlarge this inequality.
So we fit a hierarchical GLM with random effects to understand if the dataset from European Social Survey (round 9, 2018) gives evidence to such statements. 


## Download dataset

Data are easily downloadable from ESS website (free registration is required). Link for download: 
[ESS dataset](https://www.europeansocialsurvey.org/download.html?file=ESS9e03&y=2018)

STATA SAS and SPSS format available. Use round 9 (2018). All the information on the variables are in the *ESS_09_Codebook*.


## Folder structure

### Data_Cleaning

In [`this folder`](Data_Cleaning)  you can find file *dataset.R*. 
Thhis explains how to reduce the entire ESS9E02 dataset and how to make some changes on the variables that will help you understanding the context. 
First of all we chose to focus on the people between the ages of 25 and 55, since we are interested in differences in the labour market opportunities. Then we choose variables that we thought should better explain our problem. After these first steps, we take only people which have full information available for the considered variables.
New variables are introduced to make dataset more easily interpretable such as: *ctzmod*, *brnmod*, *edutre*. Inside the file some information about how we create these are reported.
After all these changes, the final result is saved on file *data_work.csv*. Despite the fact we decided to focus on South Europe for our analysis, this file presents all the region across Europe.


### Explorative_Data_Analysis

We decided to keep focus on South Europe, so from now on first part of code in every .R files is choosing *South Europe* as region. 
In [`this folder`](Explorative_Data_Analysis) you can find 10 .R files which present a frequentist approach data analysis referred to specific variables. We wanted to see how our response behaves in function of other covariates. Moreover we studied how gender impacts on every single variable and we tried to point out potential social barriers in labour market. 


### Regression_Models

In [`this folder`](Regression_Models) you can find the regression analysis we have performed, namely:
1.	Frequentist model to check the importance of each variable in order to make a first selection of the covariates ([`frequentist_glm.R`](Regression_Models/frequentist_glm.R));
2.	Bayesian Hierarchical GLM through JAGS with Spike and Slab priors using the variables that Frequentist model highlighted ([`Lasso_regression`](Regression_Models/Lasso_regression) folder);
3.	Bayesian Hierarchical GLM through JAGS with Lasso priors using the variables that Frequentist model highlighted ([`SpikeSlab_regression`](Regression_Models/SpikeSlab_regression) folder);
4.	Bayesian Hierarchical GLM through JAGS with the covariates selected through model (2) and (3) ([`Final_regression`](Regression_Models/Final_Regression) folder). 
