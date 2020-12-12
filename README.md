# Predictive Modeling in Baseball: A Logistic Regression Analysis
  
This repository contains my bachelor's thesis project, aimed at quantifying the role of chance in a *Major League Baseball* season through repeated simulations using Logistic Regression.

## Project Recap

The *step by step* structure of the analysis was the following:

1) Creation of the dataset through feature extraction and construction, starting from public resources
2) Descriptive analysis and univariate modeling
3) Model selection (through stepwise AIC and AUROC) and diagnostics
4) Simulation of 1000 *MLB* seasons by predicting the win probability of each game with the above selected model

## Repository Contents

- My full graduation thesis (written in Italian) is available as a pdf in the *`thesis`* folder. It contains a detailed description of every step of the project, including information on the methods that were used, the thoughts behind the choices that were made and a list of the bibliographical resources. The results of the analysis are also quickly syinthetized in English in the *`README`* file of the folder.

- The original datasets used for the analysis are contained in the *`data`* folder.

- The R scripts I wrote for the project are all included. Before running them, you can check the packages that are needed in the *`requirements.R`* file.
