# Predictive Modeling in Baseball: A Logistic Regression Analysis
  
This repository contains my bachelor's thesis project, aimed at quantifying the role of chance in a *Major League Baseball* season through repeated simulations using Logistic Regression.

## Project Recap

The *step by step* structure of the analysis was the following:

1) Creation of the 2015-2018 MLB games dataset through feature extraction and construction
2) Descriptive analysis and univariate regression modeling
3) Model selection (through stepwise AIC and AUROC) and diagnostics
4) Repeated simulation of the 2019 MLB season by predicting the win probability of each game with the chosen model

## Repository Contents

- My full graduation thesis (written in Italian) is available as a pdf in the *`thesis`* folder. It contains a detailed description of every step of the project, including information on the methods that were used, the thoughts behind the choices that were made and a list of the bibliographical resources. The analysis is also quickly syinthetized in English in the *`README`* file within the same folder, and on *[my website](https://amontanari.altervista.org/predicting-baseball/)*.

- The original datasets used for the analysis are contained in the *`data`* folder. These were downloaded from *[Retrosheet](https://www.retrosheet.org)*, *[Fangraphs](https://www.fangraphs.com)* and the *[Lahman Database](http://www.seanlahman.com/baseball-archive/statistics/)*. 

- The R Scripts I wrote for conducting the analysis are all included, along with the package requirements. **Note that many of the scripts cannot be run alone as they need the results obtained in the others to properly work**, hence you should run them subsequently following the steps above if you want to replicate my results.
