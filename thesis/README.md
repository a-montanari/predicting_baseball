
# Predictive Modeling in Baseball: A Logistic Regression Analysis
# Project Summary
### Università di Bologna – Bachelor’s Degree in Statistics – Academic Year 2018/2019
### Author: Alessandro Montanari – Supervisor: Professor Daniele Ritelli
  
## Introduction
  
Every future event depends on a certain amount of randomness, which is very difficult to take into account when making predictions.  
  
**The aim of the analysis will be measuring the role of chance on the results of a Major League Baseball season.**
  
## About Major League Baseball
  
- 30 teams, divided in 2 Leagues (*National* & *American*), each composed of 3 Divisions (*East*, *Center*, *West*).
- In the *Regular Season*, every team plays 162 games.
- Afterwards, the best teams in terms of winning record compete in the *Post Season*: within both Leagues, the Division Champions and the 2 best remaining teams face in a series of games to determine the League Champions. These finally face each other in the World Series to determine the final winner.

## The Role of Chance in the game
  
Random events in baseball can be of very different nature: players who perform differently from their true ability, umpires making bad calls, weird bounces of the ball, defensive errors, weather conditions, fans interfererence...  
  
In general, it’s impossibile to take them into account before they happen, but they can strongly influence the outcome of a game and, by piling up, of a whole season.  
  
## Steps
  
1) Creating a model for estimating the win probability of the two teams facing in an MLB game, as a function of some variables which value must be known before a game starts.
2) Applying the model to every game of the current season (2019), and simulating many repetitions of the final results starting from the estimated probabilities.

## The Data

Every MLB game played between 2015 and 2018 (total = 9717 games) along with the relevant statistics of the teams and the starting pitchers.  
  
Sources: *Retrosheet*, *Fangraphs*, *Lahman Database*

## The Model

The technique for fitting the model will be *Logistic Regression*, which is used when the dependent variable can assume just 2 values (conventionally: 1 = event, 0 = no event).  
  
In our case, the coding will be:  
Y = 1, The home team wins  
Y = 0, The home team loses (and so the away team wins: there are no ties in baseball)  
  
Logistic regression relates the probability of Y being equal to 1 (hence, the win probability for the home team) with the values of the independent variables (regressors).  
  
Steps for finding a good model:  
  
1) **Finding the right dependent variables.** This corresponds to answering the question: "What can decide the outcome of a game?". Intuitively, there are many factors, such as the *talent of the teams*, *home advantage*, *talent of the starting pitchers and bullpen*, and so on. An evident problem is that most of these values are unknown, so they must be estimated through performance statistics.  
2) **Model Selection.** This was done both through AIC and AUROC, in the latter case splitting the dataset into a training and test set (80%-20%) in order to avoid selection bias.
3) **Estimating the regression coefficients** through Maximum Likelihood.  

### The Selected Model

Talent of the teams:

- Average Runs Scored per Game by the Home Team
- Average Runs Scored per Game by the Away Team

Talent of the starting pitchers:

- Home Team Starting Pitcher ERA
- Away Team Starting Pitcher ERA

Talent of the relievers:

- Home Team Relief Pitchers ERA
- Away Team Relief Pitchers ERA
  
*AUROC = **0.646***  
Not incredibly good, but still the best among the many considered models, and better then randomly chosing the outcome (AUROC = 0.5).

## Predicting 2019 Results

Every game can be considered as an independent Bernoulli trial, with success probability *p* equal to the estimated home team win probability.  

By the **Law of Large Numbers**, if the same game is played many times then the frequency of wins of the home team will approximate the win probability.  
However, **in the real world only one game is played**, hence the role of chance is crucial. The result of a game can be compared to a coin toss having probability of heads equal to the estimated win probability.  
  
In other words, **every game has a random result, dependent on the estimated win probabilities**.

## 1000 Regular Seasons Simulations

The simulation of the remaining games of the 2019 season (after June 4, 2019) was repeated 1000 times, marking the final winning record obtained by each team at every iteration.

<img src="/plots/1000ros.tiff" alt="1000 Simulations of the Regular Season"
     style="float: center; margin-right: 10px;" />

The plot shows the distribution of the total number of wins obtained by all teams in each of the 1000 simulations.  
It’s clear that there is a tendency to obtain good or bad results with respect to a team’s ability, as the distributions are very different among themselves. However, it also appears evident that the level of dispersion due to pure randomness is very high: all teams except 5 (Orioles, Yankees, Astros, Royals, Dodgers) got at least once above and below the .500 mark (corresponding to 81 wins).
  
## 1000 Post Season Simulations

The PostSeason was simulated following each Regular Season, according to the estimated final standings. Below are reported the only 4 teams for which the World Series Win probability was estimated to be above 10%.

- **Los Angeles Dodgers** (WS Win Probability = **24.8%**)
- **Houston Astros** (WS Win Probability = **13.9%**)
- **Minnesota Twins** (WS Win Probability = **12.1%**)
- **New York Yankees** (WS Win Probability = **10.7%**)

Being generally low, the World Series Win Probability was consequently predicted non-zero for a lot of teams: 21 out of 30! This implies that, according to the simulation, a World Series Win is very due to randomness.

## Team Ability and Success

<img src="/plots/ability.tiff" alt="Team Ability and Success"
     style="float: center; margin-right: 10px;" />

Following the reasoning above, the plot tries to relate a team’s ability (measured through the average Run Differential per Game, on the X axis, for which the quartiles are also reported by the red dotted lines) with the probability of success in different stages of the season (on the Y axis).   
It can be seen that this relation appears very strong for the earlier stages, such as reaching the PostSeason or winning the respective Division, as the probability of these events reaches almost 1 for teams that are very good. However, the further the Post Season goes on, the weaker the relations get. In particular, it appears that very good teams don’t have a probability of winning the World Series much higher then average or slightly above average teams.  

## Conclusions
  
- Within the **Regular Season**, randomness has an important role on the distribution of a team’s number of wins.  
Generally:  
95% confidence interval: ± 10 wins with respect to the expected value

- When the **Post Season** is reached, the role of chance is even greater.  
It’s indeed probable that the best team does not win it all.  
It’s absolutely possible that an average/slightly above average team wins the World Series.


