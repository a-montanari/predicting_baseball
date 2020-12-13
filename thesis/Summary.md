
# Predictive Modeling in Baseball: A Logistic Regression Analysis
### Bachelor’s Degree in Statistical Sciences – Academic Year 2018/2019
### Author: Alessandro Montanari – Supervisor: Professor Daniele Ritelli
  
## Introduction
  
Every future event depends on a certain amount of randomness, which is very difficult to take into account when making predictions.  
  
**Aim: measuring the role of chance on the results of a Major League Baseball season**
  
## About Major League Baseball
  
- 30 teams, divided in 2 Leagues (National & American), each of them made up by 3 Divisions (East, Center, West)
- Regular Season: Every team plays 162 games
- Post Season: Within both Leagues, the Division Champions and the 2 best excluded teams face in a series of games to determine the League Champions. These finally face each other in the World Series to determine the final winner.

## The Role of Chance in the game
  
Random events can be of very different nature.  
  
Examples: Players who perform differently from their true ability, umpires making bad calls, unpredictable bounces of the ball, defensive errors, weather conditions, fans interfering with the play...  
  
In general: it’s impossibile to take them into account before they happen, but they can strongly influence the outcome of a game and even of a whole season.  
  
## Goals
  
1) Creating a model for estimating the win probability of the two teams facing in an MLB game, as a function of some variables.
2) Applying the model to every game of the current season (2019), and simulating many repetitions of the results starting from the estimated probabilities.

## The Model

Method: LOGISTIC REGRESSION  
Used when the dependent variable Y can assume just 2 values (conventionally: 1 = event, 0 = no event).  
  
In our case:  
Y = 1  The home team wins  
Y = 0  The home team loses (and so the away team wins)  
  
Logistic regression relates the probability of Y being equal to 1 (hence, the win probability for the home team) with the values of the independent variables (regressors).  
  
- Data: every MLB game played between 2015 and 2018 (total = 9717 games) along with the statistics of the teams and players.
- Sources: Retrosheet, Fangraphs, Lahman Database
  
Steps for finding a good model:  
  
1) Finding the right dependent variables: What can decide the outcome of a game?
(Ability of the teams, home advantage, ability of the starting pitchers and bullpen... many are unknown!).  
2) Coefficient Estimation (Maximum Likelihood)  
3) Model Selection (AIC, AUROC)

### The Selected Model

- Average Runs Scored per Game by the Home Team
- Average Runs Scored per Game by the Away Team
- Home Team Starting Pitcher ERA
- Away Team Starting Pitcher ERA
- Home Team Relief Pitchers ERA
- Away Team Relief Pitchers ERA
  
*AUROC = 0.646*  
Not incredibly good, but still the best among the many considered models.

## Predicting 2019 Results

Every game can be considered as an independent Bernoulli trial, with success probability p = estimated home team win probability.  

**Law of Large Numbers**: If the same game is played many times, the frequency of wins of the home team will approximate the win probability.  
**In the real world**: Only one game is played, hence chance plays a key role. The result of a game can be compared to a **coin toss** with probability of heads equal to the estimated win probability.  
  
**Every game has a random result, dependent on the estimated win probabilities**.

## 1000 Regular Seasons Simulations

The simulation of the remaining games of the 2019 season (after June 4, 2019) was repeated 1000 times, recording the final record of every team for each iteration.

<img src="/plots/1000ros.tiff" alt="1000 Simulations of the Regular Season"
     style="float: center; margin-right: 10px;" />

The plot shows the distribution of the total number of wins obtained by each team in each of the 1000 simulations.  
It’s clear that there is a tendency to obtain good or bad results with respect to a team’s ability, but it’s also clear that the level of dispersion due to pure randomness is very high: all teams except 5 got at least once above and below .500.
  
## 1000 Post Season Simulations

The PostSeason was simulated following each Regular Season, according to the estimated final standings. Below are reported the only 4 teams for which the World Series Win probability was estimated to be above 10%.

- Los Angeles Dodgers (WS Win Probability = 24.8%)
- Houston Astros (WS Win Probability = 13.9%)
- Minnesota Twins (WS Win Probability = 12.1%)
- New York Yankees (WS Win Probability = 10.7%)

## Team Ability and Success

<img src="/plots/ability.tiff" alt="Team Ability and Success"
     style="float: center; margin-right: 10px;" />

The plot tries to relate a team’s ability (measured through the average Run Differential per Game, on the X axis, for which the quartiles are also reported) with the probability of success in different stages of the season.  
It can be seen that this relation appears very strong for the earlier stages, such as reaching the PostSeason or winning the respective Division, as the probability of these events reaches almost 1 for teams that are very good. However, as the Post Season goes on, the relation is much weaker, and very good teams don’t have a probability of winning the World Series much higher then average teams.  

## Conclusions
  
- Regular Season: Randomness has an important role on the distribution of a team’s number of wins.  
Generally:
95% confidence interval: ± 10 wins with respect to the expected value
Very lucky/unlucky teams: up to ± 15 wins

- Post Season: The role of chance is even greater.  
It’s indeed probable that the best team does not win it all.  
It’s absolutely possible that an average/slightly above average team wins the World Series.


