######### SIMULATIONS #########

# This script represents the most important part of the analysis:
# it contains the main functions I wrote for simulating many times an MLB season,
# starting from the predicted probabilities obtained by the model selected before
# and the 2019 dataset containing information on the developing season.

# Required Packages

require(tibble)
require(dplyr)

###### Simulation of the REGULAR SEASON ######

#### (1) Constant pitchers, Rest of the season ####
# Function for simulating one Regular Season starting from the standings as of June 4, 2019
# Here the starting pitchers sequence is assumed to be the same for every simulation.

# One simulation

one.simulation.2019 <- function(){
  
  dati2019.ros$pred.result <- rbinom(nrow(dati2019.ros), 1,
                                     prob = dati2019.ros$prob.home)
  dati2019.ros$pred.winner <- with(dati2019.ros, ifelse(pred.result,
                                                        as.character(hteam),
                                                        as.character(vteam)))
  wins.ros <- table(dati2019.ros$pred.winner)
  wins.ros <- as.data.frame(wins.ros)
  colnames(wins.ros) <- c("Team", "wins.ros")
  
  pred.wlrecord2019 <- merge(wlrecord2019, wins.ros)
  pred.wlrecord2019$TOTWINS <- pred.wlrecord2019$W + pred.wlrecord2019$wins.ros
  pred.wlrecord2019$TOTLOSSES <- 162-pred.wlrecord2019$TOTWINS
  
  final.pred.standings <- pred.wlrecord2019[,c(1,6:7)]
  final.pred.standings$Team <- as.factor(final.pred.standings$Team)
  final.pred.standings$Team <- factor(final.pred.standings$Team,
                                      levels(final.pred.standings$Team)[ord.4])
  final.pred.standings$Team <- factor(final.pred.standings$Team,
                                      levels(final.pred.standings$Team)[ord.3])
  final.pred.standings <- merge(final.pred.standings, teams2018[,c(1,3,5)],
                                by.x = "Team", by.y = "teamID")
  final.pred.standings$DIV <- paste(final.pred.standings$lgID,
                                    final.pred.standings$divID, sep = " ")
  final.pred.standings$DIV <- as.factor(final.pred.standings$DIV)
  levels(final.pred.standings$DIV)
  final.pred.standings$DIV <- factor(final.pred.standings$DIV,
                                     levels(final.pred.standings$DIV)[ord.2])
  final.pred.standings[,5] <- NULL
  final.pred.standings <- final.pred.standings[order(final.pred.standings$DIV),]
  rownames(final.pred.standings) <- NULL
  
  return(final.pred.standings)
  
}

# 1000 simulations of the rest of the season

many.results <- NULL

for(j in 1:1000){
  many.results <- rbind(many.results, one.simulation.2019())
}


#### (2) New pitchers sequence every time, Rest of the season ####
# Here, the simulation is the same as above but the starting pitcher sequence for
# each team is sampled at each repetition of the season.

# One simulation of the rest of the season (after June 4, 2019)

one.simulation.sp <- function(){
  
  dati2019.ros <- dati2019.ros[order(dati2019.ros$date),]
  rownames(dati2019.ros) <- NULL
  dati2019.ros[,c(23:26)] <- NULL
  
  for(i in 1:30){
    for(z in seq(0, max(starters.st.ros$GamesLeft), 5)){
      startersequenceros[((z+1):(z+5)),i] <- as.vector((sample(
        starters.st.ros$Name[starters.st.ros$Team==tvec[i]],
        size = 5,
        replace = F,
        prob = starters.st.ros$prob[starters.st.ros$Team==tvec[i]])))
    }
  }
  
  for(i in 1:30){
    for(z in 1:110){
      dati2019.ros$vstarter[
        dati2019.ros$vteam==tvec[i] & dati2019.ros$vteam_ng_ros == z] <- startersequenceros[z,i]
      
      dati2019.ros$hstarter[
        dati2019.ros$hteam==tvec[i] & dati2019.ros$hteam_ng_ros == z] <- startersequenceros[z,i]
      
    }
  }
  
  dati2019.ros <- merge(dati2019.ros, starters.st.ros[,c(2,5)],
                        by.x = "hstarter", by.y = "Name")
  
  colnames(dati2019.ros)[23] <- c("ERA_hs")
  
  dati2019.ros <- merge(dati2019.ros, starters.st.ros[,c(2,5)],
                        by.x = "vstarter", by.y = "Name")
  
  colnames(dati2019.ros)[24] <- c("ERA_vs")
  
  dati2019.ros$ERA_hs_scale <- as.numeric(scale(dati2019.ros$ERA_hs,
                                                ERA19, F))
  dati2019.ros$ERA_vs_scale <- as.numeric(scale(dati2019.ros$ERA_vs, 
                                                ERA19, F))
  
  dati2019.ros$prob.home <- predict.glm(out.RSg.eras.erab,
                                        newdata = dati2019.ros,
                                        type = "response")
  
  dati2019.ros$pred.result <- rbinom(nrow(dati2019.ros),
                                     1,
                                     prob = dati2019.ros$prob.home)
  
  dati2019.ros$pred.winner <- with(dati2019.ros, ifelse(pred.result,
                                                        as.character(hteam),
                                                        as.character(vteam)))
  
  wins.ros <- table(dati2019.ros$pred.winner)
  wins.ros <- as.data.frame(wins.ros)
  colnames(wins.ros) <- c("Team","wins.ros")
  
  pred.wlrecord2019 <- merge(wlrecord2019, wins.ros)
  pred.wlrecord2019$TOTWINS <- pred.wlrecord2019$W + pred.wlrecord2019$wins.ros
  pred.wlrecord2019$TOTLOSSES <- 162-pred.wlrecord2019$TOTWINS
  final.pred.standings <- pred.wlrecord2019[,c(1,6:7)]
  
  final.pred.standings$Team <- as.factor(final.pred.standings$Team)
  
  final.pred.standings$Team <- factor(final.pred.standings$Team,
                                      levels(final.pred.standings$Team)[ord.4])
  
  final.pred.standings$Team <- factor(final.pred.standings$Team,
                                      levels(final.pred.standings$Team)[ord.3])
  
  final.pred.standings <- merge(final.pred.standings, teams2018[,c(1,3,5)],
                                by.x = "Team", by.y = "teamID")
  
  final.pred.standings$DIV <- paste(final.pred.standings$lgID,
                                    final.pred.standings$divID, sep = " ")
  final.pred.standings$DIV <- as.factor(final.pred.standings$DIV)
  levels(final.pred.standings$DIV)
  final.pred.standings$DIV <- factor(final.pred.standings$DIV,
                                     levels(final.pred.standings$DIV)[ord.2])
  final.pred.standings[,5] <- NULL
  final.pred.standings <- final.pred.standings[order(final.pred.standings$DIV),]
  rownames(final.pred.standings) <- NULL
  
  return(final.pred.standings)
  
}

# Many simulations of the rest of the season (after June 4, 2019)

many.results.sp <- NULL

pb <- txtProgressBar(min = 0, max = 1000, style = 3)
for(j in 1:1000){
  
  many.results.sp <- rbind(many.results.sp, one.simulation.sp())
  setTxtProgressBar(pb, j)
  
}

close(pb)

# Summary

many.results.sp <- merge(many.results.sp, standings19[,c(1,22)])

prediction.summary <- as.data.frame(matrix(nrow = 30, ncol = 9))

for(i in 1:30){
  prediction.summary[i,] <- 
    cbind(tvec[i],
          t(summary(
            many.results.sp[many.results.sp$Team==tvec[i],2])),
          t(quantile(many.results.sp[many.results.sp$Team==tvec[i],2], c(.025, .975)))
    )
}

prediction.summary[,2:9] <- as.numeric(unlist(prediction.summary[,2:9]))

prediction.summary$pctrange <- prediction.summary[,9] - prediction.summary[,8]
prediction.summary$range <- prediction.summary[,7] - prediction.summary[,2]

prediction.summary[,8:11] <- round(prediction.summary[,8:11],0)

colnames(prediction.summary) <- c("Squadra", "Min", "1st Qu.", "Mediana",
                                  "Media", "3rd Qu.", "Max", "2.5%",
                                  "97.5%", "Range 2.5%-97.5%", "Range Min-Max")

prediction.summary <- prediction.summary[,c(1,2,8,3:6,9,7,10,11)]
prediction.summary <- prediction.summary[ord.3,]

for(i in seq(1, 30, by = 5)){
  
  prediction.summary[i:(i+4),] <- prediction.summary[i:(i+4),][order(
    prediction.summary[i:(i+4),6], decreasing = T),]
  
}

rownames(prediction.summary) <- NULL

View(prediction.summary)

#### (3) New pitchers sequence every time, Full Season ####

# In this final case, the simulation is computed for all 162 games of a full MLB season,
# so the games until June 4, 2019 are not considered as given.
# As in the last function, the starting pitchers are sampled for each team at every
# simulation.

# One simulation of the season

one.simulation.fullseason <- function(){
  
  dati2019.proj <- dati2019.proj[order(dati2019.proj$date),]
  rownames(dati2019.proj) <- NULL
  dati2019.proj[,21:24] <- NULL
  
  for(i in 1:30){
    for(z in seq(0, 165, 5)){
      startersequence[((z+1):(z+5)),i] <- as.vector((sample(
        starters.st$Name[starters.st$Team==tvec[i]],
        size = 5,
        replace = F,
        prob = starters.st$prob[starters.st$Team==tvec[i]])))
    }
  }
  startersequence <- startersequence[1:162,]
  
  dati2019.proj$hstarter <- dati2019.proj$vstarter <- NA
  
  for(i in 1:30){
    for(z in 1:162){
      dati2019.proj$vstarter[
        dati2019.proj$vteam==tvec[i] & dati2019.proj$vteam_ng == z] <-
        startersequence[z,i]
      
      dati2019.proj$hstarter[
        dati2019.proj$hteam==tvec[i] & dati2019.proj$hteam_ng == z] <-
        startersequence[z,i]
      
    }
  }
  
  dati2019.proj <- merge(dati2019.proj, starters.st[,c(1,5)],
                         by.x = "hstarter", by.y = "Name")
  
  colnames(dati2019.proj)[21] <- c("ERA_hs")
  
  dati2019.proj <- merge(dati2019.proj, starters.st[,c(1,5)],
                         by.x = "vstarter", by.y = "Name")
  
  colnames(dati2019.proj)[22] <- c("ERA_vs")
  
  dati2019.proj$ERA_hs_scale <- as.numeric(scale(dati2019.proj$ERA_hs,
                                                 ERA19, F))
  dati2019.proj$ERA_vs_scale <- as.numeric(scale(dati2019.proj$ERA_vs,
                                                 ERA19, F))
  
  dati2019.proj$prob.home <- predict.glm(out.RSg.eras.erab,
                                         newdata = dati2019.proj,
                                         type = "response")
  dati2019.proj$pred.result <- rbinom(nrow(dati2019.proj),
                                      1,
                                      prob = dati2019.proj$prob.home)
  
  dati2019.proj$pred.winner <- with(dati2019.proj, ifelse(pred.result,
                                                          as.character(hteam),
                                                          as.character(vteam)))
  
  wins <- table(dati2019.proj$pred.winner)
  proj.wins2019 <- as.data.frame(cbind(wins, 162-wins))
  proj.wins2019 <- rownames_to_column(proj.wins2019)
  colnames(proj.wins2019) <- c("Team" ,"TOTWINS", "TOTLOSSES")
  
  proj.wins2019$Team <- as.factor(proj.wins2019$Team)
  proj.wins2019$Team <- factor(proj.wins2019$Team,
                               levels(proj.wins2019$Team)[ord.3])
  
  proj.wins2019 <- merge(proj.wins2019, teams2018[,c(1,3,5)],
                         by.x = "Team", by.y = "teamID")
  
  proj.wins2019$DIV <- paste(proj.wins2019$lgID, proj.wins2019$divID, sep = " ")
  proj.wins2019$DIV <- as.factor(proj.wins2019$DIV)
  levels(proj.wins2019$DIV)
  proj.wins2019$DIV <- factor(proj.wins2019$DIV,
                              levels(proj.wins2019$DIV)[ord.2])
  proj.wins2019[,5] <- NULL
  proj.wins2019 <- proj.wins2019[order(proj.wins2019$DIV),]
  rownames(proj.wins2019) <- NULL
  
  return(proj.wins2019)
  
}

# 1000 simulations

many.results.fullseason <- NULL

pb <- txtProgressBar(min = 0, max = 1000, style = 3)

for(j in 1:1000){
  
  many.results.fullseason <- rbind(many.results.fullseason,
                                   one.simulation.fullseason())
  setTxtProgressBar(pb, j)
}

close(pb)

# Summary

many.results.fullseason <- merge(many.results.fullseason, standings19[,c(1,22)])
prediction.summary.fs <- as.data.frame(matrix(nrow = 30, ncol = 9))

for(i in 1:30){
  prediction.summary.fs[i,] <- 
    cbind(tvec[i],
          t(summary(
            many.results.fullseason[many.results.fullseason$Team==tvec[i],2])),
          t(quantile(many.results.fullseason[many.results.fullseason$Team==tvec[i],2], c(.025, .975)))
    )
}

prediction.summary.fs[,2:9] <- as.numeric(unlist(prediction.summary.fs[,2:9]))

prediction.summary.fs$pctrange <- prediction.summary.fs[,9]-prediction.summary.fs[,8]
prediction.summary.fs$range <- prediction.summary.fs[,7] - prediction.summary.fs[,2]

prediction.summary.fs[,8:11] <- round(prediction.summary.fs[,8:11],0)

colnames(prediction.summary.fs) <- c("Squadra", "Min", "1st Qu.", "Mediana",
                                     "Media", "3rd Qu.", "Max", "2.5%",
                                     "97.5%", "Range 2.5%-97.5%", "Range Min-Max")

prediction.summary.fs <- prediction.summary.fs[,c(1,2,8,3:6,9,7,10,11)]

prediction.summary.fs <- prediction.summary.fs[ord.3,]

for(i in seq(1, 30, by = 5)){
  
  prediction.summary.fs[i:(i+4),] <- prediction.summary.fs[i:(i+4),][order(
    prediction.summary.fs[i:(i+4),6], decreasing = T),]
  
}

rownames(prediction.summary.fs) <- NULL

View(prediction.summary.fs)

###### Simulation of the POSTSEASON ######

#### (1) Just the postseason ####
# This is a very long function, as in order to properly simulate the postseason
# many rules have to be taken into consideration.
# This returns the predicted WINNER OF THE WORLD SERIES

postseason <- function(){
  
  # Step 1: Extracting the teams taking part in the postseason
  # These are the 6 division winners and the 4 wild cards.
  
  div.winners.AL <- div.winners.NL <- div.wins.AL <- div.wins.NL <- NULL
  
  max.wins.AL <- max(proj.wins2019[proj.wins2019$lgID == "AL", 2])
  
  mw.team.AL <- as.character(proj.wins2019[proj.wins2019$lgID == "AL" &
                                             proj.wins2019$TOTWINS_ERA == max.wins.AL,
                                           1])
  
  mw.team.AL <- ifelse(length(mw.team.AL) == 1,
                       mw.team.AL,
                       sample(x = mw.team.AL, size = 1))
  
  max.wins.NL <- max(proj.wins2019[proj.wins2019$lgID == "NL", 2])
  
  mw.team.NL <- as.character(proj.wins2019[proj.wins2019$lgID == "NL" &
                                             proj.wins2019$TOTWINS_ERA == max.wins.NL,
                                           1])
  
  mw.team.NL <- ifelse(length(mw.team.NL) == 1,
                       mw.team.NL,
                       sample(x = mw.team.NL, size = 1))
  for(i in 1:3){
    
    div.wins.AL[i] <- max(proj.wins2019[proj.wins2019$DIV==divisions[i],2])
  }
  
  for(i in 4:6){
    
    div.wins.NL[i-3] <- max(proj.wins2019[proj.wins2019$DIV==divisions[i],2])
  }
  
  div.winner.ALE <- as.character(
    proj.wins2019[proj.wins2019$DIV == "AL E" &
                    proj.wins2019$TOTWINS_ERA == div.wins.AL[1],1]
  )
  
  div.winner.ALE <- ifelse(length(div.winner.ALE) == 1,
                           div.winner.ALE,
                           sample(x = div.winner.ALE, size = 1))
  
  
  div.winner.ALC <- as.character(
    proj.wins2019[proj.wins2019$DIV == "AL C" &
                    proj.wins2019$TOTWINS_ERA == div.wins.AL[2],1]
  )
  
  div.winner.ALC <- ifelse(length(div.winner.ALC) == 1,
                           div.winner.ALC,
                           sample(x = div.winner.ALC, size = 1))
  
  div.winner.ALW <- as.character(
    proj.wins2019[proj.wins2019$DIV == "AL W" &
                    proj.wins2019$TOTWINS_ERA == div.wins.AL[3],1]
  )
  
  div.winner.ALW <- ifelse(length(div.winner.ALW) == 1,
                           div.winner.ALW,
                           sample(x = div.winner.ALW, size = 1))
  
  
  div.winner.NLE <- as.character(
    proj.wins2019[proj.wins2019$DIV == "NL E" &
                    proj.wins2019$TOTWINS_ERA == div.wins.NL[1],1]
  )
  
  div.winner.NLE <- ifelse(length(div.winner.NLE) == 1,
                           div.winner.NLE,
                           sample(x = div.winner.NLE, size = 1))
  
  div.winner.NLC <- as.character(
    proj.wins2019[proj.wins2019$DIV == "NL C" &
                    proj.wins2019$TOTWINS_ERA == div.wins.NL[2],1]
  )
  
  div.winner.NLC  <- ifelse(length(div.winner.NLC) == 1,
                            div.winner.NLC ,
                            sample(x = div.winner.NLC , size = 1))
  
  div.winner.NLW <- as.character(
    proj.wins2019[proj.wins2019$DIV == "NL W" &
                    proj.wins2019$TOTWINS_ERA == div.wins.NL[3],1]
  )
  
  div.winner.NLW <- ifelse(length(div.winner.NLW) == 1,
                           div.winner.NLW,
                           sample(x = div.winner.NLW, size = 1))
  
  div.winners.AL <- as.data.frame(cbind(
    c(div.winner.ALE, div.winner.ALC, div.winner.ALW),
    c(div.wins.AL)))
  
  div.winners.AL[,2] <- as.numeric(as.numeric_version(div.winners.AL[,2]))
  
  div.winners.AL <- div.winners.AL[order(div.winners.AL[,2], decreasing = T),]
  rownames(div.winners.AL) <- NULL
  
  
  div.winners.NL <- as.data.frame(cbind(
    c(div.winner.NLE, div.winner.NLC, div.winner.NLW),
    c(div.wins.NL)))
  
  div.winners.NL[,2] <- as.numeric(as.numeric_version(div.winners.NL[,2]))
  
  div.winners.NL <- div.winners.NL[order(div.winners.NL[,2], decreasing = T),]
  rownames(div.winners.NL) <- NULL
  
  
  IDwAL <- as.numeric(c(rownames(proj.wins2019
                                 [proj.wins2019$Team==div.winner.ALE,]),
                        rownames(proj.wins2019[
                          proj.wins2019$Team==div.winner.ALC,]),
                        rownames(proj.wins2019[
                          proj.wins2019$Team==div.winner.ALW,]),
                        rownames(proj.wins2019[
                          proj.wins2019$lgID == "NL",])
  ))
  
  IDwNL <- as.numeric(c(rownames(proj.wins2019[
    proj.wins2019$Team==div.winner.NLE,]),
    rownames(proj.wins2019[
      proj.wins2019$Team==div.winner.NLC,]),
    rownames(proj.wins2019[
      proj.wins2019$Team==div.winner.NLW,]),
    rownames(proj.wins2019[
      proj.wins2019$lgID == "AL",])
  ))
  
  ### WILD CARD AL
  
  wins.wc1.AL <- max(proj.wins2019[-IDwAL, 2])
  
  wc1.AL <- as.character(
    proj.wins2019[proj.wins2019$lgID == "AL" &
                    proj.wins2019$TOTWINS_ERA == wins.wc1.AL,1]
  )
  
  wc1.AL <- ifelse(length(wc1.AL) == 1,
                   wc1.AL,
                   sample(x = wc1.AL, size = 1))
  
  IDwAL.2 <- as.numeric(c(rownames(proj.wins2019
                                   [proj.wins2019$Team==div.winner.ALE,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.ALC,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.ALW,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==wc1.AL,]),
                          rownames(proj.wins2019[
                            proj.wins2019$lgID == "NL",])
  ))
  
  wins.wc2.AL <- max(proj.wins2019[-IDwAL.2, 2])
  
  wc2.AL <- as.character(
    proj.wins2019[proj.wins2019$lgID == "AL" &
                    proj.wins2019$TOTWINS_ERA == wins.wc2.AL,1]
  )
  
  wc2.AL <- ifelse(length(wc2.AL) == 1,
                   wc2.AL,
                   sample(x = wc2.AL, size = 1))
  
  wc.teams.AL <- unname(as.data.frame(rbind(
    cbind(wc1.AL, wins.wc1.AL),
    cbind(wc2.AL, wins.wc2.AL))))
  colnames(wc.teams.AL) <- c("V1", "V2")
  wc.teams.AL[,2] <- as.numeric(as.numeric_version(wc.teams.AL[,2]))
  
  
  ### WILD CARD NL
  
  wins.wc1.NL <- max(proj.wins2019[-IDwNL, 2])
  
  wc1.NL <- as.character(
    proj.wins2019[proj.wins2019$lgID == "NL" &
                    proj.wins2019$TOTWINS_ERA == wins.wc1.NL,1]
  )
  
  wc1.NL <- ifelse(length(wc1.NL) == 1,
                   wc1.NL,
                   sample(x = wc1.NL, size = 1))
  
  IDwNL.2 <- as.numeric(c(rownames(proj.wins2019
                                   [proj.wins2019$Team==div.winner.NLE,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.NLC,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.NLW,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==wc1.NL,]),
                          rownames(proj.wins2019[
                            proj.wins2019$lgID == "AL",])
  ))
  
  wins.wc2.NL <- max(proj.wins2019[-IDwNL.2, 2])
  
  wc2.NL <- as.character(
    proj.wins2019[proj.wins2019$lgID == "NL" &
                    proj.wins2019$TOTWINS_ERA == wins.wc2.NL,1]
  )
  
  wc2.NL <- ifelse(length(wc2.NL) == 1,
                   wc2.NL,
                   sample(x = wc2.NL, size = 1))
  
  wc.teams.NL <- unname(as.data.frame(rbind(
    cbind(wc1.NL, wins.wc1.NL),
    cbind(wc2.NL, wins.wc2.NL))))
  colnames(wc.teams.NL) <- c("V1", "V2")
  wc.teams.NL[,2] <- as.numeric(as.numeric_version(wc.teams.NL[,2]))
  
  # The seed is crucial, as it determines which team plays most games at home
  # in a postseason series. It is determined by the regular season record.
  
  postseason.seed <- as.data.frame(
    rbind( div.winners.AL, wc.teams.AL, div.winners.NL,  wc.teams.NL))
  
  postseason.seed <- cbind(postseason.seed, c(rep("AL", 5), rep("NL", 5)))
  colnames(postseason.seed) <- c("Team", "Wins", "League")
  
  postseason.seed <- postseason.seed[order(postseason.seed$Wins, decreasing = T),]
  rownames(postseason.seed) <- NULL
  postseason.seed <- cbind(postseason.seed, 1:10)
  colnames(postseason.seed) <- c("Team", "Wins", "League", "Seed")
  
  # Creating the postseason schedule
  
  postseason.schedule <- as.data.frame(
    bind_rows(
      as.data.frame(unname(cbind(as.character(wc1.AL), as.character(wc2.AL)))),
      
      as.data.frame(unname(cbind(as.character(mw.team.AL), "TBD"))),
      as.data.frame(unname(cbind(as.character(mw.team.AL), "TBD"))),
      as.data.frame(unname(cbind("TBD", as.character(mw.team.AL)))),
      as.data.frame(unname(cbind("TBD", as.character(mw.team.AL)))),
      as.data.frame(unname(cbind(as.character(mw.team.AL), "TBD"))),
      
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[3,1]), as.character(div.winners.AL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[3,1]), as.character(div.winners.AL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      
      as.data.frame(unname(cbind(as.character(wc1.NL), as.character(wc2.NL)))),
      
      as.data.frame(unname(cbind(as.character(mw.team.NL), "TBD"))),
      as.data.frame(unname(cbind(as.character(mw.team.NL), "TBD"))),
      as.data.frame(unname(cbind("TBD", as.character(mw.team.NL)))),
      as.data.frame(unname(cbind("TBD", as.character(mw.team.NL)))),
      as.data.frame(unname(cbind(as.character(mw.team.NL), "TBD"))),
      
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[3,1]), as.character(div.winners.NL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[3,1]), as.character(div.winners.NL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      
      as.data.frame(cbind(rep("TBD", 21), rep("TBD", 21)))
      
    )
  )
  
  postseason.schedule <- cbind(postseason.schedule, c("AL WC", rep ("AL DS 1", 5),
                                                      rep ("AL DS 2", 5), "NL WC",
                                                      rep ("NL DS 1", 5), rep ("NL DS 2", 5),
                                                      rep("AL CS", 7), rep("NL CS", 7),
                                                      rep("WS", 7)))
  
  postseason.schedule <- cbind(1:43, postseason.schedule)
  
  colnames(postseason.schedule) <- c("ID", "hteam", "vteam", "series")
  
  
  
  postseason.schedule$hstarter <- postseason.schedule$vstarter <- 
    postseason.schedule$hteam_ng <- postseason.schedule$vteam_ng <- NA
  
  tvec.ps <- as.vector(postseason.seed$Team)
  
  
  for(i in 1:10){
    for(z in 1:21){
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],2][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 6][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 5][z] <- z
      )
    }
  }
  
  for(i in 1:30){
    for(z in seq(0, 165, 4)){
      startersequence[((z+1):(z+4)),i] <- as.vector((sample(
        starters.st$Name[starters.st$Team==tvec[i]],
        size = 4,
        replace = F,
        prob = starters.st$prob[starters.st$Team==tvec[i]])))
    }
  }
  
  n <- sample(1:142, 1)
  
  startersequence.ps <- startersequence[(n):(n+19),]
  rownames(startersequence.ps) <- NULL
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[9:10] <- c("ERA_hs", "FIP_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[11:12] <- c("ERA_vs", "FIP_vs")
  
  length(predict(out.scale.era10, type = "response"))
  length(predict(out.era10, type = "response"))
  
  mean(dati2019.proj$FIP_hs)
  sum(postseason.schedule$FIP_hs[is.na(postseason.schedule$FIP_hs)==F])/
    (sum(is.na(postseason.schedule$FIP_hs)==F))
  
  postseason.schedule$FIP_hs_scale <- as.numeric(scale(postseason.schedule$FIP_hs,
                                                       mean(dati2019.proj$FIP_hs), F))
  postseason.schedule$FIP_vs_scale <- as.numeric(scale(postseason.schedule$FIP_vs,
                                                       mean(dati2019.proj$FIP_vs), F))
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       mean(dati2019.proj$ERA_hs), F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       mean(dati2019.proj$ERA_vs), F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[17] <- "RDg_vteam"
  colnames(postseason.schedule)[18] <- "RDg_hteam"
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home.ERA <- predict.glm(out.scale.era10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  postseason.schedule$prob.home.FIP <- predict.glm(out.scale.fip10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  postseason.schedule$pred.result.FIP <- rbinom(nrow(postseason.schedule),
                                                1,
                                                prob = postseason.schedule$prob.home.FIP)
  
  postseason.schedule$pred.result.ERA <- rbinom(nrow(postseason.schedule),
                                                1,
                                                prob = postseason.schedule$prob.home.ERA)
  
  postseason.schedule$pred.winner.ERA <- with(postseason.schedule, ifelse(pred.result.ERA,
                                                                          as.character(hteam),
                                                                          as.character(vteam)))
  postseason.schedule$pred.winner.FIP<- with(postseason.schedule, ifelse(pred.result.FIP,
                                                                         as.character(hteam),
                                                                         as.character(vteam)))
  
  wcwinner.AL <- postseason.schedule[1,23]
  
  postseason.schedule[2,2] <- postseason.schedule[3,2] <- postseason.schedule[4,1] <-
    postseason.schedule[5,1] <- postseason.schedule[6,2] <- wcwinner.AL
  
  wcwinner.NL <-  postseason.schedule[12,23]
  
  postseason.schedule[13,2] <- postseason.schedule[14,2] <- postseason.schedule[15,1] <-
    postseason.schedule[16,1] <- postseason.schedule[17,2] <- wcwinner.NL
  
  
  ds2.winner.AL <- ifelse(sum(postseason.schedule[7:11,23]==as.character(div.winners.AL[2,1]))>=3,
                          as.character(div.winners.AL[2,1]),
                          as.character(div.winners.AL[3,1]))
  
  ds2.winner.NL <- ifelse(sum(postseason.schedule[18:22,23]==as.character(div.winners.NL[2,1]))>=3,
                          as.character(div.winners.NL[2,1]),
                          as.character(div.winners.NL[3,1]))
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule[,9:18] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[15:16] <- c("ERA_hs", "FIP_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[17:18] <- c("ERA_vs", "FIP_vs")
  
  
  postseason.schedule$FIP_hs_scale <- as.numeric(scale(postseason.schedule$FIP_hs,
                                                       mean(dati2019.proj$FIP_hs), F))
  postseason.schedule$FIP_vs_scale <- as.numeric(scale(postseason.schedule$FIP_vs,
                                                       mean(dati2019.proj$FIP_vs), F))
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       mean(dati2019.proj$ERA_hs), F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       mean(dati2019.proj$ERA_vs), F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[23] <- "RDg_vteam"
  colnames(postseason.schedule)[24] <- "RDg_hteam"
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home.ERA <- predict.glm(out.scale.era10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  postseason.schedule$prob.home.FIP <- predict.glm(out.scale.fip10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  
  postseason.schedule$pred.result.FIP[c(2:6,13:17)] <- rbinom(
    length(postseason.schedule$pred.result.FIP[c(2:6,13:17)]),
    1,
    prob = postseason.schedule$prob.home.FIP[c(2:6,13:17)])
  
  postseason.schedule$pred.result.ERA[c(2:6,13:17)] <- rbinom(
    length(postseason.schedule$pred.result.ERA[c(2:6,13:17)]),
    1,
    prob = postseason.schedule$prob.home.ERA[c(2:6,13:17)])
  
  postseason.schedule$pred.winner.ERA <-
    with(postseason.schedule, ifelse(pred.result.ERA,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  postseason.schedule$pred.winner.FIP <-
    with(postseason.schedule, ifelse(pred.result.FIP,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  ds1.winner.AL <- ifelse(sum(postseason.schedule[2:6,13]==as.character(div.winners.AL[1,1]))>=3,
                          as.character(div.winners.AL[1,1]),
                          as.character(wcwinner.AL))
  
  ds1.winner.NL <- ifelse(sum(postseason.schedule[13:17,13]==as.character(div.winners.NL[1,1]))>=3,
                          as.character(div.winners.NL[1,1]),
                          as.character(wcwinner.NL))
  
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.AL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.AL,4],
    
    postseason.schedule[23,1] <- postseason.schedule[24,1] <- postseason.schedule[25,2] <-
      postseason.schedule[26,2] <- postseason.schedule[27,2] <- postseason.schedule[28,1] <-
      postseason.schedule[29,1] <- ds1.winner.AL,
    
    postseason.schedule[23,2] <- postseason.schedule[24,2] <- postseason.schedule[25,1] <-
      postseason.schedule[26,1] <- postseason.schedule[27,1] <- postseason.schedule[28,2] <-
      postseason.schedule[29,2] <- ds1.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.AL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.AL,4],
    
    postseason.schedule[23,2] <- postseason.schedule[24,2] <- postseason.schedule[25,1] <-
      postseason.schedule[26,1] <- postseason.schedule[27,1] <- postseason.schedule[28,2] <-
      postseason.schedule[29,2] <- ds2.winner.AL,
    
    postseason.schedule[23,1] <- postseason.schedule[24,1] <- postseason.schedule[25,2] <-
      postseason.schedule[26,2] <- postseason.schedule[27,2] <- postseason.schedule[28,1] <-
      postseason.schedule[29,1] <- ds2.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.NL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.NL,4],
    
    postseason.schedule[30,1] <- postseason.schedule[31,1] <- postseason.schedule[32,2] <-
      postseason.schedule[33,2] <- postseason.schedule[34,2] <- postseason.schedule[35,1] <-
      postseason.schedule[36,1] <- ds1.winner.NL,
    
    postseason.schedule[30,2] <- postseason.schedule[31,2] <- postseason.schedule[32,1] <-
      postseason.schedule[33,1] <- postseason.schedule[34,1] <- postseason.schedule[35,2] <-
      postseason.schedule[36,2] <- ds1.winner.NL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.NL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.NL,4],
    
    postseason.schedule[30,2] <- postseason.schedule[31,2] <- postseason.schedule[32,1] <-
      postseason.schedule[33,1] <- postseason.schedule[34,1] <- postseason.schedule[35,2] <-
      postseason.schedule[36,2] <- ds2.winner.NL,
    
    postseason.schedule[30,1] <- postseason.schedule[31,1] <- postseason.schedule[32,2] <-
      postseason.schedule[33,2] <- postseason.schedule[34,2] <- postseason.schedule[35,1] <-
      postseason.schedule[36,1] <- ds2.winner.NL
    
  )
  
  
  ## Championship series
  
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule[,15:24] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[15:16] <- c("ERA_hs", "FIP_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[17:18] <- c("ERA_vs", "FIP_vs")
  
  postseason.schedule$FIP_hs_scale <- as.numeric(scale(postseason.schedule$FIP_hs,
                                                       mean(dati2019.proj$FIP_hs), F))
  postseason.schedule$FIP_vs_scale <- as.numeric(scale(postseason.schedule$FIP_vs,
                                                       mean(dati2019.proj$FIP_vs), F))
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       mean(dati2019.proj$ERA_hs), F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       mean(dati2019.proj$ERA_vs), F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[23] <- "RDg_vteam"
  colnames(postseason.schedule)[24] <- "RDg_hteam"
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home.ERA <- predict.glm(out.scale.era10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  postseason.schedule$prob.home.FIP <- predict.glm(out.scale.fip10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  
  postseason.schedule$pred.result.FIP[23:36] <- rbinom(
    length(postseason.schedule$pred.result.FIP[23:36]),
    1,
    prob = postseason.schedule$prob.home.FIP[23:36])
  
  postseason.schedule$pred.result.ERA[23:36] <- rbinom(
    length(postseason.schedule$pred.result.ERA[23:36]),
    1,
    prob = postseason.schedule$prob.home.ERA[23:36])
  
  postseason.schedule$pred.winner.ERA <-
    with(postseason.schedule, ifelse(pred.result.ERA,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  postseason.schedule$pred.winner.FIP <-
    with(postseason.schedule, ifelse(pred.result.FIP,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  cs.winner.AL <- ifelse(sum(postseason.schedule[23:29,13]==as.character(ds1.winner.AL))>=4,
                         as.character(ds1.winner.AL),
                         as.character(ds2.winner.AL))
  
  cs.winner.NL <- ifelse(sum(postseason.schedule[30:36,13]==as.character(ds1.winner.NL))>=4,
                         as.character(ds1.winner.NL),
                         as.character(ds2.winner.NL))
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == cs.winner.AL,4] <
      postseason.seed[postseason.seed$Team == cs.winner.NL,4],
    
    postseason.schedule[37,1] <- postseason.schedule[38,1] <- postseason.schedule[39,2] <-
      postseason.schedule[40,2] <- postseason.schedule[41,2] <- postseason.schedule[42,1] <-
      postseason.schedule[43,1] <- cs.winner.AL,
    
    postseason.schedule[37,2] <- postseason.schedule[38,2] <- postseason.schedule[39,1] <-
      postseason.schedule[40,1] <- postseason.schedule[41,1] <- postseason.schedule[42,2] <-
      postseason.schedule[43,2] <- cs.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == cs.winner.AL,4] <
      postseason.seed[postseason.seed$Team == cs.winner.NL,4],
    
    postseason.schedule[37,2] <- postseason.schedule[38,2] <- postseason.schedule[39,1] <-
      postseason.schedule[40,1] <- postseason.schedule[41,1] <- postseason.schedule[42,2] <-
      postseason.schedule[43,2] <- cs.winner.NL,
    
    postseason.schedule[37,1] <- postseason.schedule[38,1] <- postseason.schedule[39,2] <-
      postseason.schedule[40,2] <- postseason.schedule[41,2] <- postseason.schedule[42,1] <-
      postseason.schedule[43,1] <- cs.winner.NL
    
  )
  
  
  ## World Series
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule[,15:24] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[15:16] <- c("ERA_hs", "FIP_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5,18)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[17:18] <- c("ERA_vs", "FIP_vs")
  
  postseason.schedule$FIP_hs_scale <- as.numeric(scale(postseason.schedule$FIP_hs,
                                                       mean(dati2019.proj$FIP_hs), F))
  postseason.schedule$FIP_vs_scale <- as.numeric(scale(postseason.schedule$FIP_vs,
                                                       mean(dati2019.proj$FIP_vs), F))
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       mean(dati2019.proj$ERA_hs), F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       mean(dati2019.proj$ERA_vs), F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,22)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[23] <- "RDg_vteam"
  colnames(postseason.schedule)[24] <- "RDg_hteam"
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home.ERA <- predict.glm(out.scale.era10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  postseason.schedule$prob.home.FIP <- predict.glm(out.scale.fip10,
                                                   newdata = postseason.schedule,
                                                   type = "response")
  
  
  postseason.schedule$pred.result.FIP[37:43] <- rbinom(
    length(postseason.schedule$pred.result.FIP[37:43]),
    1,
    prob = postseason.schedule$prob.home.FIP[37:43])
  
  postseason.schedule$pred.result.ERA[37:43] <- rbinom(
    length(postseason.schedule$pred.result.ERA[37:43]),
    1,
    prob = postseason.schedule$prob.home.ERA[37:43])
  
  postseason.schedule$pred.winner.ERA <-
    with(postseason.schedule, ifelse(pred.result.ERA,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  postseason.schedule$pred.winner.FIP <-
    with(postseason.schedule, ifelse(pred.result.FIP,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  ws.winner <- ifelse(sum(postseason.schedule[37:43,13]==as.character(cs.winner.AL))>=4,
                      as.character(cs.winner.AL),
                      as.character(cs.winner.NL))
  
  return(ws.winner)
  
}


#### (2) Rest of Regular Season + PostSeason ####

# This function simulates the remaining games of the regular season, starting
# from June 4, 2019 (the games played before are considered as given), and
# sampling the starting pitchers at the beginning of each simulation.
# This returns the REGULAR SEASON FINAL PREDICTED STANDINGS along with
# the WINNERS OF EACH POSTSEASON SERIES.

one.sim.rosandpost <- function(){
  
  dati2019.ros <- dati2019.ros[order(dati2019.ros$date),]
  rownames(dati2019.ros) <- NULL
  dati2019.ros[,23:26] <- NULL
  
  for(i in 1:30){
    startersequenceros[1:5,i] <- as.vector((sample(
      starters.st.ros$Name[starters.st.ros$Team==tvec[i]],
      size = 5,
      replace = F,
      prob = starters.st.ros$prob[starters.st.ros$Team==tvec[i]])))
    for(z in (6:max(starters.st.ros$GamesLeft))){
      startersequenceros[z,i] <- as.character(sample(
        vsetdiff(starters.st.ros$Name[starters.st.ros$Team==tvec[i]],
                 startersequenceros[(z-4):(z-1),i]),
        size = 1,
        replace = F,
        prob = vsetdiff(starters.st.ros$prob[starters.st.ros$Team==tvec[i]],
                        starters.st.ros$prob[starters.st.ros$Name %in%
                                               startersequenceros[(z-4):(z-1),i]])
        
      ))
    }
  }
  
  for(i in 1:30){
    for(z in 1:110){
      dati2019.ros$vstarter[
        dati2019.ros$vteam==tvec[i] & dati2019.ros$vteam_ng_ros == z] <- startersequenceros[z,i]
      
      dati2019.ros$hstarter[
        dati2019.ros$hteam==tvec[i] & dati2019.ros$hteam_ng_ros == z] <- startersequenceros[z,i]
      
    }
  }
  
  dati2019.ros <- merge(dati2019.ros, starters.st.ros[,c(2,5)],
                        by.x = "hstarter", by.y = "Name")
  
  colnames(dati2019.ros)[23] <- c("ERA_hs")
  
  dati2019.ros <- merge(dati2019.ros, starters.st.ros[,c(2,5)],
                        by.x = "vstarter", by.y = "Name")
  
  colnames(dati2019.ros)[24] <- c("ERA_vs")
  
  dati2019.ros$ERA_hs_scale <- as.numeric(scale(dati2019.ros$ERA_hs,
                                                ERA19, F))
  dati2019.ros$ERA_vs_scale <- as.numeric(scale(dati2019.ros$ERA_vs,
                                                ERA19, F))
  
  dati2019.ros$prob.home <- predict.glm(out.RSg.eras.erab,
                                        newdata = dati2019.ros,
                                        type = "response")
  
  dati2019.ros$pred.result <- rbinom(nrow(dati2019.ros),
                                     1,
                                     prob = dati2019.ros$prob.home)
  
  dati2019.ros$pred.winner <- with(dati2019.ros, ifelse(pred.result,
                                                        as.character(hteam),
                                                        as.character(vteam)))
  
  wins.ros <- table(dati2019.ros$pred.winner)
  wins.ros <- as.data.frame(wins.ros)
  colnames(wins.ros) <- c("Team", "wins.ros")
  
  pred.wlrecord2019 <- merge(wlrecord2019, wins.ros)
  pred.wlrecord2019$TOTWINS <- pred.wlrecord2019$W + pred.wlrecord2019$wins.ros
  pred.wlrecord2019$TOTLOSSES <- 162-pred.wlrecord2019$TOTWINS
  
  final.pred.standings <- pred.wlrecord2019[,c(1,6:7)]
  
  final.pred.standings$Team <- as.factor(final.pred.standings$Team)
  
  final.pred.standings$Team <- factor(final.pred.standings$Team,
                                      levels(final.pred.standings$Team)[ord.4])
  
  final.pred.standings$Team <- factor(final.pred.standings$Team,
                                      levels(final.pred.standings$Team)[ord.3])
  
  final.pred.standings <- merge(final.pred.standings, teams2018[,c(1,3,5)],
                                by.x = "Team", by.y = "teamID")
  
  final.pred.standings$DIV <- paste(final.pred.standings$lgID,
                                    final.pred.standings$divID, sep = " ")
  final.pred.standings$DIV <- as.factor(final.pred.standings$DIV)
  levels(final.pred.standings$DIV)
  final.pred.standings$DIV <- factor(final.pred.standings$DIV,
                                     levels(final.pred.standings$DIV)[ord.2])
  final.pred.standings[,5] <- NULL
  final.pred.standings <- final.pred.standings[order(final.pred.standings$DIV),]
  rownames(final.pred.standings) <- NULL
  
  
  div.winners.AL <- div.winners.NL <- div.wins.AL <- div.wins.NL <- NULL
  
  max.wins.AL <- max(final.pred.standings[final.pred.standings$lgID == "AL", 2])
  
  mw.team.AL <- as.character(final.pred.standings[final.pred.standings$lgID == "AL" &
                                                    final.pred.standings$TOTWINS == max.wins.AL,
                                                  1])
  
  mw.team.AL <- ifelse(length(mw.team.AL) == 1,
                       mw.team.AL,
                       sample(x = mw.team.AL, size = 1))
  
  max.wins.NL <- max(final.pred.standings[final.pred.standings$lgID == "NL", 2])
  
  mw.team.NL <- as.character(final.pred.standings[final.pred.standings$lgID == "NL" &
                                                    final.pred.standings$TOTWINS == max.wins.NL,
                                                  1])
  
  mw.team.NL <- ifelse(length(mw.team.NL) == 1,
                       mw.team.NL,
                       sample(x = mw.team.NL, size = 1))
  for(i in 1:3){
    
    div.wins.AL[i] <- max(final.pred.standings[final.pred.standings$DIV==divisions[i],2])
  }
  
  for(i in 4:6){
    
    div.wins.NL[i-3] <- max(final.pred.standings[final.pred.standings$DIV==divisions[i],2])
  }
  
  div.winner.ALE <- as.character(
    final.pred.standings[final.pred.standings$DIV == "AL E" &
                           final.pred.standings$TOTWINS == div.wins.AL[1],1]
  )
  
  div.winner.ALE <- ifelse(length(div.winner.ALE) == 1,
                           div.winner.ALE,
                           sample(x = div.winner.ALE, size = 1))
  
  
  div.winner.ALC <- as.character(
    final.pred.standings[final.pred.standings$DIV == "AL C" &
                           final.pred.standings$TOTWINS == div.wins.AL[2],1]
  )
  
  div.winner.ALC <- ifelse(length(div.winner.ALC) == 1,
                           div.winner.ALC,
                           sample(x = div.winner.ALC, size = 1))
  
  div.winner.ALW <- as.character(
    final.pred.standings[final.pred.standings$DIV == "AL W" &
                           final.pred.standings$TOTWINS == div.wins.AL[3],1]
  )
  
  div.winner.ALW <- ifelse(length(div.winner.ALW) == 1,
                           div.winner.ALW,
                           sample(x = div.winner.ALW, size = 1))
  
  
  div.winner.NLE <- as.character(
    final.pred.standings[final.pred.standings$DIV == "NL E" &
                           final.pred.standings$TOTWINS == div.wins.NL[1],1]
  )
  
  div.winner.NLE <- ifelse(length(div.winner.NLE) == 1,
                           div.winner.NLE,
                           sample(x = div.winner.NLE, size = 1))
  
  div.winner.NLC <- as.character(
    final.pred.standings[final.pred.standings$DIV == "NL C" &
                           final.pred.standings$TOTWINS == div.wins.NL[2],1]
  )
  
  div.winner.NLC  <- ifelse(length(div.winner.NLC) == 1,
                            div.winner.NLC ,
                            sample(x = div.winner.NLC , size = 1))
  
  div.winner.NLW <- as.character(
    final.pred.standings[final.pred.standings$DIV == "NL W" &
                           final.pred.standings$TOTWINS == div.wins.NL[3],1]
  )
  
  div.winner.NLW <- ifelse(length(div.winner.NLW) == 1,
                           div.winner.NLW,
                           sample(x = div.winner.NLW, size = 1))
  
  div.winners.AL <- as.data.frame(cbind(
    c(div.winner.ALE, div.winner.ALC, div.winner.ALW),
    c(div.wins.AL)))
  
  div.winners.AL[,1] <- as.character(div.winners.AL[,1])
  div.winners.AL[,2] <- as.numeric(as.numeric_version(div.winners.AL[,2]))
  
  div.winners.AL <- div.winners.AL[order(div.winners.AL[,2], decreasing = T),]
  rownames(div.winners.AL) <- NULL
  
  div.winners.NL <- as.data.frame(cbind(
    c(div.winner.NLE, div.winner.NLC, div.winner.NLW),
    c(div.wins.NL)))
  
  div.winners.NL[,1] <- as.character(div.winners.NL[,1])
  div.winners.NL[,2] <- as.numeric(as.numeric_version(div.winners.NL[,2]))
  
  div.winners.NL <- div.winners.NL[order(div.winners.NL[,2], decreasing = T),]
  rownames(div.winners.NL) <- NULL
  
  
  IDwAL <- as.numeric(c(rownames(final.pred.standings
                                 [final.pred.standings$Team==div.winner.ALE,]),
                        rownames(final.pred.standings[
                          final.pred.standings$Team==div.winner.ALC,]),
                        rownames(final.pred.standings[
                          final.pred.standings$Team==div.winner.ALW,]),
                        rownames(final.pred.standings[
                          final.pred.standings$lgID == "NL",])
  ))
  
  IDwNL <- as.numeric(c(rownames(final.pred.standings[
    final.pred.standings$Team==div.winner.NLE,]),
    rownames(final.pred.standings[
      final.pred.standings$Team==div.winner.NLC,]),
    rownames(final.pred.standings[
      final.pred.standings$Team==div.winner.NLW,]),
    rownames(final.pred.standings[
      final.pred.standings$lgID == "AL",])
  ))
  
  ### WILD CARD AL
  
  wins.wc1.AL <- max(final.pred.standings[-IDwAL, 2])
  
  wc1.AL <- as.character(
    final.pred.standings[-IDwAL,1][final.pred.standings[-IDwAL,2]==wins.wc1.AL]
  )
  
  wc1.AL <- ifelse(length(wc1.AL) == 1,
                   wc1.AL,
                   sample(x = wc1.AL, size = 1))
  
  IDwAL.2 <- as.numeric(c(rownames(final.pred.standings
                                   [final.pred.standings$Team==div.winner.ALE,]),
                          rownames(final.pred.standings[
                            final.pred.standings$Team==div.winner.ALC,]),
                          rownames(final.pred.standings[
                            final.pred.standings$Team==div.winner.ALW,]),
                          rownames(final.pred.standings[
                            final.pred.standings$Team==wc1.AL,]),
                          rownames(final.pred.standings[
                            final.pred.standings$lgID == "NL",])
  ))
  
  wins.wc2.AL <- max(final.pred.standings[-IDwAL.2, 2])
  
  wc2.AL <- as.character(
    final.pred.standings[-IDwAL.2,1][final.pred.standings[-IDwAL.2,2]==wins.wc2.AL]
  )
  
  wc2.AL <- ifelse(length(wc2.AL) == 1,
                   wc2.AL,
                   sample(x = wc2.AL, size = 1))
  
  wc.teams.AL <- unname(as.data.frame(rbind(
    cbind(wc1.AL, wins.wc1.AL),
    cbind(wc2.AL, wins.wc2.AL))))
  colnames(wc.teams.AL) <- c("V1", "V2")
  wc.teams.AL[,1] <- as.character(wc.teams.AL[,1])
  wc.teams.AL[,2] <- as.numeric(as.numeric_version(wc.teams.AL[,2]))
  
  
  ### WILD CARD NL
  
  wins.wc1.NL <- max(final.pred.standings[-IDwNL, 2])
  
  wc1.NL <- as.character(
    final.pred.standings[-IDwNL,1][final.pred.standings[-IDwNL,2]==wins.wc1.NL]
  )
  
  wc1.NL <- ifelse(length(wc1.NL) == 1,
                   wc1.NL,
                   sample(x = wc1.NL, size = 1))
  
  IDwNL.2 <- as.numeric(c(rownames(final.pred.standings
                                   [final.pred.standings$Team==div.winner.NLE,]),
                          rownames(final.pred.standings[
                            final.pred.standings$Team==div.winner.NLC,]),
                          rownames(final.pred.standings[
                            final.pred.standings$Team==div.winner.NLW,]),
                          rownames(final.pred.standings[
                            final.pred.standings$Team==wc1.NL,]),
                          rownames(final.pred.standings[
                            final.pred.standings$lgID == "AL",])
  ))
  
  wins.wc2.NL <- max(final.pred.standings[-IDwNL.2, 2])
  
  wc2.NL <- as.character(
    final.pred.standings[-IDwNL.2,1][final.pred.standings[-IDwNL.2,2]==wins.wc2.NL]
  )
  
  wc2.NL <- ifelse(length(wc2.NL) == 1,
                   wc2.NL,
                   sample(x = wc2.NL, size = 1))
  
  wc.teams.NL <- unname(as.data.frame(rbind(
    cbind(wc1.NL, wins.wc1.NL),
    cbind(wc2.NL, wins.wc2.NL))))
  colnames(wc.teams.NL) <- c("V1", "V2")
  wc.teams.NL[,1] <- as.character(wc.teams.NL[,1])
  wc.teams.NL[,2] <- as.numeric(as.numeric_version(wc.teams.NL[,2]))
  
  ##
  
  
  
  postseason.seed <- as.data.frame(
    rbind(
      
      div.winners.AL,
      wc.teams.AL,
      div.winners.NL,
      wc.teams.NL
      
    )
    
  )
  
  
  postseason.seed <- cbind(postseason.seed, c(rep("AL", 5), rep("NL", 5)))
  colnames(postseason.seed) <- c("Team", "Wins", "League")
  
  postseason.seed <- postseason.seed[order(postseason.seed$Wins, decreasing = T),]
  rownames(postseason.seed) <- NULL
  postseason.seed <- cbind(postseason.seed, 1:10)
  colnames(postseason.seed) <- c("Team", "Wins", "League", "Seed")
  
  postseason.schedule <- as.data.frame(
    rbind(
      
      as.data.frame(unname(cbind(as.character(wc1.AL), as.character(wc2.AL)))),
      
      as.data.frame(unname(cbind(as.character(div.winners.AL[1,1]), "TBD"))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[1,1]), "TBD"))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.AL[1,1])))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.AL[1,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[1,1]), "TBD"))),
      
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[3,1]), as.character(div.winners.AL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[3,1]), as.character(div.winners.AL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      
      as.data.frame(unname(cbind(as.character(wc1.NL), as.character(wc2.NL)))),
      
      as.data.frame(unname(cbind(as.character(div.winners.NL[1,1]), "TBD"))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[1,1]), "TBD"))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.NL[1,1])))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.NL[1,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[1,1]), "TBD"))),
      
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[3,1]), as.character(div.winners.NL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[3,1]), as.character(div.winners.NL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      
      as.data.frame(cbind(rep("TBD", 21), rep("TBD", 21)))
      
    )
  )
  
  postseason.schedule <- cbind(postseason.schedule, c("AL WC", rep ("AL DS 1", 5),
                                                      rep ("AL DS 2", 5), "NL WC",
                                                      rep ("NL DS 1", 5), rep ("NL DS 2", 5),
                                                      rep("AL CS", 7), rep("NL CS", 7),
                                                      rep("WS", 7)))
  
  postseason.schedule <- cbind(1:43, postseason.schedule)
  
  colnames(postseason.schedule) <- c("ID", "hteam", "vteam", "series")
  postseason.schedule$hteam <- as.character(postseason.schedule$hteam)
  postseason.schedule$vteam <- as.character(postseason.schedule$vteam)
  
  postseason.schedule$hstarter <- postseason.schedule$vstarter <- 
    postseason.schedule$hteam_ng <- postseason.schedule$vteam_ng <- NA
  
  tvec.ps <- as.vector(postseason.seed$Team)
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],2][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 6][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 5][z] <- z
      )
    }
  }
  
  for(i in 1:30){
    startersequence[1:5,i] <- as.vector((sample(
      starters.st$Name[starters.st$Team==tvec[i]],
      size = 5,
      replace = F,
      prob = starters.st$prob[starters.st$Team==tvec[i]])))
    for(z in (6:162)){
      startersequence[z,i] <- as.character(sample(
        vsetdiff(starters.st$Name[starters.st$Team==tvec[i]],
                 startersequence[(z-4):(z-1),i]),
        size = 1,
        replace = F,
        prob = vsetdiff(starters.st$prob[starters.st$Team==tvec[i]],
                        starters.st$prob[starters.st$Name %in%
                                           startersequence[(z-4):(z-1),i]])
        
      ))
    }
  }
  
  n <- sample(1:142, 1)
  
  startersequence.ps <- startersequence[(n):(n+19),]
  rownames(startersequence.ps) <- NULL
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[9] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[10] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- "RSg_vteam"
  colnames(postseason.schedule)[14] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[17:18] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[19:20] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  
  postseason.schedule$pred.result <- rbinom(nrow(postseason.schedule),
                                            1,
                                            prob = postseason.schedule$prob.home)
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result, as.character(hteam),
                                     as.character(vteam)))
  
  wcwinner.AL <- as.character(postseason.schedule[1,"pred.winner"])
  
  postseason.schedule[2,2] <- postseason.schedule[3,2] <- postseason.schedule[4,1] <-
    postseason.schedule[5,1] <- postseason.schedule[6,2] <- wcwinner.AL
  
  wcwinner.NL <-  as.character(postseason.schedule[12,"pred.winner"])
  
  postseason.schedule[13,2] <- postseason.schedule[14,2] <- postseason.schedule[15,1] <-
    postseason.schedule[16,1] <- postseason.schedule[17,2] <- wcwinner.NL
  
  
  ds2.winner.AL <- ifelse(sum(postseason.schedule[7:11,"pred.winner"]==
                                as.character(div.winners.AL[2,1]))>=3,
                          as.character(div.winners.AL[2,1]),
                          as.character(div.winners.AL[3,1]))
  
  ds2.winner.NL <- ifelse(sum(postseason.schedule[18:22,"pred.winner"]
                              ==as.character(div.winners.NL[2,1]))>=3,
                          as.character(div.winners.NL[2,1]),
                          as.character(div.winners.NL[3,1]))
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule[,9:20] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[12] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[16] <- "RSg_vteam"
  colnames(postseason.schedule)[17] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[20:21] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[22:23] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  postseason.schedule$pred.result[c(2:6,13:17)] <- rbinom(
    length(postseason.schedule$pred.result[c(2:6,13:17)]),
    1,
    prob = postseason.schedule$prob.home[c(2:6,13:17)])
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  ds1.winner.AL <- ifelse(sum(postseason.schedule[2:6,"pred.winner"]
                              ==as.character(div.winners.AL[1,1]))>=3,
                          as.character(div.winners.AL[1,1]),
                          as.character(wcwinner.AL))
  
  ds1.winner.NL <- ifelse(sum(postseason.schedule[13:17,"pred.winner"]
                              ==as.character(div.winners.NL[1,1]))>=3,
                          as.character(div.winners.NL[1,1]),
                          as.character(wcwinner.NL))
  
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.AL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.AL,4],
    
    postseason.schedule[23,1] <- postseason.schedule[24,1] <- postseason.schedule[25,2] <-
      postseason.schedule[26,2] <- postseason.schedule[27,2] <- postseason.schedule[28,1] <-
      postseason.schedule[29,1] <- ds1.winner.AL,
    
    postseason.schedule[23,2] <- postseason.schedule[24,2] <- postseason.schedule[25,1] <-
      postseason.schedule[26,1] <- postseason.schedule[27,1] <- postseason.schedule[28,2] <-
      postseason.schedule[29,2] <- ds1.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.AL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.AL,4],
    
    postseason.schedule[23,2] <- postseason.schedule[24,2] <- postseason.schedule[25,1] <-
      postseason.schedule[26,1] <- postseason.schedule[27,1] <- postseason.schedule[28,2] <-
      postseason.schedule[29,2] <- ds2.winner.AL,
    
    postseason.schedule[23,1] <- postseason.schedule[24,1] <- postseason.schedule[25,2] <-
      postseason.schedule[26,2] <- postseason.schedule[27,2] <- postseason.schedule[28,1] <-
      postseason.schedule[29,1] <- ds2.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.NL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.NL,4],
    
    postseason.schedule[30,1] <- postseason.schedule[31,1] <- postseason.schedule[32,2] <-
      postseason.schedule[33,2] <- postseason.schedule[34,2] <- postseason.schedule[35,1] <-
      postseason.schedule[36,1] <- ds1.winner.NL,
    
    postseason.schedule[30,2] <- postseason.schedule[31,2] <- postseason.schedule[32,1] <-
      postseason.schedule[33,1] <- postseason.schedule[34,1] <- postseason.schedule[35,2] <-
      postseason.schedule[36,2] <- ds1.winner.NL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.NL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.NL,4],
    
    postseason.schedule[30,2] <- postseason.schedule[31,2] <- postseason.schedule[32,1] <-
      postseason.schedule[33,1] <- postseason.schedule[34,1] <- postseason.schedule[35,2] <-
      postseason.schedule[36,2] <- ds2.winner.NL,
    
    postseason.schedule[30,1] <- postseason.schedule[31,1] <- postseason.schedule[32,2] <-
      postseason.schedule[33,2] <- postseason.schedule[34,2] <- postseason.schedule[35,1] <-
      postseason.schedule[36,1] <- ds2.winner.NL
    
  )
  
  
  ## Championship series
  
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule[,12:23] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[12] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[16] <- "RSg_vteam"
  colnames(postseason.schedule)[17] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[20:21] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[22:23] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  postseason.schedule$pred.result[23:36] <- rbinom(
    length(postseason.schedule$pred.result[23:36]),
    1,
    prob = postseason.schedule$prob.home[23:36])
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  
  cs.winner.AL <- ifelse(sum(postseason.schedule[23:29,"pred.winner"]
                             ==as.character(ds1.winner.AL))>=4,
                         as.character(ds1.winner.AL),
                         as.character(ds2.winner.AL))
  
  cs.winner.NL <- ifelse(sum(postseason.schedule[30:36,"pred.winner"]
                             ==as.character(ds1.winner.NL))>=4,
                         as.character(ds1.winner.NL),
                         as.character(ds2.winner.NL))
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == cs.winner.AL,4] <
      postseason.seed[postseason.seed$Team == cs.winner.NL,4],
    
    postseason.schedule[37,1] <- postseason.schedule[38,1] <- postseason.schedule[39,2] <-
      postseason.schedule[40,2] <- postseason.schedule[41,2] <- postseason.schedule[42,1] <-
      postseason.schedule[43,1] <- cs.winner.AL,
    
    postseason.schedule[37,2] <- postseason.schedule[38,2] <- postseason.schedule[39,1] <-
      postseason.schedule[40,1] <- postseason.schedule[41,1] <- postseason.schedule[42,2] <-
      postseason.schedule[43,2] <- cs.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == cs.winner.AL,4] <
      postseason.seed[postseason.seed$Team == cs.winner.NL,4],
    
    postseason.schedule[37,2] <- postseason.schedule[38,2] <- postseason.schedule[39,1] <-
      postseason.schedule[40,1] <- postseason.schedule[41,1] <- postseason.schedule[42,2] <-
      postseason.schedule[43,2] <- cs.winner.NL,
    
    postseason.schedule[37,1] <- postseason.schedule[38,1] <- postseason.schedule[39,2] <-
      postseason.schedule[40,2] <- postseason.schedule[41,2] <- postseason.schedule[42,1] <-
      postseason.schedule[43,1] <- cs.winner.NL
    
  )
  
  
  ## World Series
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  
  postseason.schedule[,12:23] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[12] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[16] <- "RSg_vteam"
  colnames(postseason.schedule)[17] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[20:21] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[22:23] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  
  postseason.schedule$pred.result[37:43] <- rbinom(
    length(postseason.schedule$pred.result[37:43]),
    1,
    prob = postseason.schedule$prob.home[37:43])
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  ws.winner <- ifelse(sum(postseason.schedule[37:43,"pred.winner"]
                          ==as.character(cs.winner.AL))>=4,
                      as.character(cs.winner.AL),
                      as.character(cs.winner.NL))
  
  dswinners <- c(ds1.winner.AL, ds2.winner.AL, ds1.winner.NL, ds2.winner.NL)
  worldseries <- c(cs.winner.AL, cs.winner.NL)
  
  
  final.pred.standings$PS <- ifelse(as.character(final.pred.standings$Team) %in%
                                      as.character(postseason.seed$Team),
                                    1,
                                    0)
  final.pred.standings$DivWin <- ifelse(as.character(final.pred.standings$Team) %in% 
                                          as.character(div.winners.AL$V1),
                                        1,
                                        ifelse(as.character(final.pred.standings$Team) %in% 
                                                 as.character(div.winners.NL$V1),
                                               1,
                                               0
                                        )
  )
  
  
  final.pred.standings$DSWin <- ifelse(
    as.character(final.pred.standings$Team) %in% dswinners,
    1,
    0
  )
  
  
  final.pred.standings$CSWin <- ifelse(
    as.character(final.pred.standings$Team) %in% worldseries,
    1,
    0
  )
  
  
  final.pred.standings$WSWin <- ifelse(
    as.character(final.pred.standings$Team) %in% ws.winner,
    1,
    0
  )
  
  return(final.pred.standings)
}

#### (3) Full Regular Season + PostSeason ####

# This function simulates an entire 162 games regular season, sampling the
# starting pitchers at the beginning.
# This returns the REGULAR SEASON FINAL PREDICTED STANDINGS along with
# the WINNERS OF EACH POSTSEASON SERIES.

one.sim.fullandpost <- function(){
  
  dati2019.proj <- dati2019.proj[order(dati2019.proj$date),]
  rownames(dati2019.proj) <- NULL
  dati2019.proj[,21:24] <- NULL
  
  for(i in 1:30){
    startersequence[1:5,i] <- as.vector((sample(
      starters.st$Name[starters.st$Team==tvec[i]],
      size = 5,
      replace = F,
      prob = starters.st$prob[starters.st$Team==tvec[i]])))
    for(z in (6:162)){
      startersequence[z,i] <- (sample(
        vsetdiff(starters.st$Name[starters.st$Team==tvec[i]],
                 startersequence[(z-4):(z-1),i]),
        size = 1,
        replace = F,
        prob = vsetdiff(starters.st$prob[starters.st$Team==tvec[i]],
                        starters.st$prob[starters.st$Name %in%
                                           startersequence[(z-4):(z-1),i]])
        
      ))
    }
  }
  
  
  dati2019.proj$hstarter <- dati2019.proj$vstarter <- NA
  
  for(i in 1:30){
    for(z in 1:162){
      dati2019.proj$vstarter[
        dati2019.proj$vteam==tvec[i] & dati2019.proj$vteam_ng == z] <-
        startersequence[z,i]
      
      dati2019.proj$hstarter[
        dati2019.proj$hteam==tvec[i] & dati2019.proj$hteam_ng == z] <-
        startersequence[z,i]
      
    }
  }
  
  dati2019.proj <- merge(dati2019.proj, starters.st[,c(1,5)],
                         by.x = "hstarter", by.y = "Name")
  
  colnames(dati2019.proj)[21] <- c("ERA_hs")
  
  dati2019.proj <- merge(dati2019.proj, starters.st[,c(1,5)],
                         by.x = "vstarter", by.y = "Name")
  
  colnames(dati2019.proj)[22] <- c("ERA_vs")
  
  dati2019.proj$ERA_hs_scale <- as.numeric(scale(dati2019.proj$ERA_hs,
                                                 ERA19, F))
  dati2019.proj$ERA_vs_scale <- as.numeric(scale(dati2019.proj$ERA_vs,
                                                 ERA19, F))
  
  dati2019.proj$prob.home <- predict.glm(out.RSg.eras.erab,
                                         newdata = dati2019.proj,
                                         type = "response")
  dati2019.proj$pred.result <- rbinom(nrow(dati2019.proj),
                                      1,
                                      prob = dati2019.proj$prob.home)
  
  dati2019.proj$pred.winner <- with(dati2019.proj, ifelse(pred.result,
                                                          as.character(hteam),
                                                          as.character(vteam)))
  
  wins <- table(dati2019.proj$pred.winner)
  proj.wins2019 <- as.data.frame(cbind(wins, 162-wins))
  proj.wins2019 <- rownames_to_column(proj.wins2019)
  colnames(proj.wins2019) <- c("Team" ,"TOTWINS", "TOTLOSSES")
  
  proj.wins2019$Team <- as.factor(proj.wins2019$Team)
  proj.wins2019$Team <- factor(proj.wins2019$Team,
                               levels(proj.wins2019$Team)[ord.3])
  
  proj.wins2019 <- merge(proj.wins2019, teams2018[,c(1,3,5)],
                         by.x = "Team", by.y = "teamID")
  
  proj.wins2019$DIV <- paste(proj.wins2019$lgID, proj.wins2019$divID, sep = " ")
  proj.wins2019$DIV <- as.factor(proj.wins2019$DIV)
  levels(proj.wins2019$DIV)
  proj.wins2019$DIV <- factor(proj.wins2019$DIV,
                              levels(proj.wins2019$DIV)[ord.2])
  proj.wins2019[,5] <- NULL
  proj.wins2019 <- proj.wins2019[order(proj.wins2019$DIV),]
  rownames(proj.wins2019) <- NULL
  
  
  div.winners.AL <- div.winners.NL <- div.wins.AL <- div.wins.NL <- NULL
  
  max.wins.AL <- max(proj.wins2019[proj.wins2019$lgID == "AL", 2])
  
  mw.team.AL <- as.character(proj.wins2019[proj.wins2019$lgID == "AL" &
                                             proj.wins2019$TOTWINS == max.wins.AL,
                                           1])
  
  mw.team.AL <- ifelse(length(mw.team.AL) == 1,
                       mw.team.AL,
                       sample(x = mw.team.AL, size = 1))
  
  max.wins.NL <- max(proj.wins2019[proj.wins2019$lgID == "NL", 2])
  
  mw.team.NL <- as.character(proj.wins2019[proj.wins2019$lgID == "NL" &
                                             proj.wins2019$TOTWINS == max.wins.NL,
                                           1])
  
  mw.team.NL <- ifelse(length(mw.team.NL) == 1,
                       mw.team.NL,
                       sample(x = mw.team.NL, size = 1))
  for(i in 1:3){
    
    div.wins.AL[i] <- max(proj.wins2019[proj.wins2019$DIV==divisions[i],2])
  }
  
  for(i in 4:6){
    
    div.wins.NL[i-3] <- max(proj.wins2019[proj.wins2019$DIV==divisions[i],2])
  }
  
  div.winner.ALE <- as.character(
    proj.wins2019[proj.wins2019$DIV == "AL E" &
                    proj.wins2019$TOTWINS == div.wins.AL[1],1]
  )
  
  div.winner.ALE <- ifelse(length(div.winner.ALE) == 1,
                           div.winner.ALE,
                           sample(x = div.winner.ALE, size = 1))
  
  
  div.winner.ALC <- as.character(
    proj.wins2019[proj.wins2019$DIV == "AL C" &
                    proj.wins2019$TOTWINS == div.wins.AL[2],1]
  )
  
  div.winner.ALC <- ifelse(length(div.winner.ALC) == 1,
                           div.winner.ALC,
                           sample(x = div.winner.ALC, size = 1))
  
  div.winner.ALW <- as.character(
    proj.wins2019[proj.wins2019$DIV == "AL W" &
                    proj.wins2019$TOTWINS == div.wins.AL[3],1]
  )
  
  div.winner.ALW <- ifelse(length(div.winner.ALW) == 1,
                           div.winner.ALW,
                           sample(x = div.winner.ALW, size = 1))
  
  
  div.winner.NLE <- as.character(
    proj.wins2019[proj.wins2019$DIV == "NL E" &
                    proj.wins2019$TOTWINS == div.wins.NL[1],1]
  )
  
  div.winner.NLE <- ifelse(length(div.winner.NLE) == 1,
                           div.winner.NLE,
                           sample(x = div.winner.NLE, size = 1))
  
  div.winner.NLC <- as.character(
    proj.wins2019[proj.wins2019$DIV == "NL C" &
                    proj.wins2019$TOTWINS == div.wins.NL[2],1]
  )
  
  div.winner.NLC  <- ifelse(length(div.winner.NLC) == 1,
                            div.winner.NLC ,
                            sample(x = div.winner.NLC , size = 1))
  
  div.winner.NLW <- as.character(
    proj.wins2019[proj.wins2019$DIV == "NL W" &
                    proj.wins2019$TOTWINS == div.wins.NL[3],1]
  )
  
  div.winner.NLW <- ifelse(length(div.winner.NLW) == 1,
                           div.winner.NLW,
                           sample(x = div.winner.NLW, size = 1))
  
  div.winners.AL <- as.data.frame(cbind(
    c(div.winner.ALE, div.winner.ALC, div.winner.ALW),
    c(div.wins.AL)))
  
  div.winners.AL[,1] <- as.character(div.winners.AL[,1])
  div.winners.AL[,2] <- as.numeric(as.numeric_version(div.winners.AL[,2]))
  
  div.winners.AL <- div.winners.AL[order(div.winners.AL[,2], decreasing = T),]
  rownames(div.winners.AL) <- NULL
  
  div.winners.NL <- as.data.frame(cbind(
    c(div.winner.NLE, div.winner.NLC, div.winner.NLW),
    c(div.wins.NL)))
  
  div.winners.NL[,1] <- as.character(div.winners.NL[,1])
  div.winners.NL[,2] <- as.numeric(as.numeric_version(div.winners.NL[,2]))
  
  div.winners.NL <- div.winners.NL[order(div.winners.NL[,2], decreasing = T),]
  rownames(div.winners.NL) <- NULL
  
  
  IDwAL <- as.numeric(c(rownames(proj.wins2019
                                 [proj.wins2019$Team==div.winner.ALE,]),
                        rownames(proj.wins2019[
                          proj.wins2019$Team==div.winner.ALC,]),
                        rownames(proj.wins2019[
                          proj.wins2019$Team==div.winner.ALW,]),
                        rownames(proj.wins2019[
                          proj.wins2019$lgID == "NL",])
  ))
  
  IDwNL <- as.numeric(c(rownames(proj.wins2019[
    proj.wins2019$Team==div.winner.NLE,]),
    rownames(proj.wins2019[
      proj.wins2019$Team==div.winner.NLC,]),
    rownames(proj.wins2019[
      proj.wins2019$Team==div.winner.NLW,]),
    rownames(proj.wins2019[
      proj.wins2019$lgID == "AL",])
  ))
  
  ### WILD CARD AL
  
  wins.wc1.AL <- max(proj.wins2019[-IDwAL, 2])
  
  wc1.AL <- as.character(
    proj.wins2019[-IDwAL,1][proj.wins2019[-IDwAL,2]==wins.wc1.AL]
  )
  
  wc1.AL <- ifelse(length(wc1.AL) == 1,
                   wc1.AL,
                   sample(x = wc1.AL, size = 1))
  
  IDwAL.2 <- as.numeric(c(rownames(proj.wins2019
                                   [proj.wins2019$Team==div.winner.ALE,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.ALC,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.ALW,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==wc1.AL,]),
                          rownames(proj.wins2019[
                            proj.wins2019$lgID == "NL",])
  ))
  
  wins.wc2.AL <- max(proj.wins2019[-IDwAL.2, 2])
  
  wc2.AL <- as.character(
    proj.wins2019[-IDwAL.2,1][proj.wins2019[-IDwAL.2,2]==wins.wc2.AL]
  )
  
  wc2.AL <- ifelse(length(wc2.AL) == 1,
                   wc2.AL,
                   sample(x = wc2.AL, size = 1))
  
  wc.teams.AL <- unname(as.data.frame(rbind(
    cbind(wc1.AL, wins.wc1.AL),
    cbind(wc2.AL, wins.wc2.AL))))
  colnames(wc.teams.AL) <- c("V1", "V2")
  wc.teams.AL[,1] <- as.character(wc.teams.AL[,1])
  wc.teams.AL[,2] <- as.numeric(as.numeric_version(wc.teams.AL[,2]))
  
  
  ### WILD CARD NL
  
  wins.wc1.NL <- max(proj.wins2019[-IDwNL, 2])
  
  wc1.NL <- as.character(
    proj.wins2019[-IDwNL,1][proj.wins2019[-IDwNL,2]==wins.wc1.NL]
  )
  
  wc1.NL <- ifelse(length(wc1.NL) == 1,
                   wc1.NL,
                   sample(x = wc1.NL, size = 1))
  
  IDwNL.2 <- as.numeric(c(rownames(proj.wins2019
                                   [proj.wins2019$Team==div.winner.NLE,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.NLC,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==div.winner.NLW,]),
                          rownames(proj.wins2019[
                            proj.wins2019$Team==wc1.NL,]),
                          rownames(proj.wins2019[
                            proj.wins2019$lgID == "AL",])
  ))
  
  wins.wc2.NL <- max(proj.wins2019[-IDwNL.2, 2])
  
  wc2.NL <- as.character(
    proj.wins2019[-IDwNL.2,1][proj.wins2019[-IDwNL.2,2]==wins.wc2.NL]
  )
  
  wc2.NL <- ifelse(length(wc2.NL) == 1,
                   wc2.NL,
                   sample(x = wc2.NL, size = 1))
  
  wc.teams.NL <- unname(as.data.frame(rbind(
    cbind(wc1.NL, wins.wc1.NL),
    cbind(wc2.NL, wins.wc2.NL))))
  colnames(wc.teams.NL) <- c("V1", "V2")
  wc.teams.NL[,1] <- as.character(wc.teams.NL[,1])
  wc.teams.NL[,2] <- as.numeric(as.numeric_version(wc.teams.NL[,2]))
  
  ##
  
  
  
  postseason.seed <- as.data.frame(
    rbind(
      
      div.winners.AL,
      wc.teams.AL,
      div.winners.NL,
      wc.teams.NL
      
    )
    
  )
  
  
  postseason.seed <- cbind(postseason.seed, c(rep("AL", 5), rep("NL", 5)))
  colnames(postseason.seed) <- c("Team", "Wins", "League")
  
  postseason.seed <- postseason.seed[order(postseason.seed$Wins, decreasing = T),]
  rownames(postseason.seed) <- NULL
  postseason.seed <- cbind(postseason.seed, 1:10)
  colnames(postseason.seed) <- c("Team", "Wins", "League", "Seed")
  
  postseason.schedule <- as.data.frame(
    rbind(
      
      as.data.frame(unname(cbind(as.character(wc1.AL), as.character(wc2.AL)))),
      
      as.data.frame(unname(cbind(as.character(div.winners.AL[1,1]), "TBD"))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[1,1]), "TBD"))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.AL[1,1])))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.AL[1,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[1,1]), "TBD"))),
      
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[3,1]), as.character(div.winners.AL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[3,1]), as.character(div.winners.AL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.AL[2,1]), as.character(div.winners.AL[3,1])))),
      
      as.data.frame(unname(cbind(as.character(wc1.NL), as.character(wc2.NL)))),
      
      as.data.frame(unname(cbind(as.character(div.winners.NL[1,1]), "TBD"))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[1,1]), "TBD"))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.NL[1,1])))),
      as.data.frame(unname(cbind("TBD", as.character(div.winners.NL[1,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[1,1]), "TBD"))),
      
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[3,1]), as.character(div.winners.NL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[3,1]), as.character(div.winners.NL[2,1])))),
      as.data.frame(unname(cbind(as.character(div.winners.NL[2,1]), as.character(div.winners.NL[3,1])))),
      
      as.data.frame(cbind(rep("TBD", 21), rep("TBD", 21)))
      
    )
  )
  
  postseason.schedule <- cbind(postseason.schedule, c("AL WC", rep ("AL DS 1", 5),
                                                      rep ("AL DS 2", 5), "NL WC",
                                                      rep ("NL DS 1", 5), rep ("NL DS 2", 5),
                                                      rep("AL CS", 7), rep("NL CS", 7),
                                                      rep("WS", 7)))
  
  postseason.schedule <- cbind(1:43, postseason.schedule)
  
  colnames(postseason.schedule) <- c("ID", "hteam", "vteam", "series")
  postseason.schedule$hteam <- as.character(postseason.schedule$hteam)
  postseason.schedule$vteam <- as.character(postseason.schedule$vteam)
  
  postseason.schedule$hstarter <- postseason.schedule$vstarter <- 
    postseason.schedule$hteam_ng <- postseason.schedule$vteam_ng <- NA
  
  tvec.ps <- as.vector(postseason.seed$Team)
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],2][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 6][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 5][z] <- z
      )
    }
  }
  
  for(i in 1:30){
    for(z in seq(0, 165, 4)){
      startersequence[((z+1):(z+4)),i] <- as.vector((sample(
        starters.st$Name[starters.st$Team==tvec[i]],
        size = 4,
        replace = F,
        prob = starters.st$prob[starters.st$Team==tvec[i]])))
    }
  }
  
  n <- sample(1:142, 1)
  
  startersequence.ps <- startersequence[(n):(n+19),]
  rownames(startersequence.ps) <- NULL
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[9] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[10] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- "RSg_vteam"
  colnames(postseason.schedule)[14] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[17:18] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[19:20] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  
  postseason.schedule$pred.result <- rbinom(nrow(postseason.schedule),
                                            1,
                                            prob = postseason.schedule$prob.home)
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result, as.character(hteam),
                                     as.character(vteam)))
  
  wcwinner.AL <- as.character(postseason.schedule[1,"pred.winner"])
  
  postseason.schedule[2,2] <- postseason.schedule[3,2] <- postseason.schedule[4,1] <-
    postseason.schedule[5,1] <- postseason.schedule[6,2] <- wcwinner.AL
  
  wcwinner.NL <-  as.character(postseason.schedule[12,"pred.winner"])
  
  postseason.schedule[13,2] <- postseason.schedule[14,2] <- postseason.schedule[15,1] <-
    postseason.schedule[16,1] <- postseason.schedule[17,2] <- wcwinner.NL
  
  
  ds2.winner.AL <- ifelse(sum(postseason.schedule[7:11,"pred.winner"]
                              ==as.character(div.winners.AL[2,1]))>=3,
                          as.character(div.winners.AL[2,1]),
                          as.character(div.winners.AL[3,1]))
  
  ds2.winner.NL <- ifelse(sum(postseason.schedule[18:22,"pred.winner"]
                              ==as.character(div.winners.NL[2,1]))>=3,
                          as.character(div.winners.NL[2,1]),
                          as.character(div.winners.NL[3,1]))
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])
        ==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule[,9:20] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[12] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[16] <- "RSg_vteam"
  colnames(postseason.schedule)[17] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[20:21] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[22:23] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  postseason.schedule$pred.result[c(2:6,13:17)] <- rbinom(
    length(postseason.schedule$pred.result[c(2:6,13:17)]),
    1,
    prob = postseason.schedule$prob.home[c(2:6,13:17)])
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  ds1.winner.AL <- ifelse(sum(postseason.schedule[2:6,"pred.winner"]
                              ==as.character(div.winners.AL[1,1]))>=3,
                          as.character(div.winners.AL[1,1]),
                          as.character(wcwinner.AL))
  
  ds1.winner.NL <- ifelse(sum(postseason.schedule[13:17,"pred.winner"]
                              ==as.character(div.winners.NL[1,1]))>=3,
                          as.character(div.winners.NL[1,1]),
                          as.character(wcwinner.NL))
  
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.AL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.AL,4],
    
    postseason.schedule[23,1] <- postseason.schedule[24,1] <- postseason.schedule[25,2] <-
      postseason.schedule[26,2] <- postseason.schedule[27,2] <- postseason.schedule[28,1] <-
      postseason.schedule[29,1] <- ds1.winner.AL,
    
    postseason.schedule[23,2] <- postseason.schedule[24,2] <- postseason.schedule[25,1] <-
      postseason.schedule[26,1] <- postseason.schedule[27,1] <- postseason.schedule[28,2] <-
      postseason.schedule[29,2] <- ds1.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.AL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.AL,4],
    
    postseason.schedule[23,2] <- postseason.schedule[24,2] <- postseason.schedule[25,1] <-
      postseason.schedule[26,1] <- postseason.schedule[27,1] <- postseason.schedule[28,2] <-
      postseason.schedule[29,2] <- ds2.winner.AL,
    
    postseason.schedule[23,1] <- postseason.schedule[24,1] <- postseason.schedule[25,2] <-
      postseason.schedule[26,2] <- postseason.schedule[27,2] <- postseason.schedule[28,1] <-
      postseason.schedule[29,1] <- ds2.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.NL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.NL,4],
    
    postseason.schedule[30,1] <- postseason.schedule[31,1] <- postseason.schedule[32,2] <-
      postseason.schedule[33,2] <- postseason.schedule[34,2] <- postseason.schedule[35,1] <-
      postseason.schedule[36,1] <- ds1.winner.NL,
    
    postseason.schedule[30,2] <- postseason.schedule[31,2] <- postseason.schedule[32,1] <-
      postseason.schedule[33,1] <- postseason.schedule[34,1] <- postseason.schedule[35,2] <-
      postseason.schedule[36,2] <- ds1.winner.NL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == ds1.winner.NL,4] <
      postseason.seed[postseason.seed$Team == ds2.winner.NL,4],
    
    postseason.schedule[30,2] <- postseason.schedule[31,2] <- postseason.schedule[32,1] <-
      postseason.schedule[33,1] <- postseason.schedule[34,1] <- postseason.schedule[35,2] <-
      postseason.schedule[36,2] <- ds2.winner.NL,
    
    postseason.schedule[30,1] <- postseason.schedule[31,1] <- postseason.schedule[32,2] <-
      postseason.schedule[33,2] <- postseason.schedule[34,2] <- postseason.schedule[35,1] <-
      postseason.schedule[36,1] <- ds2.winner.NL
    
  )
  
  
  ## Championship series
  
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])
        ==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z]
        <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z]
        <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  postseason.schedule[,12:23] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[12] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[16] <- "RSg_vteam"
  colnames(postseason.schedule)[17] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[20:21] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[22:23] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  postseason.schedule$pred.result[23:36] <- rbinom(
    length(postseason.schedule$pred.result[23:36]),
    1,
    prob = postseason.schedule$prob.home[23:36])
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  
  cs.winner.AL <- ifelse(sum(postseason.schedule[23:29,"pred.winner"]
                             ==as.character(ds1.winner.AL))>=4,
                         as.character(ds1.winner.AL),
                         as.character(ds2.winner.AL))
  
  cs.winner.NL <- ifelse(sum(postseason.schedule[30:36,"pred.winner"]
                             ==as.character(ds1.winner.NL))>=4,
                         as.character(ds1.winner.NL),
                         as.character(ds2.winner.NL))
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == cs.winner.AL,4] <
      postseason.seed[postseason.seed$Team == cs.winner.NL,4],
    
    postseason.schedule[37,1] <- postseason.schedule[38,1] <- postseason.schedule[39,2] <-
      postseason.schedule[40,2] <- postseason.schedule[41,2] <- postseason.schedule[42,1] <-
      postseason.schedule[43,1] <- cs.winner.AL,
    
    postseason.schedule[37,2] <- postseason.schedule[38,2] <- postseason.schedule[39,1] <-
      postseason.schedule[40,1] <- postseason.schedule[41,1] <- postseason.schedule[42,2] <-
      postseason.schedule[43,2] <- cs.winner.AL
    
  )
  
  ifelse(
    
    postseason.seed[postseason.seed$Team == cs.winner.AL,4] <
      postseason.seed[postseason.seed$Team == cs.winner.NL,4],
    
    postseason.schedule[37,2] <- postseason.schedule[38,2] <- postseason.schedule[39,1] <-
      postseason.schedule[40,1] <- postseason.schedule[41,1] <- postseason.schedule[42,2] <-
      postseason.schedule[43,2] <- cs.winner.NL,
    
    postseason.schedule[37,1] <- postseason.schedule[38,1] <- postseason.schedule[39,2] <-
      postseason.schedule[40,2] <- postseason.schedule[41,2] <- postseason.schedule[42,1] <-
      postseason.schedule[43,1] <- cs.winner.NL
    
  )
  
  
  ## World Series
  
  for(i in 1:10){
    for(z in 1:21){
      
      ifelse(
        (postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                               postseason.schedule$vteam==tvec.ps[i],1][z])==tvec.ps[i],
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 8][z] <- z,
        postseason.schedule[postseason.schedule$hteam==tvec.ps[i] |
                              postseason.schedule$vteam==tvec.ps[i], 7][z] <- z
      )
    }
  }
  
  
  for(i in 1:30){
    for(z in 1:20){
      postseason.schedule$vstarter[
        postseason.schedule$vteam==tvec[i] & postseason.schedule$vteam_ng == z] <-
        startersequence.ps[z,i]
      
      postseason.schedule$hstarter[
        postseason.schedule$hteam==tvec[i] & postseason.schedule$hteam_ng == z] <-
        startersequence.ps[z,i]
      
    }
  }
  
  
  postseason.schedule[,12:23] <- NULL
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "hstarter", by.y = "Name",
                               all.x = T)
  colnames(postseason.schedule)[12] <- c("ERA_hs")
  
  postseason.schedule <- merge(postseason.schedule, starters.st[,c(1,5)],
                               by.x = "vstarter", by.y = "Name",
                               all.x = T)
  
  colnames(postseason.schedule)[13] <- c("ERA_vs")
  
  postseason.schedule$ERA_hs_scale <- as.numeric(scale(postseason.schedule$ERA_hs,
                                                       ERA19, F))
  postseason.schedule$ERA_vs_scale <- as.numeric(scale(postseason.schedule$ERA_vs,
                                                       ERA19, F))
  
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  postseason.schedule <- merge(postseason.schedule, standings19[,c(1,20)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  
  colnames(postseason.schedule)[16] <- "RSg_vteam"
  colnames(postseason.schedule)[17] <- "RSg_hteam"
  postseason.schedule$RSg_hteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_hteam,RSg19_fs, F))
  postseason.schedule$RSg_vteam_scale <- 
    as.numeric(scale(postseason.schedule$RSg_vteam,RSg19_fs, F))
  
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "vteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[20:21] <- c("ERA_bullpen_v","ERA_bullpen_v_scale")
  postseason.schedule <- merge(postseason.schedule, relief2019_tot[,c(1,3,6)],
                               by.x = "hteam", by.y = "Team",
                               all.x = T)
  colnames(postseason.schedule)[22:23] <- c("ERA_bullpen_h","ERA_bullpen_h_scale")
  
  
  postseason.schedule <- postseason.schedule[order(postseason.schedule$ID),]
  rownames(postseason.schedule) <- NULL
  
  
  postseason.schedule$prob.home <- predict.glm(out.RSg.eras.erab,
                                               newdata = postseason.schedule,
                                               type = "response")
  
  postseason.schedule$pred.result[37:43] <- rbinom(
    length(postseason.schedule$pred.result[37:43]),
    1,
    prob = postseason.schedule$prob.home[37:43])
  
  postseason.schedule$pred.winner <-
    with(postseason.schedule, ifelse(pred.result,
                                     as.character(hteam),
                                     as.character(vteam)))
  
  ws.winner <- ifelse(sum(postseason.schedule[37:43,"pred.winner"]
                          ==as.character(cs.winner.AL))>=4,
                      as.character(cs.winner.AL),
                      as.character(cs.winner.NL))
  
  dswinners <- c(ds1.winner.AL, ds2.winner.AL, ds1.winner.NL, ds2.winner.NL)
  worldseries <- c(cs.winner.AL, cs.winner.NL)
  
  
  proj.wins2019$PS <- ifelse(as.character(proj.wins2019$Team) %in%
                               as.character(postseason.seed$Team),
                             1,
                             0)
  proj.wins2019$DivWin <- ifelse(as.character(proj.wins2019$Team) %in% 
                                   as.character(div.winners.AL$V1),
                                 1,
                                 ifelse(as.character(proj.wins2019$Team) %in% 
                                          as.character(div.winners.NL$V1),
                                        1,
                                        0
                                 )
  )
  
  proj.wins2019$DSWin <- ifelse(
    as.character(proj.wins2019$Team) %in% dswinners,
    1,
    0
  )
  
  proj.wins2019$CSWin <- ifelse(
    as.character(proj.wins2019$Team) %in% worldseries,
    1,
    0
  )
  
  proj.wins2019$WSWin <- ifelse(
    as.character(proj.wins2019$Team) %in% ws.winner,
    1,
    0
  )
  return(proj.wins2019)
}

# Simulation Summary

for(i in seq(1,30,5)){
  final.pred.standings[i:(i+4),] <- final.pred.standings[i:(i+4),][order(final.pred.standings[i:(i+4),"TOTWINS"],
                                                                         decreasing = T),]
}

final.pred.standings

fps <- final.pred.standings[,1:3]
fps$Wpct <- round(fps$TOTWINS/(fps$TOTWINS+fps$TOTLOSSES),3)

htmlTable(fps[1:15,])
htmlTable(fps[16:30,])


