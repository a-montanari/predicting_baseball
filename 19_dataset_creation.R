#### Creation of the 2019 dataset for predicting the new season results ####

# Other then the same procedures for creating the 2015-2018 dataset, this contains
# the use of projected data and the sampling of the starting pitchers sequence.

#### 2019 schedule ####

schedule2019 <- read.csv("/data/2019SKED.TXT", header = F)
schedule2019[,1] <- as.factor(schedule2019[,1])
schedule2019[,1] <- as.Date(schedule2019[,1], format = "%Y%m%d")

schedule2019 <- schedule2019[,c(1,3,4,6,7,9,10)]
colnames(schedule2019) <- c("date", "day", "vteam", "vteam_ng", "hteam", "hteam_ng", "time")

standings19 <- read.delim("/data/stands19.txt", header=FALSE)
colnames(standings19) <- c("Team","G", "W", "L", "Wpct", "RD", "RSg", "RAg",
                           "G_Ros", "W_Ros", "L_Ros", "Wpct_Ros", "RD_Ros",
                           "RSg_Ros", "RAg_Ros",
                           "W_fs", "L_fs", "Wpct_fs", "RD_fs", "RSg_fs", "RAg_fs")
levels(standings19$Team)
levels(standings19$Team) <- c("ANA", "HOU", "OAK", "TOR", "ATL",
                              "MIL", "SLN", "CHN", "ARI", "LAN",
                              "SFN", "CLE", "SEA",  "MIA", "NYN",
                              "WAS", "BAL", "SDN", "PHI", "PIT",
                              "TEX", "TBA", "BOS", "CIN", "COL",
                              "KCA", "DET", "MIN", "CHA", "NYA")

standings19$proj_TOT_RDg <- standings19$RSg_fs - standings19$RAg_Ros
standings19$actual_RDg <- standings19$RSg - standings19$RAg
standings19$missing_games_RDg <- standings19$RSg_Ros - standings19$RAg_Ros
RSg19_fs <- mean(standings19$RSg_fs)
RSg19_ros <- mean(standings19$RSg_Ros)

dati2019.proj <- merge(schedule2019, standings19[,c(1,20)],
                       by.x = "hteam", by.y = "Team")
dati2019.proj <- merge(dati2019.proj, standings19[,c(1,20)],
                       by.x = "vteam", by.y = "Team")

colnames(dati2019.proj)[9] <- "RSg_vteam"
colnames(dati2019.proj)[8] <- "RSg_hteam"

dati2019.proj$RSg_hteam_scale <- as.numeric(scale(dati2019.proj$RSg_hteam,
                                                  RSg19_fs, F))

dati2019.proj$RSg_vteam_scale <- as.numeric(scale(dati2019.proj$RSg_vteam,
                                                  RSg19_fs, F))

starters2019 <- read.csv("/data/starters2019.csv", header = T)

levels(starters2019$Team) <- c("ANA", "HOU", "OAK", "TOR", "ATL",
                               "MIL", "SLN", "CHN", "ARI", "LAN",
                               "SFN", "CLE", "SEA",  "MIA", "NYN",
                               "WAS", "BAL", "SDN", "PHI", "PIT",
                               "TEX", "TBA", "BOS", "CIN", "COL",
                               "KCA", "DET", "MIN", "CHA", "NYA")

starters.st <- read.csv("/data/steamer2019.csv", header = T)
starters.st <- starters.st[starters.st$GS>0,]
rownames(starters.st) <- NULL
starters.st$Name <- as.character(starters.st$Name)

levels(starters.st$Team) <- c("", "ANA", "HOU", "OAK", "TOR", "ATL",
                              "MIL", "SLN", "CHN", "ARI", "LAN",
                              "SFN", "CLE", "SEA",  "MIA", "NYN",
                              "WAS", "BAL", "SDN", "PHI", "PIT",
                              "TEX", "TBA", "BOS", "CIN", "COL",
                              "KCA", "DET", "MIN", "CHA", "NYA")

starters.st.ros <- read.csv("/data/steamerros2019.csv", header = T)
starters.st.ros <- starters.st.ros[starters.st.ros$GS>0,]
rownames(starters.st.ros) <- NULL
starters.st.ros$Name <- as.character(starters.st.ros$Name)

levels(starters.st.ros$Team) <- c("", "ANA", "HOU", "OAK", "TOR", "ATL",
                                  "MIL", "SLN", "CHN", "ARI", "LAN",
                                  "SFN", "CLE", "SEA",  "MIA", "NYN",
                                  "WAS", "BAL", "SDN", "PHI", "PIT",
                                  "TEX", "TBA", "BOS", "CIN", "COL",
                                  "KCA", "DET", "MIN", "CHA", "NYA")

starters.st$prob <- starters.st$GS/162

bos.start <- as.vector(sample(starters.st$Name[starters.st$Team=="BOS"], size = 162, replace = T,
                              prob = starters.st$prob[starters.st$Team=="BOS"]))
cbind(starters.st$Name[starters.st$Team=="BOS"], starters.st$prob[starters.st$Team=="BOS"])
table(bos.start)


starters.st.ros <- merge(starters.st.ros, standings19[,1:2],
                         by.x = "Team", by.y = "Team")
colnames(starters.st.ros)[22] <- "GamesLeft"
starters.st.ros$GamesLeft <- 162-starters.st.ros$GamesLeft
starters.st.ros$prob <- starters.st.ros$GS/starters.st.ros$GamesLeft

startersequence <- as.data.frame(matrix(ncol = 30))
colnames(startersequence) <- tvec

startersequence[1:30, "BOS"]
starters.st$Name[starters.st$Team=="BOS"]
1:length(starters.st$Name[starters.st$Team=="BOS"])
starters.st$prob[starters.st$Team=="BOS"]

require(vecsets)

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


#### 2019 Projections with final projected data ####

dati2019.proj$hstarter <- dati2019.proj$vstarter <- NA

for(i in 1:30){
  for(z in 1:162){
    dati2019.proj$vstarter[
      dati2019.proj$vteam==tvec[i] & dati2019.proj$vteam_ng == z] <- startersequence[z,i]
    
    dati2019.proj$hstarter[
      dati2019.proj$hteam==tvec[i] & dati2019.proj$hteam_ng == z] <- startersequence[z,i]
    
  }
}

relief2019_ros <- read_excel("/data/relief2019.xlsx", sheet = "ROS",
                             col_types = c("text",
                                           "numeric", "numeric", "numeric", 
                                           "numeric"))
relief2019_tot <- read_excel("/data/relief2019.xlsx", sheet = "TOT",
                             col_types = c("text",
                                           "numeric", "numeric", "numeric", 
                                           "numeric"))

teamsvec <- c("ANA", "HOU", "OAK", "TOR", "ATL",
              "MIL", "SLN", "CHN", "ARI", "LAN",
              "SFN", "CLE", "SEA",  "MIA", "NYN",
              "WAS", "BAL", "SDN", "PHI", "PIT",
              "TEX", "TBA", "BOS", "CIN", "COL",
              "KCA", "DET", "MIN", "CHA", "NYA")

relief2019_ros$Team <- as.factor(relief2019_ros$Team)
relief2019_tot$Team <- as.factor(relief2019_tot$Team)
levels(relief2019_ros$Team) <- teamsvec
levels(relief2019_tot$Team) <- teamsvec

ERA19.b <- weighted.mean(relief2019_tot$ERA, relief2019_tot$IP)
relief2019_tot$ERA_scale <- as.numeric(scale(relief2019_tot$ERA, ERA19.b, F))

ERA19.bros <- weighted.mean(relief2019_ros$ERA, relief2019_ros$IP)
relief2019_ros$ERA_scale <- as.numeric(scale(relief2019_ros$ERA, ERA19.bros, F))


dati2019.proj <- merge(dati2019.proj, starters.st[,c(1,5)],
                       by.x = "hstarter", by.y = "Name")
colnames(dati2019.proj)[14] <- c("ERA_hs")

dati2019.proj <- merge(dati2019.proj, starters.st[,c(1,5)],
                       by.x = "vstarter", by.y = "Name")

colnames(dati2019.proj)[15] <- c("ERA_vs")

ERA19 <- weighted.mean(starters.st$ERA, starters.st$IP)

dati2019.proj$ERA_hs_scale <- as.numeric(scale(dati2019.proj$ERA_hs, ERA19, F))
dati2019.proj$ERA_vs_scale <- as.numeric(scale(dati2019.proj$ERA_vs, ERA19, F))


dati2019.proj <- merge(dati2019.proj, relief2019_tot[,c(1,3,6)],
                       by.x = "hteam", by.y = "Team")
colnames(dati2019.proj)[18:19] <- c("ERA_bullpen_h", "ERA_bullpen_h_scale")

dati2019.proj <- merge(dati2019.proj, relief2019_tot[,c(1,3,6)],
                       by.x = "vteam", by.y = "Team")
colnames(dati2019.proj)[20:21] <- c("ERA_bullpen_v", "ERA_bullpen_v_scale")


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

#### 2018 results validation ####

dati2018 <- dati2018[order(dati2018$date),]

dati2018$prob.home.ERA <- predict.glm(out.scale.era10,
                                      newdata = dati2018,
                                      type = "response")

dati2018$prob.home.FIP <- predict.glm(out.scale.fip10,
                                      newdata = dati2018,
                                      type = "response")

dati2018$pred.result.FIP <- rbinom(nrow(dati2018),
                                   1,
                                   prob = dati2018$prob.home.FIP)

dati2018$pred.result.ERA <- rbinom(nrow(dati2018),
                                   1,
                                   prob = dati2018$prob.home.ERA)

dati2018$pred.winner.ERA <- with(dati2018, ifelse(pred.result.ERA,
                                                  as.character(hteam),
                                                  as.character(vteam)))
dati2018$pred.winner.FIP<- with(dati2018, ifelse(pred.result.FIP,
                                                 as.character(hteam),
                                                 as.character(vteam)))

wins.ERA <- table(dati2018$pred.winner.ERA)
wins.FIP <- table(dati2018$pred.winner.FIP)
wins.2018 <- as.data.frame(rbind(wins.ERA, wins.FIP, wins2018))
rownames(wins.2018) <- c("Pred. ERA", "Pred. FIP", "Actual wins")


### 2019 Projections using the projections on the remaining games (as of June 4, 2019) ####

dati2019.ros <- merge(schedule2019, standings19[,c(1,14)], by.x = "vteam", by.y = "Team")

dati2019.ros <- merge(dati2019.ros, standings19[,c(1,14)], by.x = "hteam", by.y = "Team")

colnames(dati2019.ros)[8] <- "RSg_vteam"
colnames(dati2019.ros)[9] <- "RSg_hteam"

dati2019.ros$RSg_hteam_scale <- as.numeric(scale(dati2019.ros$RSg_hteam, RSg19_ros, F))

dati2019.ros$RSg_vteam_scale <- as.numeric(scale(dati2019.ros$RSg_vteam, RSg19_ros, F))


dati2019.ros$hstarter <- dati2019.ros$vstarter <- NA


dati2019.ros$date[c(1445, 1454, 337, 835, 415, 2235, 269, 785, 803, 1539)] <- c("2019-08-12", "2019-08-03", "2019-06-08",
                                                                                "2019-08-06", "2019-07-03", "2019-06-08", "2019-07-13",
                                                                                "2019-08-26", "2019-07-15", "2019-08-05")

dati2019.ros <- dati2019.ros[dati2019.ros$date>"2019-06-03",]

dati2019.ros <- merge(dati2019.ros, standings19[,1:2],
                      by.x = "hteam", by.y = "Team")
dati2019.ros <- merge(dati2019.ros, standings19[,1:2],
                      by.x = "vteam", by.y = "Team")

colnames(dati2019.ros)[14:15] <- c("G_h", "G_v")

dati2019.ros <- dati2019.ros[order(dati2019.ros$date),]
rownames(dati2019.ros) <- NULL

#### Check whether all postponed matches have been counted

a <- 1:30
names(a) <- standings19$Team
for(i in 1:30){
  a[i] <- nrow(dati2019.ros[dati2019.ros$hteam==standings19$Team[i],])+
    nrow(dati2019.ros[dati2019.ros$vteam==standings19$Team[i],])
}

a

b <- standings19$G_Ros
names(b) <- standings19$Team
sum(a == b)

tvec2 <- as.vector(standings19$Team)

##### Sampling the starter sequence for each team

startersequenceros <- as.data.frame(matrix(ncol = 30))
colnames(startersequenceros) <- tvec


for(i in 1:30){
  startersequenceros[1:5,i] <- as.vector((sample(
    starters.st.ros$Name[starters.st.ros$Team==tvec[i]],
    size = 5,
    replace = F,
    prob = starters.st.ros$prob[starters.st.ros$Team==tvec[i]])))
  for(z in (6:max(starters.st.ros$GamesLeft))){
    startersequenceros[z,i] <- (sample(
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


dati2019.ros$hteam_ng_ros <- dati2019.ros$G_h
dati2019.ros$vteam_ng_ros <- dati2019.ros$G_v

dati2019.ros$hteam_gleft <- 162-dati2019.ros$G_h
dati2019.ros$vteam_gleft <- 162-dati2019.ros$G_v


dati2019.ros[dati2019.ros$hteam==tvec2[1]|dati2019.ros$vteam==tvec2[1],2][2]

for(i in 1:30){
  for(z in 1:(standings19[i,9])){
    
    ifelse(
      (dati2019.ros[dati2019.ros$hteam==tvec2[i] | dati2019.ros$vteam==tvec2[i],1][z])==tvec2[i],
      dati2019.ros[dati2019.ros$hteam==tvec2[i] |
                     dati2019.ros$vteam==tvec2[i], 17][z] <- z,
      dati2019.ros[dati2019.ros$hteam==tvec2[i] |
                     dati2019.ros$vteam==tvec2[i], 16][z] <- z
    )
  }
}

dati2019.ros[,c(14,15,18,19)] <- NULL


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

colnames(dati2019.ros)[16] <- c("ERA_hs")

dati2019.ros <- merge(dati2019.ros, starters.st.ros[,c(2,5)],
                      by.x = "vstarter", by.y = "Name")

colnames(dati2019.ros)[17] <- c("ERA_vs")

dati2019.ros$ERA_hs_scale <- as.numeric(scale(dati2019.ros$ERA_hs, ERA19, F))
dati2019.ros$ERA_vs_scale <- as.numeric(scale(dati2019.ros$ERA_vs, ERA19, F))

dati2019.ros <- merge(dati2019.ros, relief2019_ros[,c(1,3,6)],
                      by.x = "hteam", by.y = "Team")
colnames(dati2019.ros)[20:21] <- c("ERA_bullpen_h", "ERA_bullpen_h_scale")

dati2019.ros <- merge(dati2019.ros, relief2019_ros[,c(1,3,6)],
                      by.x = "vteam", by.y = "Team")
colnames(dati2019.ros)[22:23] <- c("ERA_bullpen_v", "ERA_bullpen_v_scale")


wlrecord2019 <- standings19[,1:4]

dati2019.ros$prob.home <- predict.glm(out.RSg.eras.erab,
                                      newdata = dati2019.ros,
                                      type = "response")

summary(dati2019.ros$prob.home)
hist(dati2019.ros$prob.home)
quantile(dati2019.ros$prob.home, c(.05,.95))

boxplot(dati2019.ros$prob.home)

dati2019.ros$pred.result <- rbinom(nrow(dati2019.ros),
                                   1,
                                   prob = dati2019.ros$prob.home)

dati2019.ros$pred.winner <- with(dati2019.ros, ifelse(pred.result,
                                                      as.character(hteam),
                                                      
                                                      as.character(vteam)))
wins.ros <- table(dati2019.ros$pred.winner)
View(wins.ros)
wins.ros <- as.data.frame(wins.ros)
colnames(wins.ros) <- c("Team", "wins.ros")

pred.wlrecord2019 <- merge(wlrecord2019, wins.ros)
pred.wlrecord2019$TOTWINS <- pred.wlrecord2019$W + pred.wlrecord2019$wins.ros
pred.wlrecord2019$TOTLOSSES <- 162-pred.wlrecord2019$TOTWINS


final.pred.standings <- pred.wlrecord2019[,c(1,6:7)]
View(final.pred.standings)

