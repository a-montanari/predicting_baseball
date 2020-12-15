###### DATA UPLOAD, FEATURE EXTRACTION AND CONSTRUCTION ######

# This script contains all the procedures regarding the creation and extraction
# of the dataset containing relevant variables about every game played
# in the Major League Baseball from 2015 to 2018.
# This will later be used for estimating the logistic regression models.

#### (1) Games of the 2015-2018 seasons ####

dati2015 <- read.csv("/data/GL2015.TXT", header=FALSE)
dati2015[,1] <- as.factor(dati2015[,1])
dati2015[,1] <- as.Date(dati2015[,1], format = "%Y%m%d")
dati2015 <- dati2015[,c(1,4,6,7,9,10,11,13,17,102,103,104,105)]
colnames(dati2015) <- c("date","vteam","vteamng","hteam","hteamng","vscore",
                        "hscore","time","park","retroid_vs","vstarter","retroid_hs","hstarter")
dati2015$year <- format(dati2015$date, "%Y")
dati2015$year <- as.factor(dati2015$year)
dati2015$month <- format(dati2015$date, "%b")
dati2015$month <- as.factor(dati2015$month)
dati2015$day <- format(dati2015$date, "%d")
dati2015$day <- as.factor(dati2015$day)
dati2015$retroid_vs <- as.character(dati2015$retroid_vs)
dati2015$retroid_hs <- as.character(dati2015$retroid_hs)

dati2016 <- read.csv("/data/GL2016.TXT", header=FALSE)
dati2016[,1] <- as.factor(dati2016[,1])
dati2016[,1] <- as.Date(dati2016[,1], format = "%Y%m%d")
dati2016 <- dati2016[,c(1,4,6,7,9,10,11,13,17,102,103,104,105)]
colnames(dati2016) <- c("date","vteam","vteamng","hteam","hteamng","vscore",
                        "hscore","time","park","retroid_vs","vstarter","retroid_hs","hstarter")
dati2016$year <- format(dati2016$date, "%Y")
dati2016$year <- as.factor(dati2016$year)
dati2016$month <- format(dati2016$date, "%b")
dati2016$month <- as.factor(dati2016$month)
dati2016$day <- format(dati2016$date, "%d")
dati2016$day <- as.factor(dati2016$day)
dati2016$retroid_vs <- as.character(dati2016$retroid_vs)
dati2016$retroid_hs <- as.character(dati2016$retroid_hs)

dati2017 <- read.csv("/data/GL2017.TXT", header=FALSE)
dati2017[,1] <- as.factor(dati2017[,1])
dati2017[,1] <- as.Date(dati2017[,1], format = "%Y%m%d")
dati2017 <- dati2017[,c(1,4,6,7,9,10,11,13,17,102,103,104,105)]
colnames(dati2017) <- c("date","vteam","vteamng","hteam","hteamng","vscore",
                        "hscore","time","park","retroid_vs","vstarter","retroid_hs","hstarter")
dati2017$year <- format(dati2017$date, "%Y")
dati2017$year <- as.factor(dati2017$year)
dati2017$month <- format(dati2017$date, "%b")
dati2017$month <- as.factor(dati2017$month)
dati2017$day <- format(dati2017$date, "%d")
dati2017$day <- as.factor(dati2017$day)
dati2017$retroid_vs <- as.character(dati2017$retroid_vs)
dati2017$retroid_hs <- as.character(dati2017$retroid_hs)

dati2018 <- read.csv("/data/GL2018.TXT", header=FALSE)
dati2018[,1] <- as.factor(dati2018[,1])
dati2018[,1] <- as.Date(dati2018[,1], format = "%Y%m%d")
dati2018 <- dati2018[,c(1,4,6,7,9,10,11,13,17,102,103,104,105)]
colnames(dati2018) <- c("date","vteam","vteamng","hteam","hteamng","vscore",
                        "hscore","time","park","retroid_vs","vstarter","retroid_hs","hstarter")
dati2018$year <- format(dati2018$date, "%Y")
dati2018$year <- as.factor(dati2018$year)
dati2018$month <- format(dati2018$date, "%b")
dati2018$month <- as.factor(dati2018$month)
dati2018$day <- format(dati2018$date, "%d")
dati2018$day <- as.factor(dati2018$day)
dati2018$retroid_vs <- as.character(dati2018$retroid_vs)
dati2018$retroid_hs <- as.character(dati2018$retroid_hs)

#### (2) Starting Pitchers, Relief Pitchers, People, Teams and Value 2015-2018 ####

pitching <- read.csv("/data/Pitching.csv", header = T)
teams <- read.csv("/data/Teams.csv", header = T)

teams$yearID <- as.factor(teams$yearID)

teams$teamID[teams$teamID=="LAA"] <- "ANA"
teams$RD <- teams$R - teams$RA
teams$RDg <- teams$RD/teams$G
teams$RSg <- teams$R/teams$G
teams$PYT <- as.numeric((teams$R^2)/((teams$R^2)+(teams$RA^2)))
teams$PYT_log <- as.numeric((teams$R^1.81)/((teams$R^1.81)+(teams$RA^1.81)))

advbatting2015 <- read.csv("/data/advbatting2015.csv", header = T)
advbatting2016 <- read.csv("/data/advbatting2016.csv", header = T)
advbatting2017 <- read.csv("/data/advbatting2017.csv", header = T)
advbatting2018 <- read.csv("/data/advbatting2018.csv", header = T)


sum(levels(advbatting2015$Team) == levels(advbatting2016$Team))
sum(levels(advbatting2015$Team) == levels(advbatting2017$Team))
sum(levels(advbatting2015$Team) == levels(advbatting2018$Team))

levels(advbatting2015$Team) <- levels(advbatting2016$Team) <-
  levels(advbatting2017$Team) <- levels(advbatting2018$Team) <-
  c("ANA", "HOU", "OAK", "TOR", "ATL",
    "MIL", "SLN", "CHN", "ARI", "LAN",
    "SFN", "CLE", "SEA",  "MIA", "NYN",
    "WAS", "BAL", "SDN", "PHI", "PIT",
    "TEX", "TBA", "BOS", "CIN", "COL",
    "KCA", "DET", "MIN", "CHA", "NYA")

teams2015 <- teams[teams$yearID==2015,]
teams2015 <- merge(teams2015, advbatting2015[,c(1,2,18,19)],
                   by.x = "teamID", by.y = "Team")
colnames(teams2015)[56] <- "wRC_plus"

teams2016 <- teams[teams$yearID==2016,]
teams2016 <- merge(teams2016, advbatting2016[,c(1,2,18,19)],
                   by.x = "teamID", by.y = "Team")
colnames(teams2016)[56] <- "wRC_plus"

teams2017 <- teams[teams$yearID==2017,]
teams2017 <- merge(teams2017, advbatting2017[,c(1,2,18,19)],
                   by.x = "teamID", by.y = "Team")
colnames(teams2017)[56] <- "wRC_plus"

teams2018 <- teams[teams$yearID==2018,]
teams2018 <- merge(teams2018, advbatting2018[,c(1,2,18,19)],
                   by.x = "teamID", by.y = "Team")
colnames(teams2018)[56] <- "wRC_plus"

teams1518 <- rbind(teams2015, teams2016, teams2017, teams2018)

t2 <- as.vector(unique(dati2018$hteam))
t3 <- as.vector(unique(teams2018$teamID))

setdiff(t2,t3)

tvec <- as.vector(unique(levels(rbind(dati2015, dati2016, dati2017, dati2018)[,4])))
length(tvec)

erarelief <- read.csv("/data/erarelief.csv", header = T)

levels(erarelief$Team) <- c("ANA", "HOU", "OAK", "TOR", "ATL",
                            "MIL", "SLN", "CHN", "ARI", "LAN",
                            "SFN", "CLE", "SEA",  "MIA", "NYN",
                            "WAS", "BAL", "SDN", "PHI", "PIT",
                            "TEX", "TBA", "BOS", "CIN", "COL",
                            "KCA", "DET", "MIN", "CHA", "NYA")

relief2015 <- read.csv("/data/relief2015.csv", header = T)
relief2015 <- merge(relief2015, teams2015[,c(1,46)], by.x = "Tm", by.y = "teamIDBR")
relief2015 <- merge(relief2015, erarelief[,c(1,2,16)],
                    by.x = c("teamID", "Season"), by.y = c("Team", "Season"))

relief2016 <- read.csv("/data/relief2016.csv", header = T)
relief2016 <- merge(relief2016, teams2016[,c(1,46)], by.x = "Tm", by.y = "teamIDBR")
relief2016 <- merge(relief2016, erarelief[,c(1,2,16)],
                    by.x = c("teamID", "Season"), by.y = c("Team", "Season"))

relief2017 <- read.csv("/data/relief2017.csv", header = T)
relief2017 <- merge(relief2017, teams2017[,c(1,46)], by.x = "Tm", by.y = "teamIDBR")
relief2017 <- merge(relief2017, erarelief[,c(1,2,16)],
                    by.x = c("teamID", "Season"), by.y = c("Team", "Season"))

relief2018 <- read.csv("/data/relief2018.csv", header = T)
relief2018 <- merge(relief2018, teams2018[,c(1,46)], by.x = "Tm", by.y = "teamIDBR")
relief2018 <- merge(relief2018, erarelief[,c(1,2,16)],
                    by.x = c("teamID", "Season"), by.y = c("Team", "Season"))

relief1518 <- rbind(relief2015,relief2016,relief2017,relief2018)

starters2015 <- read.csv("/data/starters2015.csv", header = T)
starters2016 <- read.csv("/data/starters2016.csv", header = T)
starters2017 <- read.csv("/data/starters2017.csv", header = T)
starters2018 <- read.csv("/data/starters2018.csv", header = T)

sum(levels(starters2015$Team) == levels(starters2016$Team))
sum(levels(starters2015$Team) == levels(starters2017$Team))
sum(levels(starters2015$Team) == levels(starters2018$Team))

levels(starters2015$Team) <- levels(starters2016$Team) <-
  levels(starters2017$Team) <- levels(starters2018$Team) <-
  c("ANA", "HOU", "OAK", "TOR", "ATL",
    "MIL", "SLN", "CHN", "ARI", "LAN",
    "SFN", "CLE", "SEA",  "MIA", "NYN",
    "WAS", "BAL", "SDN", "PHI", "PIT",
    "TEX", "TBA", "BOS", "CIN", "COL",
    "KCA", "DET", "MIN", "CHA", "NYA")


value2015 <- read.csv("/data/value2015.csv", header = T)
value2016 <- read.csv("/data/value2015.csv", header = T)
value2017 <- read.csv("/data/value2015.csv", header = T)
value2018 <- read.csv("/data/value2015.csv", header = T)

sum(levels(value2015$Team) == levels(value2016$Team))
sum(levels(value2015$Team) == levels(value2017$Team))
sum(levels(value2015$Team) == levels(value2018$Team))

levels(value2015$Team) <- levels(value2016$Team) <-
  levels(value2017$Team) <- levels(value2018$Team) <-
  c("ANA", "HOU", "OAK", "TOR", "ATL",
    "MIL", "SLN", "CHN", "ARI", "LAN",
    "SFN", "CLE", "SEA",  "MIA", "NYN",
    "WAS", "BAL", "SDN", "PHI", "PIT",
    "TEX", "TBA", "BOS", "CIN", "COL",
    "KCA", "DET", "MIN", "CHA", "NYA")


pitching$playerID <- as.character(pitching$playerID)

people <- read.csv("/data/People.csv", header = T)

people$playerID <- as.character(people$playerID)
people$retroID <- as.character(people$retroID)

### 2015

dati2015 <- merge(dati2015, people[,c(1,23)],
                  by.x = "retroid_vs", by.y="retroID")
dati2015 <- merge(dati2015, people[,c(1,23)],
                  by.x = "retroid_hs", by.y="retroID")
colnames(dati2015)[17:18] <- c("lahmanid_vs", "lahmanid_hs")
dati2015$lahmanid_hs <- as.character(dati2015$lahmanid_hs)
dati2015$lahmanid_vs <- as.character(dati2015$lahmanid_vs)

### 2016

dati2016 <- merge(dati2016, people[,c(1,23)],
                  by.x = "retroid_vs", by.y="retroID")
dati2016 <- merge(dati2016, people[,c(1,23)],
                  by.x = "retroid_hs", by.y="retroID")
colnames(dati2016)[17:18] <- c("lahmanid_vs", "lahmanid_hs")
dati2016$lahmanid_hs <- as.character(dati2016$lahmanid_hs)
dati2016$lahmanid_vs <- as.character(dati2016$lahmanid_vs)

### 2017

dati2017 <- merge(dati2017, people[,c(1,23)],
                  by.x = "retroid_vs", by.y="retroID")
dati2017 <- merge(dati2017, people[,c(1,23)],
                  by.x = "retroid_hs", by.y="retroID")
colnames(dati2017)[17:18] <- c("lahmanid_vs", "lahmanid_hs")
dati2017$lahmanid_hs <- as.character(dati2017$lahmanid_hs)
dati2017$lahmanid_vs <- as.character(dati2017$lahmanid_vs)

### 2018

dati2018 <- merge(dati2018, people[,c(1,23)],
                  by.x = "retroid_vs", by.y="retroID")
dati2018 <- merge(dati2018, people[,c(1,23)],
                  by.x = "retroid_hs", by.y="retroID")
colnames(dati2018)[17:18] <- c("lahmanid_vs", "lahmanid_hs")
dati2018$lahmanid_hs <- as.character(dati2018$lahmanid_hs)
dati2018$lahmanid_vs <- as.character(dati2018$lahmanid_vs)

###

pitching$yearID <- as.factor(pitching$yearID)
pitching$teamID[pitching$teamID=="LAA"] <- "ANA"

pitching2015 <- pitching[pitching$yearID==2015,]
pitching2015$FIP <- 3.134 + ((pitching2015$HR*13 + (pitching2015$BB+pitching2015$HBP)*3 - (pitching2015$SO*2))/(pitching2015$IPouts/3))

pitching2016 <- pitching[pitching$yearID==2016,]
pitching2016$FIP <- 3.147 + ((pitching2016$HR*13 + (pitching2016$BB+pitching2016$HBP)*3 - (pitching2016$SO*2))/(pitching2016$IPouts/3))

pitching2017 <- pitching[pitching$yearID==2017,]
pitching2017$FIP <- 3.158 + ((pitching2017$HR*13 + (pitching2017$BB+pitching2017$HBP)*3 - (pitching2017$SO*2))/(pitching2017$IPouts/3))

pitching2018 <- pitching[pitching$yearID==2018,]
pitching2018$FIP <- 3.161 + ((pitching2018$HR*13 + (pitching2018$BB+pitching2018$HBP)*3 - (pitching2018$SO*2))/(pitching2018$IPouts/3))


#### (3) Union of the games and the pitching data for the 2015-2018 seasons ####

## 2015

dati2015 <- merge(dati2015, pitching2015[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_vs","vteam"), by.y=c("playerID","teamID"))
colnames(dati2015)[19:21] <- c("ERA_vs","FIP_vs","G_vs")
dati2015 <- merge(dati2015, pitching2015[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_hs","hteam"), by.y=c("playerID","teamID"))
colnames(dati2015)[22:24] <- c("ERA_hs","FIP_hs","G_hs")

summary(dati2015)

## 2016
dati2016 <- merge(dati2016, pitching2016[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_vs","vteam"), by.y=c("playerID","teamID"))
colnames(dati2016)[19:21] <- c("ERA_vs","FIP_vs","G_vs")
dati2016 <- merge(dati2016, pitching2016[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_hs","hteam"), by.y=c("playerID","teamID"))
colnames(dati2016)[22:24] <- c("ERA_hs","FIP_hs","G_hs")


## 2017

dati2017 <- merge(dati2017, pitching2017[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_vs","vteam"), by.y=c("playerID","teamID"))
colnames(dati2017)[19:21] <- c("ERA_vs","FIP_vs","G_vs")
dati2017 <- merge(dati2017, pitching2017[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_hs","hteam"), by.y=c("playerID","teamID"))
colnames(dati2017)[22:24] <- c("ERA_hs","FIP_hs","G_hs")


## 2018

dati2018 <- merge(dati2018, pitching2018[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_vs","vteam"), by.y=c("playerID","teamID"))
colnames(dati2018)[19:21] <- c("ERA_vs","FIP_vs","G_vs")
dati2018 <- merge(dati2018, pitching2018[,c(1,4,20,31,8)],
                  by.x = c("lahmanid_hs","hteam"), by.y=c("playerID","teamID"))
colnames(dati2018)[22:24] <- c("ERA_hs","FIP_hs","G_hs")

#### (4) Union of the games and the team data for the 2015-2018 seasons ####

## 2015

dati2015 <- merge(dati2015, teams2015[,c(1,50,51,56)],
                  by.x = c("vteam"), by.y=c("teamID"))
colnames(dati2015)[25:27] <- c("RDg_vteam", "RSg_vteam","wRC_plus_vteam")
dati2015 <- merge(dati2015, teams2015[,c(1,50,51,56)],
                  by.x = c("hteam"), by.y=c("teamID"))
colnames(dati2015)[28:30] <- c("RDg_hteam", "RSg_hteam","wRC_plus_hteam")


## 2016

dati2016 <- merge(dati2016, teams2016[,c(1,50,51,56)],
                  by.x = c("vteam"), by.y=c("teamID"))
colnames(dati2016)[25:27] <- c("RDg_vteam", "RSg_vteam","wRC_plus_vteam")
dati2016 <- merge(dati2016, teams2016[,c(1,50,51,56)],
                  by.x = c("hteam"), by.y=c("teamID"))
colnames(dati2016)[28:30] <- c("RDg_hteam", "RSg_hteam","wRC_plus_hteam")



## 2017

dati2017 <- merge(dati2017, teams2017[,c(1,50,51,56)],
                  by.x = c("vteam"), by.y=c("teamID"))
colnames(dati2017)[25:27] <- c("RDg_vteam", "RSg_vteam","wRC_plus_vteam")
dati2017 <- merge(dati2017, teams2017[,c(1,50,51,56)],
                  by.x = c("hteam"), by.y=c("teamID"))
colnames(dati2017)[28:30] <- c("RDg_hteam", "RSg_hteam","wRC_plus_hteam")

## 2018

dati2018 <- merge(dati2018, teams2018[,c(1,50,51,56)],
                  by.x = c("vteam"), by.y=c("teamID"))
colnames(dati2018)[25:27] <- c("RDg_vteam", "RSg_vteam","wRC_plus_vteam")
dati2018 <- merge(dati2018, teams2018[,c(1,50,51,56)],
                  by.x = c("hteam"), by.y=c("teamID"))
colnames(dati2018)[28:30] <- c("RDg_hteam", "RSg_hteam","wRC_plus_hteam")

#### (5) Bullpen data ####

## 2015

dati2015 <- merge(dati2015, relief2015[,c(1, 17:19)],
                  by.x = "vteam", by.y = "teamID")
colnames(dati2015)[31:33] <- c("FIP_bullpen_v","xFIP_bullpen_v", "ERA_bullpen_v")
dati2015 <- merge(dati2015, relief2015[,c(1, 17:19)],
                  by.x = "hteam", by.y = "teamID")
colnames(dati2015)[34:36] <- c("FIP_bullpen_h","xFIP_bullpen_h", "ERA_bullpen_h")


## 2016

dati2016 <- merge(dati2016, relief2016[,c(1, 17:19)],
                  by.x = "vteam", by.y = "teamID")
colnames(dati2016)[31:33] <- c("FIP_bullpen_v","xFIP_bullpen_v", "ERA_bullpen_v")
dati2016 <- merge(dati2016, relief2016[,c(1, 17:19)],
                  by.x = "hteam", by.y = "teamID")
colnames(dati2016)[34:36] <- c("FIP_bullpen_h","xFIP_bullpen_h", "ERA_bullpen_h")

## 2017

dati2017 <- merge(dati2017, relief2017[,c(1, 17:19)],
                  by.x = "vteam", by.y = "teamID")
colnames(dati2017)[31:33] <- c("FIP_bullpen_v","xFIP_bullpen_v", "ERA_bullpen_v")
dati2017 <- merge(dati2017, relief2017[,c(1, 17:19)],
                  by.x = "hteam", by.y = "teamID")
colnames(dati2017)[34:36] <- c("FIP_bullpen_h","xFIP_bullpen_h", "ERA_bullpen_h")

## 2018

dati2018 <- merge(dati2018, relief2018[,c(1, 17:19)],
                  by.x = "vteam", by.y = "teamID")
colnames(dati2018)[31:33] <- c("FIP_bullpen_v","xFIP_bullpen_v", "ERA_bullpen_v")
dati2018 <- merge(dati2018, relief2018[,c(1, 17:19)],
                  by.x = "hteam", by.y = "teamID")
colnames(dati2018)[34:36] <- c("FIP_bullpen_h","xFIP_bullpen_h", "ERA_bullpen_h")


#### (6) More Feature Construction: estimating the "hotness" of a team ####
### Runs scored per game

## 2015

rpg2015 <- as.data.frame(matrix(nrow = max(teams2015$G), ncol = length(tvec)))
colnames(rpg2015) <- tvec

for(i in 1:nrow(dati2015)){
  for(z in tvec){
    ifelse(dati2015$hteam[i]==z,
           rpg2015[dati2015$hteamng[i],z] <- dati2015$hscore[i],
           ifelse(dati2015$vteam[i]==z,
                  rpg2015[dati2015$vteamng[i],z] <- dati2015$vscore[i],
                  NA))
  }}

teams2015 <- teams2015[order(teams2015$teamID),]

runs2015 <- 1:30
names(runs2015) <- tvec

for(y in 1:30){
  runs2015[y] <- sum(rpg2015[1:teams2015$G[y],y])
}
runs2015
sum(runs2015==teams2015$R)


## 2016

rpg2016 <- as.data.frame(matrix(nrow = max(teams2016$G), ncol = length(tvec)))
colnames(rpg2016) <- tvec

for(i in 1:nrow(dati2016)){
  for(z in tvec){
    ifelse(dati2016$hteam[i]==z,
           rpg2016[dati2016$hteamng[i],z] <- dati2016$hscore[i],
           ifelse(dati2016$vteam[i]==z,
                  rpg2016[dati2016$vteamng[i],z] <- dati2016$vscore[i],
                  NA))
  }}

teams2016 <- teams2016[order(teams2016$teamID),]

runs2016 <- 1:30
names(runs2016) <- tvec

for(y in 1:30){
  runs2016[y] <- sum(rpg2016[1:teams2016$G[y],y])
}
runs2016
sum(runs2016==teams2016$R)


## 2017

rpg2017 <- as.data.frame(matrix(nrow = max(teams2017$G), ncol = length(tvec)))
colnames(rpg2017) <- tvec

for(i in 1:nrow(dati2017)){
  for(z in tvec){
    ifelse(dati2017$hteam[i]==z,
           rpg2017[dati2017$hteamng[i],z] <- dati2017$hscore[i],
           ifelse(dati2017$vteam[i]==z,
                  rpg2017[dati2017$vteamng[i],z] <- dati2017$vscore[i],
                  NA))
  }}

teams2017 <- teams2017[order(teams2017$teamID),]

runs2017 <- 1:30
names(runs2017) <- tvec

for(y in 1:30){
  runs2017[y] <- sum(rpg2017[1:teams2017$G[y],y])
}
runs2017
sum(runs2017==teams2017$R)

## 2018

rpg2018 <- as.data.frame(matrix(nrow = max(teams2018$G), ncol = length(tvec)))
colnames(rpg2018) <- tvec

for(i in 1:nrow(dati2018)){
  for(z in tvec){
    ifelse(dati2018$hteam[i]==z,
           rpg2018[dati2018$hteamng[i],z] <- dati2018$hscore[i],
           ifelse(dati2018$vteam[i]==z,
                  rpg2018[dati2018$vteamng[i],z] <- dati2018$vscore[i],
                  NA))
  }}

teams2018 <- teams2018[order(teams2018$teamID),]

runs2018 <- 1:30
names(runs2018) <- tvec

for(y in 1:30){
  runs2018[y] <- sum(rpg2018[1:teams2018$G[y],y])
}
runs2018
sum(runs2018==teams2018$R)


### Runs allowed per game

## 2015

rapg2015 <- as.data.frame(matrix(nrow = max(teams2015$G), ncol = length(tvec)))
colnames(rapg2015) <- tvec

for(i in 1:nrow(dati2015)){
  for(z in tvec){
    ifelse(dati2015$hteam[i]==z,
           rapg2015[dati2015$hteamng[i],z] <- dati2015$vscore[i],
           ifelse(dati2015$vteam[i]==z,
                  rapg2015[dati2015$vteamng[i],z] <- dati2015$hscore[i],
                  NA))
  }}

runs.a2015 <- 1:30
names(runs.a2015) <- tvec

for(y in 1:30){
  runs.a2015[y] <- sum(rapg2015[1:teams2015$G[y],y])
}
runs.a2015

sum(runs.a2015==teams2015$RA)

## 2016

rapg2016 <- as.data.frame(matrix(nrow = max(teams2016$G), ncol = length(tvec)))
colnames(rapg2016) <- tvec

for(i in 1:nrow(dati2016)){
  for(z in tvec){
    ifelse(dati2016$hteam[i]==z,
           rapg2016[dati2016$hteamng[i],z] <- dati2016$vscore[i],
           ifelse(dati2016$vteam[i]==z,
                  rapg2016[dati2016$vteamng[i],z] <- dati2016$hscore[i],
                  NA))
  }}

runs.a2016 <- 1:30
names(runs.a2016) <- tvec

for(y in 1:30){
  runs.a2016[y] <- sum(rapg2016[1:teams2016$G[y],y])
}
runs.a2016

sum(runs.a2016==teams2016$RA)


## 2017

rapg2017 <- as.data.frame(matrix(nrow = max(teams2017$G), ncol = length(tvec)))
colnames(rapg2017) <- tvec

for(i in 1:nrow(dati2017)){
  for(z in tvec){
    ifelse(dati2017$hteam[i]==z,
           rapg2017[dati2017$hteamng[i],z] <- dati2017$vscore[i],
           ifelse(dati2017$vteam[i]==z,
                  rapg2017[dati2017$vteamng[i],z] <- dati2017$hscore[i],
                  NA))
  }}

runs.a2017 <- 1:30
names(runs.a2017) <- tvec

for(y in 1:30){
  runs.a2017[y] <- sum(rapg2017[1:teams2017$G[y],y])
}
runs.a2017

sum(runs.a2017==teams2017$RA)


## 2018

rapg2018 <- as.data.frame(matrix(nrow = max(teams2018$G), ncol = length(tvec)))
colnames(rapg2018) <- tvec

for(i in 1:nrow(dati2018)){
  for(z in tvec){
    ifelse(dati2018$hteam[i]==z,
           rapg2018[dati2018$hteamng[i],z] <- dati2018$vscore[i],
           ifelse(dati2018$vteam[i]==z,
                  rapg2018[dati2018$vteamng[i],z] <- dati2018$hscore[i],
                  NA))
  }}

runs.a2018 <- 1:30
names(runs.a2018) <- tvec

for(y in 1:30){
  runs.a2018[y] <- sum(rapg2018[1:teams2018$G[y],y])
}
runs.a2018

sum(runs.a2018==teams2018$RA)

### Run difference per game (Runs scored - Runs allowed)

rdg2015 <- rpg2015 - rapg2015
rdg2016 <- rpg2016 - rapg2016
rdg2017 <- rpg2017 - rapg2017
rdg2018 <- rpg2018 - rapg2018

### Average Run difference in the last 5 games

rdg_l5_2015 <- as.data.frame(matrix(nrow = max(teams2015$G), ncol = length(tvec)))
colnames(rdg_l5_2015) <- tvec

for(i in 6:nrow(rdg2015)){
  for(z in tvec){
    rdg_l5_2015[i,z] <- mean(rdg2015[((i-5):(i-1)),z])
  }}

for(i in 1:5){
  for(z in tvec){
    rdg_l5_2015[i,z] <- mean(rdg2015[0:(i-1),z])
  }}

rdg_l5_2015[1:5,] <- 0

##

rdg_l5_2016 <- as.data.frame(matrix(nrow = max(teams2016$G), ncol = length(tvec)))
colnames(rdg_l5_2016) <- tvec

for(i in 6:nrow(rdg2016)){
  for(z in tvec){
    rdg_l5_2016[i,z] <- mean(rdg2016[((i-5):(i-1)),z])
  }}

for(i in 1:5){
  for(z in tvec){
    rdg_l5_2016[i,z] <- mean(rdg2016[0:(i-1),z])
  }}

rdg_l5_2016[1:5,] <- 0

##

rdg_l5_2017 <- as.data.frame(matrix(nrow = max(teams2017$G), ncol = length(tvec)))
colnames(rdg_l5_2017) <- tvec

for(i in 6:nrow(rdg2017)){
  for(z in tvec){
    rdg_l5_2017[i,z] <- mean(rdg2017[((i-5):(i-1)),z])
  }}

for(i in 1:5){
  for(z in tvec){
    rdg_l5_2017[i,z] <- mean(rdg2017[0:(i-1),z])
  }}

rdg_l5_2017[1:5,] <- 0

##

rdg_l5_2018 <- as.data.frame(matrix(nrow = max(teams2018$G), ncol = length(tvec)))
colnames(rdg_l5_2018) <- tvec

for(i in 6:nrow(rdg2018)){
  for(z in tvec){
    rdg_l5_2018[i,z] <- mean(rdg2018[((i-5):(i-1)),z])
  }}

for(i in 1:5){
  for(z in tvec){
    rdg_l5_2018[i,z] <- mean(rdg2018[0:(i-1),z])
  }}

rdg_l5_2018[1:5,] <- 0

### Average Run difference in the last 10 games

rdg_l10_2015 <- as.data.frame(matrix(nrow = max(teams2015$G), ncol = length(tvec)))
colnames(rdg_l10_2015) <- tvec

for(i in 11:nrow(rdg2015)){
  for(z in tvec){
    rdg_l10_2015[i,z] <- mean(rdg2015[((i-10):(i-1)),z])
  }}


for(i in 1:10){
  for(z in tvec){
    rdg_l10_2015[i,z] <- mean(rdg2015[0:(i-1),z])
  }}

rdg_l10_2015[1:10,] <- 0

##

rdg_l10_2016 <- as.data.frame(matrix(nrow = max(teams2016$G), ncol = length(tvec)))
colnames(rdg_l10_2016) <- tvec

for(i in 11:nrow(rdg2016)){
  for(z in tvec){
    rdg_l10_2016[i,z] <- mean(rdg2016[((i-10):(i-1)),z])
  }}

for(i in 1:10){
  for(z in tvec){
    rdg_l10_2016[i,z] <- mean(rdg2016[0:(i-1),z])
  }}

rdg_l10_2016[1:10,] <- 0

##

rdg_l10_2017 <- as.data.frame(matrix(nrow = max(teams2017$G), ncol = length(tvec)))
colnames(rdg_l10_2017) <- tvec

for(i in 11:nrow(rdg2017)){
  for(z in tvec){
    rdg_l10_2017[i,z] <- mean(rdg2017[((i-10):(i-1)),z])
  }}

for(i in 1:10){
  for(z in tvec){
    rdg_l10_2017[i,z] <- mean(rdg2017[0:(i-1),z])
  }}

rdg_l10_2017[1:10,] <- 0
##

rdg_l10_2018 <- as.data.frame(matrix(nrow = max(teams2018$G), ncol = length(tvec)))
colnames(rdg_l10_2018) <- tvec

for(i in 11:nrow(rdg2018)){
  for(z in tvec){
    rdg_l10_2018[i,z] <- mean(rdg2018[((i-10):(i-1)),z])
  }}

for(i in 1:10){
  for(z in tvec){
    rdg_l10_2018[i,z] <- mean(rdg2018[0:(i-1),z])
  }}

rdg_l10_2018[1:10,] <- 0

### Adding the new indexes to the dataset


for(i in 1:nrow(dati2015)){
  for(z in tvec){
    ifelse(dati2015$hteam[i]==z,
           dati2015$RDgl5_h[i] <- rdg_l5_2015[dati2015$hteamng[i],z],
           ifelse(dati2015$vteam[i]==z,
                  dati2015$RDgl5_v[i] <- rdg_l5_2015[dati2015$vteamng[i],z],
                  NA))
  }}

for(i in 1:nrow(dati2016)){
  for(z in tvec){
    ifelse(dati2016$hteam[i]==z,
           dati2016$RDgl5_h[i] <- rdg_l5_2016[dati2016$hteamng[i],z],
           ifelse(dati2016$vteam[i]==z,
                  dati2016$RDgl5_v[i] <- rdg_l5_2016[dati2016$vteamng[i],z],
                  NA))
  }}

for(i in 1:nrow(dati2017)){
  for(z in tvec){
    ifelse(dati2017$hteam[i]==z,
           dati2017$RDgl5_h[i] <- rdg_l5_2017[dati2017$hteamng[i],z],
           ifelse(dati2017$vteam[i]==z,
                  dati2017$RDgl5_v[i] <- rdg_l5_2017[dati2017$vteamng[i],z],
                  NA))
  }}

for(i in 1:nrow(dati2018)){
  for(z in tvec){
    ifelse(dati2018$hteam[i]==z,
           dati2018$RDgl5_h[i] <- rdg_l5_2018[dati2018$hteamng[i],z],
           ifelse(dati2018$vteam[i]==z,
                  dati2018$RDgl5_v[i] <- rdg_l5_2018[dati2018$vteamng[i],z],
                  NA))
  }}


####

for(i in 1:nrow(dati2015)){
  for(z in tvec){
    ifelse(dati2015$hteam[i]==z,
           dati2015$RDgl10_h[i] <- rdg_l10_2015[dati2015$hteamng[i],z],
           ifelse(dati2015$vteam[i]==z,
                  dati2015$RDgl10_v[i] <- rdg_l10_2015[dati2015$vteamng[i],z],
                  NA))
  }}

for(i in 1:nrow(dati2016)){
  for(z in tvec){
    ifelse(dati2016$hteam[i]==z,
           dati2016$RDgl10_h[i] <- rdg_l10_2016[dati2016$hteamng[i],z],
           ifelse(dati2016$vteam[i]==z,
                  dati2016$RDgl10_v[i] <- rdg_l10_2016[dati2016$vteamng[i],z],
                  NA))
  }}

for(i in 1:nrow(dati2017)){
  for(z in tvec){
    ifelse(dati2017$hteam[i]==z,
           dati2017$RDgl10_h[i] <- rdg_l10_2017[dati2017$hteamng[i],z],
           ifelse(dati2017$vteam[i]==z,
                  dati2017$RDgl10_v[i] <- rdg_l10_2017[dati2017$vteamng[i],z],
                  NA))
  }}

for(i in 1:nrow(dati2018)){
  for(z in tvec){
    ifelse(dati2018$hteam[i]==z,
           dati2018$RDgl10_h[i] <- rdg_l10_2018[dati2018$hteamng[i],z],
           ifelse(dati2018$vteam[i]==z,
                  dati2018$RDgl10_v[i] <- rdg_l10_2018[dati2018$vteamng[i],z],
                  NA))
  }}

### Also on estimating the hotness of a team: Number of wins in the last 10 games

## 2015

w2015 <- as.data.frame(matrix(nrow = max(teams2015$G), ncol = length(tvec)))
colnames(w2015) <- tvec

for(i in 1:nrow(dati2015)){
  for(z in tvec){
    ifelse(dati2015$hteam[i]==z,
           ifelse(dati2015$hscore[i]>dati2015$vscore[i],
                  w2015[dati2015$hteamng[i],z] <- 1,
                  w2015[dati2015$hteamng[i],z] <- 0),
           ifelse(dati2015$vteam[i]==z,
                  ifelse(dati2015$vscore[i]>dati2015$hscore[i],
                         w2015[dati2015$vteamng[i],z] <- 1,
                         w2015[dati2015$vteamng[i],z] <- 0),NA))
  }}

w2015[,1:30] <- as.numeric(unlist(w2015[,1:30]))

wins2015 <- 1:30
names(wins2015) <- tvec

for(y in 1:30){
  wins2015[y] <- sum(w2015[1:teams2015$G[y],y])
}
wins2015

teams2015 <- teams2015[order(teams2015$teamID),]

sum(wins2015==teams2015$W)

## 2016

w2016 <- as.data.frame(matrix(nrow = max(teams2016$G), ncol = length(tvec)))
colnames(w2016) <- tvec

for(i in 1:nrow(dati2016)){
  for(z in tvec){
    ifelse(dati2016$hteam[i]==z,
           ifelse(dati2016$hscore[i]>dati2016$vscore[i],
                  w2016[dati2016$hteamng[i],z] <- 1,
                  w2016[dati2016$hteamng[i],z] <- 0),
           ifelse(dati2016$vteam[i]==z,
                  ifelse(dati2016$vscore[i]>dati2016$hscore[i],
                         w2016[dati2016$vteamng[i],z] <- 1,
                         w2016[dati2016$vteamng[i],z] <- 0),NA))
  }}

w2016[,1:30] <- as.numeric(unlist(w2016[,1:30]))

wins2016 <- 1:30
names(wins2016) <- tvec

for(y in 1:30){
  wins2016[y] <- sum(w2016[1:teams2016$G[y],y])
}
wins2016

teams2016 <- teams2016[order(teams2016$teamID),]

sum(wins2016==teams2016$W)

## 2017

w2017 <- as.data.frame(matrix(nrow = max(teams2017$G), ncol = length(tvec)))
colnames(w2017) <- tvec

for(i in 1:nrow(dati2017)){
  for(z in tvec){
    ifelse(dati2017$hteam[i]==z,
           ifelse(dati2017$hscore[i]>dati2017$vscore[i],
                  w2017[dati2017$hteamng[i],z] <- 1,
                  w2017[dati2017$hteamng[i],z] <- 0),
           ifelse(dati2017$vteam[i]==z,
                  ifelse(dati2017$vscore[i]>dati2017$hscore[i],
                         w2017[dati2017$vteamng[i],z] <- 1,
                         w2017[dati2017$vteamng[i],z] <- 0),NA))
  }}

w2017[,1:30] <- as.numeric(unlist(w2017[,1:30]))

wins2017 <- 1:30
names(wins2017) <- tvec

for(y in 1:30){
  wins2017[y] <- sum(w2017[1:teams2017$G[y],y])
}
wins2017

teams2017 <- teams2017[order(teams2017$teamID),]

sum(wins2017==teams2017$W)

## 2018

w2018 <- as.data.frame(matrix(nrow = max(teams2018$G), ncol = length(tvec)))
colnames(w2018) <- tvec

for(i in 1:nrow(dati2018)){
  for(z in tvec){
    ifelse(dati2018$hteam[i]==z,
           ifelse(dati2018$hscore[i]>dati2018$vscore[i],
                  w2018[dati2018$hteamng[i],z] <- 1,
                  w2018[dati2018$hteamng[i],z] <- 0),
           ifelse(dati2018$vteam[i]==z,
                  ifelse(dati2018$vscore[i]>dati2018$hscore[i],
                         w2018[dati2018$vteamng[i],z] <- 1,
                         w2018[dati2018$vteamng[i],z] <- 0),NA))
  }}

w2018[,1:30] <- as.numeric(unlist(w2018[,1:30]))

wins2018 <- 1:30
names(wins2018) <- tvec

for(y in 1:30){
  wins2018[y] <- sum(w2018[1:teams2018$G[y],y])
}
wins2018

teams2018 <- teams2018[order(teams2018$teamID),]

sum(wins2018==teams2018$W)

###

w_l10_2015 <- as.data.frame(matrix(nrow = max(teams2015$G), ncol = length(tvec)))
colnames(w_l10_2015) <- tvec

for(i in 11:nrow(w2015)){
  for(z in tvec){
    w_l10_2015[i,z] <- mean(w2015[((i-10):(i-1)),z])
  }}


for(i in 1:10){
  for(z in tvec){
    w_l10_2015[i,z] <- mean(w2015[0:(i-1),z])
  }}

w_l10_2015[1:10,] <- 0.5

##

w_l10_2016 <- as.data.frame(matrix(nrow = max(teams2016$G), ncol = length(tvec)))
colnames(w_l10_2016) <- tvec

for(i in 11:nrow(w2016)){
  for(z in tvec){
    w_l10_2016[i,z] <- mean(w2016[((i-10):(i-1)),z])
  }}


for(i in 1:10){
  for(z in tvec){
    w_l10_2016[i,z] <- mean(w2016[0:(i-1),z])
  }}

w_l10_2016[1:10,] <- 0.5

##

w_l10_2017 <- as.data.frame(matrix(nrow = max(teams2017$G), ncol = length(tvec)))
colnames(w_l10_2017) <- tvec

for(i in 11:nrow(w2017)){
  for(z in tvec){
    w_l10_2017[i,z] <- mean(w2017[((i-10):(i-1)),z])
  }}


for(i in 1:10){
  for(z in tvec){
    w_l10_2017[i,z] <- mean(w2017[0:(i-1),z])
  }}

w_l10_2017[1:10,] <- 0.5

##

w_l10_2018 <- as.data.frame(matrix(nrow = max(teams2018$G), ncol = length(tvec)))
colnames(w_l10_2018) <- tvec

for(i in 11:nrow(w2018)){
  for(z in tvec){
    w_l10_2018[i,z] <- mean(w2018[((i-10):(i-1)),z])
  }}


for(i in 1:10){
  for(z in tvec){
    w_l10_2018[i,z] <- mean(w2018[0:(i-1),z])
  }}

w_l10_2018[1:10,] <- 0.5


### Adding the new indexes to the dataset

for(i in 1:nrow(dati2015)){
  for(z in tvec){
    ifelse(dati2015$hteam[i]==z,
           dati2015$Wl10_h[i] <- w_l10_2015[dati2015$hteamng[i],z],
           ifelse(dati2015$vteam[i]==z,
                  dati2015$Wl10_v[i] <- w_l10_2015[dati2015$vteamng[i],z],
                  NA))
  }}

for(i in 1:nrow(dati2016)){
  for(z in tvec){
    ifelse(dati2016$hteam[i]==z,
           dati2016$Wl10_h[i] <- w_l10_2016[dati2016$hteamng[i],z],
           ifelse(dati2016$vteam[i]==z,
                  dati2016$Wl10_v[i] <- w_l10_2016[dati2016$vteamng[i],z],
                  NA))
  }}


for(i in 1:nrow(dati2017)){
  for(z in tvec){
    ifelse(dati2017$hteam[i]==z,
           dati2017$Wl10_h[i] <- w_l10_2017[dati2017$hteamng[i],z],
           ifelse(dati2017$vteam[i]==z,
                  dati2017$Wl10_v[i] <- w_l10_2017[dati2017$vteamng[i],z],
                  NA))
  }}


for(i in 1:nrow(dati2018)){
  for(z in tvec){
    ifelse(dati2018$hteam[i]==z,
           dati2018$Wl10_h[i] <- w_l10_2018[dati2018$hteamng[i],z],
           ifelse(dati2018$vteam[i]==z,
                  dati2018$Wl10_v[i] <- w_l10_2018[dati2018$vteamng[i],z],
                  NA))
  }}


#### (7) Starting pitchers xFIP ####

starters2015$Name <- as.character(starters2015$Name)
starters2015$Name[298] <- "Josh Smith"
starters2015$Name[309] <- "Mike Wright"
starters2015$Name[296] <- "TJ House"
starters2015$Name[77] <- "Jorge de la Rosa"
starters2015$Name[c(297,316)] <- c("Matt Boyd", "Matt Boyd")
starters2015$Name[52] <- "Lance McCullers"
starters2015$Name[c(73,151)] <- c("Michael Fiers", "Michael Fiers")
starters2015$Name[131] <- "Vincent Velasquez"
starters2015$Name[90] <- "Tom Milone"
starters2015$Name[240] <- "Fausto Carmona"

dati2015.2 <- merge(dati2015, starters2015[,c(1,2,18)],
                    by.x =c("vteam","vstarter"), by.y=c("Team", "Name"),
)


dati2015 <- dati2015.2
dati2015.2 <- NULL
colnames(dati2015)[43] <- "xFIP_vs"

dati2015.2 <- merge(dati2015, starters2015[,c(1,2,18)],
                    by.x =c("hteam","hstarter"), by.y=c("Team", "Name"))
dati2015 <- dati2015.2
dati2015.2 <- NULL
colnames(dati2015)[44] <- "xFIP_hs"

##


starters2016$Name <- as.character(starters2016$Name)
starters2016$Name[231] <- "Josh Smith"
starters2016$Name[224] <- "Mike Wright"
starters2016$Name[196] <- "Jorge de la Rosa"
starters2016$Name[114] <- "Matt Boyd"
starters2016$Name[76] <- "Lance McCullers"
starters2016$Name[93] <- "Michael Fiers"
starters2016$Name[69] <- "Vincent Velasquez"
starters2016$Name[281] <- "Joel de la Cruz"
starters2016$Name[293] <- "Fausto Carmona"
starters2016$Name[227] <- "Tom Milone"

dati2016.2 <- merge(dati2016, starters2016[,c(1,2,18)],
                    by.x =c("vteam","vstarter"), by.y=c("Team", "Name"),
)


dati2016 <- dati2016.2
dati2016.2 <- NULL
colnames(dati2016)[43] <- "xFIP_vs"

dati2016.2 <- merge(dati2016, starters2016[,c(1,2,18)],
                    by.x =c("hteam","hstarter"), by.y=c("Team", "Name"))
dati2016 <- dati2016.2
dati2016.2 <- NULL
colnames(dati2016)[44] <- "xFIP_hs"

##

starters2017$Name <- as.character(starters2017$Name)
starters2017$Name[125] <- "J.C. Ramirez"
starters2017$Name[62] <- "Matt Boyd"
starters2017$Name[35] <- "Lance McCullers"
starters2017$Name[250] <- "Michael Fiers"
starters2017$Name[113] <- "Jake Junis"
starters2017$Name[c(193,308)] <- rep("Tom Milone",2)
starters2017$Name[200] <- "Vincent Velasquez"
starters2017$Name[98] <- "Jacob Faria"

dati2017.2 <- merge(dati2017, starters2017[,c(1,2,18)],
                    by.x =c("vteam","vstarter"), by.y=c("Team", "Name"),
                    all.x = T)


dati2017 <- dati2017.2
dati2017.2 <- NULL
colnames(dati2017)[43] <- "xFIP_vs"

dati2017.2 <- merge(dati2017, starters2017[,c(1,2,18)],
                    by.x =c("hteam","hstarter"), by.y=c("Team", "Name"))

dati2017 <- dati2017.2
dati2017.2 <- NULL
colnames(dati2017)[44] <- "xFIP_hs"

##

starters2018$Name <- as.character(starters2018$Name)
starters2018$Name[242] <- "Mike Wright"
starters2018$Name[288] <- "Duane Underwood"
starters2018$Name[58] <- "Matt Boyd"
starters2018$Name[47] <- "Lance McCullers"
starters2018$Name[c(99,186)] <- rep("Michael Fiers",2)
starters2018$Name[189] <- "Josh James"
starters2018$Name[90] <- "Jake Junis"
starters2018$Name[182] <- "Tom Milone"
starters2018$Name[36] <- "Vincent Velasquez"
starters2018$Name[258] <- "Matt Festa"
starters2018$Name[264] <- "Jacob Faria"
starters2018$Name[345] <- "J.C. Ramirez"

dati2018.2 <- merge(dati2018, starters2018[,c(1,2,18)],
                    by.x =c("vteam","vstarter"), by.y=c("Team", "Name"),
)



dati2018 <- dati2018.2
dati2018.2 <- NULL
colnames(dati2018)[43] <- "xFIP_vs"

dati2018.2 <- merge(dati2018, starters2018[,c(1,2,18)],
                    by.x =c("hteam","hstarter"), by.y=c("Team", "Name"),
)

dati2018 <- dati2018.2
dati2018.2 <- NULL
colnames(dati2018)[44] <- "xFIP_hs"


#### (8) Scaling ERA, FIPe xFIP so that they have mean 0 ####

spERAmean1518 <- weighted.mean(starters1518$ERA,starters1518$IP)
spFIPmean1518 <- weighted.mean(starters1518$FIP,starters1518$IP)
spxFIPmean1518 <- weighted.mean(starters1518$xFIP,starters1518$IP)

ERA15 <- weighted.mean(starters2015$ERA,starters2015$IP)
ERA16 <- weighted.mean(starters2016$ERA,starters2016$IP)
ERA17 <- weighted.mean(starters2017$ERA,starters2017$IP)
ERA18 <- weighted.mean(starters2018$ERA,starters2018$IP)

FIP15 <- weighted.mean(starters2015$FIP,starters2015$IP)
FIP16 <- weighted.mean(starters2016$FIP,starters2016$IP)
FIP17 <- weighted.mean(starters2017$FIP,starters2017$IP)
FIP18 <- weighted.mean(starters2018$FIP,starters2018$IP)

xFIP15 <- weighted.mean(starters2015$xFIP,starters2015$IP)
xFIP16 <- weighted.mean(starters2016$xFIP,starters2016$IP)
xFIP17 <- weighted.mean(starters2017$xFIP,starters2017$IP)
xFIP18 <- weighted.mean(starters2018$xFIP,starters2018$IP)

b.ERA15 <- weighted.mean(relief2015$ERA,relief2015$IP)
b.ERA16 <- weighted.mean(relief2016$ERA,relief2016$IP)
b.ERA17 <- weighted.mean(relief2017$ERA,relief2017$IP)
b.ERA18 <- weighted.mean(relief2018$ERA,relief2018$IP)

b.FIP15 <- weighted.mean(relief2015$FIP,relief2015$IP)
b.FIP16 <- weighted.mean(relief2016$FIP,relief2016$IP)
b.FIP17 <- weighted.mean(relief2017$FIP,relief2017$IP)
b.FIP18 <- weighted.mean(relief2018$FIP,relief2018$IP)

b.xFIP15 <- weighted.mean(relief2015$xFIP,relief2015$IP)
b.xFIP16 <- weighted.mean(relief2016$xFIP,relief2016$IP)
b.xFIP17 <- weighted.mean(relief2017$xFIP,relief2017$IP)
b.xFIP18 <- weighted.mean(relief2018$xFIP,relief2018$IP)

mean(dati2015$RSg_hteam)
mean(dati2015$RSg_vteam)
mean(teams2015$RSg)

dati2015$RSg_hteam_scale <- as.numeric(scale(dati2015$RSg_hteam, T, F))
dati2015$RSg_vteam_scale <- as.numeric(scale(dati2015$RSg_vteam, T, F))
dati2015$wRC_plus_vteam_scale <- as.numeric(scale(dati2015$wRC_plus_vteam, T, F))
dati2015$wRC_plus_hteam_scale <- as.numeric(scale(dati2015$wRC_plus_hteam, T, F))
dati2015$ERA_vs_scale <- as.numeric(scale(dati2015$ERA_vs, ERA15, F))
dati2015$ERA_hs_scale <- as.numeric(scale(dati2015$ERA_hs, ERA15, F))
dati2015$FIP_vs_scale <- as.numeric(scale(dati2015$FIP_vs, FIP15, F))
dati2015$FIP_hs_scale <- as.numeric(scale(dati2015$FIP_hs, FIP15, F))
dati2015$xFIP_vs_scale <- as.numeric(scale(dati2015$xFIP_vs, xFIP15, F))
dati2015$xFIP_hs_scale <- as.numeric(scale(dati2015$xFIP_hs, xFIP15, F))
dati2015$ERA_bullpen_v_scale <- as.numeric(scale(dati2015$ERA_bullpen_v, b.ERA15, F))
dati2015$ERA_bullpen_h_scale <- as.numeric(scale(dati2015$ERA_bullpen_h, b.ERA15, F))
dati2015$FIP_bullpen_v_scale <- as.numeric(scale(dati2015$FIP_bullpen_v, b.FIP15, F))
dati2015$FIP_bullpen_h_scale <- as.numeric(scale(dati2015$FIP_bullpen_h, b.FIP15, F))
dati2015$xFIP_bullpen_v_scale <- as.numeric(scale(dati2015$xFIP_bullpen_v, b.xFIP15, F))
dati2015$xFIP_bullpen_h_scale <- as.numeric(scale(dati2015$xFIP_bullpen_h, b.xFIP15, F))

dati2016$RSg_hteam_scale <- as.numeric(scale(dati2016$RSg_hteam, T, F))
dati2016$RSg_vteam_scale <- as.numeric(scale(dati2016$RSg_vteam, T, F))
dati2016$wRC_plus_vteam_scale <- as.numeric(scale(dati2016$wRC_plus_vteam, T, F))
dati2016$wRC_plus_hteam_scale <- as.numeric(scale(dati2016$wRC_plus_hteam, T, F))
dati2016$ERA_vs_scale <- as.numeric(scale(dati2016$ERA_vs, ERA16, F))
dati2016$ERA_hs_scale <- as.numeric(scale(dati2016$ERA_hs, ERA16, F))
dati2016$FIP_vs_scale <- as.numeric(scale(dati2016$FIP_vs, FIP16, F))
dati2016$FIP_hs_scale <- as.numeric(scale(dati2016$FIP_hs, FIP16, F))
dati2016$xFIP_vs_scale <- as.numeric(scale(dati2016$xFIP_vs, xFIP16, F))
dati2016$xFIP_hs_scale <- as.numeric(scale(dati2016$xFIP_hs, xFIP16, F))
dati2016$ERA_bullpen_v_scale <- as.numeric(scale(dati2016$ERA_bullpen_v, b.ERA16, F))
dati2016$ERA_bullpen_h_scale <- as.numeric(scale(dati2016$ERA_bullpen_h, b.ERA16, F))
dati2016$FIP_bullpen_v_scale <- as.numeric(scale(dati2016$FIP_bullpen_v, b.FIP16, F))
dati2016$FIP_bullpen_h_scale <- as.numeric(scale(dati2016$FIP_bullpen_h, b.FIP16, F))
dati2016$xFIP_bullpen_v_scale <- as.numeric(scale(dati2016$xFIP_bullpen_v, b.xFIP16, F))
dati2016$xFIP_bullpen_h_scale <- as.numeric(scale(dati2016$xFIP_bullpen_h, b.xFIP16, F))


dati2017$RSg_hteam_scale <- as.numeric(scale(dati2017$RSg_hteam, T, F))
dati2017$RSg_vteam_scale <- as.numeric(scale(dati2017$RSg_vteam, T, F))
dati2017$wRC_plus_vteam_scale <- as.numeric(scale(dati2017$wRC_plus_vteam, T, F))
dati2017$wRC_plus_hteam_scale <- as.numeric(scale(dati2017$wRC_plus_hteam, T, F))
dati2017$ERA_vs_scale <- as.numeric(scale(dati2017$ERA_vs, ERA17, F))
dati2017$ERA_hs_scale <- as.numeric(scale(dati2017$ERA_hs, ERA17, F))
dati2017$FIP_vs_scale <- as.numeric(scale(dati2017$FIP_vs, FIP17, F))
dati2017$FIP_hs_scale <- as.numeric(scale(dati2017$FIP_hs, FIP17, F))
dati2017$xFIP_vs_scale <- as.numeric(scale(dati2017$xFIP_vs, xFIP17, F))
dati2017$xFIP_hs_scale <- as.numeric(scale(dati2017$xFIP_hs, xFIP17, F))
dati2017$ERA_bullpen_v_scale <- as.numeric(scale(dati2017$ERA_bullpen_v, b.ERA17, F))
dati2017$ERA_bullpen_h_scale <- as.numeric(scale(dati2017$ERA_bullpen_h, b.ERA17, F))
dati2017$FIP_bullpen_v_scale <- as.numeric(scale(dati2017$FIP_bullpen_v, b.FIP17, F))
dati2017$FIP_bullpen_h_scale <- as.numeric(scale(dati2017$FIP_bullpen_h, b.FIP17, F))
dati2017$xFIP_bullpen_v_scale <- as.numeric(scale(dati2017$xFIP_bullpen_v, b.xFIP17, F))
dati2017$xFIP_bullpen_h_scale <- as.numeric(scale(dati2017$xFIP_bullpen_h, b.xFIP17, F))

dati2018$RSg_hteam_scale <- as.numeric(scale(dati2018$RSg_hteam, T, F))
dati2018$RSg_vteam_scale <- as.numeric(scale(dati2018$RSg_vteam, T, F))
dati2018$wRC_plus_vteam_scale <- as.numeric(scale(dati2018$wRC_plus_vteam, T, F))
dati2018$wRC_plus_hteam_scale <- as.numeric(scale(dati2018$wRC_plus_hteam, T, F))
dati2018$ERA_vs_scale <- as.numeric(scale(dati2018$ERA_vs, ERA18, F))
dati2018$ERA_hs_scale <- as.numeric(scale(dati2018$ERA_hs, ERA18, F))
dati2018$FIP_vs_scale <- as.numeric(scale(dati2018$FIP_vs, FIP18, F))
dati2018$FIP_hs_scale <- as.numeric(scale(dati2018$FIP_hs, FIP18, F))
dati2018$xFIP_vs_scale <- as.numeric(scale(dati2018$xFIP_vs, xFIP18, F))
dati2018$xFIP_hs_scale <- as.numeric(scale(dati2018$xFIP_hs, xFIP18, F))
dati2018$ERA_bullpen_v_scale <- as.numeric(scale(dati2018$ERA_bullpen_v, b.ERA18, F))
dati2018$ERA_bullpen_h_scale <- as.numeric(scale(dati2018$ERA_bullpen_h, b.ERA18, F))
dati2018$FIP_bullpen_v_scale <- as.numeric(scale(dati2018$FIP_bullpen_v, b.FIP18, F))
dati2018$FIP_bullpen_h_scale <- as.numeric(scale(dati2018$FIP_bullpen_h, b.FIP18, F))
dati2018$xFIP_bullpen_v_scale <- as.numeric(scale(dati2018$xFIP_bullpen_v, b.xFIP18, F))
dati2018$xFIP_bullpen_h_scale <- as.numeric(scale(dati2018$xFIP_bullpen_h, b.xFIP18, F))


#### (9) UNION of the 2015 - 2018 obtained datasets ####

d1518 <- rbind(dati2015,dati2016,dati2017,dati2018)

### Removing a tie game: see the link below
### https://www.mlb.com/news/cubs-pirates-game-suspended-ends-in-tie/c-204121484

#### Response variable: Home Team Wins (1=TRUE, 0=FALSE)
d1518$W <- ifelse(d1518$hscore>d1518$vscore,1,0)

sum(d1518$hscore>d1518$vscore)
sum(d1518$hscore<d1518$vscore)
sum(d1518$vscore==d1518$hscore)

nrow(d1518) == sum(d1518$hscore>d1518$vscore)+sum(d1518$hscore<d1518$vscore)

compdata <- d1518
compdata[compdata$vscore==compdata$hscore,]
compdata[4160,]
compdata <- compdata[-4160,]
sum(compdata$hscore>compdata$vscore)
sum(compdata$hscore<compdata$vscore)
sum(compdata$vscore==compdata$hscore)

nrow(compdata) == sum(compdata$hscore>compdata$vscore)+sum(compdata$hscore<compdata$vscore)

compdata <- compdata[,-c(2,4,5,6,7,8,18)]
compdata$day <- format(compdata$date, "%a")
compdata$day <- as.factor(compdata$day)

parks <- read.csv("/data/parkcode.txt", header = T)

tvec <- as.vector(unique(levels(compdata$hteam)))
length(tvec)

nrow(compdata[(compdata$G_vs<10|compdata$G_hs<10),])
nrow(compdata[(compdata$G_vs<5|compdata$G_hs<5),])
nrow(compdata[(compdata$G_vs<3|compdata$G_hs<3),])

compdata10 <- compdata[which(compdata$G_vs>=10 & compdata$G_hs>=10),]
compdata5 <- compdata[which(compdata$G_vs>=5 & compdata$G_hs>=5),]
compdata3 <- compdata[which(compdata$G_vs>=3 & compdata$G_hs>=3),]