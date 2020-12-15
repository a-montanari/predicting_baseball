###### VISUALIZING THE RESULTS ######

# This scripts requires the functions specified in the simulations script.

# Required Packages

require(ggplot2)
require(ggpubr)
require(viridis)
require(DescTools)

#### (1) Rest Of the Season ####

many.results.2 <- NULL

pb <- txtProgressBar(min = 0, max = 1000, style = 3)

for(j in 1:1000){
  
  many.results.2 <- rbind(many.results.2, one.sim.rosandpost())
  setTxtProgressBar(pb, j)
}
close(pb)

summary(many.results.2)
colSums(many.results.2[,6:10])

many.results.2 <- merge(many.results.2, standings19[,c(1,22)])

ggrosandpos <- ggscatter(many.results.2, "Team", "TOTWINS",
                         color = "TOTWINS", alpha = "TOTWINS",
                         size = 3,
                         ylab = "Vittorie stagionali previste",
                         xlab = "Squadra") +
  scale_color_viridis() +
  scale_alpha(range = c(0.1,0.2)) +
  grids(color = "grey92") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(face = "bold", size = 14)) +
  scale_x_discrete(labels = tvec.extended) +
  geom_boxplot(aes(fill= DIV), outlier.shape = NA) +
  geom_hline(yintercept = 81, linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = c(5.5, 10.5, 20.5, 25.5),
             size = .5,
             linetype ="dotted") +
  geom_vline(xintercept = 15.5,
             size = .5,
             color = "red") +
  scale_y_continuous(breaks = seq(40,120,5),
                     sec.axis = sec_axis(~., breaks = seq(40,120,5))) +
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black',
           x = -Inf, xend = Inf, size = 1) +
  annotate("label", x = seq(3,28,5), y = rep(42.5,6),
           label = divisions)

tiff("/plots/1000ros.tiff", units="cm", width=29.33, height=16.5, res=300)
ggpar(ggrosandpos)
dev.off()

with(many.results.2, smoothScatter(proj_TOT_RDg,
                                   TOTWINS_FIP,
                                   xlab = "Differenza punti/partita",
                                   ylab = "Vittorie"))

prediction.summary.rp <- as.data.frame(matrix(nrow = 30, ncol = 9))


for(i in 1:30){
  
  prediction.summary.rp[i,] <- 
    cbind(tvec[i],
          t(summary(
            many.results.2[many.results.2$Team==tvec[i],2])),
          t(quantile(many.results.2[many.results.2$Team==tvec[i],2], c(.025, .975)))
    )
  
  
}


summary(many.results.2[many.results.2$PS==1,"TOTWINS"])
summary(many.results.2[many.results.2$PS==1 & 
                         many.results.2$lgID=="AL","TOTWINS"])
summary(many.results.2[many.results.2$PS==1 & 
                         many.results.2$lgID=="NL","TOTWINS"])

varianze <- 1:30
devstand <- 1:30
absmean <- 1:30

for(i in 1:30){
  
  varianze[i] <- var(many.results.fs.2[many.results.fs.2$Team==tvec[i],2])
  devstand[i] <- sqrt(varianze[i])
  
}

for(i in 1:30){
  
  absmean[i] <- MeanAD(many.results.fs.2[many.results.fs.2$Team==tvec[i],2])
  
}

prediction.summary.rp[,2:9] <- as.numeric(unlist(prediction.summary.rp[,2:9]))

prediction.summary.rp$pctrange <- prediction.summary.rp[,9] - prediction.summary.rp[,8]
prediction.summary.rp$range <- prediction.summary.rp[,7] - prediction.summary.rp[,2]

prediction.summary.rp[,8:11] <- round(prediction.summary.rp[,8:11],0)

colnames(prediction.summary.rp) <- c("Squadra", "Min", "1st Qu.", "Mediana",
                                     "Media", "3rd Qu.", "Max", "2.5%",
                                     "97.5%", "Range 2.5%-97.5%", "Range Min-Max")

prediction.summary.rp <- prediction.summary.rp[,c(1,2,8,3:6,9,7,10,11)]
prediction.summary.rp <- prediction.summary.rp[ord.3,]

for(i in seq(1, 30, by = 5)){
  
  prediction.summary.rp[i:(i+4),] <- prediction.summary.rp[i:(i+4),][order(
    prediction.summary.rp[i:(i+4),6], decreasing = T),]
  
}

rownames(prediction.summary.rp) <- NULL

View(prediction.summary.rp)

htmlTable(prediction.summary.rp)

colMeans(prediction.summary.rp[,2:11])

playoff.summary.ros <- as.data.frame(matrix(nrow = 30, ncol = 6))


for(i in 1:30){
  
  playoff.summary.ros[i,] <- 
    cbind(
      tvec[i],
      t(
        colMeans(
          many.results.2[many.results.2$Team==tvec[i],6:10]
        )
      )
    )
}

colnames(playoff.summary.ros) <- c("Team", "Postseason", "Division Win",
                                   "DS Win", "CS Win", "World Series Champions")

for(i in 2:6){
  playoff.summary.ros[,i] <- as.numeric(unlist(playoff.summary.ros[,i]))
}

ord.3

playoff.summary.ros <- playoff.summary.ros[ord.3,]
rownames(playoff.summary.ros) <- NULL

View(playoff.summary.ros)

htmlTable(playoff.summary.ros)

playoff.summary.ros <- merge(playoff.summary.ros, standings19[,c(1,22)])

playoff.summary.ros$proj_TOT_RDg
playoff.summary.ros$`World Series Champions`

tiff("/plots/ability.tiff", units="cm", width=29.33, height=16.5, res=300)

ggplot(playoff.summary.ros, aes(x = proj_TOT_RDg,
                                y = `World Series Champions`,)) +
  geom_point(alpha = .2, size = .75, color = "firebrick1") +
  geom_smooth(aes(color = "Vittoria del Campionato (WS)"), method = "loess", size = 1, se = F) +
  xlab("Talento della squadra (Differenza Punti / Partita)") + ylab("Probabilità") +
  geom_point(aes(x = proj_TOT_RDg, y = `Postseason`),
             alpha = .2, size = .75, color = "blue") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `Postseason`, color = "Qualificazione Postseason"),
              method = "loess", size = 1, se = F)+
  geom_point(aes(x = proj_TOT_RDg, y = `Division Win`),
             alpha = .2, size = .75, color = "seagreen3") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `Division Win`, color = "Vittoria Divisione"),
              method = "loess", size = 1, se = F) +
  geom_point(aes(x = proj_TOT_RDg, y = `DS Win`),
             alpha = .2, size = .75, color = "gold") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `DS Win`, color = "Vittoria Division Series"),
              method = "loess", size = 1, se = F) +
  geom_point(aes(x = proj_TOT_RDg, y = `CS Win`),
             alpha = .2, size = .75, color = "darkorange") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `CS Win`, color = "Vittoria della Lega (CS)"),
              method = "loess", size = 1, se = F) +
  scale_colour_manual(name="Risultato",
                      limits=c("Qualificazione Postseason",
                               "Vittoria Divisione",
                               "Vittoria Division Series",
                               "Vittoria della Lega (CS)",
                               "Vittoria del Campionato (WS)"),
                      values=c(`Qualificazione Postseason`="blue",
                               `Vittoria Divisione`="seagreen3",
                               `Vittoria Division Series`="gold",
                               `Vittoria della Lega (CS)` = "darkorange",
                               `Vittoria del Campionato (WS)` = "firebrick1")) +
  geom_vline(xintercept = 0, col = "red", linetype = "dashed",
             alpha = .5) +
  geom_vline(xintercept =  0.5030864, col = "red", linetype = "dashed",
             alpha = .5) +
  annotate("label", x=-0.75, y=1,label="<50%", size=5) +
  annotate("label", x=.25, y=1,label="50%-75%", size=5) +
  annotate("label", x=0.85, y=1,label=">75%", size=5) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text =element_text(size=14),
        legend.title = element_text(size=14,face="bold"))

dev.off()

summary(teams1518$RDg)

### FULL Season ####


many.results.fs.2 <- NULL
pb <- txtProgressBar(min = 0, max = 1000, style = 3)

for(j in 1:1000){
  
  many.results.fs.2 <- rbind(many.results.fs.2,
                             one.sim.fullandpost())
  setTxtProgressBar(pb, j)
}

close(pb)

summary(many.results.fs.2)
colSums(many.results.fs.2[,(6:10)])

many.results.fs.2 <- merge(many.results.fs.2, standings19[,c(1,22)])


ggfullandpos <- ggscatter(many.results.fs.2, "Team", "TOTWINS",
                          color = "TOTWINS", alpha = "TOTWINS",
                          size = 3,
                          ylab = "Predicted Total Wins") +
  scale_color_viridis() +
  scale_alpha(range = c(0.1,0.2)) +
  grids(color = "grey92") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,),
        axis.title = element_text(face = "bold", size = 14)) +
  scale_x_discrete(labels = tvec.extended) +
  geom_boxplot(aes(fill= DIV), outlier.shape = NA) +
  geom_hline(yintercept = 81, linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = c(5.5, 10.5, 20.5, 25.5),
             size = .5,
             linetype ="dotted") +
  geom_vline(xintercept = 15.5,
             size = .5,
             color = "red") +
  scale_y_continuous(breaks = seq(40,120,5),
                     sec.axis = sec_axis(~., breaks = seq(40,120,5))) +
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black',
           x = -Inf, xend = Inf, size = 1) +
  annotate("label", x = seq(3,28,5), y = rep(42.5,6),
           label = divisions)

ggpar(ggfullandpos)

with(many.results.fs.2, smoothScatter(proj_TOT_RDg,
                                      TOTWINS_ERA,
                                      xlab = "Differenza punti/partita",
                                      ylab = "Vittorie"))

prediction.summary.fs.2 <- as.data.frame(matrix(nrow = 30, ncol = 9))


for(i in 1:30){
  
  prediction.summary.fs.2[i,] <- 
    cbind(tvec[i],
          t(summary(
            many.results.fs.2[many.results.fs.2$Team==tvec[i],2])),
          t(quantile(many.results.fs.2[many.results.fs.2$Team==tvec[i],2],
                     c(.025, .975)))
    )
  
  
}

prediction.summary.fs.2[,2:9] <- as.numeric(unlist(prediction.summary.fs.2[,2:9]))

prediction.summary.fs.2$pctrange <- prediction.summary.fs.2[,9]-prediction.summary.fs.2[,8]
prediction.summary.fs.2$range <- prediction.summary.fs.2[,7] - prediction.summary.fs.2[,2]

prediction.summary.fs.2[,8:11] <- round(prediction.summary.fs.2[,8:11],0)

colnames(prediction.summary.fs.2) <- c("Squadra", "Min", "1st Qu.", "Mediana",
                                       "Media", "3rd Qu.", "Max", "2.5%",
                                       "97.5%", "Range 2.5%-97.5%", "Range Min-Max")

prediction.summary.fs.2 <- prediction.summary.fs.2[,c(1,2,8,3:6,9,7,10,11)]

prediction.summary.fs.2 <- prediction.summary.fs.2[ord.3,]

for(i in seq(1, 30, by = 5)){
  
  prediction.summary.fs.2[i:(i+4),] <- prediction.summary.fs.2[i:(i+4),][order(
    prediction.summary.fs.2[i:(i+4),6], decreasing = T),]
  
}

rownames(prediction.summary.fs.2) <- NULL

View(prediction.summary.fs.2)


playoff.summary.fs <- as.data.frame(matrix(nrow = 30, ncol = 6))


for(i in 1:30){
  
  playoff.summary.fs[i,] <- 
    cbind(
      tvec[i],
      t(
        colMeans(
          many.results.fs.2[many.results.fs.2$Team==tvec[i],6:10]
        )
      )
    )
}

colnames(playoff.summary.fs) <- c("Team", "Postseason", "Division Win",
                                  "DS Win", "CS Win", "World Series Champions")

for(i in 2:6){
  playoff.summary.fs[,i] <- as.numeric(unlist(playoff.summary.fs[,i]))
}

playoff.summary.fs <- playoff.summary.fs[ord.3,]
rownames(playoff.summary.fs) <- NULL

View(playoff.summary.fs)


playoff.summary.fs <- merge(playoff.summary.fs, standings19[,c(1,22)])

playoff.summary.fs$proj_TOT_RDg
playoff.summary.fs$`World Series Champions`

playoff.summary.fs$teamquality <-
  ifelse(playoff.summary.fs$proj_TOT_RDg < -0.5524691,
         0,
         ifelse(playoff.summary.fs$proj_TOT_RDg < -0.1987654,
                1,
                ifelse(playoff.summary.fs$proj_TOT_RDg < 0.1794571,
                       2,
                       ifelse(playoff.summary.fs$proj_TOT_RDg < 0.5030864,
                              3,
                              4))))
playoff.summary.fs$teamquality <- as.factor(playoff.summary.fs$teamquality)

levels(playoff.summary.fs$teamquality) <- c("Scarsa",
                                            "Nella media",
                                            "Sopra la media",
                                            "Ottima")


ggplot(playoff.summary.fs, aes(x = proj_TOT_RDg,
                               y = `World Series Champions`,)) +
  geom_point(alpha = .1, size = .75, color = "firebrick1") +
  geom_smooth(aes(color = "Vittoria WS"), method = "loess", size = 1, se = F) +
  xlab("Differenza Punti / Partita") + ylab("Probabilità") +
  geom_point(aes(x = proj_TOT_RDg, y = `Postseason`),
             alpha = .1, size = .75, color = "blue") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `Postseason`, color = "Postseason"),
              method = "loess", size = 1, se = F)+
  geom_point(aes(x = proj_TOT_RDg, y = `Division Win`),
             alpha = .1, size = .75, color = "seagreen3") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `Division Win`, color = "Vittoria Division"),
              method = "loess", size = 1, se = F) +
  geom_point(aes(x = proj_TOT_RDg, y = `DS Win`),
             alpha = .1, size = .75, color = "gold") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `DS Win`, color = "Vittoria DS"),
              method = "loess", size = 1, se = F) +
  geom_point(aes(x = proj_TOT_RDg, y = `CS Win`),
             alpha = .1, size = .75, color = "darkorange") + 
  geom_smooth(aes(x = proj_TOT_RDg, y = `CS Win`, color = "Vittoria CS"),
              method = "loess", size = 1, se = F) +
  geom_vline(xintercept = c(-0.1987654,  0.1794571,  0.5030864),
             linetype = "dotted", size = .5, alpha = .5) +
  scale_colour_manual(name="Risultato",
                      limits=c("Postseason", "Vittoria Division", "Vittoria DS",
                               "Vittoria CS", "Vittoria WS"),
                      values=c(Postseason="blue", `Vittoria Division`="seagreen3",
                               `Vittoria DS`="gold", `Vittoria CS` = "darkorange",
                               `Vittoria WS` = "firebrick1"))+
  geom_rect(aes(xmin = -Inf, xmax = -0.1987654, ymin = -Inf,
                ymax = +Inf),
            fill = "red", alpha = .005) +
  geom_rect(aes(xmin = -0.1987654, xmax = 0.1794571, ymin = -Inf,
                ymax = +Inf),
            fill = "yellow", alpha = .005) +
  geom_rect(aes(xmin = 0.1794571, xmax = 0.5030864, ymin = -Inf,
                ymax = +Inf),
            fill = "green", alpha = .005) +
  geom_rect(aes(xmin = 0.5030864, xmax = +Inf, ymin = -Inf,
                ymax = +Inf),
            fill = "blue", alpha = .005)


summary(teams1518$RDg)

quantile(teams1518$RDg, c(.25, .40, .60, .75))
(-0.5524691-0.1987654)/2
(-0.1987654+0.1794571)/2
(0.1794571+0.5030864)/2