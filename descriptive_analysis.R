#### DESCRIPTIVE ANALYSIS ####

# This script includes all the descriptive preliminary analysis on
# the variables of the dataset.

# Packages required

require(htmlTable)
require(dplyr)
require(ggplot2)
require(ggpubr)

# Function for creating summary tables in HTML

table.summary <- function(dat, arr = 4, non.arr = NULL){
  
  dat <- as.data.frame(dat)
  outp <- as.data.frame(matrix(nrow = 8, ncol = ncol(dat)))
  
  for(i in 1:ncol(dat)){
    d <- round(summary(dat[,i]),arr)
    names(d) <- NULL
    outp[1:6,i] <- d
    outp[7,i] <- round(sqrt(var(dat[,i])), arr)
    outp[8,i] <- round(sqrt(var(dat[,i]))/abs(mean(dat[,i])), arr)
  }
  
  colnames(outp) <- colnames(dat)
  rownames(outp) <- c("Minimo", "1° Quartile", "Mediana",
                      "Media", "3° Quartile", "Massimo", "D. Standard", "C.V.")
  
  ifelse(length(non.arr==ncol(dat)),
         outp <- outp,
         outp <- txtRound(outp, scientific = F, digits = arr,
                          excl.cols = non.arr))
  return(htmlTable(outp))
}

starters2015$year <- rep(2015, nrow(starters2015))
starters2016$year <- rep(2016, nrow(starters2016))
starters2017$year <- rep(2017, nrow(starters2017))
starters2018$year <- rep(2018, nrow(starters2018))

starters1518 <- rbind(starters2015, starters2016, starters2017, starters2018)

pitching2015 <- merge(pitching2015, people[,c(1,25)],
                      by.x = "playerID", by.y = "playerID")

pitching2016 <- merge(pitching2016, people[,c(1,25)],
                      by.x = "playerID", by.y = "playerID")

pitching2017 <- merge(pitching2017, people[,c(1,25)],
                      by.x = "playerID", by.y = "playerID")

pitching2018 <- merge(pitching2018, people[,c(1,25)],
                      by.x = "playerID", by.y = "playerID")

starters1518 %>% summarise(weighted.mean(ERA, IP))
starters1518 %>% summarise(weighted.mean(FIP, IP))
starters1518 %>% summarise(weighted.mean(xFIP, IP))
quantile(compdata$ERA_hs, c(.05, .95))
quantile(compdata$ERA_vs, c(.05, .95))

write.csv(starters1518$IP, "/data/IP.csv")
ipsp <- read_excel("/data/ipsp.xlsx", col_types = c("numeric", 
                                              "numeric", "numeric", "numeric"))

starters1518$IPdec <- ipsp$IPdec
starters1518$IPouts <- ipsp$Ipouts

sum(starters1518$IPouts==starters1518$IPdec*3) == nrow(starters1518)

ERAIP <- weight(starters1518$ERA, starters1518$IP, digits = 5)
FIPIP <- weight(starters1518$FIP, starters1518$IP, digits = 5)
xFIPIP <- weight(starters1518$xFIP, starters1518$IP, digits = 5)


ERAIP2 <- weight(starters1518$ERA, starters1518$IPouts)
FIPIP2 <- weight(starters1518$FIP, starters1518$IPouts)
xFIPIP2 <- weight(starters1518$xFIP, starters1518$IPouts)


table.summary(cbind(starters1518$ERA, starters1518$FIP, starters1518$xFIP))
table.summary(cbind(ERAIP,FIPIP,xFIPIP))
table.summary(cbind(ERAIP2,FIPIP2,xFIPIP2))

summary(starters1518[starters1518$ERA>15, "IP"])
summary(starters1518[starters1518$ERA<1, "IP"])

summary(starters1518[starters1518$FIP>15, "IP"])
summary(starters1518[starters1518$FIP<1, "IP"])

summary(starters1518[starters1518$xFIP>15, "IP"])
summary(starters1518[starters1518$xFIP<1, "IP"])

summary(starters1518$IP)
quantile(starters1518$ERA, .90)
quantile(starters1518$FIP, .90)
quantile(starters1518$xFIP, .90)

sum(starters1518$IP)/sum(starters1518$GS)

#### Histograms and Boxplots ####

#### Starting Pitchers ERA, FIP, xFIP ###

tiff("/plots/histerasp.tiff", units="cm", width=16, height=11, res=300)
par(mfrow=c(1,2))
hist(ERAIP2, breaks = 50, freq = F,
     col = "cornflowerblue",
     xlim = c(0,200),
     ylim = c(0,0.2),
     xlab = "ERA Lanciatori partenti",
     ylab = "Densità",
     main = "")
hist(ERAIP2, xlim = c(0,10), breaks = 500, freq = F, col = "cornflowerblue",
     xlab = "ERA Lanciatori partenti",
     ylab = "Densità",
     main = "")
mtext("Istogramma della ERA pesata per inning lanciati
      dei lanciatori partenti di MLB (2015-2018)",
      side = 3, line = -2.5, outer = T,
      cex = 1.2, font = 2)
dev.off()


tiff("/plots/histfipsp.tiff", units="cm", width=16, height=11, res=300)
par(mfrow=c(1,2))
hist(FIPIP2, breaks = 20, xlim = c(0,40),
     ylim = c(0,0.3),
     freq = F, col = "darkorange",
     xlab = "FIP Lanciatori partenti",
     ylab = "Densità",
     main = "")
hist(FIPIP2, xlim = c(0,10), ylim = c(0,0.5),
     breaks = 100, freq = F,
     col = "darkorange",
     xlab = "FIP Lanciatori partenti",
     ylab = "Densità",
     main = "")
mtext("Istogramma della FIP pesata per inning lanciati
      dei lanciatori partenti di MLB (2015-2018)",
      side = 3, line = -2.5, outer = T,
      cex = 1.2, font = 2)
dev.off()

tiff("/plots/histxfipsp.tiff", units="cm", width=16, height=11, res=300)
par(mfrow=c(1,2))
hist(xFIPIP2, breaks = 20, ylim = c(0,0.4),
     freq = F, col = "limegreen",
     xlab = "xFIP Lanciatori partenti",
     ylab = "Densità",
     main = "")
hist(xFIPIP2, breaks = 100, ylim = c(0,0.6),
     xlim = c(0,10),
     freq = F, col = "limegreen",
     xlab = "xFIP Lanciatori partenti",
     ylab = "Densità",
     main = "")
mtext("Istogramma della xFIP pesata per inning lanciati
      dei lanciatori partenti di MLB (2015-2018)",
      side = 3, line = -2.5, outer = T,
      cex = 1.2, font = 2)
dev.off()

summary(compdata[,c(13,16)])

summary(compdata[,c(12,15)])

summary(compdata[,c(38,39)])
hist(starters1518$ERA, breaks = 500, xlim = c(0,10))
hist(compdata$ERA_hs, breaks = 50, xlim = c(0,10))
hist(compdata$ERA_vs, breaks = 300, xlim = c(0,10))

hist(starters1518[, (16)], breaks = 50)
hist(starters1518[, (17)], breaks = 50)
hist(starters1518[, (18)], breaks = 50)

summary(starters1518[starters1518$IP>20, c(16,17,18)])

hist(starters1518[starters1518$IP>20, (16)], breaks = 50)
hist(starters1518[starters1518$IP>20, (17)], breaks = 50)
hist(starters1518[starters1518$IP>20, (18)], breaks = 50)


summary(teams1518$`wRC plus`)


summary(teams1518[,56])

table.summary(dat = teams1518[,c(50,51,56)], arr = 4, non.arr = 3)
table.summary(dat = teams1518$wRC_plus, non.arr = 1)

#### Relief Pitchers ERA, FIP, xFIP ###

par(mfrow=c(1,1))

tiff("/plots/plot1.tiff", units="cm", width=16, height=11, res=300)
hist(teams1518$RDg, breaks = 10, col = "firebrick1",
     main = "Istogramma della differenza punti per partita (RDg)
     per le squadre di MLB (2015-2018)", xlab ="RDg", ylab = "Frequenza",
     ylim=c(0,30))
dev.off()
tiff("/plots/plot2.tiff", units="cm", width=16, height=11, res=300)
hist(teams1518$RSg, breaks = 10, col = "seagreen",
     main = "Istogramma dei punti segnati per partita (RSg)
     per le squadre di MLB (2015-2018)", xlab ="RSg", ylab = "Frequenza",
     ylim = c(0,25))
dev.off()
tiff("/plots/plot3.tiff", units="cm", width=16, height=11, res=300)
hist(teams1518$`wRC+`, breaks = 10, 
     col = "gold",
     main = "Istogramma dei wRC+
     per le squadre di MLB (2015-2018)", xlab ="wRC+", ylab = "Frequenza",
     ylim = c(0,35))
dev.off()

compdata$W <- as.factor(compdata$W)


bxpera1 <- ggplot(compdata, aes(W, ERA_hs)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("ERA partente casa") +
  theme(legend.position = "top")
bxpera1 <- ggpar(bxpera1, ylim = c(1.5,7.5))

bxpera2 <- ggplot(compdata, aes(W, ERA_vs)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("ERA partente trasferta") +
  theme(legend.position = "top")
bxpera2 <- ggpar(bxpera2, ylim = c(1.5,7.5))


tiff("/plots/boxplotera.tiff", units="cm", width=16, height=11, res=300)
bxp.era <- ggarrange(bxpera1,bxpera2, ncol = 2, nrow = 1)
annotate_figure(bxp.era,
                top = text_grob("Distribuzione della ERA del lanciatore partente
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()


bxpfip1 <- ggplot(compdata, aes(W, FIP_hs)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("FIP partente casa") +
  theme(legend.position = "top")
bxpfip1 <- ggpar(bxpfip1, ylim = c(2,7))

bxpfip2 <- ggplot(compdata, aes(W, FIP_vs)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("FIP partente trasferta") +
  theme(legend.position = "top")
bxpfip2 <- ggpar(bxpfip2, ylim = c(2,7))


tiff("/plots/boxplotfip.tiff", units="cm", width=16, height=11, res=300)
bxp.fip <- ggarrange(bxpfip1,bxpfip2, ncol = 2, nrow = 1)
annotate_figure(bxp.fip,
                top = text_grob("Distribuzione della FIP del lanciatore partente
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()

bxpxfip1 <- ggplot(compdata, aes(W, xFIP_hs)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("xFIP partente casa") +
  theme(legend.position = "top")
bxpxfip1 <- ggpar(bxpxfip1, ylim = c(2,6.5))

bxpxfip2 <- ggplot(compdata, aes(W, xFIP_vs)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("xFIP partente trasferta") +
  theme(legend.position = "top")
bxpxfip2 <- ggpar(bxpxfip2, ylim = c(2,6.5))


tiff("/plots/boxplotxfip.tiff", units="cm", width=16, height=11, res=300)
bxp.xfip <- ggarrange(bxpxfip1,bxpxfip2, ncol = 2, nrow = 1)
annotate_figure(bxp.xfip,
                top = text_grob("Distribuzione della xFIP del lanciatore partente
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()

#### home and visiting team RDG ####

bxp3 <- ggplot(compdata, aes(W, RDg_hteam)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("RDg squadra in casa") +
  theme(legend.position = "top")
bxp3

bxp4 <- ggplot(compdata, aes(W, RDg_vteam)) +
  geom_boxplot(aes(group = W,fill = W),outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("RDg squadra in trasferta") +
  theme(legend.position = "top")

ggpar(bxp4)

tiff("/plots/boxplot1.tiff", units="cm", width=16, height=11, res=300)
bxp.1 <- ggarrange(bxp3,bxp4, ncol = 2, nrow = 1)
annotate_figure(bxp.1,
                top = text_grob("Distribuzione della differenza punti per partita (RDg)
per la squadra in casa e per la squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()


bxprs1 <- ggplot(compdata, aes(W, RSg_hteam)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("RSg squadra in casa") +
  theme(legend.position = "top")
ggpar(bxprs1)

bxprs2 <- ggplot(compdata, aes(W, RSg_vteam)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("RSg squadra in trasferta") +
  theme(legend.position = "top")
ggpar(bxprs2)

tiff("/plots/boxplot2.tiff", units="cm", width=16, height=11, res=300)
bxp.2 <- ggarrange(bxprs1,bxprs2, ncol = 2, nrow = 1)
annotate_figure(bxp.2,
                top = text_grob("Distribuzione dei punti segnati per partita (RSg)
per la squadra in casa e per la squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()

bxpwrc1 <- ggplot(compdata, aes(W, wRC_plus_hteam)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("wRC+ squadra in casa") +
  theme(legend.position = "top")
ggpar(bxpwrc1)

bxpwrc2 <- ggplot(compdata, aes(W, wRC_plus_vteam)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("wRC+ squadra in trasferta") +
  theme(legend.position = "top")
ggpar(bxpwrc2)

tiff("/plots/boxplot3.tiff", units="cm", width=16, height=11, res=300)
bxp.3 <- ggarrange(bxpwrc1,bxpwrc2, ncol = 2, nrow = 1)
annotate_figure(bxp.3,
                top = text_grob("Distribuzione dei punti creati (wRC+)
per la squadra in casa e per la squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()


###

bxprel1 <-ggboxplot(compdata, "W", "FIP_bullpen_h", fill = "W")
ggpar(bxprel1)

bxprel2 <-ggboxplot(compdata, "W", "FIP_bullpen_v", fill = "W")
ggpar(bxprel2)

bxprel3 <-ggboxplot(compdata, "W", "xFIP_bullpen_h", fill = "W")
ggpar(bxprel3)

bxprel4 <-ggboxplot(compdata, "W", "xFIP_bullpen_v", fill = "W")
ggpar(bxprel4)


bxpform1 <- ggboxplot(compdata, "W", "RDgl5_h", fill = "W")
ggpar(bxpform1)

bxpform2 <- ggboxplot(compdata, "W", "RDgl5_v", fill = "W")
ggpar(bxpform2)

bxpform3 <- ggboxplot(compdata, "W", "RDgl10_h", fill = "W")
ggpar(bxpform3)

bxpform4 <- ggboxplot(compdata, "W", "RDgl10_v", fill = "W")
ggpar(bxpform4)


bxpform5 <- ggboxplot(compdata, "W", "Wl10_h", fill = "W")
ggpar(bxpform5)

bxpform6 <- ggboxplot(compdata, "W", "Wl10_v", fill = "W")
ggpar(bxpform6)

hist(teams2015$RDg)
hist(teams2016$RDg)
hist(teams2017$RDg)
hist(teams2018$RDg)

hist(teams1518$RDg)
ggdensity(teams1518, "RDg")

ggqqplot(teams1518$RDg)
shapiro.test(teams1518$RDg)


rdgh4cat <- ifelse(compdata$RDg_hteam< -0.562, 0,
                   ifelse(compdata$RDg_hteam<0,1,
                          ifelse(compdata$RDg_hteam<0.513,2,3)))

rdgv4cat <- ifelse(compdata$RDg_vteam< -0.562,0,
                   ifelse(compdata$RDg_vteam<0,1,
                          ifelse(compdata$RDg_vteam<0.513,2,3)))

rdgh4cat <- as.factor(rdgh4cat)
levels(rdgh4cat) <- c("<0.562", "[0.562-0)", "[0-0.513)", ">=0.513")
rdgv4cat <- as.factor(rdgv4cat)
levels(rdgv4cat) <- c("<0.562", "[0.562-0)", "[0-0.513)", ">=0.513")


addmargins(table(compdata$W, rdgh4cat))
prop.table(table(compdata$W, rdgh4cat),2)

addmargins(table(compdata$W, rdgv4cat))
prop.table(table(compdata$W, rdgv4cat),2)

summary(compdata$ERA_hs)
summary(compdata$ERA_vs)

erahs4cat <- ifelse(compdata$ERA_hs<3.51,0,
                    ifelse(compdata$ERA_hs<4.18,1,
                           ifelse(compdata$ERA_hs<4.9,2,3)))

eravs4cat <- ifelse(compdata$ERA_vs<3.51,0,
                    ifelse(compdata$ERA_vs<4.18,1,
                           ifelse(compdata$ERA_vs<4.9,2,3)))

hist(compdata$ERA_hs[compdata$ERA_hs<10], breaks = 50)
hist(compdata$ERA_vs[compdata$ERA_vs<10], breaks = 50)


ggdensity(compdata10$ERA_vs)
ggqqplot(compdata10$ERA_vs)

ggdensity(compdata10$ERA_hs)
ggqqplot(compdata10$ERA_hs)

erahs4cat <- as.factor(erahs4cat)
levels(erahs4cat) <- c("<3.51", "[3.51-4.18)", "[4.18-4.90)", ">=4.90")
addmargins(table(compdata$W, erahs4cat))
prop.table(table(compdata$W, erahs4cat),2)

eravs4cat <- as.factor(eravs4cat)
levels(eravs4cat) <- c("<3.51", "[3.51-4.18)", "[4.18-4.90)", ">=4.90")
addmargins(table(compdata$W, eravs4cat))
prop.table(table(compdata$W, eravs4cat),2)

table.summary(cbind(relief1518$ERA, relief1518$FIP, relief1518$xFIP))

write.csv(relief1518$IP, "/data/reliefIP.csv")
iprp <- read_excel("/data/iprp.xlsx", col_types = c("numeric", 
                                              "numeric", "numeric", "numeric"))

relief1518$IPdec <- iprp$IPdec
relief1518$IPouts <- iprp$IPouts

cbind(relief1518$IP, relief1518$IPdec, relief1518$IPouts)

ERAb <- weight(relief1518$ERA, relief1518$IPouts)
FIPb <- weight(relief1518$FIP, relief1518$IPouts)
xFIPb <- weight(relief1518$xFIP, relief1518$IPouts)

table.summary(cbind(ERAb, FIPb, xFIPb))


par(mfrow=c(1,1))

tiff("/plots/histerab.tiff", units="cm", width=16, height=11, res=300)
hist(ERAb, freq = F,
     breaks = 20,
     col = "cornflowerblue",
     xlim = c(2,6),
     ylim = c(0,0.8),
     xlab = "ERA Bullpen",
     ylab = "Densità",
     main = "Istogramma della ERA pesata per inning lanciati
    dei bullpen delle squadre di MLB (2015-2018)")
dev.off()


tiff("/plots/histfipb.tiff", units="cm", width=16, height=11, res=300)
hist(FIPb, breaks = 10, freq = F,
     col = "darkorange",
     xlim = c(3,5.5),
     #ylim = c(0,0.8),
     xlab = "FIP Bullpen",
     ylab = "Densità",
     main = "Istogramma della FIP pesata per inning lanciati
    dei bullpen delle squadre di MLB (2015-2018)")
dev.off()

tiff("/plots/histxfipb.tiff", units="cm", width=16, height=11, res=300)
hist(xFIPb, breaks = 10, freq = F,
     col = "limegreen",
     xlim = c(3,5.5),
     #ylim = c(0,0.8),
     xlab = "xFIP Bullpen",
     ylab = "Densità",
     main = "Istogramma della xFIP pesata per inning lanciati
    dei bullpen delle squadre di MLB (2015-2018)")
dev.off()




bxpera3 <- ggplot(compdata, aes(W, ERA_bullpen_h)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("ERA bullpen casa") +
  theme(legend.position = "top")

bxpera4 <- ggplot(compdata, aes(W, ERA_bullpen_v)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("ERA bullpen trasferta") +
  theme(legend.position = "top")


tiff("/plots/boxploterabullpen.tiff", units="cm", width=16, height=11, res=300)
bxp.erab <- ggarrange(bxpera3,bxpera4, ncol = 2, nrow = 1)
annotate_figure(bxp.erab,
                top = text_grob("Distribuzione della ERA dell'insieme dei rilievi
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()


bxpfip3 <- ggplot(compdata, aes(W, FIP_bullpen_h)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("FIP bullpen casa") +
  theme(legend.position = "top")

bxpfip4 <- ggplot(compdata, aes(W, FIP_bullpen_v)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("FIP bullpen trasferta") +
  theme(legend.position = "top")


tiff("/plots/boxplotfipbullpen.tiff", units="cm", width=16, height=11, res=300)
bxp.fipb <- ggarrange(bxpfip3,bxpfip4, ncol = 2, nrow = 1)
annotate_figure(bxp.fipb,
                top = text_grob("Distribuzione della FIP dell'insieme dei rilievi
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()


bxpxfip3 <- ggplot(compdata, aes(W, xFIP_bullpen_h)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("FIP bullpen casa") +
  theme(legend.position = "top")

bxpxfip4 <- ggplot(compdata, aes(W, xFIP_bullpen_v)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("FIP bullpen trasferta") +
  theme(legend.position = "top")


tiff("/plots/boxplotxfipbullpen.tiff", units="cm", width=16, height=11, res=300)
bxp.xfipb <- ggarrange(bxpxfip3,bxpxfip4, ncol = 2, nrow = 1)
annotate_figure(bxp.xfipb,
                top = text_grob("Distribuzione della xFIP dell'insieme dei rilievi
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()
###


summary(compdata$RDgl10_h)
summary(compdata$RDgl10_v)
summary(teams1518$RDg)

hist(compdata$RDgl10_h)
hist(compdata$RDgl10_v)

summary(compdata$RDgl5_h)
summary(compdata$RDgl5_v)

hist(compdata$RDgl5_h)
hist(compdata$RDgl5_v)

summary(compdata$Wl10_h)
summary(compdata$Wl10_v)

hist(compdata$Wl10_h)
hist(compdata$Wl10_v)

mavg <- compdata[compdata$hteam=="BOS"|compdata$vteam=="BOS",]
mavg <- mavg[order(mavg$date),]
rownames(mavg) <- NULL

mavgbos <- 1:nrow(mavg)
puntibos <- 1:nrow(mavg)

for(i in 1:nrow(mavg)){
  mavgbos[i] <- ifelse(
    mavg$hteam[i] == "BOS",
    mavg$RDgl10_h[i],
    mavg$RDgl10_v[i]
  )
  puntibos[i] <-  ifelse(
    mavg$hteam[i] == "BOS",
    (mavg$hscore[i] - mavg$vscore[i]),
    (mavg$vscore[i] - mavg$hscore[i])
  )
}

plot(puntibos[1:162], type = "l")
lines(mavgbos[1:162], type = "l", col = "red")

summary(puntibos)
summary(mavgbos)


#


bxprdg1 <- ggplot(compdata, aes(W, RDgl10_h)) +
  geom_boxplot(aes(group = W,fill = W),
               outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("RDg 10 partite precedenti casa") +
  theme(legend.position = "top")
bxprdg1 <- ggpar(bxprdg1, ylim = c(-4,4))

bxprdg2 <- ggplot(compdata, aes(W, RDgl10_v)) +
  geom_boxplot(aes(group = W,fill = W),
               outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("RDg 10 partite precedenti trasferta") +
  theme(legend.position = "top")
bxprdg2 <- ggpar(bxprdg2, ylim = c(-4,4))


tiff("/plots/boxplotrdg10.tiff", units="cm", width=16, height=11, res=300)
bxp.rdg10 <- ggarrange(bxprdg1,bxprdg2, ncol = 2, nrow = 1)
annotate_figure(bxp.rdg10,
                top = text_grob("Distribuzione della RDg nelle 10 precedenti partite
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()


bxprdg3 <- ggplot(compdata, aes(W, RDgl5_h)) +
  geom_boxplot(aes(group = W,fill = W),
               outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("RDg 5 partite precedenti casa") +
  theme(legend.position = "top")
bxprdg3 <- ggpar(bxprdg3, ylim = c(-5.5,5.5))

bxprdg4 <- ggplot(compdata, aes(W, RDgl5_v)) +
  geom_boxplot(aes(group = W,fill = W),
               outlier.shape = NA) +
  xlab("Vittoria della squadra di casa") +
  ylab("RDg 5 partite precedenti trasferta") +
  theme(legend.position = "top")
bxprdg4 <- ggpar(bxprdg4, ylim = c(-5.5,5.5))


tiff("/plots/boxplotrdg5.tiff", units="cm", width=16, height=11, res=300)
bxp.rdg5 <- ggarrange(bxprdg3,bxprdg4, ncol = 2, nrow = 1)
annotate_figure(bxp.rdg5,
                top = text_grob("Distribuzione della RDg nelle 5 precedenti partite
della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()


bxpw1 <- ggplot(compdata, aes(W, Wl10_h)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("W pct 10 partite precedenti casa") +
  theme(legend.position = "top")

bxpw2 <- ggplot(compdata, aes(W, Wl10_v)) +
  geom_boxplot(aes(group = W,fill = W)) +
  xlab("Vittoria della squadra di casa") +
  ylab("W pct 10 partite precedenti trasferta") +
  theme(legend.position = "top")


tiff("/plots/boxplotwl10.tiff", units="cm", width=16, height=11, res=300)
bxp.w10 <- ggarrange(bxpw1,bxpw2, ncol = 2, nrow = 1)
annotate_figure(bxp.w10,
                top = text_grob("Distribuzione della percentuale di vittorie nelle 10
precedenti partite della squadra in casa e della squadra in trasferta
in funzione dei valori assunti da Y",
                                face = "bold",
                                size = 14))
dev.off()

#### Response Variable ####

nrow(starters1518[starters1518$ERA>10,])/nrow(starters1518)
nrow(starters1518[starters1518$FIP>10,])/nrow(starters1518)
nrow(starters1518[starters1518$xFIP>10,])/nrow(starters1518)

nrow(starters1518)



htmlTable(rbind(
  addmargins(table(compdata$W)),
  c(round(prop.table(table(compdata$W)),4),1)
)
)

prop.table(table(compdata$W))
