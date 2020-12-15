##### MODEL DIAGNOSTICS #####

# This script contains the diagnostics of the selected models, useful for assessing
# the goodness of the assumptions.

# Packages Requirements
require(tidyverse)
require(broom)
require(ggplot2)
require(ggpubr)
require(fmsb)
require(DescTools)
require(mlbench)
require(tidyr)

####

plot(out.scale.era)
plot(out.scale.fip)
plot(out.scale.xfip)
plot(out.xfip.bullpen)
plot(out.wrc)
plot(out.wrc.bullpen)
plot(out.pytlog_era10)
plot(out.pytlog_fip10)

summary(out.rdg)
summary(out.fip)

comp.models <- c(out.rdg, out.era, out.fip, out.scale.era, out.scale.fip, out.scale.xfip,
                 out.xfip.bullpen, out.wrc, out.wrc.bullpen, out.pytlog_era, out.pytlog_fip,
                 out.pyt_era, out.pyt_fip)

AIC(out.rdg, out.era, out.fip, out.scale.era, out.scale.fip, out.scale.xfip,
    out.xfip.bullpen, out.wrc, out.wrc.bullpen, out.pytlog_era, out.pytlog_fip,
    out.pyt_era, out.pyt_fip)
BIC(out.rdg, out.era, out.fip, out.scale.era, out.scale.fip, out.scale.xfip,
    out.xfip.bullpen, out.wrc, out.wrc.bullpen, out.pytlog_era, out.pytlog_fip,
    out.pyt_era, out.pyt_fip)

confronto <- (round(cbind(PseudoR2(out.scale.era10, "all"), PseudoR2(out.pytlog_era10, "all"),
                          PseudoR2(out.scale.fip10, "all"), PseudoR2(out.pytlog_fip10, "all")),3))

colnames(confronto) <- c("ERA + RDG", "ERA + PYT", "FIP + RDG", "FIP + PYT")
View(confronto)

# Select only numeric predictors

vifmodint <- (vif(out.RSg.eras.erab))
vifmodint <- as.data.frame(vifmodint)
htmlTable(txtRound(t(vifmodint), 4))

mydata <- compdata[,c(38,39,42,43,48,49)]
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
probabilities <- predict(out.RSg.eras.erab, type = "response")
logit = log(probabilities/(1-probabilities))
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

compdata[6487,]
compdata[7299,]
compdata[4532,]

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(out.RSg.eras.erab, which = 4, id.n = 3)


View(compdata[c(2917,5900,7351),])

out.RSg.eras.erab.data <- augment(out.RSg.eras.erab) %>% 
  mutate(index = 1:n())

fit <- (out.RSg.eras.erab.data %>% top_n(3, .cooksd))
View(fit)

(exp(fit$.fitted))/(1+exp(fit$.fitted))

tiff("/plots/stand_res.tiff", units="cm", width=16, height=11, res=300)
ggplot(out.RSg.eras.erab.data, aes(index, .std.resid)) + 
  geom_point(aes(color = W), alpha = .5) +
  xlab("Osservazione") +
  theme_bw() +
  ylab("Residuo standardizzato") +
  ggtitle("Distribuzione dei reisdui standardizzati di Pearson",
          subtitle = "Modello: W ~ RSg squadre + ERA partenti + ERA bullpen")
dev.off()

par(mfrow=c(1,1), mar=c(5,5,3,3))
plot(residuals.glm(out.RSg.eras.erab, type = "pearson"))
plot(residuals.glm(out.RSg.eras.erab, type = "deviance")^2)

devres <- residuals.glm(out.RSg.eras.erab, type = "deviance")
devres2 <- devres^2
plot(devres)
devres2[devres2>4]


devpear <- residuals.glm(out.RSg.eras.erab, type = "pearson")
devpear2 <- devpear^2
plot(devpear)
devpear2[devpear2>5]

plot(out.RSg.eras.erab)

compdata$prob.home <- predict.glm(out.RSg.eras.erab,
                                  type = "response")

tiff("/plots/influence.tiff", units="cm", width=24, height=16.5, res=300)
influencePlot(out.RSg.eras.erab,col="red", ylab = "Residui Studentizzati",
              xlab = "h (leverage)", id = F,
              main = "Distribuzione dei residui studentizzati per il valore di leverage")
text(x=c(0.0026, 0.0030, 0.0037, 0.0062, 0.0062), y=c(2.35, 2.25, 2.40,
                                                      1.72, -1),
     labels = c("5900", "2917", "7351", "7782", "5240"))
dev.off()

inf.obs <- (compdata[c(2917,5239,5899,7350,7781),
                     c(1:3,38:39,42:43,48:49,54,56)])
View(inf.obs)

inf.obs$date <- as.character(inf.obs$date)
htmlTable(txtRound(inf.obs, digits = 3, excl.cols = c(3,10)))

modinf<-update(out.RSg.eras.erab,subset=c(-2917,-5240,
                                          -5900,-7351,-7782))

comparison <- compareCoefs(modinf,out.RSg.eras.erab)
htmlTable(txtRound(comparison,4))

compdata[2917,]


tiff("/plots/linearity.tiff", units="cm", width=16, height=16, res=300)
par(mfrow=c(3,2),mai = c(.3,.55,.3,.3))

smoothScatter(mydata[mydata$predictors=="RSg_hteam_scale",3],
              mydata[mydata$predictors=="RSg_hteam_scale",1],
              ylim = c(-4,4), xlim = c(-4,5),
              xlab = "Predittore",
              ylab = "Logit",
              main = "RSg squadra in casa")

smoothScatter(mydata[mydata$predictors=="RSg_vteam_scale",3],
              mydata[mydata$predictors=="RSg_vteam_scale",1],
              ylim = c(-4,4), xlim = c(-4,5),
              xlab = "Predittore",
              ylab = "Logit",
              main = "RSg squadra in trasferta")

smoothScatter(mydata[mydata$predictors=="ERA_hs_scale",3],
              mydata[mydata$predictors=="ERA_hs_scale",1],
              ylim = c(-4,4), xlim = c(-4,5),
              xlab = "Predittore",
              ylab = "Logit",
              main = "ERA partente casa")

smoothScatter(mydata[mydata$predictors=="ERA_vs_scale",3],
              mydata[mydata$predictors=="ERA_vs_scale",1],
              ylim = c(-4,4), xlim = c(-4,5),
              xlab = "Predittore",
              ylab = "Logit",
              main = "ERA partente trasferta")

smoothScatter(mydata[mydata$predictors=="ERA_bullpen_h_scale",3],
              mydata[mydata$predictors=="ERA_bullpen_h_scale",1],
              ylim = c(-4,4), xlim = c(-4,5),
              xlab = "Predittore",
              ylab = "Logit",
              main = "ERA rilievi casa")

smoothScatter(mydata[mydata$predictors=="ERA_bullpen_v_scale",3],
              mydata[mydata$predictors=="ERA_bullpen_v_scale",1],
              ylim = c(-4,4), xlim = c(-4,5),
              xlab = "Predittore",
              ylab = "Logit",
              main = "ERA rilievi trasferta")

dev.off()

par(mfrow=c(1,1), mar=c(5,5,3,3))

### compdata10: ERA

step.model

mydata2 <- compdata[,c("RSg_hteam_scale", "RSg_vteam_scale", "ERA_hs_scale", "ERA_vs_scale",
                       "FIP_hs_scale", "xFIP_hs_scale", "xFIP_vs_scale",
                       "ERA_bullpen_h_scale", "ERA_bullpen_v_scale")]
predictors <- colnames(mydata2)

probabilities2 <- predict(step.model, type = "response")
mydata2 <- mydata2 %>%
  mutate(logit = log(probabilities2/(1-probabilities2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata2, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

par(mfrow=c(2,2))

mydata2$predictors <- as.factor(mydata2$predictors)
mydata$predictors <- as.factor(mydata$predictors)

par(mfrow=c(3,3))
for(i in levels(mydata2$predictors)){
  smoothScatter(mydata2[mydata2$predictors==i,3],
                mydata2[mydata2$predictors==i,1],
                ylim = c(-4,4),
                xlab = "Predittore",
                ylab = "Logit",
                main = paste(i))
}

par(mfrow=c(2,3))
for(i in levels(mydata$predictors)){
  smoothScatter(mydata[mydata$predictors==i,3],
                mydata[mydata$predictors==i,1],
                ylim = c(-4,4),
                xlab = "Predittore",
                ylab = "Logit",
                main = paste(i))
}




linmod1 <- mydata2[mydata2$predictors=="ERA_hs",]
linmod2 <- mydata2[mydata2$predictors=="ERA_vs",]
linmod3 <- mydata2[mydata2$predictors=="RDg_hteam",]
linmod4 <- mydata2[mydata2$predictors=="RDg_vteam",]

summary(lm(logit ~ predictor.value, data = linmod1))
summary(lm(logit ~ predictor.value, data = linmod2))
summary(lm(logit ~ predictor.value, data = linmod3))
summary(lm(logit ~ predictor.value, data = linmod4))

plot(out.scale.fip, which = 4, id.n = 3)

compdata[2917,]
compdata[7351,]
compdata[23,]

summary(out.scale.era)
summary(out.scale.era3)
summary(out.scale.era10)

summary(out.scale.fip)
summary(out.scale.fip3)
summary(out.scale.fip10)

out.scale.fip.data <- augment(out.scale.fip) %>% 
  mutate(index = 1:n())

View(out.scale.era10.data %>% top_n(3, .cooksd))

ggplot(out.scale.era10.data, aes(index, .std.resid)) + 
  geom_point(aes(color = W), alpha = .5) +
  theme_bw()

### compdata10: FIP

mydata3 <- compdata10[,c(18,23,42,43)]
predictors <- colnames(mydata3)

probabilities3 <- predict(out.scale.fip10, type = "response")
mydata3 <- mydata3 %>%
  mutate(logit = log(probabilities3/(1-probabilities3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata3, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(out.scale.fip10, which = 4, id.n = 3)

out.scale.fip10.data <- augment(out.scale.fip10) %>% 
  mutate(index = 1:n())

View(out.scale.fip10.data %>% top_n(3, .cooksd))
out.scale.fip10.data$W <- as.factor(out.scale.fip10.data$W)
ggplot(out.scale.fip10.data, aes(index, .std.resid)) + 
  geom_point(aes(color = W), alpha = .5) +
  theme_bw()

car::vif(out.scale.fip10)
car::vif(out.scale.era10)

res1 <- residuals.glm(out.scale.fip10, type = "deviance")
summary(res1^2)

### pytlog

mydata4 <- compdata10[,c(40:41,57:56)]
predictors <- colnames(mydata4)

probabilities4 <- predict(out.pytlog_era10, type = "response")
mydata4 <- mydata4 %>%
  mutate(logit = log(probabilities4/(1-probabilities4))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata4, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(out.pytlog_era10, which = 4, id.n = 3)

out.wrc.bullpen.data <- augment(out.wrc.bullpen) %>% 
  mutate(index = 1:n())

View(out.scale.fip10.data %>% top_n(3, .cooksd))
out.scale.fip10.data$W <- as.factor(out.scale.fip10.data$W)
ggplot(out.scale.fip10.data, aes(index, .std.resid)) + 
  geom_point(aes(color = W), alpha = .5) +
  theme_bw()

car::vif(out.scale.fip10)
car::vif(out.scale.era10)


#### Quadratic terms #### 

scaledata10 <- scaledata10 %>%
  mutate(ERA_hs2 = ERA_hs^2) %>% 
  mutate(ERA_vs2 = ERA_vs^2) %>%
  mutate(ERA_hsroot = ERA_hs^(1/2)) %>% 
  mutate(ERA_vsroot = ERA_vs^(1/2)) %>%
  mutate(RDg_h_2 = RDg_hteam^2) %>% 
  mutate(RDg_v_2 = RDg_vteam^2) %>%
  mutate(RDg_h_scale = scale(RDg_hteam)) %>%
  mutate(RDg_v_scale = scale(RDg_vteam))

out.quad <-  glm(W ~ RDg_h_scale + RDg_v_scale + ERAhs_scale + ERAvs_scale,
                 data = scaledata10, family = "binomial")

summary(out.quad)

mydata4 <- scaledata10[,c(31,32,22,23)]
predictors <- colnames(mydata3)

probabilities3 <- predict(out.quad, type = "response")
mydata4 <- mydata4 %>%
  mutate(logit = log(probabilities3/(1-probabilities3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata4, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

### FIP

mydata <- compdata10[,c(28,29,18,19)]
predictors <- colnames(mydata)

probabilities <- predict(out.scale.fip, type = "response")
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

linmod5 <- mydata[mydata$predictors=="FIP_hs_scale",]
linmod6 <- mydata[mydata$predictors=="FIP_vs_scale",]
linmod7 <- mydata[mydata$predictors=="RDg_hteam",]
linmod8 <- mydata[mydata$predictors=="RDg_vteam",]

summary(lm(logit ~ predictor.value, data = linmod5))
summary(lm(logit ~ predictor.value, data = linmod6))
summary(lm(logit ~ predictor.value, data = linmod7))
summary(lm(logit ~ predictor.value, data = linmod8))

plot(out.scale.fip, which = 4, id.n = 3)

compdata[5893,]

out.scale.fip.data <- augment(out.scale.fip) %>% 
  mutate(index = 1:n())

out.scale.fip.data %>% top_n(3, .cooksd)

ggplot(out.scale.fip.data, aes(index, .std.resid)) + 
  geom_point(aes(color = W), alpha = .5) +
  theme_bw()

out.scale.fip.data %>% 
  filter(abs(.std.resid) > 3)


##### Controllo delle stime: dopo 60 partite sono corrette?

rdg.means.2015 <- 1:30
names(rdg.means.2015) <- tvec

for(y in 1:30){
  rdg.means.2015[y] <- mean(rdg2015[1:teams2015$G[y],y])
}
rdg.means.2015

#

rdg.means.2016 <- 1:30
names(rdg.means.2016) <- tvec

for(y in 1:30){
  rdg.means.2016[y] <- mean(rdg2016[1:teams2016$G[y],y])
}
rdg.means.2016

#

rdg.means.2017 <- 1:30
names(rdg.means.2017) <- tvec

for(y in 1:30){
  rdg.means.2017[y] <- mean(rdg2017[1:teams2017$G[y],y])
}
rdg.means.2017

#

rdg.means.2018 <- 1:30
names(rdg.means.2018) <- tvec

for(y in 1:30){
  rdg.means.2018[y] <- mean(rdg2018[1:teams2018$G[y],y])
}
rdg.means.2018

#


diff15 <- (rdg.means.2015-colMeans(rdg2015[1:60,]))

max(abs(diff15))

diff16 <- (rdg.means.2016-colMeans(rdg2016[1:60,]))

max(abs(diff16))

diff17 <- (rdg.means.2017-colMeans(rdg2017[1:60,]))

max(abs(diff17))

diff18 <- (rdg.means.2018-colMeans(rdg2018[1:60,]))

max(abs(diff18))


test15 <- 1:30

for(i in 1:30){
  
  test <- t.test((rdg2015[1:teams2015$G[i],i]), (rdg2015[1:60,i]))
  test15[i] <- test$p.value
  
}

test15
min(test15)

#### Marginal effects ####

marginal.era <- logitmfx(W ~ RDg_vteam + RDg_hteam + ERA_vs_scale +
                           ERA_hs_scale, data = reg.data, atmean = F)

marginal.era

marginal.fip <- logitmfx(W ~ RDg_vteam + RDg_hteam + FIP_vs_scale +
                           FIP_hs_scale, data = reg.data, atmean = F)

marginal.fip


round(cbind(marginal.era$mfxest[,1], marginal.fip$mfxest[,1]),5)

### Plot setup ###

mfxdat1 <- mfxdat2 <- data.frame(V1 = attributes(marginal.era$mfxest[,1])$names)

mfxdat1$me <- marginal.era$mfxest[,1]
mfxdat2$me <- marginal.fip$mfxest[,1]

### Interval bounds for marginal effects

mfxdat1$upper <- mfxdat1$me + 2*marginal.era$mfxest[,2]
mfxdat1$lower <- mfxdat1$me - 2*marginal.era$mfxest[,2]

mfxdat2$upper <- mfxdat2$me + 2*marginal.fip$mfxest[,2]
mfxdat2$lower <- mfxdat2$me - 2*marginal.fip$mfxest[,2]

### coefplot

theme_set(theme_bw())
p1 <- ggplot(mfxdat1, aes(V1, me,  ymin = lower,ymax= upper)) +
  labs(x="Variable", y = "Marginal Effect") + 
  geom_point(aes(x = V1, y = me)) + 
  geom_errorbar(size=.3,width=.2) + 
  geom_hline(yintercept=0) + 
  coord_flip() 


p2 <- ggplot(mfxdat2, aes(V1, me,  ymin = lower,ymax= upper)) +
  labs(x="Variable", y = "Marginal Effect") + 
  geom_point(aes(x = V1, y = me)) + 
  geom_errorbar(size=.3,width=.2) + 
  geom_hline(yintercept=0) + 
  coord_flip() 

ggarrange(p1,p2, labels = c("ERA", "FIP"), hjust = -2)


#### Predictive test ####

summary(out.scale.xfip10)

newdata <- as.data.frame(t(c(1.2407, -0.2182, -1.26, -2.44)))
newdata2 <- as.data.frame(t(c(1.2407, -0.2182, -1.86, -1.6)))
newdata3 <- as.data.frame(t(c(-0.2182, 1.2407,-0.83, -1.07)))

colnames(newdata) <- names(out.scale.era10$coefficients)[2:5]
colnames(newdata2) <- names(out.scale.fip10$coefficients)[2:5]
colnames(newdata3) <- names(out.scale.xfip10$coefficients)[2:5]

predict.glm(out.scale.era10, newdata = newdata, type="response")
predict.glm(out.scale.fip10, newdata = newdata2, type="response")
predict.glm(out.scale.xfip10, newdata = newdata3, type="response")


#### Plot predicted probability ####

### ERA ###

out.era10 <- glm(W~ERA_vs + ERA_hs + RDg_vteam + RDg_hteam,
                 data = compdata10, family = "binomial")

summary(compdata10$ERA_hs)
quantile(compdata10$ERA_hs, c(.025,.975))

eravalues<-(seq(0,12,length.out = nrow(compdata10)))

summary(compdata10$RDg_vteam)
quantile(compdata10$RDg_vteam, c(.25,.50,.75))

## RDG_v = -1
datipred <- matrix((c(eravalues,
                      rep(-1,nrow(compdata10)),
                      rep(4.2,nrow(compdata10)),
                      rep(0,nrow(compdata10))
)),
nrow = nrow(compdata10), ncol = 4)
colnames(datipred) <- c("ERA_vs","RDg_vteam", "ERA_hs", "RDg_hteam")
datipred <- as.data.frame(datipred)
prediction1 <- predict.glm(out.era10, newdata = datipred, type = "response")


## RDG_v = 0

datipred2 <- matrix((c(eravalues,
                       rep(0,nrow(compdata10)),
                       rep(4.2,nrow(compdata10)),
                       rep(0,nrow(compdata10))
)),
nrow = nrow(compdata10), ncol = 4)
colnames(datipred2) <- c("ERA_vs","RDg_vteam", "ERA_hs", "RDg_hteam")
datipred2 <- as.data.frame(datipred2)
prediction2 <- predict.glm(out.era10, newdata = datipred2, type = "response")

## RDG_v = 0.5

datipred3 <- matrix((c(eravalues,
                       rep(1,nrow(compdata10)),
                       rep(4.2,nrow(compdata10)),
                       rep(0,nrow(compdata10))
)),
nrow = nrow(compdata10), ncol = 4)
colnames(datipred3) <- c("ERA_vs","RDg_vteam", "ERA_hs", "RDg_hteam")
datipred3 <- as.data.frame(datipred3)
prediction3 <- predict.glm(out.era10, newdata = datipred3, type = "response")

## Dataframe for the plot
dfplot <- rbind(cbind((prediction1),(rep(-1,nrow(compdata10))), eravalues),
                cbind((prediction2),(rep(0,nrow(compdata10))), eravalues),
                cbind((prediction3),(rep(1,nrow(compdata10))), eravalues))
dfplot<- as.data.frame(dfplot)
colnames(dfplot) <- c("PredP", "RDG_vteam", "ERA_vs")
dfplot$RDG_vteam <- as.factor(dfplot$RDG_vteam)

## Plot
plotpred1 <- ggplot(dfplot, aes(x= ERA_vs, y = PredP, color = RDG_vteam)) +
  geom_line(size=1) +
  labs(x = "Visiting SP ERA",
       y = "Predicted probability",
       subtitle = " ") +
  ggtitle(" ") +
  scale_color_discrete(name = "Run Difference/G
  Visiting Team", labels = c(-1, 0, 1)) +
  geom_vline(xintercept = 4.2)

plotpred1