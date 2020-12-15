####### MODEL SELECTION #######

# This script contains all the selection procedures that were used to chose
# which variables to include and which to discard from the final model.

# Packages Requirements:
require(epitools)
require(poisbinom)
require(DescTools)
require(tidyverse)
require(caret)
require(MASS)
require(car)
require(pROC)
require(tidyverse)
require(caret)
require(tibble)

#### (1) AIC of the Complete Models ####

# The models specified in the "logistic_regression.R" file are here tested
# in terms of AIC

aic.comp <- AIC(out.rdgera, out.rdgfip, out.rdgxfip,
                out.RSg.eras.erab, out.RSg.fips.fipb, out.RSg.xFIPs.xFIPb,
                out.wRC_plus.eras.erab, out.wRC_plus.fips.fipb, out.wRC_plus.xFIPs.xFIPb)

aic.comp <- txtRound(aic.comp, 3, excl.cols = 1)
aic.comp <- aic.comp[order(aic.comp$AIC, decreasing = F),]
htmlTable(aic.comp)

summary(out.RSg.eras.erab)

htmlTable(round(cbind((summary(out.RSg.eras.erab))$coefficients,
                      exp(out.RSg.eras.erab$coefficients)),4),
          rnames = c("(Intercetta)", "RSg casa", "RSg trasf.",
                     "ERA P. casa", "ERA P. trasf.",
                     "ERA B. casa", "ERA B. trasf"))

coefff <- round(out.RSg.eras.erab$coefficients, 3)
names(coefff) <- NULL

samp <- sample(1:nrow(compdata),1)
compdata[samp,]
d1518[443,]

predict.glm(out.RSg.eras.erab, newdata = d1518[443,], type="response")

predict.glm(step.model, newdata = d1518[443,], type="response")

####### (2) STEPWISE SELECTION #######

reg.data <- compdata[,c(54,18,21,38:53,30:35)]

full.model <- glm(W ~ ., data = reg.data, family = "binomial")
summary(full.model)


step.model <- full.model %>% stepAIC(trace = F, direction = "both",
                                     k = qchisq(0.05, 1, lower.tail = F))

summary(step.model)

htmlTable(round(cbind((summary(step.model))$coefficients,
                      exp(step.model$coefficients)),4),
          rnames = c("(Intercetta)", "RSg casa", "RSg trasf.",
                     "ERA P. trasf.", "ERA P. casa",
                     "FIP P. casa", "xFIP P. trasf.", "xFIP P. casa",
                     "ERA B. trasf", "ERA B. casa"))
n <- 24

tot <- 1:24

for(k in 1:24){
  
  tot[k] <- (factorial(n))/(factorial(k)*factorial(n-k))
  
}

sum(tot)

vif(step.model)

fit1 <- glm(W ~ ., reg.data, family = "binomial")
fit2 <- glm(W ~ 1, reg.data, family = "binomial")

back.model <- stepAIC(fit1,direction="backward", trace = F,
                      k = qchisq(0.05, 1, lower.tail = F))
summary(back.model)
forw.model <- stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2),
                      trace = F, k = qchisq(0.05, 1, lower.tail = F))
summary(forw.model)
step2 <- stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2), trace = F,
                 k = qchisq(0.05, 1, lower.tail = F))
summary(step2)

#### Estimating the predictive quality of the model ####

# Trying to predict win/loss by selecting a cutoff value is not the final aim
# of this analysis; but it might be worth testing it nonetheless.
# Note that the following accuracy measures, however, suffer of selection bias,
# since they're being computed on the same observations used to fit the model.

summary(out.rdgfip)
pred.fip.compdata <- predict(out.rdgfip, type = "response")

pred.fip.compdata <- as.data.frame(cbind(pred.fip.compdata,compdata$W))
colnames(pred.fip.compdata) <- c("probability", "result")
pred.fip.compdata$result <- as.factor(pred.fip.compdata$result)
levels(pred.fip.compdata$result) <- c("L","W")

pred.fip.compdata <- pred.fip.compdata[order(pred.fip.compdata$prediction, decreasing = T),]

pred.fip.compdata$prediction <- ifelse(pred.fip.compdata$probability>0.5,"W", "L")

sum(pred.fip.compdata$prediction==pred.fip.compdata$result)/nrow(pred.fip.compdata)

####

summary(out.out.all.rs.scale)
pred.bullpen.compdata <- predict(out.all.rs.scale, type = "response")

pred.bullpen.compdata <- as.data.frame(cbind(pred.bullpen.compdata,compdata$W))
colnames(pred.bullpen.compdata) <- c("probability", "result")
pred.bullpen.compdata$result <- as.factor(pred.bullpen.compdata$result)
levels(pred.bullpen.compdata$result) <- c("L","W")

pred.bullpen.compdata <- pred.bullpen.compdata[order(pred.bullpen.compdata$prediction, decreasing = T),]

pred.bullpen.compdata$prediction <- ifelse(pred.bullpen.compdata$probability>0.5,"W", "L")

sum(pred.bullpen.compdata$prediction==pred.bullpen.compdata$result)/nrow(pred.bullpen.compdata)

####



out.scale.era10 <- glm(W ~ RDg_hteam + RDg_vteam + ERA_hs_scale + ERA_vs_scale,
                       data = compdata10, family = "binomial")
out.scale.era3 <- glm(W ~ RDg_hteam + RDg_vteam + ERA_hs_scale + ERA_vs_scale,
                      data = compdata3, family = "binomial")
summary(out.scale.era10)

exp(out.scale.era10$coefficients)

out.scale.fip10 <- glm(W ~ RDg_hteam + RDg_vteam + FIP_hs_scale + FIP_vs_scale,
                       data = compdata10, family = "binomial")
out.scale.fip3 <- glm(W ~ RDg_hteam + RDg_vteam + FIP_hs_scale + FIP_vs_scale,
                      data = compdata3, family = "binomial")
summary(out.scale.fip10)

exp(out.scale.fip10$coefficients)


out.scale.era <- glm(W ~ RDg_hteam + RDg_vteam + ERA_hs_scale + ERA_vs_scale,
                     data = compdata, family = "binomial")
summary(out.scale.era)
exp(out.scale.era$coefficients)

out.scale.era.interaction <- glm(W ~ RDg_hteam*RDg_vteam*ERA_hs_scale*ERA_vs_scale,
                                 data = compdata, family = "binomial")
summary(out.scale.era.interaction)

out.scale.fip <- glm(W ~ RDg_hteam + RDg_vteam + FIP_hs_scale + FIP_vs_scale,
                     data = compdata, family = "binomial")
summary(out.scale.fip)
exp(out.scale.fip$coefficients)

pred.era10 <- predict(out.scale.era10, type = "response")
pred.era10 <- as.data.frame(cbind(pred.era10,compdata10$W))
colnames(pred.era10) <- c("probability", "result")
pred.era10$result <- as.factor(pred.era10$result)
levels(pred.era10$result) <- c("L","W")

pred.era10$pred.random <- with(pred.era10, rbinom(nrow(pred.era10), 1, probability))
pred.era10$pred.random <- as.factor(pred.era10$pred.random)
levels(pred.era10$pred.random) <- c("L","W")
sum(pred.era10$pred.random==pred.era10$result)/nrow(pred.era10)
pred.era10$pred.level <- ifelse(pred.era10$probability > 0.5, "W", "L")
sum(pred.era10$pred.level==pred.era10$result)/nrow(pred.era10)



pred.fip10 <- predict(out.scale.fip10, type = "response")
pred.fip10 <- as.data.frame(cbind(pred.fip10,compdata10$W))
colnames(pred.fip10) <- c("probability", "result")
pred.fip10$result <- as.factor(pred.fip10$result)
levels(pred.fip10$result) <- c("L","W")

pred.fip10$pred.random <- with(pred.fip10, rbinom(nrow(pred.fip10), 1, probability))
pred.fip10$pred.random <- as.factor(pred.fip10$pred.random)
levels(pred.fip10$pred.random) <- c("L","W")
sum(pred.fip10$pred.random==pred.fip10$result)/nrow(pred.fip10)
pred.fip10$pred.level <- ifelse(pred.fip10$probability > 0.5, "W", "L")
sum(pred.fip10$pred.level==pred.fip10$result)/nrow(pred.fip10)


#### Predictive Quality by splitting the data in a train set and test set ####

## This is a possible way to avoid selection bias

## testing normality of predictors

par(mfrow=c(1,1))

hist(compdata$FIP_vs_scale)
ggqqplot(compdata$FIP_vs_scale)

hist(compdata10$FIP_hs_scale)
ggqqplot(compdata10$FIP_hs_scale)

shapiro.test(teams1518$RDg)

hist(compdata10$RDg_hteam+compdata10$RDg_vteam)
ggqqplot(compdata10$RDg_hteam+compdata10$RDg_vteam)


cormat <- (cor(compdata[,c(46,18,20,34:45)]))

## Splitting of the data ##

set.seed(123)
training.sample <- reg.data$W %>%
  createDataPartition(p=0.8, list = F)

train.data <- reg.data[training.sample,]
test.data <- reg.data[-training.sample,]
test.data$W <- as.factor(test.data$W)

summary(out.scale.era)
summary(out.all.rs.scale10)

par(mfrow=c(1,1))

## Model selection on the train set ##
## The AREA UNDER ROC CURVE was used to assess the quality of the models

model.fip <- glm(W ~ RDg_hteam + RDg_vteam + FIP_hs_scale +
                   FIP_vs_scale, family = "binomial", data = train.data)
summary(model.fip)

prob.fip <- model.fip %>% predict(test.data, type = "response")
pred.result.fip <- as.factor(ifelse(prob.fip>0.5, 1, 0))
mean(pred.result.fip == test.data$W)

confusionMatrix(pred.result.fip, test.data$W, positive = "1")

fip.roc <- roc(test.data$W, prob.fip)
plot.roc(fip.roc, print.auc = T, print.auc.adj = c(0,4),
         print.auc.cex = (1.5))
text(0.5, 1.05, "RDg squadre + FIP partenti", font = 2)

#

model.era <- glm(W ~ RDg_vteam + RDg_hteam + ERA_vs_scale +
                   ERA_hs_scale, family = "binomial", data = train.data)
summary(model.era)

prob.era <- model.era %>% predict(test.data, type = "response")
pred.result.era <- as.factor(ifelse(prob.era>0.5385, 1, 0))
mean(pred.result.era == test.data$W)

addmargins(table(pred.result.era, test.data$W))

confusionMatrix(pred.result.era, test.data$W, positive = "1")

era.roc <- roc(test.data$W, prob.era)
plot.roc(era.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "RDg squadre + ERA partenti", font = 2)

#

model.fip.bullpen <- glm(W ~ RSg_vteam_scale + RSg_hteam_scale + FIP_vs_scale +
                           FIP_hs_scale + FIP_bullpen_v_scale +
                           FIP_bullpen_h_scale, family = "binomial",
                         data = train.data)
summary(model.fip.bullpen)

prob.fip.bullpen <- model.fip.bullpen %>% predict(test.data, type = "response")
pred.result.fip.bullpen <- as.factor(ifelse(prob.fip.bullpen>.5375, 1, 0))
mean(pred.result.fip.bullpen == test.data$W)

confusionMatrix(pred.result.fip.bullpen, test.data$W, positive = "1")

fipbullpen.roc <- roc(test.data$W, prob.fip.bullpen)
plot.roc(fipbullpen.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "RSg squadre + FIP partenti + FIP bullpen", font = 2)

#

model.era.bullpen <- glm(W ~ RSg_vteam_scale + RSg_hteam_scale + ERA_vs_scale +
                           ERA_hs_scale + ERA_bullpen_v_scale +
                           ERA_bullpen_h_scale, family = "binomial",
                         data = train.data)
summary(model.era.bullpen)

prob.era.bullpen <- model.era.bullpen %>% predict(test.data, type = "response")
pred.result.era.bullpen <- as.factor(ifelse(prob.era.bullpen>0.540, 1, 0))
mean(pred.result.era.bullpen == test.data$W)

confusionMatrix(pred.result.era.bullpen, test.data$W, positive = "1")


erabullpen.roc <- roc(test.data$W, prob.era.bullpen)
plot.roc(erabullpen.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "RSg squadre + ERA partenti + ERA bullpen", font = 2)

#

model.xfip <- glm(W ~ RDg_vteam + RDg_hteam + xFIP_vs_scale +
                    xFIP_hs_scale, family = "binomial", data = train.data)

summary(model.xfip)
summary(out.scale.xfip10)

prob.xfip <- model.xfip %>% predict(test.data, type = "response")
pred.result.xfip <- as.factor(ifelse(prob.xfip>0.541, 1, 0))
mean(pred.result.xfip == test.data$W)

confusionMatrix(pred.result.xfip, test.data$W, positive = "1")

xfip.roc <- roc(test.data$W, prob.xfip)
plot.roc(xfip.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "RDg squadre + xFIP partenti", font = 2)

#

model.xfip.bullpen <- glm(W ~ RSg_vteam_scale + RSg_hteam_scale + xFIP_vs_scale +
                            xFIP_hs_scale + xFIP_bullpen_v_scale +
                            xFIP_bullpen_h_scale, family = "binomial",
                          data = train.data)
summary(model.xfip.bullpen)
summary(out.xfip.bullpen)

prob.xfip.bullpen <- model.xfip.bullpen %>% predict(test.data, type = "response")
pred.result.xfip.bullpen <- as.factor(ifelse(prob.xfip.bullpen>0.542, 1, 0))
mean(pred.result.xfip.bullpen == test.data$W)

confusionMatrix(pred.result.xfip.bullpen, test.data$W, positive = "1")

xfipbullpen.roc <- roc(test.data$W, prob.xfip.bullpen)
plot.roc(xfipbullpen.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "RSg squadre + xFIP partenti + xFIP bullpen", font = 2)
#


model.wrc.era <- glm(W ~ wRC_plus_vteam_scale + wRC_plus_hteam_scale + ERA_vs_scale +
                       ERA_hs_scale + ERA_bullpen_v_scale + ERA_bullpen_h_scale
                     , data = train.data, family = "binomial")

summary(model.wrc.era)

prob.wrc.era <- model.wrc.era %>% predict(test.data, type = "response")
pred.result.wrc.era <- as.factor(ifelse(prob.wrc.era>0.539, 1, 0))
mean(pred.result.wrc.era == test.data$W)

confusionMatrix(pred.result.wrc.era, test.data$W, positive = "1")

wrc.era.roc <- roc(test.data$W, prob.wrc.era)
plot.roc(wrc.era.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "wRC+ squadre + ERA partenti + ERA bullpen", font = 2)

#

model.wrc.fip <- glm(W ~ wRC_plus_vteam_scale + wRC_plus_hteam_scale + FIP_vs_scale +
                       FIP_hs_scale + FIP_bullpen_v_scale + FIP_bullpen_h_scale
                     , data = train.data, family = "binomial")

summary(model.wrc.fip)

prob.wrc.fip <- model.wrc.fip %>% predict(test.data, type = "response")
pred.result.wrc.fip <- as.factor(ifelse(prob.wrc.fip>0.539, 1, 0))
mean(pred.result.wrc.fip == test.data$W)

confusionMatrix(pred.result.wrc.fip, test.data$W, positive = "1")

wrc.fip.roc <- roc(test.data$W, prob.wrc.fip)
plot.roc(wrc.fip.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "wRC+ squadre + FIP partenti + FIP bullpen", font = 2)

#

model.wrc.xfip <- glm(W ~ wRC_plus_vteam_scale + wRC_plus_hteam_scale + xFIP_vs_scale +
                        xFIP_hs_scale + xFIP_bullpen_v_scale + xFIP_bullpen_h_scale
                      , data = train.data, family = "binomial")

summary(model.wrc.xfip)

prob.wrc.xfip <- model.wrc.xfip %>% predict(test.data, type = "response")
pred.result.wrc.xfip <- as.factor(ifelse(prob.wrc.xfip>0.539, 1, 0))
mean(pred.result.wrc.xfip == test.data$W)

confusionMatrix(pred.result.wrc.xfip, test.data$W, positive = "1")

wrc.xfip.roc <- roc(test.data$W, prob.wrc.xfip)
plot.roc(wrc.xfip.roc, print.auc = T, print.auc.adj = c(0,5),
         print.auc.cex = (1.5))
text(0.5, 1.05, "wRC+ squadre + xFIP partenti + xFIP bullpen", font = 2)



AIC(model.era, model.era.bullpen, model.fip, model.fip.bullpen, model.xfip,
    model.xfip.bullpen, model.wrc.era, model.wrc.fip, model.wrc.xfip)


full.model.test <- glm(W ~ ., data = test.data, family = "binomial")
summary(full.model.test)

step.model.test <- full.model.test %>% stepAIC(trace = F, direction = "both")

summary(step.model.test)


paste(names(coefficients(step.model.test))[2:8], collapse = " + ")


### Curve ROC Plots ####

PseudoR2(out.RSg.eras.erab, "all")


tiff("/plots/roc_mod_comp.tiff", units="cm", width=16, height=16, res=300)

par(mfrow = c(3,3),
    oma = c(0,0,0,0),
    mar = c(0,0,0,0))

#
plot.roc(erabullpen.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "RSg squadre +
ERA partenti +
ERA bullpen", font = 2, cex = .90)

#

plot.roc(era.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "RDg squadre +
ERA partenti", font = 2, cex = .90)
#

plot.roc(wrc.era.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "wRC+ squadre +
ERA partenti +
ERA bullpen", font = 2, cex = .90)
#text(0.5, 1.05, "wRC+ squadre + ERA partenti + ERA bullpen", font = 2)

#

plot.roc(fip.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "RDg squadre +
FIP partenti", font = 2, cex = .90)
#text(0.5, 1.05, "RDg squadre + FIP partenti", font = 2)

#

plot.roc(fipbullpen.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "RSg squadre +
FIP partenti +
FIP bullpen", font = 2, cex = .90)
#text(0.5, 1.05, "RSg squadre + FIP partenti + FIP bullpen", font = 2)

#
plot.roc(xfip.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "RDg squadre +
xFIP partenti", font = 2, cex = .90)
#text(0.5, 1.05, "RDg squadre + xFIP partenti", font = 2)

#

plot.roc(wrc.fip.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "wRC+ squadre +
FIP partenti +
FIP bullpen", font = 2, cex = .90)
#text(0.5, 1.05, "wRC+ squadre + FIP partenti + FIP bullpen", font = 2)

#

plot.roc(xfipbullpen.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "RSg squadre +
xFIP partenti +
xFIP bullpen", font = 2, cex = .90)
#text(0.5, 1.05, "RSg squadre + xFIP partenti + xFIP bullpen", font = 2)

#

plot.roc(wrc.xfip.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1))
text(0.3, 0.2, "wRC+ squadre +
xFIP partenti +
xFIP bullpen", font = 2, cex = .90)
#text(0.5, 1.05, "wRC+ squadre + xFIP partenti + xFIP bullpen", font = 2)

dev.off()

par(mfrow=c(1,1))


names(step.model$coefficients)[2:10]

paste((names(step.model$coefficients)[2:10]), collapse = " + ")

model.step <- glm(W ~ RSg_hteam_scale + RSg_vteam_scale + ERA_vs_scale +
                    ERA_hs_scale + FIP_hs_scale + xFIP_vs_scale + xFIP_hs_scale +
                    ERA_bullpen_v_scale + ERA_bullpen_h_scale,
                  data = train.data, family = "binomial")

summary(model.step)

prob.step <- model.step %>% predict(test.data, type = "response")
pred.result.step <- as.factor(ifelse(prob.step>0.5, 1, 0))
mean(pred.result.step == test.data$W)

confusionMatrix(pred.result.step, test.data$W, positive = "1")

step.roc <- roc(test.data$W, prob.step)

tiff("/plot/roc_mod_step.tiff", units="cm", width=11, height=11, res=300)
plot.roc(step.roc, print.auc = T, print.auc.adj = c(0,1),
         print.auc.cex = (1.5))
text(0.26, 0.2, "Modello stepwise", font = 2)
dev.off()


quantile(teams1518$RSg, c(.05,.95)) - mean(teams1518$RSg)

quantile(compdata$ERA_bullpen_h_scale, c(.05,.95))
summary(compdata$ERA_vs_scale)




bos18 <- c((compdata[compdata$hteam =="BOS" & compdata$year == 2018, "prob.home"]),
           (1-(compdata[compdata$vteam =="BOS" & compdata$year == 2018, "prob.home"])))

sum(bos18)
var.bin <- (mean(bos18)*(1-mean(bos18)))*length(bos18)
var.poi <- 1:length(bos18)

for(i in 1:length(bos18)){
  var.poi[i] <- (bos18[i]*(1-bos18[i]))
}

sqrt(sum(var.poi))
var.bin

pbinom(65, 100, 0.6, lower.tail = T) - pbinom(55, 100, 0.6, lower.tail = T)
pbinom(10, 10, 0.6, lower.tail = T) - pbinom(0, 10, 0.6, lower.tail = T)


bos10 <- bos18[2:11]
mean(bos10)
bos100 <- bos18[62:161]
mean(bos100)

length(bos10)

ppoisbinom(66, bos100, lower_tail = T) - ppoisbinom(56, bos100, lower_tail = T)

binom.exact(5, 10, conf.level = 0.95)
0.7196771*162-0.5197871*162
0.812914*10-0.187086*10

yan18 <- c((compdata[compdata$hteam =="NYA" & compdata$year == 2018, "prob.home"]),
           (1-(compdata[compdata$vteam =="NYA" & compdata$year == 2018, "prob.home"])))

sum(yan18)
var.bin <- (mean(yan18)*(1-mean(yan18)))*length(yan18)
var.poi <- 1:length(yan18)

for(i in 1:length(yan18)){
  var.poi[i] <- (yan18[i]*(1-yan18[i]))
}

sum(var.poi)
(var.bin)

prediction.summary.fs
