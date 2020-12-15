####### REGRESSION MODELS ########

# In this script you can find all the Logistic Regression Models estimated on the
# full dataset of games from 2015 to 2018.

#### LOGISTIC UNIVARIATE REGRESSION ####

#### (1) W ~ home team RDG ####


htmlTable(round(cbind((summary(out.wRCh))$coefficients,
                      exp(out.wRCh$coefficients)),4))

out.rdgh <- glm(W ~ RDg_hteam, data = compdata, family = "binomial")

htmlTable(round(cbind((summary(out.rdgh))$coefficients,
                      exp(out.rdgh$coefficients)),4))

our.rdghlev <- glm(W ~ rdgh4cat, data = compdata, family = "binomial")
summary(our.rdghlev)
exp(our.rdghlev$coefficients)

#### (2) W ~ visiting team RDG ####

out.rdgv <- glm(W ~ RDg_vteam, data = compdata, family = "binomial")

summary(out.rdgv)
exp(out.rdgv$coefficients)

htmlTable(round(cbind((summary(out.rdgv))$coefficients,
                      exp(out.rdgv$coefficients)),4))

our.rdgvlev <- glm(W ~ rdgv4cat, data = compdata, family = "binomial")
summary(our.rdgvlev)
exp(our.rdgvlev$coefficients)

#### (3) W ~ home team RSg ####

out.rsgh <- glm(W ~ RSg_hteam_scale, data = compdata, family = "binomial")

htmlTable(round(cbind((summary(out.rsgh))$coefficients,
                      exp(out.rsgh$coefficients)),4))

#### (4) W ~ visiting team RSG ####


out.rsgv <- glm(W ~ RSg_vteam_scale, data = compdata, family = "binomial")

htmlTable(round(cbind((summary(out.rsgv))$coefficients,
                      exp(out.rsgv$coefficients)),4))

#### (5) W ~ home team wRC ####

out.wRCh <- glm(W ~ wRC_plus_hteam_scale, data = compdata, family = "binomial")

htmlTable(round(cbind((summary(out.wRCh))$coefficients,
                      exp(out.wRCh$coefficients)),4),
          rnames = c("(Intercetta)", "wRC+ casa"))
#### (6) W ~ away team wRC ####


out.wRCv <- glm(W ~ wRC_plus_vteam_scale, data = compdata, family = "binomial")

htmlTable(round(cbind((summary(out.wRCv))$coefficients,
                      exp(out.wRCv$coefficients)),4),
          rnames = c("(Intercetta)", "wRC+ trasferta"))

#### (7) W ~ home team starting pitcher ERA ####

out.erahs <- glm(W ~ ERA_hs, data = compdata, family = "binomial")

summary(out.erahs)
exp(out.erahs$coefficients)

out.erahslev <- glm(W ~ erahs4cat, data = compdata, family = "binomial")
summary(out.erahslev)
exp(out.erahslev$coefficients)

#### (8) W ~ visiting team starting pitcher ERA ####

out.eravs <- glm(W ~ ERA_vs, data = compdata, family = "binomial")

summary(out.eravs)
exp(out.eravs$coefficients)

out.eravslev <- glm(W ~ eravs4cat, data = compdata, family = "binomial")
summary(out.eravslev)
exp(out.eravslev$coefficients)

#### (9) W ~ home team starting pitcher FIP ####

out.fiphs <- glm(W ~ FIP_hs, data = compdata, family = "binomial")

summary(out.fiphs)
exp(out.fiphs$coefficients)


#### (10) W ~ visiting team starting pitcher FIP ####

out.fipvs <- glm(W ~ FIP_vs, data = compdata, family = "binomial")

summary(out.fipvs)
exp(out.fipvs$coefficients)

#### (11) W ~ month ####

out.month <- glm(W ~ month, data = compdata, family = "binomial")

summary(out.month)
exp(out.month$coefficients)

#### (12) W ~ day of the week ####

out.day <- glm(W ~ day, data = compdata, family = "binomial")

summary(out.day)
exp(out.day$coefficients)

#### (13) W ~ ballpark ####

out.park <- glm(W ~ park, data = compdata, family = "binomial")

summary(out.park)
exp(out.park$coefficients)

#### (14) W ~ time of the game ####

out.time <- glm(W ~ time, data = compdata, family = "binomial")

summary(out.time)
exp(out.time$coefficients)

summary(compdata$RSg_hteam)
summary(compdata$RSg_vteam)
summary(teams1518$RSg)

#### LOGISTIC MULTIVARIATE REGRESSION ####

#### (1) W ~ Rdgh + Rdgv ####

out.rdg <- glm(W ~ RDg_hteam + RDg_vteam, data = compdata, family = "binomial")
summary(out.rdg)
htmlTable(round(cbind((summary(out.rdg))$coefficients,
                      exp(out.rdg$coefficients)),4),
          rnames = c("(Intercetta)", "RDg casa", "RDg trasferta"))
exp(out.rdg$coefficients)

#### (2) W ~ Rsgh + Rsgv ####

out.rsg <- glm(W ~ RSg_hteam_scale + RSg_vteam_scale,
               data = compdata, family = "binomial")
summary(out.rsg)
htmlTable(round(cbind((summary(out.rsg))$coefficients,
                      exp(out.rsg$coefficients)),4),
          rnames = c("(Intercetta)", "RSg casa", "RSg trasferta"))
exp(out.rsg$coefficients)

compdata$wRC_plus_hteam_scale <- as.numeric(scale(compdata$wRC_plus_hteam, T, F))
summary(compdata$wRC_plus_hteam_scale)
compdata$wRC_plus_vteam_scale <- as.numeric(scale(compdata$wRC_plus_vteam, T, F))
summary(compdata$wRC_plus_vteam_scale)

#### (3) W ~ wrch + wrcv ####

out.wrc <- glm(W ~ wRC_plus_hteam_scale + wRC_plus_vteam_scale,
               data = compdata, family = "binomial")
summary(out.wrc)
htmlTable(round(cbind((summary(out.wrc))$coefficients,
                      exp(out.wrc$coefficients)),4),
          rnames = c("(Intercetta)", "wRC+ casa", "wRC+ trasferta"))
exp(out.wrc$coefficients)


#### (4) W ~ ERAh + ERAv ####

out.era <- glm(W ~ ERA_hs_scale + ERA_vs_scale,
               data = compdata, family = "binomial")
summary(out.era)
htmlTable(round(cbind((summary(out.era))$coefficients,
                      exp(out.era$coefficients)),4),
          rnames = c("(Intercetta)", "ERA SP casa", "ERA SP trasferta"))

#### (5) W ~ FIPh + FIPv ####

out.fip <- glm(W ~ FIP_hs_scale + FIP_vs_scale,
               data = compdata, family = "binomial")
summary(out.fip)

htmlTable(round(cbind((summary(out.fip))$coefficients,
                      exp(out.fip$coefficients)),4),
          rnames = c("(Intercetta)", "FIP P casa", "FIP P trasf."))
exp(out.fip$coefficients)

#### (6) W ~ xFIPh + xFIPv ####

out.xfip <- glm(W ~ xFIP_hs_scale + xFIP_vs_scale,
                data = compdata, family = "binomial")
summary(out.xfip)

htmlTable(round(cbind((summary(out.xfip))$coefficients,
                      exp(out.xfip$coefficients)),4),
          rnames = c("(Intercetta)", "xFIP P casa", "xFIP P trasf."))
exp(out.xfip$coefficients)

#### (7) W ~ ERA bullpen h + ERA bullpen v ####

out.era.bullpen <- glm(W ~ ERA_bullpen_h_scale + ERA_bullpen_v_scale,
                       data = compdata, family = "binomial")
summary(out.era.bullpen)
htmlTable(round(cbind((summary(out.era.bullpen))$coefficients,
                      exp(out.era.bullpen$coefficients)),4),
          rnames = c("(Intercetta)", "ERA B casa", "ERA B trasf."))

#### (8) W ~ FIP bullpen h + FIP bullpen v ####

out.fip.bullpen <- glm(W ~ FIP_bullpen_h_scale + FIP_bullpen_v_scale,
                       data = compdata, family = "binomial")
summary(out.fip.bullpen)
htmlTable(round(cbind((summary(out.fip.bullpen))$coefficients,
                      exp(out.fip.bullpen$coefficients)),4),
          rnames = c("(Intercetta)", "FIP B casa", "FIP B trasf."))

#### (9) W ~ xFIP bullpen h + xFIP bullpen v ####

out.xfip.bullpen <- glm(W ~ xFIP_bullpen_h_scale + xFIP_bullpen_v_scale,
                        data = compdata, family = "binomial")
summary(out.xfip.bullpen)
htmlTable(round(cbind((summary(out.xfip.bullpen))$coefficients,
                      exp(out.xfip.bullpen$coefficients)),4),
          rnames = c("(Intercetta)", "xFIP B casa", "xFIP B trasf."))

#### (10) W ~ RDGl5 h + RDGl5 v ####

out.rdgl5 <- glm(W ~ RDgl5_h + RDgl5_v,
                 data = compdata, family = "binomial")
summary(out.rdgl5)
htmlTable(round(cbind((summary(out.rdgl5))$coefficients,
                      exp(out.rdgl5$coefficients)),4),
          rnames = c("(Intercetta)", "RDg 5 casa", "RDg 5 trasf."))

#### (11) W ~ RDGl10 h + RDGl10 v ####

out.rdgl10 <- glm(W ~ RDgl10_h + RDgl10_v,
                  data = compdata, family = "binomial")
summary(out.rdgl10)
htmlTable(round(cbind((summary(out.rdgl10))$coefficients,
                      exp(out.rdgl10$coefficients)),4),
          rnames = c("(Intercetta)", "RDg 10 casa", "RDg 10 trasf."))

#### (12) W ~ Wl10 h + Wl10 v ####

out.wl10 <- glm(W ~ Wl10_h + Wl10_v,
                data = compdata, family = "binomial")
summary(out.wl10)
htmlTable(round(cbind((summary(out.wl10))$coefficients,
                      exp(out.wl10$coefficients)),4),
          rnames = c("(Intercetta)", "W% 10 casa", "W% 10 trasf."))

