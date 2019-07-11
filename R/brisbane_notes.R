list.files("data")
load("data/starling.RData")
library(lme4)
library(ggplot2)
ggplot(dataf,
       aes(mnth,stmass, colour=roostsitu)) +
  geom_point() +
  geom_line(aes(group=subject)) +
  facet_wrap(~roostsitu)

m1 <- lmer(stmass ~ mnth*roostsitu + (1|subject),
     data=dataf)

summary(m1)

library(lmerTest)

m2 <- lmer(stmass ~ mnth*roostsitu + (1|subject),
           data=dataf)
class(m2)
summary(m2, ddf="Kenward-Roger")

###
plot(m2)
plot(m2, type=c("p","smooth"),
     col.line="red")
plot(m2,
     sqrt(abs(resid(.)))~fitted(.))
plot(m2,roostsitu~resid(.))
library(lattice)
qqmath(m2 ,col=dataf$roostsitu,
       id=0.25)

ranef(m2)

dotplot(ranef(m2))
qqmath(ranef(m2))

fixef(m2)

predict(m2)
nd <- data.frame(mnth="Jan",roostsitu="tree") ## , subject="tree5")
predict(m2,newdata=nd, re.form=NA)

library(emmeans)
emmeans(m2, ~roostsitu)
emmeans(m2, ~mnth)
as.data.frame(emmeans(m2, ~mnth+roostsitu))
pairs(emmeans(m2, ~roostsitu))
###
library(dotwhisker)
dwplot(m1)
##
car::Anova(m1, test="F")
m0 <- update(m1, . ~ . - mnth:roostsitu)
m0 <- update(m1, . ~ mnth + roostsitu + (1|subject))

?car::Anova

AIC(m0,m1)
library(bbmle)
AICtab(m0,m1)
anova(m0,m1)
m0

library(pbkrtest)
suppressMessages(PBmodcomp(m1,m0))

  ## ~ ... + temp*time + ... + (temp|time) ...

gm1 <- glmer(incidence/size ~ period + (1|herd),
            data=cbpp,
            weights=size,
            family=binomial)
## glmer(cbind(incidence, size-incidence) ~ ...)
cbpp$obs <- factor(1:nrow(cbpp))

gm2 <- update(gm1, . ~ . + (1|obs))
AICtab(gm1,gm2)

library(glmmTMB)
gm3 <- glmmTMB(incidence/size ~ period + (1|herd),
             data=cbpp,
             weights=size,
             family=betabinomial)
summary(gm3)
library(broom.mixed)
AICtab(gm1,gm2,gm3)
dwplot(list(gm1,gm2,gm3))

L <- load("data/culcita.RData")
gm4 <- glmer(predation~ttt + (1|block), data=culcita_dat,
      family=poisson)
confint(gm4, parm="beta_")
culcita_dat$anypred <- as.numeric(culcita_dat$pred>0)
## try binomial model instead of Poisson -- "was
## there any predation?" vs. "how many events"
gm5 <- update(gm4, anypred ~ ., family=binomial)

aa <- allFit(gm5)
summary(aa)

gm6 <- update(gm5, . ~ ttt + (ttt|block),
   control=glmerControl(optimizer="bobyqa",
            optCtrl=list(maxfun=1e6)))

## what now????
rePCA(gm6)