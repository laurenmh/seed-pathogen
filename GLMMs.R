## This script uses glmms for germination and pathogen abundance
## to quantify the variance within vs. among moms

library(corrplot)
library(lme4)
library(car)
library(tidyverse)
library(lavaan)


####################################### Start with Festuca
Fsite<-read.csv('Data/FesRoe Site data.csv')
Fdat<-read.csv('Data/FesRoe germination and pathogens FINAL 2019_7_17.csv')

####### First do a PCA of Festuca climate variables
# can't have more variables than observations, so eliminate the most highly correlated from the pca
corrplot(cor(Fsite[,c('Latitude','LastyrElevation','SpringPPT','SpringTminC','SpringTmeanC','SpringTmaxC',
                      'SpringTdewmeanC','SpringVPDmin','SpringVPDmax','SoilPercentNitrogen')]),addCoef.col='black')
# pick 6 least-correlated variables and re-scale to 0 mean and 1 sd
Fsite_scaled<-apply(Fsite[,c('SpringPPT','SpringTminC','SpringTmeanC','SpringTdewmeanC','SpringVPDmin','SoilPercentNitrogen')],2,scale,scale=TRUE,center=TRUE)
Fpca<-princomp(Fsite_scaled[,c('SpringPPT','SpringTminC','SpringTmeanC','SpringTdewmeanC','SpringVPDmin','SoilPercentNitrogen')])
biplot(Fpca)
summary(Fpca)
# get first 3 principle components which together explain > 90% of variation
Fsite$PC1<-Fpca$scores[,1]
Fsite$PC2<-Fpca$scores[,2]
Fsite$PC3<-Fpca$scores[,3]
# what do these represent?
loadings(Fpca)
# PC1 is mostly an axis of wet and cold vs. dry and hot
# PC2 is mostly an axis of high vpd and soil N and low Tmean vs. low vpd and soil N and high Tmean
# PC3 is mostly an axis of high soil N and low vpd and Tmin vs. low soil N and high vpd and Tmin


###### Merge climate PCs into performance data set
# First make the population names consistent
unique(Fdat$SiteName)%in%unique(Fsite$SiteName) # not all the same
unique(Fdat$SiteName)
# Fill in missing site names
Fdat[which(Fdat$SiteName==''),]
Fdat$SiteName[Fdat$SiteName==''&!is.na(Fdat$IND)]<-"Upper Table Rock"
# Fix differences between data sets
unique(Fsite$SiteName)
Fdat$SiteName<-as.character(Fdat$SiteName)
Fdat$SiteName[Fdat$SiteName=='Hazel Dell Upper']<-"Hazel Dell-upper"
Fdat$SiteName[Fdat$SiteName=='Whidby Island']<-"Whidbey Island"
unique(Fdat$SiteName)%in%unique(Fsite$SiteName) # not all the same
# now merge datasets
Fdat<-merge(Fdat,Fsite,by=c('SiteName')) %>%
  mutate(PathogenRichness = Nr..kinds,
         Germ = Germ.)

###### Fit germination models
# germination as a function of climate PCs and plant density, with mom nested within pop as random effects
Fgerm<-glmer(Germ.~PC1+PC2+PC3+PlantDensity +(1|SiteName/SiteInd),family='binomial',data=Fdat,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(Fgerm)
# there is no variance explained by site after accounting for climate effects
# this is generating the 'singular fit' warning
Fgerm2<-glmer(Germ.~PC1+PC2+PC3+PlantDensity +(1|SiteInd),family='binomial',data=Fdat,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
anova(Fgerm,Fgerm2)
# the likelihood ratio test confirms that there is no support for including a random effect of site
Fgerm3<-glm(Germ.~PC1+PC2+PC3+PlantDensity,family='binomial',data=Fdat)
anova(Fgerm2,Fgerm3)
# however, the random effect of mom is highly significant, strong support for variation among moms in germination success
summary(Fgerm2)
as.data.frame(VarCorr(Fgerm2)) # 2.277 is the variance in germination due to moms
# there is a significant effect of the first two climate PCs, but not the third or plant density
# germination decreases with increasing PC1 (wetter and colder Tmin and Tmean)
# germination increases with increasing PC2 (higher soil N and VPDmin and colder Tmean)


##### Fit as an SEM
## a mess, figuring out the data structure

myvar<-c("SiteName", "Region", "SiteInd", "GreenScale", "Germ", "PC1", "PC2", "PC3", "PlantDensity", "PathogenRichness")

Fdat2 <- Fdat[myvar] %>%
  tbl_df() %>%
  group_by(SiteName, SiteInd) %>%
  mutate(avgRich = mean(PathogenRichness, na.rm = T)) %>%
  mutate(perGerm = sum(Germ)/n()) %>%
  tbl_df()

###### Fit germination models
# germination as a function of climate PCs and plant density, with mom nested within pop as random effects
Fgerm<-glmer(Germ~PC1+PC2+PC3+PlantDensity + avgRich +(1|SiteName/SiteInd),family='binomial',data=Fdat2,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(Fgerm)
# there is no variance explained by site after accounting for climate effects
# this is generating the 'singular fit' warning
Fgerm2<-glmer(Germ~PC1+PC2+PC3+PlantDensity + avgRich +(1|SiteInd),family='binomial',data=Fdat2,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
anova(Fgerm,Fgerm2)
# the likelihood ratio test confirms that there is no support for including a random effect of site
Fgerm3<-glm(Germ~PC1+PC2+PC3+PlantDensity + avgRich ,family='binomial',data=Fdat2)
anova(Fgerm2,Fgerm3)
# however, the random effect of mom is highly significant, strong support for variation among moms in germination success
summary(Fgerm2)
as.data.frame(VarCorr(Fgerm2)) # 2.277 is the variance in germination due to moms
# there is a significant effect of the first two climate PCs, but not the third or plant density
# germination decreases with increasing PC1 (wetter and colder Tmin and Tmean)
# germination increases with increasing PC2 (higher soil N and VPDmin and colder Tmean)

Frich <-lmer(avgRich~PC1+PC2+PlantDensity +(1|SiteName),data=mydat2)
summary(Frich)
ggplot(mydat2, aes(x=PC1, y =avgRich)) + geom_point() + geom_smooth()

mydat2 <- aggregate(.~SiteName + SiteInd, data=mydat, base::mean) %>%
  tbl_df()

ggplot(mydat2, aes(x=perGerm, y=PlantDensity)) + geom_point() + geom_smooth(se = F, method = lm)
ggplot(mydat2, aes(y=perGerm, x=PathogenRichness)) + geom_point() + geom_smooth(se = F, method = lm)
ggplot(mydat2, aes(y=perGerm, x=PC1)) + geom_point() + geom_smooth(se = F, method = lm)
ggplot(mydat2, aes(y=perGerm, x=PC2)) + geom_point() + geom_smooth(se = F, method = lm)
ggplot(mydat2, aes(x=PlantDensity, y=PC1)) + geom_point() + geom_smooth(se = F, method = lm)
ggplot(mydat2, aes(y=avgRich, x=PC2)) + geom_point() + geom_smooth(se = F, method = lm)
ggplot(mydat2, aes(y=avgRich, x=PC1)) + geom_point() + geom_smooth(se = F, method = lm)


mydat3 <- aggregate(.~SiteName, data=mydat, base::mean)

l <- lm(Germ ~ PC1 + PC2, data = mydat2)
summary(l)


model<-'
perGerm ~PC1 +  PC2 + PlantDensity+ avgRich
avgRich ~ PC1 + PC2 + PlantDensity
PlantDensity ~ PC1 + PC2
'

fita <- sem(model,std.ov=T,missing="ml", data=mydat3, cluster = "SiteName")
summary(fita, fit.measures=TRUE,rsquare=T) 

ggplot(mydat3, aes(x=PC2, y=PlantDensity)) + geom_point()

model <- '
level: 1
perGerm ~ avgRich

level: 2
perGerm~  PC1 + PC2 + PlantDensity
PlantDensity ~ PC1 + PC2
avgRich ~ PC1 + PC2
PlantDensity ~~ avgRich
'

model <- '
perGerm ~ avgRich + PC1 + PC2 + PlantDensity
PlantDensity ~ PC1 + PC2
avgRich ~ PC1 + PC2
PlantDensity ~~ avgRich
'



model <- '
level: 1
Germ ~ avgRich

level: 2
Germ~  PC1 + PC2 + PlantDensity
avgRich ~ PC1 + PC2 + PlantDensity
'


model <- '
perGerm ~ avgRich + PC1 + PlantDensity
avgRich ~ PC1 + PlantDensity
'



model <- '
level: 1
perGerm ~ avgRich + PC1 + PC2 + PlantDensity
avgRich ~ PC1 + PC2 + PlantDensity

level: 2
PlantDensity ~ PC1 + PC2
'

fit1 <- sem(model,std.ov=T,missing="ml", data=mydat2, cluster = "SiteName") #this uses robust chi square, which I've read is good and conservative
summary(fit1, fit.measures=TRUE,rsquare=T)


model <- '
perGerm ~ avgRich + PC1 + PC2 + PlantDensity
avgRich ~ PC1 + PC2 
PlantDensity ~ PC1 + PC2
'

fit1 <- sem(model,std.ov=T,missing="ml", data=mydat3) #this uses robust chi square, which I've read is good and conservative
summary(fit1, fit.measures=TRUE,rsquare=T)


l <- lm(avgRich ~ PlantDensity, data = mydat3)
summary(l)

###### Fit pathogen richness models
hist(Fdat$Nr..kinds) # model as poisson distributed
Fpath<-glmer(Nr..kinds~PC1+PC2+PC3+PlantDensity+(1|SiteName/SiteInd),data=Fdat,family='poisson',control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(Fpath)
# test significance of random effects with likelihood ratio test
Fpath2<-glmer(Nr..kinds~PC1+PC2+PC3+PlantDensity+(1|SiteInd),data=Fdat,family='poisson',control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
anova(Fpath,Fpath2) # Strong support for variation among sites (and convergence problems when site is removed from the model)
Fpath3<-glmer(Nr..kinds~PC1+PC2+PC3+PlantDensity+(1|SiteName),data=Fdat,family='poisson',control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
anova(Fpath,Fpath3) # Strong support for variation among moms within site (and convergence problems when mom is removed from the model)
# strong support for variation in pathogen richness among sites and among moms within sites
as.data.frame(VarCorr(Fpath)) # .00797 is the variance among moms and .01774 is the variance among sites in pathogen richness
summary(Fpath)
# there is no significant effect of climate or plant density on pathogen richness

# save workspace
save.image("~/HOPS project/seed-pathogen/Festuca glmms.RData")
