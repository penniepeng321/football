#gee
#apply gee to regression of game results versus network measures
#gee r packages
# install.packages('gee')
# install.packages('geepack')
# install.packages('geeglm')
# install.packages('multgee')
# install.packages('MESS')
# install.packages('CRTgeeDR')
# install.packages('repolr')
# install.packages('BCgee')
# install.packages('wgeesel')#calculate QIC, RJ to compare gee models
# install.packages('PGEE')#penalized gee, model selection


#we apply gee glm for game results

#game result: win/loss win=1, loss=0, binary, logistic regression
#covariates are continuous

#we observe long time series for each object, we can use undefined correlation structure
#in modeling gee

#longitudinal data
#one perspective:
# 1. all teams
# 2. all games for the team
# 3. games are repeated measures for the team
# 4. game network data are covariates
# 5. opponents rankings are covariates

#another perspective:
# 1. all players
# 2. all games for the player
# 3. games are repeated measures for the player
# 4. game network data are covariates
# 5. opponents team rankings are covariates

#we organize data in the framework under these two perspectives
#25 teams
#2856 matches
#1263 players

#seems more interpretable from team perspective, how teams win
#anyway players know how to perform in social network, how players win

#read in data
setwd('e:/generalized_estimating_equations')
library(readxl)
library(sqldf)
library(geepack)
library(tidyr)
library(dplyr)
library(wgeesel)
library(PGEE)

#read in original football data
data <- read_excel("e:/social_network/PN12--17.xlsx",sheet=1,col_names = T,
                   col_types = NULL ,na="", skip=0)
#str(data)

#cui yi xiong helps identify mistakes in names.csv
#re-read in names.csv as qw7
qw7 <- read_excel("e:/social_network/names1.xlsx",sheet=1,col_names = T,
                  col_types = NULL ,na="", skip=0)
#str(qw7)
#replace original names in dataset with unified names
#replace passer's name
qwer1=match(data$Passer,qw7$oname)#oname is original name in initial data
#sum(is.na(qwer1))
data$Passer=qw7$rname[qwer1]#rname is unified name
#replace receiver's name
qwer2=match(data$Receiver,qw7$oname)
#sum(is.na(qwer2))
data$Receiver=qw7$rname[qwer2]

table(data$W.D.L)
# 0      1      3 
# 141250 102168 133480
#games results: ordinal with three levels
#use multinomial logistic regression (glm)
table(data$Rposition)
# 0     1     2     3     4     5 
# 65320 64145 61195 81058 61962 43218 
table(data$`Rstarter/Subs`)
# 1      2 
# 297954  78944 
table(data$RNationality)
# 1      2 
# 262018 114880

#find opponents teams ranking from original data
#consider player ranking (average player ranking from rival team)?
rankingset=data %>%
  group_by(Team,Year,Mon,Day) %>%
  summarise(n=n(),
            teamrank=mean(Ranking),
            rivalrank=mean(RankingO))
#one fear is that opponent team ranking RankingO is too strong and
#other covariates are not significant, pseudo signals with strong
#correlation with RankingO are falsely taken to be significant
#let's try and see
#str(rankingset)

#str(data)
#get match-wise information from original dataset
data1=data[,c(1,2,3,4,5,6,7,8,9)]
#str(data1)
data2=data1[!duplicated(data1),]
#str(data2)
#2856 matches

allpassnumset=data %>%
  group_by(Team,Year,Mon,Day) %>%
  summarise(n=n(),
            allpassnum=sum(Passes))

data3=merge(data2,allpassnumset,
            by.x=c('Team','Year','Mon','Day'),
            by.y=c('Team','Year','Mon','Day'),
            all.x=T,
            all.y=T)
#str(data3)

#use team, year, mon and day to merge it with network_bymatch dataset dbase
#we will merge it later after getting network_bymatch 
#1.summarized by team
#2.summarized by player

#read in computed network measures for match*team*player
dbase = read.table(
  file = 'e:/social_network/network_bymatch20191007.csv',
  header = T,
  sep = ',',
  as.is = T
)
#str(dbase)

#conduct analysis from player perspective 

dbase1=merge(dbase,data3,
             by.x=c('team','year','mon','day'),
             by.y=c('Team','Year','Mon','Day'),
             all.x=T,
             all.y=T)
#nrow(dbase)
#nrow(dbase1)

#summarize network_bymatch by player
#*players are patients
#*matches are trials
#*WDLs are trial results
#*teams are multi-centers, ie clinical centers
#*network measures are patients' covariates

dbase1$isW=as.numeric(dbase1$W.D.L==3)
dbase1$isD=as.numeric(dbase1$W.D.L==1)
dbase1$isL=as.numeric(dbase1$W.D.L==0)
#we fit models respectively for isW, isD, isL

#add name id
#1-1 mapping between player names and name id
#nameseq in dbase2 is within each match not global
tmp=unique(dbase1$name)
tmplen=length(tmp)#1263
new=data.frame(name=tmp,
               nameid=c(1:tmplen))

dbase2=merge(dbase1,new,
             by.x=c('name'),
             by.y=c('name'),
             all.x=T,
             all.y=T)
#str(dbase2)

#add player information/covariates into dbase2
#get passer info from data
tpy1=data[,c('Year','Mon','Day','Team','Teamo','Passer','Passer.Number','PNationality',
             'PPosition','Starter/subs','%Time Played','Height','Weight','Age')]
#str(tpy1)
#alter names
colnames(tpy1)<-c('year','mon','day','team','Teamo','name','Number','Nationality',
                  'Position','Starter/subs','%Time Played','Height','Weight','Age')
#get receiver info from data
tpy2=data[,c('Year','Mon','Day','Team','Teamo','Receiver','Rnumber','RNationality',
             'Rposition','Rstarter/Subs','R%Played','Rheight','Rweight','Rage')]
#str(tpy2)
#alter names
colnames(tpy2)<-c('year','mon','day','team','Teamo','name','Number','Nationality',
                  'Position','Starter/subs','%Time Played','Height','Weight','Age')
tpy3=rbind(tpy1,tpy2)
#str(tpy3)
#remove duplicates
tpy3=tpy3[!duplicated(tpy3),]
#nrow(tpy3)
#35793

#merge dbase2 with tpy3 so that player info is added
#str(dbase2);str(tpy3)
tpy4=merge(dbase2,tpy3,
             by.x=c('name','team','year','mon','day','Teamo'),
             by.y=c('name','team','year','mon','day','Teamo'),
             all.x=T)
#str(tpy4)
#35769
tpy4=tpy4[order(tpy4$X),]
#drop names where position=0
tpy4=tpy4[tpy4$Position!=0,]
#str(tpy4)
#29800

dbase2=tpy4
dbase2=dbase2[order(dbase2$X),]
#str(dbase2)

#below shows two ways of fitting geeglm:
#1. use geeglm to fit gee to full model, but there is no function in this package
#   to calculate information criterion to compare gee models, fit model1 to model20
#2. use wgee combined with QIC.gee so that it computes QIC criterion which can be
#   used to compare gee models, re-fit model1 to model20

#note: 1. we fit models respectively for isW, isL, isD
#      2. we fit models for full model and models for each set of network measures
#         respectively

#1. geeglm
#full model
model1=geeglm(isW~degree+closeness+betweenness+stress+neigh_tol_tol+ML+Position+Nationality+`Starter/subs`+`%Time Played`+Height+Weight+Age,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model1)
model2=geeglm(isD~degree+closeness+betweenness+stress+neigh_tol_tol+ML+Position+Nationality+`Starter/subs`+`%Time Played`+Height+Weight+Age,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model2)
model3=geeglm(isL~degree+closeness+betweenness+stress+neigh_tol_tol+ML+Position+Nationality+`Starter/subs`+`%Time Played`+Height+Weight+Age,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model3)

#2. wgee+QIC.gee
#full model
model1=wgee(isW~degree+closeness+betweenness+stress+neigh_tol_tol+ML+Position+Nationality+`Starter/subs`+`%Time Played`+Height+Weight+Age,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model1)
QIC.gee(model1)
model2=wgee(isD~degree+closeness+betweenness+stress+neigh_tol_tol+ML+Position+Nationality+`Starter/subs`+`%Time Played`+Height+Weight+Age,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model2)
QIC.gee(model2)
model3=wgee(isL~degree+closeness+betweenness+stress+neigh_tol_tol+ML+Position+Nationality+`Starter/subs`+`%Time Played`+Height+Weight+Age,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model3)
QIC.gee(model3)

#1. other models under geeglm
#notice that degree=indegree+outdegree
model4=geeglm(isL~indegree+outdegree,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model4)

model5=geeglm(isW~indegree+outdegree,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model5)

model6=geeglm(isD~indegree+outdegree,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model6)

#indegree appears to be more relevant than outdegree

model7=geeglm(isW~closeness,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model7)

model8=geeglm(isW~betweenness,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model8)

model9=geeglm(isW~stress,
              id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model9)

model10=geeglm(isW~neigh_out_in+neigh_out_out+neigh_in_in+neigh_in_out+
                 neigh_tol_in+neigh_tol_out,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model10)
#neighbourhood indegree appears to be more relevant than outdegree

model11=geeglm(isW~neigh_out_in+neigh_in_in+neigh_tol_in,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model11)

model12=geeglm(isW~neigh_tol_in,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model12)

model13=geeglm(isW~neigh_out_in+neigh_in_in,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model13)

model14=geeglm(isW~neigh_in_in,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model14)

model15=geeglm(isW~neigh_tol_tol,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model15)

model16=geeglm(isW~Ranking+RankingO+ML,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model16)

model17=geeglm(isW~indegree+closeness+betweenness+stress+Ranking+RankingO+ML,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model17)
#correlation between indegree, closeness and betweenness is strong
#we may not place all of them in the same model

model18=geeglm(isW~indegree+stress+Ranking+RankingO+ML,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model18)
#correlation between indegree and stress is okay

model19=geeglm(isW~closeness+stress+Ranking+RankingO+ML,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model19)
#correlation between stress and closeness is strong

model20=geeglm(isW~betweenness+stress+Ranking+RankingO+ML,
               id=nameid,family=binomial(link='logit'),data=dbase2)
summary(model20)
#correlation between betweenness and stress is strong

#use anova to compare models
#anova(model1,model20)
#too complex, never gonna happen

#2. other models under wgee+QIC.gee
#use QIC to compare models, QIC is information criterion 
#based upon quasi-likelihood used in gee 
#install wgeesel

model1=wgee(isW~degree+closeness+betweenness+neigh_tol_tol+ML+n+allpassnum,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model1)

model2=wgee(isD~degree+closeness+betweenness+neigh_tol_tol+ML+n+allpassnum,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model2)

model3=wgee(isL~degree+closeness+betweenness+neigh_tol_tol+ML+n+allpassnum,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model3)

#notice that degree=indegree+outdegree
model4=wgee(isL~indegree+outdegree,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model4)

model5=wgee(isW~indegree+outdegree,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model5)

model6=wgee(isD~indegree+outdegree,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model6)

#indegree appears to be more relevant than outdegree

model7=wgee(isW~closeness,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model7)

model8=wgee(isW~betweenness,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model8)

model9=wgee(isW~stress,
            id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model9)

model10=wgee(isW~neigh_out_in+neigh_out_out+neigh_in_in+neigh_in_out+
               neigh_tol_in+neigh_tol_out,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model10)
#neighbourhood indegree appears to be more relevant than outdegree

model11=wgee(isW~neigh_out_in+neigh_in_in+neigh_tol_in,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model11)

model12=wgee(isW~neigh_tol_in,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model12)

model13=wgee(isW~neigh_out_in+neigh_in_in,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model13)

model14=wgee(isW~neigh_in_in,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model14)

model15=wgee(isW~neigh_tol_tol,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model15)

model16=wgee(isW~Ranking+RankingO+ML,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model16)

model17=wgee(isW~indegree+closeness+betweenness+stress+Ranking+RankingO+ML,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model17)
#correlation between indegree, closeness and betweenness is strong
#we may not place all of them in the same model

model18=wgee(isW~indegree+stress+Ranking+RankingO+ML,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model18)
#correlation between indegree and stress is okay

model19=wgee(isW~closeness+stress+Ranking+RankingO+ML,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model19)
#correlation between stress and closeness is strong

model20=wgee(isW~betweenness+stress+Ranking+RankingO+ML,
             id=dbase2$nameid,family="binomial",data=dbase2,corstr="exchangeable")
summary(model20)
#correlation between betweenness and stress is strong


#checked model fit between wgee and geeglm, they are same, though they 
#are from different r packages

#calculate QIC to compare these gee models
c(QIC.gee(model1),QIC.gee(model2),QIC.gee(model3),QIC.gee(model4),
  QIC.gee(model5),QIC.gee(model6),QIC.gee(model7),QIC.gee(model8),
  QIC.gee(model9),QIC.gee(model10),QIC.gee(model11),QIC.gee(model12),
  QIC.gee(model13),QIC.gee(model14),QIC.gee(model15),QIC.gee(model16),
  QIC.gee(model17),QIC.gee(model18),QIC.gee(model19),QIC.gee(model20))

c(RJ.gee(model1),RJ.gee(model2),RJ.gee(model3),RJ.gee(model4),
  RJ.gee(model5),RJ.gee(model6),RJ.gee(model7),RJ.gee(model8),
  RJ.gee(model9),RJ.gee(model10),RJ.gee(model11),RJ.gee(model12),
  RJ.gee(model13),RJ.gee(model14),RJ.gee(model15),RJ.gee(model16),
  RJ.gee(model17),RJ.gee(model18),RJ.gee(model19),RJ.gee(model20))

RJ.gee(model18)#3533
RJ.gee(model1)#4929

#an example of wgee
#data(imps) 
#fit <- wgee(Y ~ Drug+Sex+Time, data=imps, id=imps$ID, family="binomial", corstr="exchangeable")
#QIC.gee(fit)


#sample matches data, build prediction, calculate prediction error


#another model to compare with gee after getting team-wise summary statistics 
#build team id
tmp=unique(dbase2$team)
tmplen=length(tmp)
newt=data.frame(team=tmp,
                teamid=c(1:tmplen))
dbase3=merge(dbase2,newt,
             by.x=c('team'),
             by.y=c('team'),
             all.x=T,
             all.y=T)
str(dbase3)

model21=wgee(isW~team,
             id=dbase2$nameid,family="binomial",data=dbase3,corstr="exchangeable")
summary(model21)

tmpframe=data.frame(team=names(model21$beta),
                    coefficient=as.numeric(model21$beta))
tmpframe=tmpframe[order(tmpframe$coefficient),]
tmpframe

#historical average ranking of these teams
str(data)
tmp=data[,c(1,2,3,4,7)]
str(tmp)
tmp1=tmp %>%
  group_by(Team) %>%
  summarise(n=n(),
            teamrank=mean(Ranking))
tmp1=as.data.frame(tmp1)
tmp1$team1=paste('team',tmp1$Team,sep='')

tmp2=merge(tmpframe,tmp1,
           by.x=c('team'),
           by.y=c('team1'),
           all.x=T,
           all.y=T)
tmp2=tmp2[order(tmp2$coefficient),]
tmp2
#compare two models: anova model for team variable, summary statistic for historical team rank
#results do not agree
#results agree for most teams
#but not for Beijing Guoan and Guangzhou R&F
#using teamid does not serve our goal well
#we seek team-wise summary statistic based upon individual network measures

#conduct analysis from team perspective 

#add match id
tmp=dbase3[,c(1,3,4,5)]
tmp1=tmp[!duplicated(tmp),]
tmplen=nrow(tmp1)
tmp1$matchid=c(1:tmplen)
str(tmp1)
#2856 matches

dbase4=merge(dbase3,tmp1,
             by.x=c('team','year','mon','day'),
             by.y=c('team','year','mon','day'),
             all.x=T,
             all.y=T)
str(dbase4)

#summarize network measures by matches (matchid)

teamsum=dbase4 %>%
  group_by(matchid) %>%
  summarise(n=n(),
            v1=sd(closeness,na.rm=T),
            v2=max(closeness,na.rm=T)-min(closeness,na.rm=T),
            v3=max(closeness,na.rm=T),
            v4=sd(closeness,na.rm=T)/mean(closeness,na.rm=T),
            v5=sd(betweenness,na.rm=T),
            v6=max(betweenness,na.rm=T)-min(betweenness,na.rm=T),
            v7=max(betweenness,na.rm=T),
            v8=sd(betweenness,na.rm=T)/mean(betweenness,na.rm=T),
            v9=sd(stress,na.rm=T),
            v10=max(stress,na.rm=T)-min(stress,na.rm=T),
            v11=max(stress,na.rm=T),
            v12=sd(stress,na.rm=T)/mean(stress,na.rm=T),
            v13=sd(indegree,na.rm=T),
            v14=max(indegree,na.rm=T)-min(indegree,na.rm=T),
            v15=max(indegree,na.rm=T),
            v16=sd(indegree,na.rm=T)/mean(indegree,na.rm=T),
            v17=sd(outdegree,na.rm=T),
            v18=max(outdegree,na.rm=T)-min(outdegree,na.rm=T),
            v19=max(outdegree,na.rm=T),
            v20=sd(outdegree,na.rm=T)/mean(outdegree,na.rm=T))

tmp=dbase4[,c(1:4,23:32,35)]
str(tmp)
tmp1=tmp[!duplicated(tmp),]
str(tmp1)#2856 rows

str(dbase4)

teamdata=merge(tmp1,teamsum,
      by.x=c('matchid'),
      by.y=c('matchid'),
      all.x=T,
      all.y=T)

str(teamdata)
#add team id
teamdata1=merge(teamdata,newt,
      by.x=c('team'),
      by.y=c('team'),
      all.x=T,
      all.y=T)
str(teamdata1)

table(teamdata1$teamid)
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25 
# 175 176  88  90  30 179 180  30 120 149  30  59 150 180 179  60 179 179 120 149  59  29 177  30  59
table(teamdata1$ML)
# Away Home 
# 1428 1428 

sum(is.na(teamdata1))#290
apply(teamdata1,2,function(x) sum(is.na(x)))#na's are in v4
#290 matches have closeness equal to zero, or NA closeness
#we drop v4 in our analysis
nrow(teamdata1)#2856 matches

#analyze team-wise data using penalized gee
#calculate tuning parameter lambda
# CVfit(formula="teamdata1$isW~teamdata1$ML+teamdata1$Ranking+teamdata1$RankingO+
# teamdata1$allpassnum+teamdata1$v1+teamdata1$v2+teamdata1$v3+teamdata1$v5+teamdata1$v6+
# teamdata1$v7+teamdata1$v8+teamdata1$v9+
# teamdata1$v10+teamdata1$v11+teamdata1$v12+teamdata1$v13+teamdata1$v14+teamdata1$v15+
# teamdata1$v16+teamdata1$v17+teamdata1$v18+teamdata1$v19+teamdata1$v20+teamdata1$teamid", 
#       data=teamdata1,
#       id=teamdata1$teamid, family = binomial(link = "logit"),
#       fold = 4, lambda.vec = c(1:100), pindex = NULL, eps = 10^-6, maxiter = 30,
#       tol = 10^-3)

# CVfit(data = teamdata1, formula="isW~ML+Ranking+RankingO",
#       id = teamid, family = binomial(link = "logit"),
#       fold = 4, lambda.vec = c(1:10), pindex = NULL, eps = 10^-6, maxiter = 30,
#       tol = 10^-3)

#fit penalized gee model
# model21=PGEE(isW~ML+Ranking+RankingO+allpassnum+v1+v2+v3+v5+v6+v7+v8+v9+
#        v10+v11+v12+v13+v14+v15+v16+v17+v18+v19+v20+teamid, 
#      id=teamid, data=teamdata1, na.action = NULL, family = binomial(link = "logit"),
#      corstr = "exchangeable", Mv = NULL, beta_int = NULL, R = NULL, scale.fix = TRUE,
#      scale.value = 1, lambda=200, pindex = NULL, eps = 10^-6, maxiter = 30, tol = 10^-3,
#      silent = TRUE)
# #lambda specifies a vector of tuning parameters to choose from using cross validation
# 
# summary(model21)

#fit normal gee model
#unpenalized
# model22 <- MGEE(isW~ML+Ranking+RankingO+allpassnum+v1+v2+v3+v5+v6+v7+v8+v9+
#                  v10+v11+v12+v13+v14+v15+v16+v17+v18+v19+v20+teamid, id = teamid, 
#                data = teamdata1, na.action = NULL,
#                family = binomial(link = "logit"), corstr = "exchangeable", Mv = NULL,
#               R = NULL, scale.fix = TRUE,
#                scale.value = 1, maxiter = 30, tol = 10^-3, silent = TRUE)
# summary(model22)

library(gee)
model22=gee(isW~ML+Ranking+RankingO+allpassnum+v1+v2+v3+v5+v6+v7+v8+v9+
              v10+v11+v12+v13+v14+v15+v16+v17+v18+v19+v20,id=teamid,
            data=teamdata1,family = binomial(link = "logit"), corstr = "exchangeable")
#gee has estimates diverging with all variables in it with unstructured correlation
#so it is impossible to do penalized gee for model selection then
#so we use exchangeable correlation to reduce number of parameters
summary(model22)

#now we fit model with only max values of these measures
model23=gee(isW~ML+Ranking+RankingO+allpassnum+v3+v7+v11+v15+v19,id=teamid,
            data=teamdata1,family = binomial(link = "logit"), corstr = "exchangeable")
summary(model23)


