#use social network to analyze football data
#some social networks packages in r
# install.packages('ergm')
# install.packages('igraph')
# install.packages('network')
# install.packages('statnet')
# install.packages('RSiena')
# install.packages('rSoNIA')
# install.packages('sna')
# install.packages('networkD3')
# install.packages('latentnet')

# install.packages("readxl")


library(readxl)
library(sqldf)
library(igraph)
library(sna)

setwd('e:/social_network')
data <- read_excel("PN12--17.xlsx",sheet=1,col_names = T,
                   col_types = NULL ,na="", skip=0)
str(data)
#check duplicates
data1=data[!duplicated(data),]
#check missing values proportion
sum(is.na(data1))
#no missing value
str(data1)

#plot(table(data1$Team))
c1=table(data1$Team)
str(c1)
#25 teams
#their relative frequency is
as.data.frame(c1)
str(data1)

library(tidyr)
library(dplyr)
#install.packages('tidyr')

data1 %>%
  group_by(Team,Year,Mon,Day) %>%
  summarise(n=n(),
            mrank=mean(Ranking),
            m1rank=mean(RankingO))
# data1 %>%
#   group_by(Team,Year) %>%
#   summarise(n=n(),
#             mrank=mean(Ranking),
#             m1rank=mean(RankingO))

qw1=data1 %>%
  group_by(Team,Year) %>%
  summarise(n=n(),
            mrank=mean(Ranking),
            m1rank=mean(RankingO))

qw0=data1 %>%
  group_by(Team) %>%
  summarise(n=n(),
            mrank=mean(Ranking),
            m1rank=mean(RankingO))

#install.packages('lattice')
library(lattice)
xyplot(mrank~Year,data =qw1,groups=Team,type='l')

as.data.frame(table(data1$Teamo))

qw2=as.data.frame(table(data1$Passer))
str(qw2)
qw3=as.data.frame(table(data1$Passer.Number))
str(qw3)
qw4=as.data.frame(table(data1$Receiver))
str(qw4)

qw5=as.data.frame(table(data1$Rnumber))
str(qw5)

qw6=as.data.frame(table(c(data1$Passer,data1$Receiver)))
str(qw6)
#2070

#slightly different duplicate names
qwer=match(qw6$Var1,data1$Receiver)
qw6$year=data1$Year[qwer]
qw6$month=data1$Mon[qwer]
qw6$day=data1$Day[qwer]
qw6$team=data1$Team[qwer]
qw6$rnumber=data1$Rnumber[qwer]
qw6$rname=qw6$Var1

#sort qw6 by team and rnumber
qw7=qw6[with(qw6, order(team, rnumber,year)), ]

#qw7$var1 is factor
#want it to be character
#calculate similarity measure of var1 ie original names
qw7$oname=as.character(qw7$Var1)
#unlist(strsplit(qw7$oname[1],''))
#break string into elements(letters, sapces)
#count number of common elements
#divided by number of all elements
#this is similarity measure

#same team and number to unify names
fgrow=nrow(qw6)
tmp1=unlist(strsplit(qw7$oname[1],''))
similarity_measure=c()
#tuning parameter
tuning=0.45
#increase tuning threshold, more unified names
#decrease tuning threshold, less unified names
#about 1400~ players, choose tuning until 1400 is reached

for(i in 2:fgrow){
  #break string into elements
  tmp2=unlist(strsplit(qw7$oname[i],''))
  lin1=length(intersect(tmp1,tmp2))
  lin2=max(length(tmp1),length(tmp2))
  similarity_measure=c(similarity_measure,lin1/lin2)
  if(qw7$team[i]==qw7$team[i-1]&
     qw7$rnumber[i]==qw7$rnumber[i-1]&
     lin1/lin2>tuning){
    qw7$rname[i]=qw7$rname[i-1]
  }
  tmp1=tmp2
}

length(unique(qw7$rname))
#1333

write.table(qw7, 
            file='names1.csv',
            sep=c(','), col.names = NA)

length(unique(qw7$Var1))
#2070

#25 teams * 57 rnumbers = 1425

#cui yi xiong helps identify mistakes in names.csv
#re-read in names.csv as qw7
qw7 <- read_excel("names1.xlsx",sheet=1,col_names = T,
                   col_types = NULL ,na="", skip=0)
str(qw7)

#replace original names in dataset with unified names
qwer1=match(data1$Passer,qw7$oname)
#sum(is.na(qwer1))
data1$rpasser=qw7$rname[qwer1]

qwer2=match(data1$Receiver,qw7$oname)
#sum(is.na(qwer2))
data1$rreceiver=qw7$rname[qwer2]

length(unique(c(data1$rpasser,data1$rreceiver)))
#1333
length(unique(c(data1$rpasser)))
#1204
length(unique(c(data1$rreceiver)))
#1263

data1=data1[!duplicated(data1),]
str(data1)
#376898*31
#no duplicate after unifying names

#check sum of passes per match == total passes
#summarize match-by-match
qw8=data1 %>%
  group_by(Team,Year,Mon,Day,Teamo) %>%
  summarise(n=n(),
            sum_pass=sum(Passes))
max(data1$`Total pass`)#121
str(qw8)
#2856*7
#team year mon day teamo n sum_pass

#one team has only at most one match on one day
max(qw8$sum_pass)#638

qw9=data1 %>%
  group_by(Team,Year,Mon,Day) %>%
  summarise(n=n(),
            sum_pass=sum(Passes))
max(data1$`Total pass`)#121
str(qw9)
#2856*6
#team year mon day teamo n sum_pass

#one team has only at most one match on one day
max(qw9$sum_pass)#638
qw9[which.max(qw9$sum_pass),]

#total pass is total number of passes per player per match
#update total pass
str(data1)#376898*31

#calculate total_pass for each passer per match
qw10=data1 %>%
  group_by(Team,Year,Mon,Day,rpasser) %>%
  summarise(n=n(),
            sum_pass=sum(Passes))

str(qw10)

#add calculated total_pass for each passer per match to original data1
#added updated data is qw11
# qw11=sqldf("select a.*,b.sum_pass,b.n from data1 a qw10 b
#       where a.Team = b.Team and a.Year=b.Year and a.Mon=b.Mon and a.Day=b.Day and a.rpasser=b.rpasser",
#       row.names = TRUE)
# str(qw11)#376627*33

qw11=merge(data1,qw10,by=c('Team','Year','Mon','Day','rpasser'),
           all.x=T)
str(qw11)#376898*33

nrow(data1)
#376898
nrow(data)
#376898

head(data.frame(qw11$`Total pass`,qw11$sum_pass))

#create marker for each match
qw12=data1 %>%
  group_by(Team,Year,Mon,Day) %>%
  summarise(n=n())

str(qw12)
#2856*5
#2856 matches
#create match id in match-grouped dataset
qw12$matchid=c(1:nrow(qw12))

#add match id to original dataset qw11
#added updated data is qw13
# qw13=sqldf("select a.*,b.matchid,b.n as nobs from qw11 a, qw12 b
#       where a.Team = b.Team and a.Year=b.Year and a.Mon=b.Mon and a.Day=b.Day",
#            row.names = TRUE)
# 
# str(qw13)#376627*36

qw13=merge(qw11,qw12,by=c('Team','Year','Mon','Day'),
           all.x=T)
str(qw13)#376898*35

#nobs is number of observations per match
#observation is player-by-player interaction summary per match
#n is number of observations per passer per match
#sum_pass is summation of passes per passer per match

#calculate adjacency matrix per match
#calculate various measures using adjacency matrix
nrow(qw13)#number of observations(data entries)=376898
nrow(qw12)#number of matches=2856
#calculate weight of links in network
qw13$lweight=qw13$Passes/qw13$sum_pass


#qw13 summarize calculate sum_pass for receivers
qw13_1=qw13 %>%
  group_by(Team,Year,Mon,Day,rreceiver) %>%
  summarise(n=n(),
            sum_pass_in=sum(Passes))
str(qw13_1)
str(qw13)

#add sum_pass_in ie total number of passes received by receiver
#to data qw13
# qw13_2=sqldf("select a.*,b.sum_pass_in from qw13 a, qw13_1 b
#       where a.Team = b.Team and a.Year=b.Year and a.Mon=b.Mon and a.Day=b.Day and a.rreceiver=b.rreceiver",
#            row.names = TRUE)
# str(qw13_2)#376602

qw13_2=merge(qw13,qw13_1,by=c('Team','Year','Mon','Day','rreceiver'),
            all.x=T)
str(qw13_2)#376898*37

#passes/sum_pass based upon receivers as link_weight
qw13_2$rlink=qw13_2$Passes/qw13_2$sum_pass_in
#use this link_weight as weight in graph

#number of passes<number of receivers
#adjacency matrix should be square?
as.data.frame(table(qw13$nobs))
#ranges from 77 to 156
#not square
#adjacency matrix across matches are not the same shape
#degree centrality index for every passer per match
qw14=qw13_2 %>%
  group_by(Team,Year,Mon,Day,rpasser) %>%
  summarise(n=n(),sum_out_i=sum(Passes))
qw15=qw13_2 %>%
  group_by(Team,Year,Mon,Day,rreceiver) %>%
  summarise(n=n(),sum_in_i=sum(Passes))
qw16=qw13_2 %>%
  group_by(Team,Year,Mon,Day) %>%
  summarise(n=n(),sum_all=sum(Passes))
qw1416=sqldf("select a.*,b.sum_all from qw14 a, qw16 b
      where a.Team = b.Team and a.Year=b.Year and a.Mon=b.Mon and a.Day=b.Day",
             row.names = TRUE)
nrow(qw1416)#32592
nrow(qw14)#32592
nrow(qw16)#2856
length(unique(data1$rreceiver))#1263
length(unique(qw13_2$rreceiver))#1263
length(unique(qw13_2$matchid))#2856

qw1516=sqldf("select a.*,b.sum_all from qw15 a, qw16 b
      where a.Team = b.Team and a.Year=b.Year and a.Mon=b.Mon and a.Day=b.Day",
             row.names = TRUE)
nrow(qw15)#35728
nrow(qw16)#2856
nrow(qw1516)#35728

qw1416$dci_out=qw1416$sum_out_i/qw1416$sum_all
qw1516$dci_in=qw1516$sum_in_i/qw1516$sum_all

nrow(qw1416)#32592 passer*match
nrow(qw1516)#35728 receiver*match

qw141516=merge(qw1416,qw1516,
               by.x=c('Team','Year','Mon','Day','rpasser'),
               by.y=c('Team','Year','Mon','Day','rreceiver'),
               all.x=T,
               all.y=T)
nrow(qw141516)#35729
#in 1 match, #passer=#receiver+1 
str(qw141516)
length(unique(qw141516$rpasser))#1263
length(unique(qw13_2$rpasser))#1204
length(unique(qw13_2$rreceiver))#1263


#storage of network measures
#for a specific match, calculate network
i=1
#get one match id
match_id=qw12$matchid[i]
#get all interactions for this match
all_inter=qw13_2[qw13_2$matchid==match_id,]
#str(all_inter)#144*35

#use this link weight rlink in qw13_2 and closeness formula in the book
#get all actor list for this match
actors=data.frame(name=unique(union(all_inter$rreceiver,all_inter$rpasser)))
relations=data.frame(from=all_inter$rpasser,
                     to=all_inter$rreceiver,
                     friendship=all_inter$Passes)
#create graph from dataframe
#g1=graph_from_data_frame(relations, directed=TRUE, vertices=actors)

#an example of igraph
# E<- rbind( c(1,3,3), c(3,4,1), c(4,2,2), c(1,5,1), c(5,6,2), c(6,2,10))
# colnames(E) <- c("from", "to", "capacity")
# g1 <- graph_from_data_frame(as.data.frame(E))
# max_flow(g1, source=V(g1)["1"], target=V(g1)["2"])

#to calculate closeness index for each receiver per match
# closeness_index=closeness(g1,vids=V(g1),mode='out')
# str(closeness_index)
# closeness_index1=data.frame(name=names(closeness_index),
#                             close=closeness_index)
# closeness_index1
# all_inter1=merge(all_inter,closeness_index1,
#       by.x=c('rreceiver'),
#       by.y=c('name'),
#       all.x=T)
# all_inter1
# 
# #betweenness
# betweenness(g1,v=V(g1))

#calculate adjacency matrix
# long_table=data.frame(rpasser=all_inter$rpasser,
#                       rreceiver=all_inter$rreceiver,
#                       passes=all_inter$Passes)
# tmp=unique(all_inter$rreceiver)
# add_selftoself=data.frame(rpasser=tmp,
#                           rreceiver=tmp,
#                           passes=rep(0,length(tmp)))
# long_table=rbind(long_table,add_selftoself)
# 
# long_table[with(all_inter, order(rreceiver,rpasser)), ]

#number actors
actors$actorseq=c(1:nrow(actors))
relations$fromseq=actors$actorseq[match(relations$from,actors$name)]
relations$toseq=actors$actorseq[match(relations$to,actors$name)]

#null adjacency matrix
adj_mat=matrix(NA,nrow(actors),nrow(actors))

#create adjacency matrix
for(i in 1:nrow(relations)){
  adj_mat[relations$fromseq[i],relations$toseq[i]]=relations$friendship[i]
}

#NAs are 0
adj_mat[is.na(adj_mat)]=0
#adj_mat

player_info=data.frame(team=rep(all_inter$Team[1],nrow(actors)),
           year=rep(all_inter$Year[1],nrow(actors)),
           mon=rep(all_inter$Mon[1],nrow(actors)),
           day=rep(all_inter$Day[1],nrow(actors)),
           name=actors$name,
           nameseq=actors$actorseq)

str(player_info)
player_info

#no matter we use rlink or passes in adj_mat, closeness is the same
#betweenness is the same

player_info$closeness=closeness(adj_mat)
player_info$betweenness=betweenness(adj_mat)

#find shortest path r packages
#in sna, stresscent computes stress centrality scores
#https://med.bioinf.mpi-inf.mpg.de/netanalyzer/help/2.7/#degreeDist

player_info$stress=stresscent(adj_mat)
#centralization(adj_mat)

#the in-degree of a node n is the number of incoming edges 
#the out-degree is the number of outgoing edges

#when we use passes in adj_mat, indegree is count integer
#when we use rlink in adj_mat, indegree is 1

player_info$indegree=degree(adj_mat,cmode="indegree")

#when we use passes in adj_mat, outdegree is count integer
#when we use rlink in adj_mat, outdegree is *.****

player_info$outdegree=degree(adj_mat,cmode="outdegree")

#in this r package, degree=indegree+outdegree
#when we use passes in adj_mat, degree is count integer
#when we use rlink in adj_mat, degree is *.****

player_info$degree=degree(adj_mat)

#I think we should use passes in adj_mat
#indegree and outdegree should be count integer

#i still need to check whether calculations are correct or not using cytoscape
#or check the relation between these measures and other summary measures


# write.table(all_inter, 
#             file='example.csv',
#             sep=c(','), col.names = NA)

#network connectivity
#The connectivity of a node is the number of its neighbors.
#The neighborhood connectivity of a node n is defined as the average connectivity of all neighbors of n
#The neighborhood connectivity distribution gives the average of the neighborhood connectivities of all nodes n with k neighbors

# #Draw a random graph
# g<-rgraph(10,tp=2/9)
# #Show the total partial out-neighborhoods
# neigh<-neighborhood(g,9,neighborhood.type="out",return.all=TRUE)

#sna.neighborhood extracts neighborhood of nodes from graph
# neighborhood(dat, order, neighborhood.type = c("in", "out", "total"),
#              mode = "digraph", diag = FALSE, thresh = 0, return.all = FALSE,
#              partial = TRUE)
# Arguments
# dat one or more graphs. graph is represented using adjacency matrix
# order order of the neighborhood to extract.
# neighborhood.type
# neighborhood type to employ.
# mode "digraph" if dat is directed, otherwise "graph".
# diag logical; do the diagonal entries of dat contain valid data?
# thresh dichotomization threshold to use for dat; edges whose values are greater than
#         thresh are treated as present.
# return.all logical; return neighborhoods for all orders up to order?
# partial logical; return partial (rather than cumulative) neighborhoods?

#extract first-order-out neighborhood of all actors
neigh_out=neighborhood(adj_mat,1,neighborhood.type="out",return.all=TRUE,partial=FALSE)

#extract first-order-in neighborhood of all actors
neigh_in=neighborhood(adj_mat,1,neighborhood.type="in",return.all=TRUE,partial=FALSE)

#extract first-order-total (both in and out) neighborhood of all actors
neigh_tol=neighborhood(adj_mat,1,neighborhood.type="total",return.all=TRUE,partial=FALSE)

#function: calculate average indegree of 
nei_fun=function(x){
  nei_indegree=x*player_info$indegree
  nei_outdegree=x*player_info$outdegree
  nei_degree=x*player_info$degree
  nei_indegree_avg=mean(nei_indegree,na.rm=T)
  nei_outdegree_avg=mean(nei_outdegree,na.rm=T)
  nei_degree_avg=mean(nei_degree,na.rm=T)
  return(c(nei_indegree_avg,
           nei_outdegree_avg,
           nei_degree_avg))
}

#dim(neigh_out)#1*13*13
#13 1*13 matrices
#str(neigh_out)
#x=neigh_out[,,1]

neighbor=apply(neigh_out,3,nei_fun)
#str(neighbor)
player_info$neigh_out_in=neighbor[1,]
player_info$neigh_out_out=neighbor[2,]
player_info$neigh_out_tol=neighbor[3,]
#neighborhood total degree=neighborhood indegree+neighborhood outdegree

neighbor=apply(neigh_in,3,nei_fun)
#str(neighbor)
player_info$neigh_in_in=neighbor[1,]
player_info$neigh_in_out=neighbor[2,]
player_info$neigh_in_tol=neighbor[3,]
#neighborhood total degree=neighborhood indegree+neighborhood outdegree

neighbor=apply(neigh_tol,3,nei_fun)
#str(neighbor)
player_info$neigh_tol_in=neighbor[1,]
player_info$neigh_tol_out=neighbor[2,]
player_info$neigh_tol_tol=neighbor[3,]
#neighborhood total degree=neighborhood indegree+neighborhood outdegree

#use gapply function to calculate network measures of neighborhood
# #Generate a random graph
# g<-rgraph(6)
# #Calculate the degree of g using gapply
# all(gapply(g,1,rep(1,6),sum)==degree(g,cmode="outdegree"))
# all(gapply(g,2,rep(1,6),sum)==degree(g,cmode="indegree"))
# all(gapply(g,c(1,2),rep(1,6),sum)==degree(symmetrize(g),cmode="freeman")/2)
# #Find first and second order neighborhood means on some variable
# gapply(g,c(1,2),1:6,mean)
# gapply(g,c(1,2),1:6,mean,distance=2)

# gapply(X, MARGIN, STATS, FUN, ..., mode = "digraph", diag = FALSE,
#        distance = 1, thresh = 0, simplify = TRUE)
# Arguments
# X one or more input graphs.
# MARGIN a vector giving the “margin” of X to be used in calculating neighborhoods. 1
# indicates rows (out-neighbors), 2 indicates columns (in-neighbors), and c(1,2)
# indicates rows and columns (total neighborhood).
# STATS the vector or matrix of vertex statistics to be used.
# FUN the function to be applied. In the case of operators, the function name must be
# quoted.
# mode "graph" if X is a simple graph, else "digraph".
# diag boolean; are the diagonals of X meaningful?
# distance the maximum geodesic distance at which neighborhoods are to be taken. 1 signifies
# first-order neighborhoods, 2 signifies second-order neighborhoods, etc.
# thresh the threshold to be used in dichotomizing X.
# simplify boolean; should we attempt to coerce output to a vector if possible?

num_actors=nrow(actors)

#first-order out-neighbors
gapply(adj_mat,1,rep(1,num_actors),sum,distance=1,mode="digraph")
degree(adj_mat,cmode="outdegree")
g=adj_mat
all(gapply(g,1,rep(1,num_actors),sum)==degree(g,cmode="outdegree"))
#FALSE
#problem: is outdegree wrong now that these two do not match
#checked sum of all passes out for individual
#wrong:all(gapply(g,1,rep(1,num_actors),sum)
#correct:degree(g,cmode="outdegree")

connectedness(adj_mat)
#Krackhardt’s connectedness for a digraph G is equal to the fraction of all dyads, fi; jg, 
#such that
#there exists an undirected path from i to j in G.
#appears to be 1 for all matches

prestige(adj_mat,cmode="domain.proximity")

#loop through all matches for networks
#store network measures from every match
network_bymatch=c()

for(i in 1:nrow(qw12)){
  
  #get one match id
  match_id=qw12$matchid[i]
  #get all interactions for this match
  all_inter=qw13_2[qw13_2$matchid==match_id,]
  #get all actor list for this match
  actors=data.frame(name=unique(union(all_inter$rreceiver,all_inter$rpasser)))
  relations=data.frame(from=all_inter$rpasser,
                       to=all_inter$rreceiver,
                       friendship=all_inter$Passes)

  #number actors
  actors$actorseq=c(1:nrow(actors))
  relations$fromseq=actors$actorseq[match(relations$from,actors$name)]
  relations$toseq=actors$actorseq[match(relations$to,actors$name)]
  
  #null adjacency matrix
  adj_mat=matrix(NA,nrow(actors),nrow(actors))
  #create adjacency matrix
  for(i in 1:nrow(relations)){
    adj_mat[relations$fromseq[i],relations$toseq[i]]=relations$friendship[i]
  }
  adj_mat[is.na(adj_mat)]=0
  
  player_info=data.frame(team=rep(all_inter$Team[1],nrow(actors)),
                         year=rep(all_inter$Year[1],nrow(actors)),
                         mon=rep(all_inter$Mon[1],nrow(actors)),
                         day=rep(all_inter$Day[1],nrow(actors)),
                         name=actors$name,
                         nameseq=actors$actorseq)
  
  player_info$closeness=closeness(adj_mat)
  player_info$betweenness=betweenness(adj_mat)
  player_info$stress=stresscent(adj_mat)
  player_info$indegree=degree(adj_mat,cmode="indegree")
  player_info$outdegree=degree(adj_mat,cmode="outdegree")
  player_info$degree=degree(adj_mat)
  #extract first-order-out neighborhood of all actors
  neigh_out=neighborhood(adj_mat,1,neighborhood.type="out",return.all=TRUE,partial=FALSE)
  
  #extract first-order-in neighborhood of all actors
  neigh_in=neighborhood(adj_mat,1,neighborhood.type="in",return.all=TRUE,partial=FALSE)
  
  #extract first-order-total (both in and out) neighborhood of all actors
  neigh_tol=neighborhood(adj_mat,1,neighborhood.type="total",return.all=TRUE,partial=FALSE)
  
  neighbor=apply(neigh_out,3,nei_fun)
  #str(neighbor)
  player_info$neigh_out_in=neighbor[1,]
  player_info$neigh_out_out=neighbor[2,]
  player_info$neigh_out_tol=neighbor[3,]
  #neighborhood total degree=neighborhood indegree+neighborhood outdegree
  
  neighbor=apply(neigh_in,3,nei_fun)
  #str(neighbor)
  player_info$neigh_in_in=neighbor[1,]
  player_info$neigh_in_out=neighbor[2,]
  player_info$neigh_in_tol=neighbor[3,]
  #neighborhood total degree=neighborhood indegree+neighborhood outdegree
  
  neighbor=apply(neigh_tol,3,nei_fun)
  #str(neighbor)
  player_info$neigh_tol_in=neighbor[1,]
  player_info$neigh_tol_out=neighbor[2,]
  player_info$neigh_tol_tol=neighbor[3,]
  #neighborhood total degree=neighborhood indegree+neighborhood outdegree
  
  network_bymatch=rbind(network_bymatch,player_info)
}

write.table(network_bymatch,
            file='network_bymatch20191007.csv',
            sep=c(','), col.names = NA)

nrow(network_bymatch)
#35729
nrow(qw141516)
#35729
#no data lost, cool













