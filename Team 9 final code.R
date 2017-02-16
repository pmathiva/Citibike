

#Install the requisite packages
install.packages("dplyr")
install.packages("fields")

library(plyr)
library(dplyr)
library(fields)

df <- read.csv('combineddata.csv', header = TRUE)

#Question 1 ---- average distance of rides
#
#Function to calculate distance between 2 lon/lat points
getdist <- function(x)
{
  start = matrix( cbind( as.numeric(x[7]),as.numeric(x[6]) ), nrow =1)
  end = matrix( cbind( as.numeric(x[11]),as.numeric(x[10]) ),nrow =1)
  print (start)
  print (end)
  dist = rdist.earth(start, end, miles = TRUE, R = NULL)
  return (dist)
  
}

df$dist = apply(df,1,getdist) #Apply distance function to all rows in the dataframe
mean(df$dist) #Average distance of rides (in miles)

#Question 2  ----  which station has max usage during the least busy hour in th eday
#
df$hour = (as.POSIXlt( as.character (df$starttime), format='%m/%d/%Y %H:%M:%S' ))$hour
df$hour = as.factor(df$hour)

hour_count = plyr::count(df,"hour")
hour_count$frac_total = hour_count$freq/sum(hour_count$freq)

hour_station_count = plyr::count(df, c("hour","start.station.id"))
total = merge(hour_station_count, hour_count, by ="hour")
total$frac_station = total$freq.x/total$freq.y
total$ratio = total$frac_station/total$frac_total

total[which.max(total$ratio),]
head( df[df$start.station.id==312,] , n = 1)

#Question 3
#Extract Start Station ID & End Station ID from database
#101663 rides
same_station_start_end <- subset(df, start.station.id==end.station.id)
76-100
#Percentage of the whole, having same origin and destination
Percentage_Same_of_Total <- ((dim(same_station_start_end)/dim(bikedat))*100) 
Percentage_Same_of_Total

#Question 4 
#Major station for Start
#Pershing Square North
tail(names(sort(table(df$start.station.name))), 1)

#Major station for End
#Pershing Square North
tail(names(sort(table(df$end.station.name))), 1)

#Major start-end pair
start_end_count = plyr::count(df, c("start.station.id" ,"end.station.id"))
start_end_count[which.max(start_end_count$freq),]


#Question 5 ---- months with max and min usage
df$month = (as.POSIXlt( as.character (df$starttime), format='%m/%d/%Y %H:%M:%S' ))$mon
df$month = as.factor(df$month)

month_count = plyr::count(df,"month")
month_count[which.max(month_count$freq),]
month_count[which.min(month_count$freq),]

#Question 6 ---------graph
#1.Analyse rides:
#delete start time and stop time of da dataframe:
df <- df[,-2:-3]
#change column names of df dataframe:
names(df)[2:9] = c('sid','sname','slat','slon','eid','ename','elat','elon')
#count the frequency of each same ride:
count_same_rides <- ddply(df, c("sid", "eid"), summarise, fre=length(sid))
#extract top5 rides: only 5 rides >2000 
topN_rides <- count_same_rides[which(count_same_rides[,3]>1500),] 
topN_rides <- topN_rides[-1,]
snames <- c('West St & Chambers St','12 Ave & W 40 St','Central Park S & 6 Ave','Central Park S & 6 Ave','Central Park S & 6 Ave','Central Park S & 6 Ave','Yankee Ferry Terminal','Yankee Ferry Terminal','Soissons Landing','Soissons Landing')
enames <- c('12 Ave & W 40 St','West St & Chambers St','Central Park S & 6 Ave','5 Ave & E 73 St','5 Ave & E 78 St','5 Ave & E 88 St','Yankee Ferry Terminal','Soissons Landing','Yankee Ferry Terminal','Soissons Landing')
ppt <- data.frame(topN_rides$sid,snames,topN_rides$eid,enames,topN_rides$fre)
names(ppt)[1:5]<- c('start.id','start.station.name','end.id','end.station.name','frequency')
# Visualization..(Stations as nodes,rides(edges) weighted by fre)
#draw as social network:
install.packages('igraph')
library(igraph)
g <- graph.data.frame(topN_rides, directed = TRUE)
plot(g,layout=layout.fruchterman.reingold,vertex.size=0.1,edge.arrow.size=0.3,edge.width = E(g)$fre/750)  
#draw as google map:  △run #2 first
extract = station_frequncy[c('32','55','152','229','389','459','465','484'),]
extract1 = station_frequncy[c('480','19','34','51','54','71','98','141','158'),]
least10 <- data.frame(
                        c(3219,40.729193,-73.976655),
                        c(3017,40.751483,-73.996764),
                          c(160,40.44535,-73.978238),
                            c(3432,40.662987,-73.976918),
                              c(3362,40.7781314,-73.96069399),
                                c(3376,40.7647185194434,-73.962220698595),
                                  c(3332,40.681990442707,-73.9907902479172),
                                    c(3434,40.7902541733042,-73.9771834015846))
Least10=t(least10)
Least10=as.data.frame(Least10)
names(Least10)[1:3]<-c("sid","slat","slon")
station_frequncy$sid=32
library(ggmap)
ggmap(get_googlemap(center = 'New York', zoom=12, maptype='roadmap'),extent='panel')+
  geom_point(data=extract,aes(x=slon,y=slat),colour = 'red',alpha=1)+
  geom_text(data=extract,colour="red2",aes(x=slon,y=slat,label=sname,fontface=1),size=4,hjust=1,vjust=0)  

#2.Analyse stations:
#Count unique start stations 
station_frequncy <- ddply(df,.(sid,sname,slat,slon),summarize,fre=length(sid)) #usage of total 626 stations(unique(df$start.station.id)
write.csv(station_frequncy,file='R.csv')
top10_stations <- R[1:10,]
# Visualization:draw top10 used stations
library(ggmap)
ggmap(get_googlemap(center = 'New York', zoom=12, maptype='roadmap'),extent='panel')+
  geom_point(data=top10_stations,aes(x=slon,y=slat),colour = 'blue',alpha=1)+
  geom_text(data=top10_stations,colour="red2",aes(x=slon,y=slat,label=sid,fontface=1),size=4,hjust=1,vjust=0)

#Analyse riding durations
#count distance based on lon and lat
library(geosphere)
to_minute=data.frame(df$tripduration/60,df[,2:9])
del_dup_mins <- to_minute[!duplicated(to_minute$df.tripduration.60),]
ana_min <- as.numeric(del_dup_mins[,1])

#mins :45       75  105   135
#cost :$14.95  +2.5 +6.5  +9
percentage:
length(which(ana_min>45))/17100  # =84.6%, means more than 84.6% people who bought annual plan payed for extra riding
length(which(ana_min>75))/17100
length(which(ana_min>105))/17100
length(which(ana_min>135))/17100 # =53.6%

mean(ana_min)

distance <- data.frame(diag(distm(unique_rides[,4:5],unique_rides[,8:9])))
duration_distance <- data.frame(del_dup_mins$df.tripduration.60,distance)
del_zero <- duration_distance[-(which(duration_distance$diag.distm.unique_rides...4.5...unique_rides...8.9...==0)),]
del_zero <- del_zero[which(del_zero$distance<2000),]
del_zero <- del_zero[which(del_zero$duration<2000),]
del_zero <- del_zero[which(del_zero$distance>800),]
del_zero <- del_zero[which(del_zero$duration>800),]
del_zero <- data.frame(del_zero$duration,del_zero$distance)
names(del_zero)[1:2]=c('duration','distance')
model <- lm(del_zero$distance~del_zero$duration)
plot(del_zero$duration,del_zero$distance)
abline(model,col='blue')
#Q7
library(igraph)
edge.betweenness(g)
edge.connectivity(g)
centralization.degree(g)
larger540 <- filter(count_same_rides,count_same_rides[,3] > 540)
g <- graph.data.frame(larger540, directed = TRUE)
plot(g,layout=layout.fruchterman.reingold,vertex.size=0.1,edge.arrow.size=0.3,edge.width = E(g)$V1/750)

