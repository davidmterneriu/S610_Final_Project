# Code Results 

rm(list=ls())
setwd("/Users/davidterner/Desktop/Indiana/Year 3/S610/Final Project")
library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(ape)
library(tictoc)
library(tidyverse)
library(ggforce)
source("Distance Data Script.R")

#Load Data 
OPR_test_data <- read_csv("Tests/OPR_test_data.csv", col_types = cols(X1 = col_skip()))


#OPR_test_data work ONLY------------------------
#Constructing Distance matrix 
OPR_sub=OPR_test_data[OPR_test_data$Year==2013,]

tic()
dis_mat=distance_matrix(OPR_sub$fips,OPR_sub$lon,OPR_sub$lat)
toc()

#Getting Distance Histogram  (lower triangle entries excluding diag)
dist_dens=dis_mat[lower.tri(dis_mat)]%>%as.numeric()
hist(dist_dens,breaks = 50,col = "blue",xlab = "Distance (miles)",main="Distance Histogram")
quantile(dist_dens)%>%round(2)

#Retrieving extreme distance pairs-----
#max_ind=which(dis_mat == max(dis_mat), arr.ind = TRUE)
#OPR_sub[OPR_sub$fips==rownames(dis_mat)[max_ind[1]],]
#OPR_sub[OPR_sub$fips==rownames(dis_mat)[max_ind[2]],]

#min_ind=which(dis_mat == min(dis_mat[dis_mat>0]), arr.ind = TRUE)
#OPR_sub[OPR_sub$fips==rownames(dis_mat)[min_ind[1]],]
#OPR_sub[OPR_sub$fips==rownames(dis_mat)[min_ind[2]],]

#Moran's I for different dmax over time


d_max=max(dist_dens)
dist_seq=grid_spacing(20,d_max,50,2)
year_seq=2013:2017

tic()
test=moran_time_dist(OPR_test_data$OPR,OPR_test_data$Year,dis_mat,dist_seq,year_seq)
toc()
#492.282 seconds 

ggplot(data=test,aes(x=distance,y=MoransI,color=as.factor(year)))+geom_line()+
  theme_bw()+
  labs(x="Distance", y="Moran's I",color="Year")+
  facet_zoom(xlim = c(20, 100),zoom.size=1)


test%>%group_by(year)%>%summarise(dbar=distance[which.max(MoransI)])
test%>%group_by(year)%>%summarise(max=max(MoransI),mean=mean(MoransI),sd=sd(MoransI))


#Local Moran's I for 2013 
tic()
lmoran_data=LocalMoran(OPR_sub$OPR,dis_mat,dmax=120,scaling=TRUE,p.test="two.sided")
#2249.345 seconds
toc()
#write.csv(lmoran_data,"lmoran_data_2013.csv")



lmoran_data_2013 <- read_csv("Tests/lmoran_data_2013.csv",col_types = cols(X1 = col_skip()))

#Density plot lmoran_data 
OPR_sub$moranI_local_OPR=lmoran_data_2013$moranI
#OPR_sub$moranI_local_OPR_p=lmoran_data$p.value

quantile(OPR_sub$moranI_local_OPR,c(.05,.95))

OPR_local_sum=OPR_sub%>%group_by(rMapState)%>%summarize(med_local_I=median(moranI_local_OPR),med=median(OPR))


#OPR_loc_max=max(OPR_sub$moranI_local_OPR)
#OPR_loc_min=min(OPR_sub$moranI_local_OPR)
#OPR_sub$lmoran_trans=2*(OPR_sub$moranI_local_OPR-OPR_loc_min)/(OPR_loc_max-OPR_loc_min)-1

OPR_sub[order(OPR_sub$moranI_local_OPR,decreasing = TRUE)[1:5],]
OPR_sub[order(OPR_sub$moranI_local_OPR,decreasing = FALSE)[1:5],]

ggplot(data=OPR_sub,aes(x=moranI_local_OPR))+geom_density()



#Map 
#Better Maps 
library(maps)
library(socviz)

OPR_sub$fips=OPR_sub$fips%>%as.character()
OPR_sub$localMoran=ifelse(abs(OPR_sub$moranI_local_OPR)<=20,OPR_sub$moranI_local_OPR,NA)
county_map1=county_map
county_map1$id=gsub("^[0]+","",county_map1$id)%>%as.character()
county_full <- left_join(county_map1, OPR_sub, by = c("id"="fips"))

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat.x,
                          fill = localMoran, 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Local Moran I: 2013",subtitle = "Abs Local Moran I < 20")
p1

#Minnesota only 
Minnesota=county_full[county_full$state=="MN",]

#Local Moran OPR 
p <- ggplot(data = Minnesota,
            mapping = aes(x = long, y = lat.x,
                          fill = moranI_local_OPR, 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Minnesota (2013)",subtitle = "Moran OPR",fill="Local Moran I")
p1

#OPR Rates 
p <- ggplot(data = Minnesota,
            mapping = aes(x = long, y = lat.x,
                          fill = OPR, 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Minnesota (2013)",subtitle = "OPR",fill="OPR")
p1


#OPR+Population+UR Data----------------------
rm(list=ls())
setwd("/Users/davidterner/Desktop/Indiana/Year 3/S610/Final Project")
library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(ape)
library(tictoc)
library(tidyverse)
library(ggforce)
library(maps)
library(socviz)
source("Distance Data Script.R")


test_data_unemployment_new <- read_csv("Tests/test_data_unemployment_new.csv")

#Looking to 2013 only 
test_data=test_data_unemployment_new[test_data_unemployment_new $Year==2013,]

#getting distance matrix
dis_mat=distance_matrix(test_data$fips,test_data$lon,test_data$lat)

d_max=max(dis_mat)
dist_seq=grid_spacing(20,d_max,50,2)


lambda_seq=quantile(test_data$population,c(.1,.5,.9))

tic()
pop_low=MoranI_pop(y=test_data$OPR,pop=test_data$population,dist_mat=dis_mat,dist_seq=dist_seq,lambda=lambda_seq[1])
pop_med=MoranI_pop(y=test_data$OPR,pop=test_data$population,dist_mat=dis_mat,dist_seq=dist_seq,lambda=lambda_seq[2])
pop_hi=MoranI_pop(y=test_data$OPR,pop=test_data$population,dist_mat=dis_mat,dist_seq=dist_seq,lambda=lambda_seq[3])
toc()

pop_master=rbind.data.frame(pop_low,pop_med,pop_hi)

colnames(pop_master)=c("MoranI","p.value","population","Distance")
ggplot(data=pop_master,aes(x=Distance,y=MoranI,color=as.factor(population)))+geom_line()+
  theme_bw()+
  labs(color="Population (lambda)",subtitle ="Population levels correspond to \n 10th/50th/90th percentiles",y="Moran's I")+
  facet_zoom(xlim = c(20, 50),zoom.size=1)


#----------Hotspots


#OPR Hotspots 
local_GC_data=Getis_Ord_local_z(y=test_data$OPR,dist_mat =dis_mat,dmax = 100 )

#UR Hotspots 
local_GC_UR=Getis_Ord_local_z(y=test_data$UN_RATE,dist_mat =dis_mat,dmax = 100 )

local_GC_data$data="OPR"
local_GC_UR$data="UR"
local_GC_data$fips=test_data$fips
local_GC_UR$fips=test_data$fips
#Hotspots
spots_df=rbind.data.frame(local_GC_data,local_GC_UR)

ggplot(data=spots_df,aes(x=Gi,color=data))+stat_ecdf(size=1)+theme_bw()+
  labs(color="Data Type",x="Gi Score",caption = "2013 observations only")

test_data$gi_OPR=local_GC_data$Gi
test_data$gi_UR=local_GC_UR$Gi


cor.test(test_data$OPR,test_data$gi_OPR)

test_data$fips=as.character(test_data$fips)


county_map1=county_map
county_map1$id=gsub("^[0]+","",county_map1$id)%>%as.character()
county_full <- left_join(county_map1, test_data, by = c("id"="fips"))

#OPR Hot Spot 
p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat.x,
                          fill =gi_OPR , 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Gi: OPR",subtitle = "2013",fill="Gi")
p1

#UR Unemployment 
p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat.x,
                          fill =gi_UR , 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Gi: UR",subtitle = "2013",fill="Gi")
p1


#New England Sample 
New_England_list=c("VT","MA","RI","CT","NH","VT","ME")
New_England=county_full[county_full$state %in% New_England_list,]
p <- ggplot(data = New_England,
            mapping = aes(x = long, y = lat.x,
                          fill =gi_OPR , 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Gi: OPR",subtitle = "2013",fill="Gi")
p1

#2014 Hotspots 
#Looking to 2014 only 
test_dataf=test_data_unemployment_new[test_data_unemployment_new $Year==2014,]

#Which/number of fips are not in 2014 from 2013 
fips_code=unique(test_dataf$fips)[(unique(test_dataf$fips) %in% unique(test_data$fips)==FALSE)]

test_dataf[test_dataf$fips %in% fips_code,]


#getting distance matrix
dis_matf=distance_matrix(test_dataf$fips,test_data$lon,test_data$lat)


#OPR Hotspots 
local_GC_data_f=Getis_Ord_local_z(y=test_dataf$OPR,dist_mat =dis_matf,dmax = 100 )
local_GC_data_f$fips=test_dataf$fips%>%as.character()

local_GC_data_f$year=2014
#Emergence of new hotspots. 


dis_mat=distance_matrix(test_data$fips,test_data$lon,test_data$lat)
#OPR Hotspots 
local_GC_data=Getis_Ord_local_z(y=test_data$OPR,dist_mat =dis_mat,dmax = 100 )

local_GC_data$fips=test_data$fips%>%as.character()
local_GC_data$year=2013

master_spot=rbind.data.frame(local_GC_data,local_GC_data_f)
#Eliminate fips with p.value>=.1

ecdf_fun <- function(x,perc) ecdf(x)(perc)



master_spotf=master_spot[master_spot$p.value<.1,]


ecdf_fun(master_spotf$Gi[master_spotf$year==2013],0)
ecdf_fun(master_spotf$Gi[master_spotf$year==2014],0)

ggplot(data=master_spotf,aes(x=Gi,color=as.factor(year)))+
  stat_ecdf()+
  theme_bw()+
  geom_vline(xintercept = 0,linetype="dotted")+
  labs(color="Year")
  

spot_compare=local_GC_data
spot_compare=spot_compare%>%inner_join(local_GC_data_f,by="fips")

#Eliminate NA 
spot_compare=spot_compare[complete.cases(spot_compare),]
spot_count=dim(spot_compare)[1]
#Direction of change 
spot_compare=spot_compare%>%mutate(spot_change=ifelse(Gi.y-Gi.x>0,
                                                      ifelse(spot.x=="Hot","Hot getting Hotter","Cold getting Hotter"),
                                                      ifelse(spot.x=="Hot","Hot getting Colder","Cold getting Colder")))%>%
  mutate(change=Gi.y-Gi.x)





spot_sum=spot_compare%>%group_by(spot_change)%>%
  summarise(counts=n(),
            mean_change=mean(change))

spot_sum$share=spot_sum$counts/spot_count
spot_sum$adj_mean=spot_sum$share*spot_sum$mean_change
#net change
sum(spot_sum$adj_mean)%>%round(4)

#which counties are getting hotter
hotter_df=spot_compare[spot_compare$spot_change=="Hot getting Hotter",]
test_dataf$fips=test_dataf$fips%>%as.character()
hotter_df=hotter_df%>%inner_join(select(test_dataf,fips,rMapState))





# Change in unemployment 
test_dataUN=test_data_unemployment_new[test_data_unemployment_new$Year<=2014,]

hot_fips=unique(hotter_df$fips)

test_dataUN=test_dataUN[test_dataUN$fips %in% hot_fips,]


test_dataUN=select(test_dataUN,fips,rMapState,Year,UN_RATE)

test_dataUN=test_dataUN%>%spread(key=Year,value=UN_RATE)
#write.csv(test_dataUN,"hot_un_opr.csv")

test_dataUN$un_change=test_dataUN$`2014`-test_dataUN$`2013`
test_dataUN$fips=as.character(test_dataUN$fips)
#Merging 

hotter_df=hotter_df%>%inner_join(test_dataUN,by="fips")

mod1=lm(data=hotter_df,change~un_change+rMapState.x)
summary(mod1)

#Change in general
test_dataUNf=test_data_unemployment_new[test_data_unemployment_new$Year<=2014,]
test_dataUNf=select(test_dataUNf,fips,rMapState,Year,UN_RATE)
test_dataUNf=test_dataUNf%>%spread(key=Year,value=UN_RATE)

test_dataUNf$un_change=test_dataUNf$`2014`-test_dataUNf$`2013`
test_dataUNf$fips=as.character(test_dataUNf$fips)

spot_compare=spot_compare%>%inner_join(test_dataUNf,by="fips")

mod1=lm(data=spot_compare,change~un_change*rMapState)
summary(mod1)

#Map of Gi changes-----

county_map1=county_map
county_map1$id=gsub("^[0]+","",county_map1$id)%>%as.character()
county_full <- left_join(county_map1, spot_compare, by = c("id"="fips"))
p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill =change , 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c(option = "C")+labs(title = "OPR Spot Score",subtitle = "2013-2014",fill="Gi Change")


mean_change=spot_compare%>%group_by(rMapState)%>%summarize(mean=mean(change))%>%
  arrange(desc(mean))

county_map1=county_map
county_map1$id=gsub("^[0]+","",county_map1$id)%>%as.character()
county_full <- left_join(county_map1, spot_compare, by = c("id"="fips"))

NElist=c("VT","MA","RI","CT","NH","VT","ME","NY","NJ","PA")
NE=county_full[county_full$state %in% New_England_list,]

p <- ggplot(data = NE,
            mapping = aes(x = long, y = lat,
                          fill =change , 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c(option = "C")+labs(title = "OPR Spot Score",subtitle = "2013-2014",fill="Gi Change")
p1
