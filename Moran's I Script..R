#Coding Moran's I 
rm(list=ls())
setwd("/Users/davidterner/Desktop/Indiana/Year 3/S610/Final Project")
library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(ape)
library(tictoc)
library(housingData)
source("Distance Data Script.R")

#Last library has median geographical coordinates of centroids of U.S. counties
county_data=geoCounty
county_data$fips=gsub("^[0]+","",county_data$fips)%>%as.character()
Op_data <- read_excel("Op_data.xlsx")
Op_data$FIPS=as.character(Op_data$FIPS)

#Test Data
#test_df=county_data[1:200,]

#sample_count=200

#test_df=sample_n(county_data, 3075)
#test_df=county_data[county_data$state=="NY",]
test_df=county_data

test_df=test_df%>%inner_join(select(Op_data,FIPS,`Opioid Prescribing Rate`,Year),by=c("fips"="FIPS"))%>%
  rename(OPR=`Opioid Prescribing Rate`)

test_df=test_df[order(test_df$Year),]

#table(test_df$Year)
#Need to make sure that each year has the same counties

fips_count=table(test_df$fips)%>%as.data.frame()
fips_count=fips_count$Var1[fips_count$Freq==5]%>%as.character()

test_df=test_df[test_df$fips%in%fips_count,]



#Test Sequence
dist_seq=seq(20,200,by=20)
year_seq=2013:2017





m_length=length(dist_seq)*length(year_seq)

result_df=matrix(0,m_length,3)
counter=1
for (i in 1:length(year_seq)){
  temp_df=test_df[test_df$Year==year_seq[i],]
  for (j in 1:length(dist_seq)){
    result_df[counter,1]=year_seq[i]
    result_df[counter,2]=moranI(temp_df$fips,temp_df$lon,temp_df$lat,temp_df$OPR,dist_seq[j])
    result_df[counter,3]=dist_seq[j]
    counter=counter+1
  }
}

result_df=as.data.frame(result_df)
colnames(result_df)=c("Year","MoranI","Dmax")

ggplot(data = result_df,aes(x=Dmax,y=MoranI,color=as.factor(Year)))+geom_line()+
  theme_bw()+
  labs(x="Distance",y="Moran's I",color="Year",title="Random Sample")



#Compare with other Implementation of Moran's I 
temp_df=test_df[test_df$Year==year_seq[1],]


dis.mat=distance_matrix(test_df$fips,test_df$lon,test_df$lat)

#dist_seq=seq(25,30,by=5)
#moran_time_dist(test_df$OPR,test_df$Year,dis.mat,dist_seq,2015)

#dist.mat=distance_matrix(temp_df$fips,temp_df$lon,temp_df$lat)
w_mat=weight_distance_matrix(dist.mat,20)

quantile(dist.mat)

ape::Moran.I(temp_df$OPR,w_mat)

moranI(temp_df$OPR,w_mat,scaling = TRUE)

dist_seq=seq(25,2800,by=25)
year_seq=2013:2017

tic()
test=moran_time_dist(test_df$OPR,test_df$Year,dis.mat,dist_seq,year_seq)
toc()
#Takes 980.833 seconds to run with all years 
#1114.28 after obs count adjustments are made
write_csv(test,"moran_data_2013-7_v2.csv")


g1=ggplot(data=test,aes(x=distance,y=MoransI,color=as.factor(year)))+geom_line(size=1)+
  theme_bw()+
  labs(y="Moran's I",color="Year",title="Opioid Prescribing Rate vs Distance")

#plotly::ggplotly(g1)





#Misc--------

#county_data$count_code=paste(county_data$county,county_data$state,sep="-")
#county_list=unique(county_data$count_code)
#n=length(county_list)

#Generate Unique County Pairs
#counter=0
#county_pairs=matrix(0,n*(n-1)/2,2)

#for (i in 1:(n-1)){
#  for(j in (i+1):n){
#    counter=counter+1
#    county_pairs[counter,1]=county_list[i]
#    county_pairs[counter,2]=county_list[j]
#    }
#  }
#county_pairs=as.data.frame(county_pairs)
#names(county_pairs)=c("coun.x","coun.y")

#county_pairs=county_pairs%>%
#  inner_join(select(county_data,count_code,fips,lon,lat),by=c("coun.x"="count_code"))
#county_pairs=county_pairs%>%
#  inner_join(select(county_data,count_code,fips,lon,lat),by=c("coun.y"="count_code"))

#county_pairs=county_pairs%>%mutate(distance=gcd.hf(lon.x,lat.x,lon.y,lat.y))


#hist(county_pairs$distance)
#quantile(county_pairs$distance,seq(0,1,by=1/10))