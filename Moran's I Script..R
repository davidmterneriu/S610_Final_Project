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

Op_data$FIPS%>%unique()%>%length()

table(Op_data$Year)

#Test Data
#test_df=county_data[1:200,]

#sample_count=200

#test_df=sample_n(county_data, 3075)
#test_df=county_data[county_data$state=="NY",]
test_df=county_data

test_df=test_df%>%inner_join(select(Op_data,FIPS,`Opioid Prescribing Rate`,Year),by=c("fips"="FIPS"))%>%
  rename(OPR=`Opioid Prescribing Rate`)

test_df=test_df[order(test_df$Year),]

#table(test_df$fips)

test_df$fips%>%unique()%>%length()

#table(test_df$Year)

#Venn Diagrams 
library(gplots)


set1=test_df$fips[test_df$Year==2013]
set2=test_df$fips[test_df$Year==2014]
set3=test_df$fips[test_df$Year==2015]
set4=test_df$fips[test_df$Year==2016]
set5=test_df$fips[test_df$Year==2017]

#vp=venn.diagram(list(`2013`=set1,`2014`=set2,`2015`=set3,`2016`=set4,`2017`=set5),fill = 2:6, alpha = 0.3, filename = NULL)
#dev.new()
venn(list(`2013`=set1,`2014`=set2,`2015`=set3,`2016`=set4,`2017`=set5))
legend(title = "FIPS OPR Data Overlap")
#grid.draw(vp)


load("~/Desktop/Indiana/Year 3/S610/Final Project/Tests/test_data_unemployment_new.RData")
test_data=data_to_be_cleaned
set1=test_data$fips[test_data$Year==2013]
set2=test_data$fips[test_data$Year==2014]
set3=test_data$fips[test_data$Year==2015]
set4=test_data$fips[test_data$Year==2016]
set5=test_data$fips[test_data$Year==2017]
venn(list(`2013`=set1,`2014`=set2,`2015`=set3,`2016`=set4,`2017`=set5))

table(test_data$Year)

test_data$fips%>%unlist%>%unique()%>%length()

#Need to make sure that each year has the same counties
#----Computing Local Moran's I for both unemployment and OPR 
#test_df<- read_csv("test_data.csv")
fips_count=table(test_df$fips)%>%as.data.frame()

test1=test_df%>%inner_join(fips_count,by=c("fips"="Var1"))
test1=test1[test1$Freq==5,]
test1=test1[,-10]
write.csv(test1,"OPR_test_data.csv")

fips_count=fips_count$Var1[fips_count$Freq==5]%>%as.character()
year_seq=2013:2017
temp_df=test_df[test_df$Year==year_seq[1],]
dis.mat=distance_matrix(temp_df$fips,temp_df$lon,temp_df$lat)

tic()
a=LocalMoran(temp_df$OPR,dis.mat,120,scaling=TRUE,p.test="two.sided")
#2202.172 sec elapsed
toc()


lMoran2013=cbind.data.frame(fips=temp_df$fips,localMoran=a[[1]],p.value=a[[2]],year=2013)
write.csv(lMoran2013,"lMoran2013.csv")

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



temp_df=test_df[test_df$state=="MN"&test_df$Year==2013,]

dis.mat=distance_matrix(temp_df$fips,temp_df$lon,temp_df$lat)
source("Distance Data Script.R")



Getis_Ord_local(temp_df$OPR,dis.mat,60)
hist(a$local_G)



a=apply(dis.mat,1,FUN =Rfast::nth,2,descending = F )%>%max()
hist(a)





#dist_seq=seq(25,30,by=5)
#moran_time_dist(test_df$OPR,test_df$Year,dis.mat,dist_seq,2015)

#dist.mat=distance_matrix(temp_df$fips,temp_df$lon,temp_df$lat)
w_mat=weight_distance_matrix(dis.mat,2000)





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
#write_csv(test,"moran_data_2013-7_v2.csv")


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

source("Distance Data Script.R") 

#Testing weight_distance_matrix





A <- matrix(rnorm(100,10,1), 10, 10)
A=A%*% t(A)
diag(A)<-0

popA=rnorm(10,100,20)
popA[c(1,10)]=10^(9)

dmax=max(A)

local_GC(popA,A,dmax = dmax)

w_mat=weight_distance_matrix(A,dmax,popA,lambda=min(popA),options="population")


Getis_Ord_local_z(popA,dist_mat=A,dmax=dmax)



lambda=quantile(popA,.8)%>%as.numeric()

weight_distance_matrix(A,dmax=dmax,pop=popA,lambda,"population")

LocalMoran(popA,A,1050)



