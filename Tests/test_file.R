rm(list=ls())
setwd("/Users/davidterner/Desktop/Indiana/Year 3/S610/Final Project")
source("Distance Data Script.R")
library(tidyverse)

rand_locations=runif(16,min=-180,max=180)
rand_locations=matrix(rand_locations,4,4)

#Testing gcd.hf
gcd.hf(rand_locations[,1],rand_locations[,2],rand_locations[,3],rand_locations[,4])%>%print()

#Testing dist_matrix 
rand_locations=runif(16,min=-180,max=180)
rand_locations=matrix(rand_locations,8,2)
rand_locations=as.data.frame(rand_locations)
colnames(rand_locations)=c("long","lat")
rand_locations$geo=1:8
dist_mat=distance_matrix(rand_locations$geo,rand_locations$long,rand_locations$lat)
dist_mat%>%print()

#Testing weight_distance_matrix
dmax=mean(dist_mat[dist_mat>0])
pop=runif(8,20,80)
wmat=weight_distance_matrix(dist_mat,dmax)
wmat%>%print()
#weight_distance_matrix(dist_mat,pop,dmax,lambda=10)%>%print()

#Testing moranI
y1=runif(8,0,100)
test_moran=moranI(y=y1,w_mat=wmat)%>%print()

#Testing grid_spacing 
d_seq=grid_spacing(a=median(dist_mat[dist_mat>0])+20,b=max(dist_mat),n=5,theta=1.1)%>%print()

#Testing moran_time_dist
y1=runif(16,0,100)
test_years=c(rep(1,8),rep(2,8))
years=c(1,2)

moran_time_dist(y=y1,y_years=test_years,dist_mat,d_seq,years)%>%print()


#Testing MoranI_pop
y1=runif(8,0,100)
pop=runif(8,20,40)
MoranI_pop(y=y1,pop=pop,dist_mat,d_seq,20)%>%print()


#Testing Local Moran
LocalMoran(y=y1,dist_mat,dmax)%>%print()


#Testing Getis_Ord
Getis_Ord(y=y1,w_mat=wmat)%>%print()


#Testing Getis_Ord_local_z

Getis_Ord_local_z(y=y1,dist_mat,dmax)%>%print()


#Testing local_GC

local_GC(y=y1,dist_mat,dmax)%>%print()
