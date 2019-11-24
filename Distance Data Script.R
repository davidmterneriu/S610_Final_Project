# Working with Distance data

setwd("/Users/davidterner/Desktop/Indiana/Year 3/S610/Final Project")
library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(housingData)

gcd.hf <- function(long1, lat1, long2, lat2) {
  #closely following: https://www.r-bloggers.com/great-circle-distance-calculations-in-r/
  R <- 3958.8 # Earth mean radius in miles 
  deg2rad <- function(deg) return(deg*pi/180)
  long1=deg2rad(long1)
  long2=deg2rad(long2)
  lat1=deg2rad(lat1)
  lat2=deg2rad(lat2)
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in miles
}
gcd.hf=Vectorize(gcd.hf)


distance_matrix=function(geo,long,lat){
  #Variable description-----------------------------------
  #INPUTS:
  #geo: list of geographic id units (index g), length n
  #long/lat: coordinates of g in degrees 
  #OUTPUT:
  #nxn square and symmetric distance matrix 
  master_df=cbind.data.frame(geo,long,lat)%>%
    unique.data.frame()
  geo=master_df$geo
  long=master_df$long
  lat=master_df$lat
  n=length(geo)
  test_df=expand.grid(geo,geo)%>%as.data.frame()
  test_df=test_df%>%inner_join(master_df,by=c("Var1"="geo"))%>%
    rename("long1"="long","lat1"="lat")%>%
    inner_join(master_df,by=c("Var2"="geo"))%>%
    rename("long2"="long","lat2"="lat")
  test_df$dist=gcd.hf(test_df$long1,test_df$lat1,test_df$long2,test_df$lat2)
  #Making the square matrix 
  myMat <- matrix(0, n, n, dimnames = list(geo, geo))
  # fill in the matrix with matrix indexing on row and column names
  myMat[as.matrix(test_df[c("Var1", "Var2")])] <- test_df[["dist"]]
  return(myMat)
}

moranI=function(geo,long,lat,y,dmax){
  #Variable description-----------------------------------
  #INPUTS:
  #geo: list of geographic id units (index g)
  #long/lat: coordinates of g in degrees 
  #y: variable of interest
  #dmax: max distance 
  #OUTPUT: 
  #moran: moran's I (e.g. distance weighted correlation)
  #zi: z-score
  #p.value.tw: two-sided p-value

  #Create square distance matrix given long/lat coordinates
  w_mat=distance_matrix(geo,long,lat)
  w_mat=w_mat^(-1)
  diag(w_mat) <- 0
  w_mat[w_mat<(dmax)^(-1)]=0
  W=sum(w_mat)
  N=length(geo)
  ymean=mean(y)
  #Create C_ij matrix
  C=matrix(0,N,N)
  for(i in 1:N){
    for(j in 1:N){
      C[i,j]=(y[i]-ymean)*(y[j]-ymean)
    }
  }
  moran=N*sum(w_mat*C)/(W*sum((y-ymean)^2))
  
  #Time to get z-value
  EI=-1/(N-1)
  S0=W
  S1=sum((w_mat+t(w_mat))^2)
  S2=sum((colSums(w_mat)+rowSums(w_mat))^2)
  S3=(1/N*sum((y-ymean)^4))/(1/N*sum((y-ymean)^2)^2)
  S4=(N^2-3*N+3)*S1+N*S2+3*W^2
  S5=(N^2-N)*S1
  VI=(N*S4-S3*S5)/((N-1)*(N-2)*(N-3)*W^2)-(EI)^2
  
  zi=(moran-EI)/sqrt(VI)
  p.value.tw=2*(1-pnorm(zi))
  #result=cbind.data.frame(moranI=moran,zscore=zi,p.value.tw)
  #result=data.frame(moran=moran,p.value=p.value.tw)
  result=moran
  return(result)
}
moranI=Vectorize(moranI,vectorize.args="dmax")










