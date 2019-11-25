# Working with Distance data

setwd("/Users/davidterner/Desktop/Indiana/Year 3/S610/Final Project")
library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
#Used for fast sorting 
library(doBy)

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

weight_distance_matrix=function(dist_mat,dmax,pop,lambda,options="none for now"){
  #dmax: max distance
  if (options!="population"){
    w_mat=dist_mat^(-1)
    diag(w_mat) <- 0
    w_mat[w_mat<(dmax)^(-1)]=0}
  else{
    #browser()
    n=length(pop)
    dis_pop=numeric(n)
    for (i in 1:n ){
      region_pop=pop[i]%>%as.numeric()
      if(region_pop>lambda){
        k=1
        dis_pop[i]=k
      }else{
          for(j in 2:n){
          #Looking for closet neighbors/adding their population to region
          neigh_index=which.minn(dist_mat[i,],j)%>%tail(1)
          region_pop=region_pop+pop[neigh_index]
          if (region_pop>lambda | j==n){
            k=dist_mat[i,neigh_index]
            break
          }
          }
        k=dist_mat[i,neigh_index] 
      }
      dis_pop[i]=k
    }
    w_mat=dist_mat
    w_mat[w_mat>dmax]<-0
    w_mat=sweep(w_mat,2,dis_pop, '/')^(-1)
    diag(w_mat) <- 0
    #w_mat=dis_pop
  }
  return(w_mat)
}






moranI=function(y,w_mat,scaling=FALSE,p.test="two.sided"){
  #Variable description-----------------------------------
  #INPUTS:
  #y: variable of interest
  #weight_max: square distance adjusted weight matrix 
  #scaling: default to FALSE; if TRUE, moran's I should fall between [-1,1]
  #OUTPUT: 
  #moran: moran's I (e.g. distance weighted correlation)
  #p.value: one-sided p-value
  if(scaling){
    rsum=apply(w_mat,1,sum)
    rsum[rsum==0]<-1
    weight=w_mat/rsum
  }else{
    weight=w_mat
  }
  W=sum(weight)
  #Adjusting for zero weighted observations
  A=sum(weight>0)/2
  N=min(length(y),A)
  y_dmean=y-mean(y)
  C=sum(weight*y_dmean %o%y_dmean)
  v=sum(y_dmean^2)
  moran=(N/W)*(C/v)
  
  if(scaling==TRUE){
    adj=W/N*(sd(apply(weight,1,sum)*y_dmean)/sqrt(v/(N-1)))
    moran=moran/adj
  }
  
  #Time to get z-value
  EI=-1/(N-1)
  S1=1/2*sum((weight+t(weight))^2)
  S2=sum((apply(weight,2,sum)+apply(weight,1,sum))^2)
  S3=(sum(y_dmean^4)/N)/(v/N)^2
  S4=(N^2-3*N+3)*S1-N*S2+3*W^2
  S5=(N^2-N)*S1-2*N*S2+6*W^2
  
  VI=(N*S4-S3*S5)/((N-1)*(N-2)*(N-3)*W^2)-EI^2
  sdV=sqrt(VI)
  p.value=pnorm(moran,mean=EI,sd=sdV)
  if(p.test=="two.sided"){
    p.value=ifelse(moran<=EI,2*p.value,2*(1-p.value))
  }else{
    p.value=1-p.value
  }
  result=list(moranI=moran,p.value=p.value)
  return(result)
}




moran_time_dist=function(y,y_years,dist_mat,dist_seq,years){
  n=length(years)
  m=length(dist_seq)
  result_df=matrix(0,n*m,4)
  counter=1
  for(i in 1:n){
    ytemp=y[y_years==years[i]]
    for(j in 1:m){
      w_mat=weight_distance_matrix(dist_mat,dist_seq[j])
      test_m=moranI(ytemp,w_mat,scaling = TRUE)
      result_df[counter,1]=test_m[[1]]
      result_df[counter,2]=test_m[[2]]
      result_df[counter,3]=years[i]
      result_df[counter,4]=dist_seq[j]
      counter=counter+1
    }
  }
  result_df=as.data.frame(result_df)
  colnames(result_df)=c("MoransI","p.value","year","distance")
  return(result_df)
}



MoranI_pop=function(y,pop,dist_mat,dist_seq,lambda){
  n=length(lambda)
  m=length(dist_seq)
  result_df=matrix(0,n*m,4)
  counter=1
  for(i in 1:n){
    for(j in 1:m){
      w_mat=weight_distance_matrix(dist_mat,dist_seq[j],pop,lambda[i],options="population")
      test_m=moranI(y,w_mat,scaling = TRUE)
      result_df[counter,1]=test_m[[1]]
      result_df[counter,2]=test_m[[2]]
      result_df[counter,3]=lambda[i]
      result_df[counter,4]=dist_seq[j]
      counter=counter+1
    }
  }
}


