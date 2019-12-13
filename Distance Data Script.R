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
  master_df=cbind.data.frame(geo,long,lat) %>% unique.data.frame()
  geo=master_df$geo
  long=master_df$long
  lat=master_df$lat
  n=length(geo)
  test_df=expand.grid(geo,geo)%>%as.data.frame()
  test_df=test_df%>%inner_join(master_df,by=c("Var1"="geo"))%>%
    rename("long1"="long","lat1"="lat")%>%
    inner_join(master_df,by=c("Var2"="geo"))%>%
    rename("long2"="long","lat2"="lat")
  #If geo-codes are read in as numeric data, this lead to future problems. R will try to use geo-codes as 
  #matrix indicies as opposed to row/col names. 
  test_df$Var1=as.character(test_df$Var1)
  test_df$Var2=as.character(test_df$Var2)
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
    #browser()
    w_mat=dist_mat
    w_mat[w_mat>dmax]<-0
    w_mat=sweep(w_mat,2,dis_pop, '/')^(-1)
    diag(w_mat) <- 0
    w_mat[is.infinite(w_mat)]<-0
    
  }
  return(w_mat)
}






moranI=function(y,w_mat,scaling=FALSE,p.test="two.sided"){
  #Variable description-----------------------------------
  #INPUTS:
  #y: variable of interest
  #weight_max: square distance adjusted weight matrix 
  #scaling: default to FALSE; if TRUE, distance-weights sum to unity
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
      #browser()
      w_mat=weight_distance_matrix(dist_mat,dist_seq[j],pop,lambda[i],options="population")
      test_m=moranI(y,w_mat,scaling = TRUE)
      result_df[counter,1]=test_m[[1]]
      result_df[counter,2]=test_m[[2]]
      result_df[counter,3]=lambda[i]
      result_df[counter,4]=dist_seq[j]
      counter=counter+1
    }
  }
  return(result_df)
}


LocalMoran=function(y,dist_mat,dmax,scaling=FALSE,p.test="two.sided"){
  #INPUTS:
  #y: a column vector with data to be spatially-correlated 
  #dist_mat: distance matrix
  #dmax: size of distance band 
  #scaling: default to FALSE; if TRUE, distance-weights sum to unity
  
  #Checks that each geography i has at least 2 neighbors
  dmax_true=apply(dist_mat,1,FUN =Rfast::nth,3,descending = F )%>%max()%>%
    round(0)
  
  if(dmax_true>dmax){
    ms=paste("dmax is too small. Choose dmax greater than:",dmax_true,sep=" ")
    return(message(ms))}
  w_mat=weight_distance_matrix(dist_mat,dmax)
  n=length(y)
  moran=numeric(n)
  pstore=numeric(n)

  for (i in 1:n){
    #Need a way to make sure that averaged y only takes into account i's dmax determined neighbors 
    y_ind=which(w_mat[i,]>0)
    ytemp=y[c(i,y_ind)]
    mean_y=mean(ytemp)
    y_dmean=y-mean(y)
    y_dmean[-y_ind]<-0
  if(scaling){
    rsum=apply(w_mat,1,sum)
    rsum[rsum==0]<-1
    weight=w_mat/rsum
  }else{
    weight=w_mat
  }
  #Adjusting for zero weighted observations
    N=sum(weight[i,]>0)+1
    SI2=sum(weight[,-i])/(N-1)-(mean_y)^2
    Im=(y[i]-mean_y)/SI2*(sum(weight[,-i]*y_dmean[-i]))
    moran[i]=Im
  #Getting p.values 
    EI=-1*(sum(weight[,-i]))/N
    b2=N*(sum((y_dmean[-i])^4))/((sum((y_dmean[-i])^2))^2)
    C=sum(weight[,-i]*t(weight[-i,]))
    VI=(N-b2)*sum(weight[,-i])/(N-1)-(2*b2-N)*C/((N-1)*(N-2))-(EI)^2
    sdV=sqrt(VI)
    p.value=pnorm(Im,mean=EI,sd=sdV)
    if(p.test=="two.sided"){
      p.value=ifelse(Im<=EI,2*p.value,2*(1-p.value))
    }else{
      p.value=1-p.value
    }
    pstore[i]=p.value
  }
  result=list(moranI=moran,p.value=pstore)
  return(result)
}

#---------------

Getis_Ord=function(y,w_mat,p.test="one.sided"){
  #Variable description-----------------------------------
  #INPUTS:
  #y: variable of interest
  #weight_max: square distance adjusted weight matrix 
  #OUTPUT: 
  #General G: A measure of how concentrated high/low values are for a given study area
  #Spot Type: The difference between G and E[G]; if positive, return hot spot 
  #           (i.e. high-values cluster togther), o.w. cold spot. 
  #p.value: one-sided p-value
  browser()
  weight=w_mat
  W=sum(weight)
  
  numerator=t(y)%*%weight%*%y%>%as.numeric
  denom=t(y)%*%y%>%as.numeric
  
  #Adjusting for zero weighted observations
  A=sum(weight>0)/2
  N=min(length(y),A)

  G=numerator/denom
 
  #Time to get z-value
  EG=W/(N*(N-1))
  # Getting Variance 
  #https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/h-general-g-additional-math.htm
  
  S1=1/2*sum((weight+t(weight)^2))
  S2=sum((2*colSums(weight))^2)
  D0=(N^2-3*N+3)*S1-N*S2+3*W^2
  D1=-1*((N^2-N)*S1-2*N*S2+6*W^2)
  D2=-1*(2*N-(N+3)*S2+6*W^2)
  D3=4*(N-1)*S1-2*(N+1)*S2+8*W^2
  D4=S1-S2+W^2
  C=(sum(y)^2-sum(y^2))^2*N*(N-1)*(N-2)*(N-3)
  B=D3*sum(y)*sum(y^3)+D4*sum(y)^4
  A=D0*sum(y^2)^2+D1*sum(y^4)+D2*sum(y)^2*sum(y^2)
  VG=(A+B)/C-EG^2
  #VG=ifelse(VG<0,.Machine$double.eps,VG)
  sdV=sqrt(VG)
  p.value=pnorm(G,mean=EG,sd=sdV)
  if(p.test=="two.sided"){
    p.value=ifelse(G<=EG,2*p.value,2*(1-p.value))
  }else{
    p.value=1-p.value
  }
  
  result=list(General_G=G,Spot=G-EG,p.value=p.value)
  return(result)
}


Getis_Ord_local=function(y,dist_mat,dmax,p.test="two.sided"){
  #INPUTS:
  #y: a column vector with data to be spatially-correlated 
  #dist_mat: distance matrix
  #dmax: size of distance band 
  #
  #OUTPUTS: 
  #local_G: nonstanardized, distance-weighted average 
  #z.score: zscore computed from expected local_G and its variance 
  #Checks that each geography i has at least 1 neighbor
  #p.value: one-sided p-value (option dependent)
  dmax_true=apply(dist_mat,1,FUN =Rfast::nth,2,descending = F )%>%max()%>%
    round(0)
  
  if(dmax_true>dmax){
    ms=paste("dmax is too small. Choose dmax greater than:",dmax_true,sep=" ")
    return(message(ms))}
  w_mat=weight_distance_matrix(dist_mat,dmax)
  n=length(y)
  local_G=numeric(n)
  local_Gz=numeric(n)
  local_p=numeric(n)
  #browser()
  for (i in 1:n){
    ind=w_mat[i,]>0
    ind[i]<-TRUE
    ind2=ind==FALSE
    y_temp=y
    y_temp[ind2]<-0
    GI=sum(w_mat[,i]*y_temp)/sum(y_temp)
    WI=sum(w_mat[,i])
    #Adjusting for zero weighted observations
    N=sum(w_mat[i,]>0)+1
    EGI=WI/N
    ybar=mean(y_temp[y_temp>0])
    sy=sum(y_temp^2)-ybar^2
    VGI=(sy*WI*(N-WI))/(ybar*(N-1))
    sdV=sqrt(VGI)
    ZGI=sum(w_mat[,i]*y_temp)-ybar*sum(w_mat[,i]^2)
    denom=sqrt(sy)*sqrt((N*sum(w_mat[,i])-sum(w_mat[,i])^2)/(N-1))
    ZGI=ZGI/denom
    local_G[i]=GI
    local_Gz[i]=ZGI
    p.value=pnorm(GI,mean=EGI,sd=sdV)
    if(p.test=="two.sided"){
      p.value=ifelse(GI<=EGI,2*p.value,2*(1-p.value))
    }else{
      p.value=1-p.value
    }
    local_p[i]=p.value
  }
  result=list(local_G=local_G,p.value=local_p)
  return(result)
  
}



Getis_Ord_local_z=function(y,dist_mat,dmax){
  #INPUTS:
  #y: a column vector with data to be spatially-correlated 
  #dist_mat: distance matrix
  #dmax: size of distance band 
  #
  #OUTPUTS: 
  #local_G: nonstanardized, distance-weighted average 
  #z.score: zscore computed from expected local_G and its variance 
  #Checks that each geography i has at least 1 neighbor
  #p.value: one-sided p-value (option dependent)
  dmax_true=apply(dist_mat,1,FUN =Rfast::nth,2,descending = F )%>%max()%>%
    round(0)
  
  if(dmax_true>dmax){
    ms=paste("dmax is too small. Choose dmax greater than:",dmax_true,sep=" ")
    return(message(ms))}
  w_mat=weight_distance_matrix(dist_mat,dmax)
  n=length(y)
  local_Gz=numeric(n)
  #local_p=numeric(n)
  #browser()
  for (i in 1:n){
    ind=w_mat[i,]>0
    ind[i]<-TRUE
    n_true=sum(ind)
    ind2=ind==FALSE
    y_temp=y
    y_temp[ind2]<-0
    ybar=mean(y_temp[y_temp>0])
    numerator=sum(w_mat[i,]*y_temp)-ybar*sum(w_mat[i,])
    S=sqrt(sum(y_temp^2)/n_true-ybar^2)
    S1=sqrt((n_true*sum(w_mat[i,]^2)-sum(w_mat[i,]^2))/(n_true-1))
    local_Gz[i]=numerator/(S*S1)
  }
  result=data.frame(Gi=local_Gz,p.value=2*pnorm(-abs(local_Gz)),spot=ifelse(local_Gz<0,"Cold","Hot"))
  return(result)
}

local_GC=function(y,dist_mat,dmax){
  #INPUTS:
  #y: a column vector with data to be spatially-correlated 
  #dist_mat: distance matrix
  #dmax: size of distance band 
  #
  #OUTPUTS: 
  #local_G: nonstanardized, distance-weighted average 
  #p.value: two.sided pvalue 
  #Checks that each geography i has at least 1 neighbor
  dmax_true=apply(dist_mat,1,FUN =Rfast::nth,2,descending = F )%>%max()%>%
    round(0)
  
  if(dmax_true>dmax){
    ms=paste("dmax is too small. Choose dmax greater than:",dmax_true,sep=" ")
    return(message(ms))}
  w_mat=weight_distance_matrix(dist_mat,dmax)
  n=length(y)
  local_C=numeric(n)
  local_Cz=numeric(n)
  for (i in 1:n){
    ind=w_mat[i,]>0
    ind[i]<-TRUE
    n_true=sum(ind)
    ind2=ind==FALSE
    y_temp=y
    y_temp[ind2]<-0
    ybar=mean(y_temp[y_temp>0])
    ysig=sd(y_temp[y_temp>0])
    num=sum(w_mat[i,]*y_temp)-ybar*sum(w_mat[i,])
    s=sqrt(sum(y_temp^2)/n_true-(ybar)^2)
    denom=sqrt(1/(n_true-1)*(n_true*sum(w_mat[i,]^2)-(sum(w_mat[i,]))^2))*s
    local_C[i]=num/denom
    z_temp=ifelse(y_temp==0,0,(y_temp-ybar)/ysig)
    local_Cz[i]=1/2*sum(w_mat[i,]*(z_temp[i]-z_temp)^2)
  }
  result=data.frame(local_Cz,p.value=pnorm(abs(-local_Cz)))
  return(result)
}


grid_spacing=function(a,b,n,theta){
  #Inputs: 
  #a: start point 
  #b: end point 
  #n: number of grid points 
  #theta: tuning paramter; if unity, then grid spacing is linear
  #Output: 
  #x: 1xn vector of grid points 
  x=numeric(n)
  for (i in 1:n){
    x[i]=a+(b-a)*((i-1)/(n-1))^(theta)
  }
  return(x)
  
  #Outputs: 
}
