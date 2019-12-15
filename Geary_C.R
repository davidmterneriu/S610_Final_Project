data1 = read.csv('~/test_data_unemployment_new.csv',header = TRUE)


############## the following code are made by David to deal with distance ##############
library(parallel)
library(dplyr )
library(readr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(readxl)
library(doBy)

## Longitude/latitude coordinates for start/endpoint in degrees (nx1 vectors)
## Haversine great circle distance in miles
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
  }
  return(w_mat)
}




################## Global Geary's C for Opioid Prescription Rate #############################

## input
# "year" is 2013, 2014, 2015, 2016, 2017
# "Dmax" is dmax in weight_distance_matrix function

find_global_Geary_C = function(year, Dmax, Lambda){
  
  year = year %>% as.integer(.)
  
  a_dist_matrix = distance_matrix(data1$fips[data1$Year == year], data1$lon[data1$Year == year] , data1$lat[data1$Year == year])
  a_weight_dis_matrix = weight_distance_matrix(a_dist_matrix,dmax = Dmax, pop = data1$population[data1$Year == year] ,lambda = Lambda ,options="none for now")
  N = sum( data1$Year == year )
  #data1$fips = lapply(data1$fips, function(x){if(nchar(x)==4){ paste0("0",x) } else {x} }   )
  data1$fips = data1$fips %>% as.character(.)
  x_bar = sum(data1$OPR[ data1$Year == year ] )/ N
  numerator = 0
  
  denominator = 2 * sum(a_weight_dis_matrix) * sum( (data1$OPR[data1$Year == year ] - x_bar )^2 ) 
  
  for (i in colnames(a_weight_dis_matrix)){
    
    for (j in rownames(a_weight_dis_matrix)){
      
      numerator =  numerator + a_weight_dis_matrix[i, j] * 
        ( data1$OPR[data1$Year == year & data1$fips == i] - data1$OPR[data1$Year == year & data1$fips == j] )^2
      
    }
    
    
  }
  
  C = (N-1) * numerator / denominator
  
  # #print("Geary's C for Year", year, "is",  C, "given dmax = ", Dmax)
  # 
  # 
  # ############# Tthe following part is about significant test ###########
  # 
  # S0 = sum(a_weight_dis_matrix)
  # S1 = 2* S0
  # 
  # S2 = 0
  # 
  # for(i in 1:nrow(a_weight_dis_matrix)){
  #   
  #   S2 = S2 + ( (a_weight_dis_matrix[i, ] %*% a_weight_dis_matrix[, i] ) %>% as.integer(.) %>% "*"(.,4) )
  #   
  # }
  # 
  # 
  # K = sum( (data1$OPR[data1$Year == year]-x_bar )^4 / N )  / (sum( (data1$OPR[data1$Year == year]-x_bar )^2 / N ) )^2
  # 
  # # term1 = (N-1)*S1*( N^2 -3*n + 3 -(N-1)*K  )
  # # term2 = 0.25 * (N-1) * S2 * (N^2 + 3 * N -6- (N^2 -N+2)*K )
  # # term3 = (N^2 -3 -K*(N-1)^2) * S0^2
  # # term4 = N*(N-2)*(N-3)*S0^2
  # # 
  # # Var_C = ( term1-term2+term3 ) / term4
  # # EC = -1/(N-1)
  # # z_score = (C-EC)/sqrt(Var_C)
  # 
  # Var_C = (1/(2*(N+1)*S0^2)) * ((2*S1+S2)*(N-1)-4*S0^2)
  # EC = -1/(N-1)
  # z_score = (C-EC)/sqrt(Var_C)
  # pvalue = pnorm(-abs(z_score))
  # 
  # # print("C is ", C)
  # # print("Variance is ", Var_C)
  # # print("z_score is ", z_score)
  # # print("p-value is ", pvalue)
  
  return(c( C ))
  
}



################## Global Geary's C for Unemployment Rate #############################

## input
# "year" is 2013, 2014, 2015, 2016, 2017
# "Dmax" is dmax in weight_distance_matrix function

find_global_Geary_C_UN_RATE = function(year, Dmax, Lambda){
  
  year = year %>% as.integer(.)
  
  a_dist_matrix = distance_matrix(data1$fips[data1$Year == year], data1$lon[data1$Year == year] , data1$lat[data1$Year == year])
  a_weight_dis_matrix = weight_distance_matrix(a_dist_matrix,dmax = Dmax, pop = data1$population[data1$Year == year] ,lambda = Lambda ,options="none for now")
  N = sum( data1$Year == year )
  data1$fips = lapply(data1$fips, function(x){if(nchar(x)==4){ paste0("0",x) } else {x} }   )
  data1$fips = data1$fips %>% as.character(.)
  x_bar = sum(data1$UN_RATE[ data1$Year == year ] )/ N
  numerator = 0
  
  denominator = 2 * sum(a_weight_dis_matrix) * sum( (data1$UN_RATE[data1$Year == year ] - x_bar )^2 ) 
  
  for (i in colnames(a_weight_dis_matrix)){
    
    for (j in rownames(a_weight_dis_matrix)){
      
      numerator =  numerator + a_weight_dis_matrix[i, j] * 
        ( data1$UN_RATE[data1$Year == year & data1$fips == i] - data1$UN_RATE[data1$Year == year & data1$fips == j] )^2
      
    }
    
    
  }
  
  C = (N-1) * numerator / denominator
  
  # #print("Geary's C for Year", year, "is",  C, "given dmax = ", Dmax)
  # 
  # 
  # ############# Tthe following part is about significant test ###########
  # S0 = sum(a_weight_dis_matrix)
  # S1 = 2* S0
  # 
  # S2 = 0
  # 
  # for(i in 1:nrow(a_weight_dis_matrix)){
  #   
  #   S2 = S2 + ( (a_weight_dis_matrix[i, ] %*% a_weight_dis_matrix[, i] ) %>% as.integer(.) %>% "*"(.,4) )
  #   
  # }
  # 
  # 
  # K = sum( (data1$UN_RATE[data1$Year == year]-x_bar )^4 / N )  / (sum( (data1$UN_RATE[data1$Year == year]-x_bar )^2 / N ) )^2
  # 
  # # term1 = (N-1)*S1*( N^2 -3*n + 3 -(N-1)*K  )
  # # term2 = 0.25 * (N-1) * S2 * (N^2 + 3 * N -6- (N^2 -N+2)*K )
  # # term3 = (N^2 -3 -K*(N-1)^2) * S0^2
  # # term4 = N*(N-2)*(N-3)*S0^2
  # # 
  # # Var_C = ( term1-term2+term3 ) / term4
  # # EC = -1/(N-1)
  # # z_score = (C-EC)/sqrt(Var_C)
  # 
  # Var_C = (1/(2*(N+1)*S0^2)) * ((2*S1+S2)*(N-1)-4*S0^2)
  # EC = -1/(N-1)
  # z_score = (C-EC)/sqrt(Var_C)
  # pvalue = pnorm(-abs(z_score))
  # 
  # # print("C is ", C)
  # # print("Variance is ", Var_C)
  # # print("z_score is ", z_score)
  # # print("p-value is ", pvalue)
  
  return(c( C ) )
  
}




# NOT WORKING WELL ########## compute and plot local Geary's C ###########
# a_local_Geary_C = find_local_Geary_C(2013, 2000, 50000)


########  compute and plot Geary's C for Opioid Prescription Rate with a range of dmax and lambda #########

Dmax = c(200, 400, 600, 800, 1000, 3000)
year = c(2013, 2014, 2015, 2016, 2017)

GearyC_OPR = data.frame("year" = rep(year,each = 6), "Dmax" = Dmax, "C" = 0)

GearyC_OPR$C = mcmapply(find_global_Geary_C, GearyC_OPR$year, GearyC_OPR$Dmax, 500000)

############   compute and plot Geary's C for Unemployment Rate with a range of dmax and lambda  ###########


Dmax = c(200, 400, 600, 800, 1000, 3000)
year = c(2013, 2014, 2015, 2016, 2017)

GearyC_UN_RATE = data.frame("year" = rep(year,each = 6), "Dmax" = Dmax, "C" = 0)

GearyC_UN_RATE$C = mcmapply(find_global_Geary_C_UN_RATE, GearyC_UN_RATE$year, GearyC_UN_RATE$Dmax, 500000)


































