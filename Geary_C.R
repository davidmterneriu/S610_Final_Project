#rm(list = ls())




setwd()


data1 = read.csv('D://Box Sync//Fall 2019//R//Stats 610 Final Project//Medicare & Medicaid Opioid//Consolidated Data//test_data.csv',header = TRUE)
#data2 = read.csv('D://Box Sync//Fall 2019//R//Stats 610 Final Project//Medicare & Medicaid Opioid//Consolidated Data//test_data_unemployment.csv',header = TRUE)


################### more cleaning on the data sets  ##########################

##################### share this section with David #########################

library("dplyr")

# Replace comma's by decimal points
data1$lon = data1$lon %>% as.character() %>% gsub(",",".", .) %>% as.double(.)
data1$lat = data1$lat %>% as.character() %>% gsub(",",".", .) %>% as.double(.)
# data2$lat = data2$lat %>% as.character() %>% gsub(",",".", .) %>% as.double(.)
# data2$lat = data2$lat %>% as.character() %>% gsub(",",".", .) %>% as.double(.)
# data2$UN_RATE = data2$UN_RATE %>% as.character() %>% gsub(",",".", .) %>% as.double(.)



################## data cleaning finished ############################


############## the following code are made by David to deal with distance ##############


library(readr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(readxl)
#Used for fast sorting 
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



## Geographic identifier with longitude/latitude coordinates (nx1 vectors)
## Square and symmetric distance matrix D
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
  test_df$dist=gcd.hf(test_df$long1,test_df$lat1,test_df$long2,test_df$lat2)
  #Making the square matrix 
  myMat <- matrix(0, n, n, dimnames = list(geo, geo))
  # fill in the matrix with matrix indexing on row and column names
  myMat[as.matrix(test_df[c("Var1", "Var2")])] <- test_df[["dist"]]
  return(myMat)
}


# Input: D
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



################## Global Geary's C #############################

## input
# "year" is 2013, 2014, 2015, 2016, 2017
# "Dmax" is dmax in weight_distance_matrix function

find_global_Geary_C = function(year, Dmax){
  
  year = year %>% as.integer(.)
  
  a_dist_matrix = distance_matrix(data1$fips[data1$Year == year], data1$lon[data1$Year == year] , data1$lat[data1$Year == year])
  a_weight_dis_matrix = weight_distance_matrix(a_dist_matrix,dmax = Dmax, pop = data1$population[data1$Year == year] ,lambda = 50000 ,options="none for now")
  N = sum( data1$Year == year )
  data1$fips = lapply(data1$fips, function(x){if(nchar(x)==4){ paste0("0",x) } else {x} }   )
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
  
  C = N * numerator / denominator
  
  return(C)
  
  print("Geary's C for Year", year, "is",  C, "given dmax = ", Dmax)
  
}


######### for Year 2013 ##########
# a_dist_matrix_2013 = distance_matrix(data1$fips[data1$Year == 2013], data1$lon[data1$Year == 2013] , data1$lat[data1$Year == 2013])
# 
# a_weight_dis_matrix_2013 = weight_distance_matrix(a_dist_matrix_2013,dmax = 10000, pop = data1$population[data1$Year == 2013] ,lambda = 50000 ,options="none for now")
# 
# # N is the number of observations for each year
# N = sum( data1$Year == 2013 )
# 
# data1$fips = lapply(data1$fips, function(x){if(nchar(x)==4){ paste0("0",x) } else {x} }   )
# data2$fips = lapply(data2$fips, function(x){if(nchar(x)==4){ paste0("0",x) } else {x} }   )
# 
# data1$fips = data1$fips %>% as.character(.)
# data2$fips = data2$fips %>% as.character(.)
# 
# # x_bar is the meaning of the variable OPR
# 
# x_bar_2013 = sum(data1$OPR[ data1$Year == 2013 ] )/ N
# x_bar_2014 = sum(data1$OPR[ data1$Year == 2014 ] )/ N
# x_bar_2015 = sum(data1$OPR[ data1$Year == 2015 ] )/ N
# x_bar_2016 = sum(data1$OPR[ data1$Year == 2016 ] )/ N
# x_bar_2017 = sum(data1$OPR[ data1$Year == 2017 ] )/ N
# 
# 
# # Geary's C
# 
# numerator = 0
# 
# denominator = 2 * sum(a_weight_dis_matrix_2013) * sum( (data1$OPR[data1$Year == 2013 ] - x_bar_2013 )^2 ) 
# 
# for (i in colnames(a_weight_dis_matrix_2013)){
#   
#   for (j in rownames(a_weight_dis_matrix_2013)){
#     
#     numerator =  numerator + a_weight_dis_matrix_2013[i, j] * 
#       ( data1$OPR[data1$Year == 2013 & data1$fips == i] - data1$OPR[data1$Year == 2013 & data1$fips == j] )^2
#   }
# }
# 
################################ It turns out that Geary's C for year 2013 is 0.870001  #################################
# C = N * numerator / denominator



########### Local Geary's C #############


find_local_Geary_C = function(year, Dmax){
  
  
  year = year %>% as.integer(.)
  
  a_dist_matrix = distance_matrix(data1$fips[data1$Year == year], data1$lon[data1$Year == year] , data1$lat[data1$Year == year])
  a_weight_dis_matrix = weight_distance_matrix(a_dist_matrix,dmax = Dmax, pop = data1$population[data1$Year == year] ,lambda = 50000 ,options="none for now")
  
  
  local_Geary_C = data.frame("fips" = data1$fips %>% unlist(.) , "c" = rep(0, length(data1$fips[data1$Year == year])))

  local_c = function(x){ 
    
    c = 0
    
    for(j in colnames(a_weight_dis_matrix)){
      
      c = c + a_weight_dis_matrix[x, j] * ( data1$OPR[data1$Year == year & data1$fips == x] - data1$OPR[data1$Year == year & data1$fips == j] )^2
      
    } 
    
    return(c)
    
    }
  
  local_Geary_C$c = lapply(local_Geary_C$fips, local_c )

  
  return(local_Geary_C)
  
  print("Local Geary's C for Year", year, "is",  local_Geary_C, "given dmax = ", Dmax)
  

}







########## make a heatmap for Opioid Prescription Rate ########

plot_OPR = function(){
  
  library(tigris)
  options(tigris_class = "sf")
  shape = counties(state = NULL, cb = T)
  
  data1$fips = lapply(data1$fips, function(x){if(nchar(x)==4){ paste0("0",x) } else {x} }   )
  data1$fips = data1$fips %>% as.character(.)
  
  data1 = left_join(data1, shape,  by=c("fips"="GEOID")  )
  
  data1 = data1[, -(12:18)]
  
  data1 %>%
    ggplot(aes(geometry=geometry, fill = OPR, color = OPR)) +
    facet_wrap(~Year, ncol=2) +
    geom_sf() +
    coord_sf() +
    scale_fill_viridis(direction=-1) +
    scale_color_viridis(direction=-1) +
    theme_void() +
    theme(panel.grid.major = element_line(colour = 'transparent')) +
    labs(title="Opioid Prescription Rate over Years") %>% return(.)
  
  ## It takes about 3 minutes to finish plotting

}

plot_OPR()

























































