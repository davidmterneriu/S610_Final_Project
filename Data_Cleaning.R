#Data Cleaning
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
test_df=county_data

test_df=test_df%>%inner_join(select(Op_data,FIPS,`Opioid Prescribing Rate`,Year),by=c("fips"="FIPS"))%>%
  rename(OPR=`Opioid Prescribing Rate`)



#Cleaning up population/unemployment data
#unemployment data 
unemployment_data <- read_excel("unemployment_data.xlsx")
unemployment_data$fips=paste(unemployment_data$STATE_FIPS,unemployment_data$COUNTY_FIPS,sep="")
unemployment_data$fips=gsub("^[0]+","",unemployment_data$fips)%>%as.character()
unemployment_data=select(unemployment_data,"fips","Year","UN_RATE")



#US county population 
UScounty_pop_data <- read_csv("UScounty_pop_data.csv")
UScounty_pop_data=UScounty_pop_data%>%gather(key="Year",value=population,8:12)

UScounty_pop_data$Year=gsub("POPESTIMATE","",UScounty_pop_data$Year)%>%as.numeric()
UScounty_pop_data$state=state.abb[match(UScounty_pop_data$STNAME,state.name)]
UScounty_pop_data=select(UScounty_pop_data,"CTYNAME","state","Year","population")

test_df=test_df%>%inner_join(UScounty_pop_data,by=c("county"="CTYNAME","state"="state","Year"="Year"))
test_df_un=test_df%>%inner_join(unemployment_data,by=c("fips","Year"))

#Saving data
write_csv(test_df,"test_data.csv")
write_csv(test_df_un,"test_data_unemployment.csv")


#Cleaning Map Data 
#devtools::install_github("UrbanInstitute/urbnmapr")
#library(urbnmapr)
#library(viridisLite)

#US_cont=urbnmapr::states
#Remove Alaska and Hawaii
#rem_states=c("Alaska","Hawaii")
#US_cont=US_cont[US_cont$state_name %in% rem_states==FALSE,]


#test_data <- read_csv("test_data.csv")
#test_county=counties
#test_county$fips=gsub("^[0]+","",test_county$county_fips)%>%as.numeric()

#test_data2013=test_data[test_data$Year==2013,]

#map2013=test_data2013%>%inner_join(test_county,by=c("fips"))


#map2013 %>%
#  ggplot(aes(long, lat.y, group = group, fill = OPR)) +
#  geom_polygon(color = NA) +
#  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#  labs(fill = "Opioid Prescription Rate 2013")+
#  scale_fill_viridis_c( na.value = "black10")
  #scale_fill_gradient2(low=scales::muted("blue"),mid="white",high=scales::muted("red"),na.value = "grey50")
