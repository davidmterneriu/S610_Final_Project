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
write_csv2(test_df,"test_data.csv")
write_csv2(test_df_un,"test_data_unemployment.csv")
