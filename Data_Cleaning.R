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
library(maps)
library(socviz)
library(urbnmapr)
library(viridisLite)
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
library(urbnmapr)
library(viridisLite)

#US_cont=urbnmapr::states
#Remove Alaska and Hawaii
#rem_states=c("Alaska","Hawaii")
#US_cont=US_cont[US_cont$state_name %in% rem_states==FALSE,]

quantile(test_data$localMoran)

hist(test_data$localMoran)

test_data <- read_csv("lMoran2013.csv")
test_data=test_data[,-1]
test_county=counties
test_county$fips=gsub("^[0]+","",test_county$county_fips)%>%as.numeric()

#test_data2013=test_data[test_data$Year==2013,]

map2013=test_data %>%merge(test_county,by=c("fips"),all = TRUE)


g1=map2013 %>%
  ggplot(aes(long, lat, group = group, fill = localMoran)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "localMoran 2013")+
  scale_fill_viridis_c( na.value = "grey")
  #scale_fill_gradient2(low=scales::muted("blue"),mid="white",high=scales::muted("red"),na.value = "grey50")


#Better Maps 
library(maps)
library(socviz)
us_states <- map_data("state")

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

typeof(county_map$id)


test_data <- read_csv("lMoran2013.csv")
test_data=test_data[,-1]
test_data$fips=test_data$fips%>%as.character()
test_data$localMoran=ifelse(abs(test_data$localMoran)<=20,test_data$localMoran,NA)
county_map1=county_map
county_map1$id=gsub("^[0]+","",county_map1$id)%>%as.character()
county_full <- left_join(county_map1, test_data, by = c("id"="fips"))

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = localMoran, 
                          group = group))

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Local Moran I: 2013",subtitle = "Abs Local Moran I < 20")
p1

plotly::ggplotly(p1)


# Other data 

test_data <- read_csv("test_data.csv")
test_data$fips=test_data$fips%>%as.character()
test_data=test_data[test_data$Year==2013,]

county_map1=county_map
county_map1$id=gsub("^[0]+","",county_map1$id)%>%as.character()

county_full <- left_join(county_map1, test_data, by = c("id"="fips"))
p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat.x,
                          fill = OPR, 
                          group = group,label=county))+geom_polygon(color = "white", size = 0.05)

p1 <- p + geom_polygon(color = "white", size = 0.05) + coord_equal()+ggthemes::theme_map()+
  scale_fill_viridis_c()+labs(title = "Opioid Prescription Rates: 2013")

p1


ggplot(data = county_full,
       mapping = aes(x = long, y = lat.x,
                     fill = OPR, 
                     group = group))+geom_polygon(color = "white", size = 0.05)
