##processing data for analysis

setwd("~/Documents/github/coralreef/")

library(dplyr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(maptools)
library(rgdal)
library(broom)
library(mapproj)
library(viridis)
library(scales)
library(cowplot)
library(forcats)
library(ggpubr)

##  Preparing dataset
data <- read.csv("coralreef_gender_cleaned_final_2020.csv",header=TRUE)
data <- data %>% filter(is.na(Excluded)) %>% distinct()
data <- filter(data,!is.na(Pub_year))
data$Journal <- as.character(data$Journal)

##Coding first and last authors
id <- unique(data$Article.ID)
x <- c(1:n_distinct(data$Article.ID))
first_last <- data.frame(Article.ID=integer(),Author_order=integer(),First_last=character())

dat <- data %>% select(Article.ID, Author_order) %>% distinct()

for (i in x){
  y <- id[i]
  sub <- dat %>% filter(Article.ID == y) %>% distinct()
  z <- c(1:nrow(sub))
  l <- max(sub$Author_order)
  if(length(z) == 1){
    sub[1,3] <- c("single")
  } else {
    for(i in z){
      if (sub[i,2] == 1) {
        sub[i,3] <- c("first")
      } else if (sub[i,2] == l) {
        sub[i,3] <- c("last")
      } else {
        sub[i,3] <- c("other")
      }
    }
  }
  first_last <- rbind(first_last,sub)
}

colnames(first_last) <- c("Article.ID","Author_order","First_last")

df <- left_join(data,first_last,by=c("Article.ID","Author_order"))
df$Study.Country <- as.character(df$Study.Country)

#Separating out countries for stats and mapping
df_c <- select(df, Article.ID, Study.Country) %>% distinct()
df_c <- df_c %>% filter(!is.na(Study.Country)) %>% distinct()

#Fix syntax and delimiter between countries, fixing mispellings and typos
df_c$Study.Country <- gsub(', ',',',df_c$Study.Country)
df_c$Study.Country <- gsub('Congo,The Democratic Republic Of The','Congo, The Democratic Republic Of',df_c$Study.Country)
df_c$Study.Country <- gsub('Congo,The Democratic Republic Of','Congo, The Democratic Republic Of',df_c$Study.Country)
df_c$Study.Country <- gsub('Iran,Islamic Republic Of','Iran, Islamic Republic Of',df_c$Study.Country)
df_c$Study.Country <- gsub('Korea,South','Republic of Korea',df_c$Study.Country)
df_c$Study.Country <- gsub('Korea,North',"Korea, The Democratic People's Republic Of",df_c$Study.Country)
df_c$Study.Country <- gsub('Micronesia,Federated States of','Micronesia, Federated States of',df_c$Study.Country)
df_c$Study.Country <- gsub('Taiwan,Province Of China','Taiwan, Province Of China',df_c$Study.Country)
df_c$Study.Country <- gsub('Tanzania,United Republic Of','Tanzania, United Republic Of',df_c$Study.Country)
df_c$Study.Country <- gsub(' US Virgin Islands','United States Virgin Islands',df_c$Study.Country)
df_c$Study.Country <- gsub('US Virgin Isands','United States Virgin Islands',df_c$Study.Country)
df_c$Study.Country <- gsub('US Virgin Islands','United States Virgin Islands',df_c$Study.Country)
df_c$Study.Country <- gsub('USA','United States',df_c$Study.Country)
df_c$Study.Country <- gsub(' Northern Mariana Islands','Northern Mariana Islands',df_c$Study.Country)
df_c$Study.Country <- gsub('Brazil ','Brazil',df_c$Study.Country)
df_c$Study.Country <- gsub('Cook Ilsands','Cook Islands',df_c$Study.Country)
df_c$Study.Country <- gsub("DRC", "Congo, The Democratic Republic Of", df_c$Study.Country)
df_c$Study.Country <- gsub("HI", "United States", df_c$Study.Country)
df_c$Study.Country <- gsub("imor Leste", "Timor-Leste", df_c$Study.Country)
df_c$Study.Country <- gsub("Chagos Archipelago","Chagos", df_c$Study.Country)
df_c$Study.Country <- gsub("New Calednoia","New Caledonia", df_c$Study.Country)
df_c$Study.Country <- gsub("Phillipines","Philippines", df_c$Study.Country)
df_c$Study.Country <- gsub("Pitcairn Islands","Pitcairn", df_c$Study.Country)
df_c$Study.Country <- gsub("St. Vincent","Saint Vincent", df_c$Study.Country)
df_c$Study.Country <- gsub("Saudia Arabia","Saudi Arabia", df_c$Study.Country)
df_c$Study.Country <- gsub("Saint Vincent And The Grenadines","Saint Vincent and the Grenadines", df_c$Study.Country)

df_c$Study.Country <- gsub('(\\w),(\\w)','\\1;\\2',df_c$Study.Country)

#Splitting countries out onto separate lines
c <- strsplit(df_c$Study.Country, split=";")
dataframe <- data.frame(ID=rep(df_c$Article.ID, sapply(c, length)), Study.Country = unlist(c))
colnames(dataframe) <- c("Article.ID","Study_Country")
dataframe <- distinct(dataframe)

#Add new column to dataframe
df2 <- left_join(df,dataframe,by="Article.ID") 

country.names <- df2 %>% select(Study_Country) %>% distinct() %>% filter(Study_Country != "") %>% filter(!is.na(Study_Country)) %>% distinct()  %>% arrange(Study_Country)

#Fixing regional/global
df2$Study_Country <- gsub('Global',NA,df2$Study_Country)
df2$Study_Country <- gsub('ABNJ',NA,df2$Study_Country)

#Creating new column for mapping purposes (territories without ISO 3166 codes are mapping under territory holder code), adjusting names to official "mapping" names
df2$Mapping_Country <- df2$Study_Country
df2$Mapping_Country <- gsub('Andaman and Nicobar','India',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Venezuela','Venezuela (Bolivarian Republic of)',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Wake Atoll','United States Minor Outlying Islands',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Swaziland','Eswatini',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Palmyra','United States Minor Outlying Islands',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Chagos Archipelago','British Indian Ocean Territory',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Saba','Bonaire, Sint Eustatius, Saba',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Bonaire','Bonaire, Sint Eustatius, Saba',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Scotland','United Kingdom',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Corsica (France)','France',df2$Mapping_Country)
df2$Mapping_Country <- gsub('Clipperton Atoll','United Kingdom',df2$Mapping_Country)

#Fixing study_type syntax
df_s <- select(df2, Article.ID, Laboratory) %>% distinct()
df_s <- df_s %>% filter(!is.na(Laboratory)) %>% distinct()

#Fix syntax and delimiter between countries, fixing mispellings and typos
df_s$Laboratory <- gsub(', ',',',df_s$Laboratory)
df_s$Laboratory <- gsub(',',';',df_s$Laboratory)
df_s$Laboratory <- gsub('; ',';',df_s$Laboratory)
df_s$Laboratory <- gsub('; ',';',df_s$Laboratory)
df_s$Laboratory <- gsub('Desk Studies','Desk studies',df_s$Laboratory)
df_s$Laboratory <- gsub('Field-based;Synthesis Modelling','Field-based;Synthesis;Modelling',df_s$Laboratory)
df_s$Laboratory <- gsub('Modeling','Modelling',df_s$Laboratory)
df_s$Laboratory[df_s$Laboratory == "Correspondence"] <- c("Editorials/Correspondence")
df_s$Laboratory[df_s$Laboratory == "Editorials"] <- c("Editorials/Correspondence")
df_s$Laboratory[df_s$Laboratory == ""] <- c("Undetermined")

#Splitting countries out onto separate lines
s <- strsplit(df_s$Laboratory, split=";")
dataframe_s <- data.frame(ID=rep(df_s$Article.ID, sapply(s, length)), Study_Type = unlist(s))
colnames(dataframe_s) <- c("Article.ID","Study_Type")
dataframe_s <- distinct(dataframe_s)

#Add new column to dataframe
df2 <- left_join(df2,dataframe_s,by="Article.ID") 

colnames(df2) <- c("Done","Excluded","Reviewer","Article.ID","Author","Author_gender","Country_affiliation","Title","Keywords","Abstract","Study.Country","Territory.disputed","Study_region","Study_type","Marine_realm","Author.Based.In.Country","Author.Based.In.Sovereign","Times.Cited","DOI","Affiliation","Journal","Author_order","Pub_year","First_last","Study_Country","Mapping_Country","Study_Type")

##Clean affiliation data into new column
df_a <- df2 %>% select(Article.ID,Author_order,Author,Country_affiliation) %>% distinct()
df_a$Mapping_affiliation <- df_a$Country_affiliation
df_a$Mapping_affiliation <- gsub('(\\w\\w) (\\d\\d\\d\\d\\d) United States','United States',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('^(\\w+) United States','United States',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('^(\\w+) (\\w+) United States','United States',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('^United States (\\w+) (\\d+)','United States',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('^United States (\\w+)$','United States',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub(' $','',df_a$Mapping_affiliation, perl=T)
df_a$Mapping_affiliation <- gsub(' \\(United Kingdom\\)','',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('^(\\w\\w), United States','United States',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('Fr Polynesia','French Polynesia',df_a$Mapping_affiliation)
df_a$Mapping_affiliation[df_a$Mapping_affiliation == 'STRI, Panama - Corrected from AA United States (not accurate on paper and only has one affiliation)'] <-'Panama'
df_a$Mapping_affiliation <- gsub('Papua N Guinea','Papua New Guinea',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('Phillipines','Philippines',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('Tanzania$','Tanzania, United Republic Of',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('The Netherlands','Netherlands',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('Trinid & Tobago','Trinidad and Tobago',df_a$Mapping_affiliation)
df_a$Mapping_affiliation <- gsub('UK','United Kingdom',df_a$Mapping_affiliation)

df_a <- df_a %>% select(Article.ID,Author_order,Mapping_affiliation) %>% distinct()
df2 <- left_join(df2,df_a,by=c("Article.ID","Author_order"))

##Cleaning data for gender
df2$Author_gender <- gsub("FEmale","Female",df2$Author_gender)
df2$Author_gender <- gsub("Female ","Female",df2$Author_gender)
df2$Author_gender <- gsub("Male ","Male",df2$Author_gender)

#Assuming that unknown code implies undetermined
df2$Author_gender[df2$Author_gender == "Unknown"] <- "Undetermined"
df2$Author_gender[df2$Author_gender == ""] <- "Undetermined"
df2$Author_gender[is.na(df2$Author_gender)] <- "Undetermined"

##Gender proportions per paper
df_g <- df2 %>% select(Article.ID, Author_gender,Author_order) %>% distinct() %>% filter(Author_gender != "Undetermined" & Author_gender != "Not found") %>% distinct()

id <- unique(df_g$Article.ID)
x <- c(1:n_distinct(df_g$Article.ID))
gp <- data.frame(Article.ID=integer(),Author_gender=character(),Author_order=integer(),Prop_authorship=numeric())

dat <- df_g %>% select(Article.ID, Author_order, Author_gender) %>% distinct()

for (i in x){
  y <- id[i]
  sub <- dat %>% filter(Article.ID == y) %>% distinct()
  z <- nrow(sub)
  sub[4] <- 1/z
  gp <- rbind(gp,sub)
}

colnames(gp) <- c("Article.ID","Author_order","Author_gender","Prop_authorship")
gp <- gp %>% select(-Author_gender) %>% distinct()
df2 <- left_join(df2,gp,by=c("Article.ID","Author_order")) %>% distinct()

#Clean marine realm data
realm <- df2 %>% select(Article.ID,Marine_realm) %>% distinct() %>% filter(Marine_realm != "")
realm$Marine_realm <- gsub(", ",",", realm$Marine_realm)
realm$Marine_realm <- as.character(realm$Marine_realm)
m <- strsplit(realm$Marine_realm, split=",")
m_df <- data.frame(ID=rep(realm$Article.ID, sapply(m, length)), id = unlist(m))
colnames(m_df) <- c("Article.ID","id")
m_df <- distinct(m_df)
m_df$id[m_df$id == "Caribbean"] <- "Tropical Atlantic"

df2 <- left_join(df2,m_df,by="Article.ID") %>% distinct()

df2$Prop_authorship <- as.numeric(df2$Prop_authorship)

##Author contributions per paper
df_dc <- df2 %>% select(Article.ID, Author_order) %>% distinct()
id <- unique(df_dc$Article.ID)
k <- c(1:n_distinct(df_dc$Article.ID))
divprop <- data.frame(Article.ID=integer(),Author_order=integer(),Prop_overall_authorship=numeric())

dat <- df_dc %>% select(Article.ID, Author_order) %>% distinct()
dat[3] <- c("")

for (i in k){
  y <- id[i]
  sub <- dat %>% filter(Article.ID == y) %>% distinct()
  z <- n_distinct(sub$Author_order)
  sub[3] <- 1/z
  divprop <- rbind(divprop,sub)
}

colnames(divprop) <- c("Article.ID","Author_order","Prop_overall_authorship")
df2 <- left_join(df2,divprop,by=c("Article.ID","Author_order")) %>% distinct()

##Fixing journal names
df2$Journal <- gsub("Plos One","PLOS ONE",df2$Journal)
df2$Journal <- gsub("PLoS One","PLOS ONE",df2$Journal)

##Assign regions

saveRDS(df2,file="final_data_for_analysis_02282020.rds")
