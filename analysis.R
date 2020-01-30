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

##  Preparing dataset
data <- read.csv("coralreef_gender_cleaned_final_2020.csv",header=TRUE)
data <- data %>% filter(is.na(Excluded)) %>% distinct()
data <- filter(data,!is.na(Pub_year))

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

##Summary statistics
n <- n_distinct(df2$Article.ID)
j <- unique(df2$Journal)
y <- c(min(df2$Pub_year):max(df2$Pub_year))
a <- unique(df2$Author)
c <- unique(df2$Country_affiliation)

##Growth in publications
growth <- df2 %>% select(Article.ID,Pub_year) %>% distinct()
g <- count(growth,Pub_year)

gg <- ggplot(g, aes(Pub_year,n)) + 
  geom_col(fill="steelblue") +
  scale_x_continuous(breaks=seq(2003,2019,1), name="Publication year") +
  scale_y_continuous(breaks=seq(0,200,50), name="Number of articles") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
  )
  
plot(gg)

##Geographical distribution of study countries
##Plot countries
#load in full country list
country <- read.csv("country_code_web.csv", head=TRUE, sep=",")
names(country) <- c("Study_country", "Code","Territory","Sovereignty")
country$Study_country <- as.character(country$Study_country)

##Count number of studies for all countries and arrange by region
country_count <- matrix(nrow=nrow(country), ncol=2)
rownames(country_count) <- country$Study_country
colnames(country_count) <- c("Study_country", "counts")

df_country <- df2 %>% select(Article.ID,Mapping_Country) %>% filter(!is.na(Mapping_Country)) %>% distinct()

#Calculate in for loop and write to blank matrix
for (c in country$Study_country){
  subset <- filter(df_country, Mapping_Country == c)
  country_count[c,1] <- c
  country_count[c,2] <- as.numeric(n_distinct(subset$Article.ID))
}
#Remove rownames and reformat data types
rownames(country_count) = NULL
country_count <- as.data.frame(country_count, stringsAsFactors=FALSE)
countries <- left_join(country,country_count,by="Study_country")

countries$counts <- as.numeric(countries$counts)
countries <- as.data.frame(countries)
countries_zero <- filter(countries, counts == 0)
countries <- filter(countries,counts != 0)

map <- readShapeSpatial("TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
map <- fortify(map, region="ISO3")

##Define custom breaks for choropleth map
min <- min(countries$counts) 
max <- max(countries$counts)
pretty_breaks <- c(5,25,50,100,250)
brks <- c(min,pretty_breaks,max)
#labels <- c("1-5","6-25","26-50","51-100","101-250","251-459")
labels <- c()
for(i in 1:length(brks)){
  labels <- c(labels,round(brks[i + 1], 2))
}
labels <- labels[1:length(labels)-1]

countries$brks2 <- cut(countries$counts,
                      breaks=brks,
                      include.lowest=TRUE,
                      labels=labels)

brks_scale <- levels(countries$brks2)
labels_scale <- brks_scale

cc <- ggplot() + 
  geom_map(data=countries, aes(map_id=Code, fill=brks2),map=map) + 
  geom_map(data=countries_zero, aes(map_id=Code),fill="#f0f0f0",map=map) + 
  expand_limits(x=map$long,y=map$lat) + 
  theme(panel.background = element_rect(fill = "grey50", colour = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_equal() +
  theme(legend.position = "bottom")


cd <- cc +
  scale_fill_manual(
    values=(brewer.pal(7, "BuGn")[2:7]),
    breaks=brks_scale,
    name="Number of articles",
    drop=FALSE,
    labels=labels_scale,
    guide=guide_legend(
      direction="horizontal",
      #keyheight= unit(2, units="mm"),
      #keywidth = unit(70/length(labels), units="mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 1,
      nrow=1,
      byrow=T,
      reverse=F,
      label.position="bottom"
      )
    )

plot(cd)

#scale_fill_gradient2(low="#deebf7",mid="#9ecae1",high="#08519c",midpoint=(max(countries$counts)/2),limits=c(0,max(countries$counts)))

#Summarize regions
#Plot marine realms
m_lyr <- readOGR(dsn="MEOW-TNC", layer="meow_ecos",verbose=FALSE)
m_lyr_fortified <- tidy(m_lyr,region="REALM")

##Plot countries
map_lyr <- readOGR(
  dsn="TM_WORLD_BORDERS-0.3",
  layer="TM_WORLD_BORDERS-0.3",
  verbose=FALSE
)

ggmap <- tidy(map_lyr,region="ISO3")

#Clean marine realm data
realm <- df2 %>% select(Article.ID,Marine_realm) %>% distinct() %>% filter(Marine_realm != "")
realm$Marine_realm <- gsub(", ",",", realm$Marine_realm)
realm$Marine_realm <- as.character(realm$Marine_realm)
m <- strsplit(realm$Marine_realm, split=",")
m_df <- data.frame(ID=rep(realm$Article.ID, sapply(m, length)), id = unlist(m))
colnames(m_df) <- c("Article.ID","id")
m_df <- distinct(m_df)

#Count studies per realm
marine_count <- count(m_df,id)
marine_count$id <- as.character(as.factor(marine_count$id))
marine_data <- m_lyr_fortified %>% left_join(marine_count, by="id")
marine_data$n[is.na(marine_data$n)] <- 0
marine_data_zero <- marine_data %>% filter(n == 0)
marine_data <- marine_data %>% filter(n != 0)

ggplot() + 
  geom_polygon(data=marine_data, aes(fill=n,x=long,y=lat,group=group)) +
  geom_polygon(data=marine_data_zero, aes(x=long,y=lat,group=group), fill="#7c7c7c") +
  geom_polygon(data=ggmap, aes(x=long,y=lat,group=group), fill="#ffffff") +
  scale_fill_gradient2(low="#deebf7",mid="#9ecae1",high="#08519c") +
  theme_minimal() +
  theme(
    #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#000000", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank()
  )

#Summarized in a table
marine_data_summary <- marine_data %>% select(id, n) %>% distinct() %>% arrange(desc(n))

###Types of studies
##What study types are encountered?
type <- df2 %>% select(Article.ID,Study_Type) %>% distinct()
type_count <- count(type,Study_Type) %>% arrange(desc(n))

st <- ggplot(type_count, aes(Study_Type,n)) + 
  geom_col(fill="steelblue") +
  scale_y_continuous(breaks=seq(0,1100,100), name="Number of articles") +
  xlab("Study type") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
  )

plot(st)

##What study topics are encountered?
#Create corpus of titles and abstracts
ta <- df2 %>% select(Article.ID,Title,Abstract) %>% distinct() %>% unite("Text",c("Title","Abstract"),sep="; ")
text <- ta$Text
docs <- Corpus(VectorSource(text))
#Clean corpus
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("coral","corals","reefs","reef"))

#Create document-term-matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
wc <- data.frame(word = names(words),freq=words)

#Generate word cloud
set.seed(1234) 
wordcloud(words = wc$word, freq = wc$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"), scale=c(2.5,0.25))

##Characteristics of authors
##Cleaning data
gender <- df2 %>% select(-Done,-Excluded,-Reviewer,-Title,-Keywords,-Abstract,-DOI,-Affiliation,-Study_type,-Study.Country) %>% distinct()
gender$Author_gender <- gsub("FEmale","Female",gender$Author_gender)
gender$Author_gender <- gsub("Female ","Female",gender$Author_gender)
gender$Author_gender <- gsub("Male ","Male",gender$Author_gender)

#Assuming that unknown code implies undetermined
gender$Author_gender[gender$Author_gender == "Unknown"] <- "Undetermined"
gender$Author_gender[gender$Author_gender == ""] <- "Undetermined"
gender$Author_gender[is.na(gender$Author_gender)] <- "Undetermined"

#Percent authors successfully identified and summary stats
authors <- gender %>% select(Author_gender,Author) %>% distinct()

n_a <- n_distinct(authors$Author)
gender_stats <- count(authors,Author_gender)

#Checking where undetermined authors are from (potential country bias)
gender_undet <- gender %>% filter(Author_gender == "Undetermined" | Author_gender == "Not found") %>% distinct()
country_bias <- gender_undet %>% select(Article.ID,Author_gender,Country_affiliation) %>% distinct()
cb <- count(country_bias,Country_affiliation,Author_gender)

##Gender proportions per paper and aggregate
gender_prop <- gender %>% select(Article.ID, Author_gender,Author_order) %>% distinct() %>% filter(Author_gender != "Undetermined" & Author_gender != "Not found") %>% distinct()

id <- unique(gender_prop$Article.ID)
x <- c(1:n_distinct(gender_prop$Article.ID))
gp <- data.frame(Article.ID=integer(),Author_gender=character(),Author_order=integer(),Prop_authorship=numeric())

dat <- gender_prop %>% select(Article.ID, Author_order, Author_gender) %>% distinct()

for (i in x){
  y <- id[i]
  sub <- dat %>% filter(Article.ID == y) %>% distinct()
  z <- nrow(sub)
  sub[4] <- round(1/z, digits=2)
  gp <- rbind(gp,sub)
}

colnames(gp) <- c("Article.ID","Author_order","Author_gender","Prop_authorship")
gender2 <- left_join(gender,gp,by=c("Article.ID","Author_order"))

#Percent male vs. female vs. other authors
gs_plot <- filter(gender_stats,Author_gender != "Not found")
gs <- ggplot(gs_plot, aes(x="",y=n,fill=Author_gender)) +
  geom_bar(width=1, stat="identity") +
  scale_fill_brewer(palette="Accent") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position='bottom') +
  geom_text(aes(y=n, label=percent(n/n_a)), size=3, position=position_stack(vjust=0.5)) +
  ylab("Number of authors") +
  guides(fill=guide_legend(title="Gender"))
  

plot(gs)

##Gender analysis

#Percent first and last authors and other position, male vs. female vs. other
ratio <- gender %>% select(Article.ID,Author_gender,First_last) %>% distinct()
x <- n_distinct(ratio$Article.ID)
rg <- count(ratio,Author_gender,First_last) %>% filter(First_last != "other")

ggplot(rg, aes(x=First_last,y=n)) +
  geom_bar(aes(fill=Author_gender), stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Accent") +
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    legend.position='right') +
  ylab("Number of articles") +
  xlab("Author position") +
  guides(fill=guide_legend(title="Gender"))

#Change over time
gender_time <- gender %>% select(Article.ID,Author_gender,First_last,Pub_year) %>% filter(First_last != "other") %>% filter(Author_gender != "Undetermined") %>% filter(Author_gender != "Not found") %>% distinct() %>% group_by(Pub_year,First_last)
gt <- count(gender_time,Author_gender,First_last)

ggplot(gt, aes(Pub_year,n)) + 
  geom_col(aes(fill=Author_gender)) +
  facet_grid(rows=vars(First_last), scales="free") +
  scale_x_continuous(breaks=seq(2003,2019,1), name="Publication year") +
  scale_y_continuous(breaks=seq(0,200,50), name="Number of articles") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
  ) +
  theme(
    legend.position='bottom') +
  ylab("Number of articles") +
  xlab("Author position") +
  scale_fill_brewer(palette = "Accent") +
  guides(fill=guide_legend(title="Gender"),
         guide=guide_legend(
           direction="horizontal",
           #keyheight= unit(2, units="mm"),
           #keywidth = unit(70/length(labels), units="mm"),
           title.position = "top",
           title.hjust = 0.5,
           label.hjust = 1,
           label.position="bottom"
         ))

##Affiliation analysis

#How often first or last author was based in study country

#Percent first, last, and other author orders that were based in study country

#How often anyone was based in the study country

#For each country, those who were based in the country, what author position they tended to occupy

##Co-authorship network

#Who is working with whom? What defines these clusters?

