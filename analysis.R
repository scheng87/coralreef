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
library(geosphere)

#Read in data
df2 <- readRDS("final_data_for_analysis_03022020.rds")

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
country <- read.csv("country_code_web.csv", head=TRUE, sep=",",stringsAsFactors = FALSE)
names(country) <- c("Study_country", "Code","Territory","Sovereignty","Latitude","Longitude","Reef_area","Region","Income")
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

##Overall gender composition in author pool
#Percent authors successfully identified
authors <- gender %>% select(Author_gender,Author) %>% distinct()
n_a <- n_distinct(authors$Author)
gender_stats <- count(authors,Author_gender)
gs_plot <- filter(gender_stats,Author_gender != "Not found")

#Checking where undetermined authors are from (potential country bias)
gender_undet <- df2 %>% filter(Author_gender == "Undetermined" | Author_gender == "Not found") %>% distinct() %>% select(Article.ID,Author_gender,Mapping_affiliation) %>% distinct()
cb <- count(gender_undet,Mapping_affiliation,Author_gender)

#Plotting gender distribution of author pool
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


##Summarize gender over different variables
##Remove undetermined and not found entries
gender_mf <- df2 %>% filter(Author_gender != "Undetermined" & Author_gender != "Not found")

##Over time
#Proportion (overall male vs. female contribution)
gp_year <- gender_mf %>% select(Article.ID,Author_gender,Author,Pub_year,Prop_authorship) %>% distinct()
gpy <- aggregate(gp_year$Prop_authorship, by=list(Year=gp_year$Pub_year,Gender=gp_year$Author_gender), FUN=sum)

#Author position
gc_year <- gender_mf %>% select(Article.ID,Author_gender,First_last,Pub_year) %>% filter(First_last != "other") %>% distinct() %>% group_by(Pub_year,First_last)
gcy <- count(gc_year,Pub_year,Author_gender,First_last)

##Plotting
ggplot(gcy, aes(Pub_year,n)) + 
  geom_col(aes(fill=Author_gender),position=position_dodge()) +
  facet_grid(rows=vars(First_last), scales="free") +
  scale_x_continuous(breaks=seq(2003,2019,1), name="Publication year") +
  ylab("Number of articles") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
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

ggplot(gpy, aes(Year,x)) + 
  geom_col(aes(fill=Gender),position=position_dodge()) +
  scale_x_continuous(breaks=seq(2003,2019,1), name="Publication year") +
  scale_y_continuous(breaks=seq(0,120,10), name="Proportion of published articles") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
  ) +
  theme(
    legend.position='bottom') +
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

##By Country of Affiliation
#Proportion (overall male vs. female contribution)
gp_affil <- gender_mf %>% select(Article.ID,Author_gender,Author,Mapping_affiliation,Prop_authorship) %>% distinct()
gpa <- aggregate(gp_affil$Prop_authorship, by=list(Affiliation=gp_affil$Mapping_affiliation,Gender=gp_affil$Author_gender), FUN=sum) %>% mutate(Affiliation = fct_reorder(Affiliation, desc(x)))
gpa1 <- gpa %>% filter(Affiliation == "Australia" | Affiliation == "United States")
gpa2 <- gpa %>% filter(Affiliation != "Australia") %>% filter(Affiliation != "United States")

#Author position
gc_affil <- gender_mf %>% select(Article.ID,Author_gender,First_last,Mapping_affiliation) %>% filter(First_last != "other") %>% distinct()
gca <- count(gc_affil,Mapping_affiliation, Author_gender,First_last) %>% mutate(Mapping_affiliation = fct_reorder(Mapping_affiliation, desc(n)))
gca1 <- gca %>% filter(Mapping_affiliation == "Australia" | Mapping_affiliation == "United States")
gca2 <- gca %>% filter(Mapping_affiliation != "Australia") %>% filter(Mapping_affiliation != "United States")

##Plotting
a <- ggplot(gca1, aes(Mapping_affiliation,n)) + 
  geom_col(aes(fill=Author_gender),position=position_dodge())+
  facet_grid(rows=vars(First_last), scales="free") +
  scale_fill_brewer(palette = "Accent") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    axis.title.x=element_blank(),
    legend.position='bottom') +
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

b <- ggplot(gpa1, aes(Affiliation,x)) + 
  geom_col(aes(fill=Gender)) +
  scale_y_continuous(breaks=seq(0,800,50)) +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    axis.title.x=element_blank(),
    legend.position='bottom') +
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

c <- ggplot(gca2, aes(Mapping_affiliation,n)) + 
  geom_col(aes(fill=Author_gender),position=position_dodge())+
  facet_grid(rows=vars(First_last), scales="free") +
  scale_fill_brewer(palette = "Accent") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    axis.title.x=element_blank(),
    legend.position='bottom') +
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

d <- ggplot(gpa2, aes(Affiliation,x)) + 
  geom_col(aes(fill=Gender)) +
  scale_y_continuous(breaks=seq(0,800,10)) +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    axis.title.x=element_blank(),
    legend.position='bottom') +
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

fig_affil <- ggarrange(a,b,c,d,
          labels=c("A","B","C","D"),
          ncol=2, nrow=2,
          common.legend=TRUE,
          legend="bottom")

annotate_figure(fig_affil,
                left=text_grob("Number of articles", color="black",rot=90),
                bottom=text_grob("Country of affiliation", color="black"))

##By Marine Realm of Study
#Proportion (overall male vs. female contribution)
gp_marine <- gender_mf %>% select(Article.ID,Author_gender,Author,id,Prop_authorship) %>% filter(!is.na(id)) %>% filter(id != "Global") %>% filter(id != "Freshwater") %>% distinct()
gpm <- aggregate(gp_marine$Prop_authorship, by=list(Realm=gp_marine$id,Gender=gp_marine$Author_gender), FUN=sum) %>% mutate(Realm = fct_reorder(Realm, desc(x)))

#Author position
gc_marine <- gender_mf %>% select(Article.ID,Author_gender,First_last,id) %>% filter(First_last != "other") %>% filter(!is.na(id)) %>% filter(id != "Global") %>% filter(id != "Freshwater") %>% distinct()
gcm <- count(gc_marine,id,Author_gender,First_last) %>% mutate(id = fct_reorder(id, desc(n)))

##Plotting
ggplot(gcm, aes(id,n)) + 
  geom_col(aes(fill=Author_gender),position=position_dodge()) +
  facet_grid(rows=vars(First_last), scales="free") +
  ylab("Number of articles") +
  xlab("Marine realm studied") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
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

ggplot(gpm, aes(Realm,x)) + 
  geom_col(aes(fill=Gender)) +
  scale_y_continuous(breaks=seq(0,800,50), name="Proportion of published articles") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
  xlab("Marine realm studied") +
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

##By study type
#Proportion (overall male vs. female contribution)
gp_study <- gender_mf %>% select(Article.ID,Author_gender,Author,Study_Type,Prop_authorship) %>% distinct()
gps <- aggregate(gp_study$Prop_authorship, by=list(Study_Type=gp_study$Study_Type,Gender=gp_study$Author_gender), FUN=sum) %>% mutate(Study_Type = fct_reorder(Study_Type, desc(x)))

#Author position
gc_study <- gender_mf %>% select(Article.ID,Author_gender,First_last,Study_Type) %>% filter(First_last != "other") %>% distinct()
gcs <- count(gc_study,Study_Type,Author_gender,First_last)
gcs <- rbind(gcs,c("Editorials/Correspondence","Female","single",0))
gcs <- rbind(gcs,c("Synthesis","Female","single",0))
gcs <- rbind(gcs,c("Undetermined","Female","single",0))
gcs$n <- as.integer(gcs$n)
gcs <- gcs %>% mutate(Study_Type = fct_reorder(Study_Type, desc(n)))

##Plotting
ggplot(gcs, aes(Study_Type,n)) + 
  geom_col(aes(fill=Author_gender),position=position_dodge()) +
  facet_grid(rows=vars(First_last), scales="free") +
  ylab("Number of articles") +
  xlab("Study type") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
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

ggplot(gps, aes(Study_Type,x)) + 
  geom_col(aes(fill=Gender)) +
  scale_y_continuous(breaks=seq(0,1100,50), name="Proportion of published articles") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
  xlab("Study type") +
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

##By Journal
#Proportion (overall male vs. female contribution)
gender_mf$Journal[gender_mf$Journal == "PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA"] <- "PNAS"
gender_mf$Journal[gender_mf$Journal == "PROCEEDINGS OF THE ROYAL SOCIETY B-BIOLOGICAL SCIENCES"] <- "ProcB"
gender_mf$Journal[gender_mf$Journal == "MARINE ECOLOGY PROGRESS SERIES"] <- "MEPS"
gender_mf$Journal[gender_mf$Journal == "Plos One"] <- "PLoS One"

gp_journal <- gender_mf %>% select(Article.ID,Author_gender,Author,Journal,Prop_authorship) %>% distinct()
gpj <- aggregate(gp_journal$Prop_authorship, by=list(Journal=gp_journal$Journal,Gender=gp_journal$Author_gender), FUN=sum) %>% mutate(Journal = fct_reorder(Journal, desc(x)))

#Author position
gc_journal <- gender_mf %>% select(Article.ID,Author_gender,First_last,Journal) %>% filter(First_last != "other") %>% distinct()
gcj <- count(gc_journal,Journal,Author_gender,First_last) %>% mutate(Journal = fct_reorder(Journal, desc(n)))

##Plotting
ggplot(gcj, aes(Journal,n)) + 
  geom_col(aes(fill=Author_gender),position=position_dodge()) +
  facet_grid(rows=vars(First_last), scales="free") +
  ylab("Number of articles") +
  xlab("Journal") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
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

ggplot(gpj, aes(Journal,x)) + 
  geom_col(aes(fill=Gender)) +
  scale_y_continuous(breaks=seq(0,1100,50), name="Proportion of published articles") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
  xlab("Journal") +
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

##Times Cited
#Proportion (overall male vs. female contribution)
gp_cite <- gender_mf %>% select(Article.ID,Author_gender,Author,Prop_authorship) %>% distinct()
gpc <- aggregate(gp_cite$Prop_authorship, by=list(Article.ID=gp_cite$Article.ID,Gender=gp_cite$Author_gender), FUN=sum) %>% arrange(Article.ID)
cite <- gender_mf %>% select(Article.ID,Times.Cited) %>% distinct() 
gpc <- left_join(gpc,cite,by="Article.ID")
gpc <- gpc %>% filter(Gender != "Male")

#Author position
gc_cite <- gender_mf %>% select(Article.ID,Author_gender,First_last,Times.Cited) %>% filter(First_last != "other") %>% distinct()
gcs <- count(gc_cite,Times.Cited,Author_gender,First_last)

##Plotting
ggplot(gpc, aes(x=x, y=Times.Cited)) +
  geom_point(size=1.5, color="#7FC97F", alpha=0.6,stroke=0.5) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,2750,250)) +
  xlab("Proportion of female authorship (per article)") +
  ylab("Number of citations")

##Affiliation analysis
diversity <- df2 %>% select(Article.ID,Author,Author_order,First_last,Author_gender,Author.Based.In.Country,Author.Based.In.Sovereign,Mapping_Country,Mapping_affiliation,Prop_overall_authorship) %>% distinct()
diversity$Author.Based.In.Country <- as.character(diversity$Author.Based.In.Country)
diversity$Author.Based.In.Sovereign <- as.character(diversity$Author.Based.In.Sovereign)
diversity$Author.Based.In.Country <- gsub("^No ","No",diversity$Author.Based.In.Country)
diversity$Author.Based.In.Country <- gsub("yes","Yes",diversity$Author.Based.In.Country)
diversity$Author.Based.In.Country[is.na(diversity$Author.Based.In.Country)] <- "Undetermined"
diversity$Author.Based.In.Country[diversity$Author.Based.In.Country == ""] <- "Undetermined"

#How often first or last author was based in study country
da <- diversity %>% select(Article.ID,First_last,Author.Based.In.Country) %>% distinct() %>% filter(First_last != "other") %>% distinct()
dac <- count(da,First_last,Author.Based.In.Country)

##Number of single country publications vs. multiple country publications
dna <- diversity %>% select(Article.ID,Author,First_last,Mapping_affiliation,Author_gender) %>% distinct()
dna$num_aff_coun <- c("")
dna$single <- c("")

p <- c(1:n)
id <- unique(dna$Article.ID)
for (x in p){
  i <- id[x]
  sub <- filter(dna, Article.ID == i) %>% distinct()
  dna$num_aff_coun[dna$Article.ID == i] <- n_distinct(sub$Mapping_affiliation)
  if (n_distinct(sub$Mapping_affiliation) == 1){
    dna$single[dna$Article.ID == i] <- c("Yes")
  } else
    dna$single[dna$Article.ID == i] <- c("No")
}

dna_plot <- dna %>% select(Article.ID,single) %>% distinct() %>% count(single)

p1 <- ggplot(dna_plot,aes(x="",y=n,fill=single)) +
  geom_col(width=1) +
  ylab("Number of articles") +
  xlab("") +
  scale_fill_brewer(palette = "Accent",labels=c("International","National")) +
  guides(fill=guide_legend(title="Country authorship"),
         guide=guide_legend(
           direction="horizontal",
           #keyheight= unit(2, units="mm"),
           #keywidth = unit(70/length(labels), units="mm"),
           title.position = "top",
           title.hjust = 0.5,
           label.hjust = 1,
           label.position="bottom"
         ))
  
  p1 + coord_flip()
  
#Proportion of male and female first, last uthors who involved in an international/multiple country collaboration
  
dna_plot2 <- dna %>% select(Article.ID,Mapping_affiliation,single,First_last,Author_gender) %>% distinct() %>% count(single,First_last,Author_gender) %>% filter(First_last != "other") %>% filter(First_last != "single") %>% filter(Author_gender != "Not found") %>% filter(Author_gender != "Undetermined")

p2 <- ggplot(dna_plot2, aes(x=Author_gender,y=n,fill=single)) + 
  geom_col(position=position_dodge()) +
  facet_grid(rows=vars(First_last), scales="free") +
  ylab("Number of articles") +
  xlab("Gender") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
  scale_fill_brewer(palette = "Accent",labels=c("International","National")) +
  guides(fill=guide_legend(title=""),
         guide=guide_legend(
           direction="horizontal",
           #keyheight= unit(2, units="mm"),
           #keywidth = unit(70/length(labels), units="mm"),
           title.position = "top",
           title.hjust = 0.5,
           label.hjust = 1,
           label.position="bottom"
         ))

p2 + coord_flip()

#Proportion of author team  and author position from study country
#By filtering for only yes in country, should eliminate any authors who have multiple affiliations that are causing a double count in the statistics. Out of country affiliation is dropped (however, still important to see this?)
dp <- diversity %>% select(Article.ID,Author.Based.In.Country,Author,Prop_overall_authorship) %>% distinct()
dpp <- aggregate(dp$Prop_overall_authorship, by=list(Article.ID=dp$Article.ID,InCountry=dp$Author.Based.In.Country), FUN=sum) %>% arrange(Article.ID)
dppp <- spread(dpp,InCountry,x)
dppp[is.na(dppp)] <- 0

dpp_c <- count(dppp,Yes)
colnames(dpp_c) <- c("Prop.Based.In.Country","n")
ggplot(dpp_c, aes(x=Prop.Based.In.Country, y=n)) +
  geom_point(size=1.5, color="#7FC97F", alpha=0.6,stroke=0.5) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,1000,100)) +
  xlab("Proportion of author team based in study country") +
  ylab("Number of articles")

ggplot(dac, aes(First_last,n)) + 
  geom_col(aes(fill=Author.Based.In.Country),position=position_dodge()) +
  ylab("Number of articles") +
  xlab("Author position") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1),
    legend.position='bottom') +
  scale_fill_brewer(palette = "Accent") +
  guides(fill=guide_legend(title="Is the author based in the study country?"),
         guide=guide_legend(
           direction="horizontal",
           #keyheight= unit(2, units="mm"),
           #keywidth = unit(70/length(labels), units="mm"),
           title.position = "top",
           title.hjust = 0.5,
           label.hjust = 1,
           label.position="bottom"
         ))

##Co-authorship network

#Who is working with whom?

#For each country, where are all authors from

kk <- df2 %>% select(Article.ID,Author,Mapping_affiliation,Mapping_Country,Study_region) %>% distinct()
kk_non <- kk %>% filter(is.na(Mapping_Country))
kk2 <- kk %>% filter(!is.na(Mapping_Country)) %>% filter(!is.na(Mapping_affiliation)) %>% select(-Author) %>% count(Mapping_affiliation,Mapping_Country)

kk2 <- kk2 %>% left_join()
#Checking typos and format
# l <- full_join(kk2,country,by=c("Mapping_Country" = "Study_country")) %>% filter(is.na(Latitude))
# k <- full_join(kk2,country,by=c("Mapping_affiliation"="Study_country")) %>% filter(is.na(Latitude))

#map("world",col='#1A1A1A',fill=TRUE,bg="black",lwd=0.001)

#pal <- colorRampPalette(c("#333333","white","#1292db"))
#colors <- pal(100)

# Download NASA night lights image
download.file("https://www.nasa.gov/specials/blackmarble/2016/globalmaps/BlackMarble_2016_01deg.jpg", 
              destfile = "BlackMarble_2016_01deg.jpg", mode = "wb")
# Load picture and render
earth <- readJPEG("BlackMarble_2016_01deg.jpg", native = TRUE)
earth <- rasterGrob(earth, interpolate = TRUE)

#Summarize dataset
kk2_sub <-kk2[order(kk2$n),]
max <- max(kk2_sub$n)

#Take subset of country data
coord <- select(country,Study_country,Latitude,Longitude,Region)

#Connect latitude and longitudes
data_plot <- kk2_sub %>% left_join(coord,by=c("Mapping_affiliation"="Study_country"))
colnames(data_plot) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent")
data_plot <- data_plot %>% left_join(coord,by=c("Mapping_Country"="Study_country"))
colnames(data_plot) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent","travellat","travellon","travelcontinent")

#Create two dataframes - 1) for different affil/study locations and one for the same
foreign=as.data.frame(matrix(nrow=1,ncol=9))
colnames(foreign) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent","travellat","travellon","travelcontinent")
domestic=as.data.frame(matrix(nrow=1,ncol=9))
colnames(domestic) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent","travellat","travellon","travelcontinent")

for(i in c(1:nrow(data_plot))){
  if(data_plot$Mapping_affiliation[i] == data_plot$Mapping_Country[i]){
    tmp <- as.data.frame(data_plot[i,])
    domestic <- bind_rows(domestic, tmp)
  } else if(data_plot$Mapping_affiliation[i] != data_plot$Mapping_Country[i]){
    tmp <- as.data.frame(data_plot[i,])
    foreign <- bind_rows(foreign,tmp)
  }
}

summary <- foreign %>% select(-Mapping_affiliation,-Mapping_Country)
summary <- arrange(summary,n)
summary <- summary[c(2,3,4,5,6,7,1)]
summary <- summary %>% filter(!is.na(homelon))

domestic <- domestic %>% select(-Mapping_affiliation,-Mapping_Country)
domestic <- arrange(domestic,desc(n))
domestic <- domestic[c(2,3,4,5,6,7,1)]
domestic <- domestic %>% filter(!is.na(homelon))

# A function that makes a dateframe per connection (we will use these connections to plot each lines)
data_for_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, group){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  inter$group=NA
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
    inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
  }else{
    inter$group=as.character(paste(group))
  }
  return(inter)
}

# Complete dataframe for plotting
data_ready_plot=data.frame()
for(i in c(1:nrow(summary))){
  tmp=data_for_connection(summary$homelon[i], summary$homelat[i], summary$travellon[i], summary$travellat[i], i)
  tmp$homecontinent=summary$homecontinent[i]
  tmp$n=summary$n[i]
  data_ready_plot=bind_rows(data_ready_plot, tmp)
}
data_ready_plot$homecontinent=factor(data_ready_plot$homecontinent, levels=c("East Asia and Pacific","Europe and Central Asia","Latin America and the Caribbean","Middle East and North Africa","North America","South Asia","Sub-Saharan Africa"))
#data_ready_plot$homecontinent=factor(data_ready_plot$homecontinent, levels=c("Low-Income","Lower-Middle Income","Upper-Middle Income","High-Income","Not categorized"))

# Plot

worldmap <- map_data ("world")

mybreaks <- c(1,10,50,100,500,1500)
mybreaks2 <- c(1,50,150)
p <- ggplot(worldmap) +
  geom_map(data=worldmap,map=worldmap,aes(x=long,y=lat,map_id=region),col="gray25",fill="black") +
  #annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
  geom_line(data=data_ready_plot, aes(x=lon, y=lat, group=group, colour=homecontinent, alpha=n), size=0.7) +
  geom_point(data=domestic,aes(x=homelon,y=homelat,size=n,colour=homecontinent),alpha=0.6) +
  scale_size_continuous(name="Number of authors working in own country",range=c(1,12),breaks=mybreaks) +
  scale_alpha_continuous(name="Number of authors working in another country",range=c(0.2,1),breaks=mybreaks2) +
  scale_color_brewer(palette="Accent") +
  theme_void() +
  labs(color="Affiliation of researcher",alpha="Number of authors working in another country") +
  theme(
    panel.background=element_rect(fill="black","colour"="black"),
    legend.text = element_text(color="black",size=17),
    legend.margin=margin(0.2,0.2,0.2,0.2,"cm"),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.background=element_rect(fill="white",linetype="solid",color="gray25"),
    legend.title=element_text(color="black",size=20,face="bold"),
    legend.justification="center"
    #panel.background = element_rect(fill = "black", colour = "black"), 
    #panel.spacing=unit(c(0,0,0,0), "null"),
    #plot.margin=grid::unit(c(0,0,0,0), "cm"),
  ) +
  guides(linetype = guide_legend(override.aes = list(size = 2))) +
  ggplot2::annotate("text", x = -175, y = 80, hjust = 0, size = 11, label = paste("Where coral reef scientists do research"), color = "white") +
  ggplot2::annotate("text", x = -175, y = 70, hjust = 0, size = 8, label = paste("Each line represents the contribution of authors from one country to research in another | Increasingly opaque lines represent higher numbers of authors"), color = "white", alpha = 0.5) +
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 

# Save at PNG
ggsave("WherePeopleDoResearch_byRegion.png", width = 36, height = 15.22, units = "in", dpi = 90)

#Version two with income
#For territories, what proportion of authors are from the territory vs. the sovereign nation?



