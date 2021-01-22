##Final figures for paper (11/28/2020)

##processing data for analysis

setwd("~/Documents/github/coralreef/")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
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
library(reshape)
library(igraph)
library(networkD3)

Matrix2Edge <- function(x){
  final <- data.frame("a"=character(),"b"=character()) #an empty dataframe to add the edge list to
  
  for (i in 1:(ncol(x)-1)){
    id = paste("V", i, sep="")
    edge_list <- melt(x, id = (id))
    x[id] <- NULL
    keeps <- c(id, "value")
    edge_list <- edge_list[keeps]
    colnames(edge_list) <- colnames(final)
    final <- rbind(final,edge_list)
  }
  final <- final[!(final$b=="" | final$b==" "),]
  return(final)
}

#Read in data
df2 <- readRDS("final_data_for_analysis_12172020.rds")
df2 <- filter(df2, Pub_year != 2019)

#load in full country list
country <- read.csv("country_code_web.csv", head=TRUE, sep=",",stringsAsFactors = FALSE)
names(country) <- c("Study_country", "Code","Territory","Sovereignty","Latitude","Longitude","Reef_area","Region","Income","Global")
country$Study_country <- as.character(country$Study_country)

## SECTION 1 - Summary/overview
n <- n_distinct(df2$Article.ID)
j <- unique(df2$Journal)
y <- c(min(df2$Pub_year):max(df2$Pub_year))
a <- unique(df2$Author)
c <- unique(df2$Mapping_affiliation)

#Percent authors successfully identified
authors <- df2 %>% select(Author_gender,Author) %>% distinct()
n_a <- n_distinct(authors$Author)
gender_stats <- count(authors,Author_gender)
gs_plot <- filter(gender_stats,Author_gender != "Not found")

#Checking where undetermined authors are from (potential country bias)
gender_undet <- df2 %>% filter(Author_gender == "Undetermined" | Author_gender == "Not found") %>% distinct() %>% select(Article.ID,Author_gender,Mapping_affiliation) %>% distinct()
cb <- count(gender_undet,Mapping_affiliation)

#Affiliations
af <- df2 %>% select(Author,Mapping_affiliation) %>% distinct()
a.f <- count(af,Mapping_affiliation) %>% arrange(desc(n))
pp <- filter(af,Mapping_affiliation == "United States" | Mapping_affiliation == "Australia" | Mapping_affiliation == "United Kingdom" | Mapping_affiliation == "France" | Mapping_affiliation == "Germany" | Mapping_affiliation == "Japan") %>% distinct()
n_distinct(pp$Author)

#Types of studies
st <- df2 %>% select(Article.ID,Study_Type) %>% distinct()
s.t <- count(st,Study_Type)

##FIGURE 1 - Plotting growth in studies through author contribution from different countries
qq <- df2 %>% select(Mapping_affiliation,Pub_year)
q2 <- count(qq,Mapping_affiliation,Pub_year)

q2.2 <- q2 %>% 
  arrange(Mapping_affiliation,Pub_year) %>%
  group_by(Mapping_affiliation) %>%
  mutate("total" = cumsum(n)) %>% 
  filter(Mapping_affiliation == "United States" | Mapping_affiliation == "Australia" | Mapping_affiliation == "United Kingdom" | Mapping_affiliation == "France")

pdf("Fig1_us+aus+uk+france.pdf",height=5,width=11)
ggplot(data=q2.2, aes(x=Pub_year,y=total,group=Mapping_affiliation,color=Mapping_affiliation)) +
  geom_line() +
  theme (legend.position="bottom") +
  scale_x_continuous(breaks=seq(2003,2018,1)) +
  geom_label(aes(label = Mapping_affiliation,x=2018), nudge_x = 0.35, size = 4) +
  xlab("Publication year") + 
  ylab("Total articles") +
  guides(fill=guide_legend(title="Country of affiliation"))
dev.off()

q2.1 <- q2 %>%
  arrange(Mapping_affiliation,Pub_year) %>%
  group_by(Mapping_affiliation) %>%
  mutate("total" = cumsum(n)) %>%
  filter(Mapping_affiliation == "Spain" | Mapping_affiliation == "Netherlands" | Mapping_affiliation == "Germany" | Mapping_affiliation == "Italy" | Mapping_affiliation == "Sweden" | Mapping_affiliation == "Israel" | Mapping_affiliation == "Saudi Arabia" | Mapping_affiliation == "Indonesia" | Mapping_affiliation =="Philippines" | Mapping_affiliation == "Japan" | Mapping_affiliation == "New Caledonia" | Mapping_affiliation == "New Zealand" | Mapping_affiliation == "French Polynesia" | Mapping_affiliation == "Canada" | Mapping_affiliation == "Mexico" | Mapping_affiliation == "Panama" | Mapping_affiliation == "Brazil")

pdf("Fig1_rest.pdf",height=8,width=11)
ggplot(data=q2.1, aes(x=Pub_year,y=total,group=Mapping_affiliation,color=Mapping_affiliation)) +
  geom_line() +
  theme (legend.position="bottom") +
  scale_x_continuous(breaks=seq(2003,2018,1)) +
  geom_label(aes(label = Mapping_affiliation,x=2018), nudge_x = 0.35, size = 4) +
  xlab("Publication year") + 
  ylab("Total articles") +
  guides(fill=guide_legend(title="Country of affiliation"))
dev.off()


##Section 2 - Diversity of authors

#Total number of articles per year
apy <- df2 %>% select(Article.ID,Pub_year) %>% distinct %>% count(Pub_year)

##Gender analysis

##Summarize gender over different variables
##Remove undetermined and not found entries
gender_mf <- df2 %>% filter(Author_gender != "Undetermined" & Author_gender != "Not found")

##Over time
#Proportion (overall male vs. female contribution)
gp_year <- gender_mf %>% select(Article.ID,Author_gender,Author,Pub_year,Prop_authorship) %>% distinct()
gpy <- aggregate(gp_year$Prop_authorship, by=list(Year=gp_year$Pub_year,Gender=gp_year$Author_gender), FUN=sum)
gpy$First_last <- c("overall")

#Author position
gc_year <- gender_mf %>% select(Article.ID,Author_gender,First_last,Pub_year) %>% filter(First_last != "other" & First_last != "single") %>% distinct() %>% group_by(Pub_year,First_last)
gcy <- count(gc_year,Pub_year,Author_gender,First_last)

#Combine datasets
gppy <- gpy %>% select(Year,First_last,Gender,x)
colnames(gcy) <- c("Year","First_last","Gender","x")

dat <- bind_rows(gppy,gcy)

##Plotting
# pdf(file="Figure1_gender_over_time.pdf",height=6,width=11)
# ggplot(gpy, aes(Year,x)) + 
#   geom_col(aes(fill=Gender),position="fill") +
#   scale_x_continuous(breaks=seq(2003,2018,1), name="Publication year") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1)
#   ) +
#   theme(
#     legend.position='bottom') +
#   ylab("Percent of published articles") +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# dev.off()

pdf(file="Figure2_gender_diversity_time.pdf",height=6, width=8)
dat %>% 
  mutate(First_last=factor(First_last, levels=c("overall","first","last"))) %>%
  ggplot( aes(Year,x)) + 
    geom_col(aes(fill=Gender),position="fill") +
    facet_grid(rows=vars(First_last), scales="free") +
    scale_x_continuous(breaks=seq(2003,2018,1), name="Publication year") +
    ylab("Percent of published articles") +
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
dev.off()

##By Country of Affiliation
#Author position
gp_affil <- gender_mf %>% select(Article.ID,Author_gender,Author,Mapping_affiliation,Prop_authorship) %>% distinct()

gc_affil <- gender_mf %>% select(Article.ID,Author_gender,First_last,Mapping_affiliation) %>% filter(First_last != "other") %>% distinct()
gca <- count(gc_affil,Mapping_affiliation, Author_gender,First_last) %>% mutate(Mapping_affiliation = fct_reorder(Mapping_affiliation, desc(n)))
#gca1 <- gca %>% filter(Mapping_affiliation == "Australia" | Mapping_affiliation == "United States")
gca2 <- gca %>% filter(Mapping_affiliation != "Australia") %>% filter(Mapping_affiliation != "United States") %>% filter(n > 9)

#Adjusting country list for proportion to reflect only countries whos authors have contributed to at least 20 papers in last 15 years
l <- gp_affil %>% select(Article.ID,Mapping_affiliation) %>% distinct()
ll <- l %>% count(Mapping_affiliation) %>% filter (n >19)

gp_affil2 <- left_join(ll,gp_affil,by=c("Mapping_affiliation"))

gpa <- aggregate(gp_affil2$Prop_authorship, by=list(Affiliation=gp_affil2$Mapping_affiliation,Gender=gp_affil2$Author_gender), FUN=sum) %>% mutate(Affiliation = fct_reorder(Affiliation, desc(x)))
#gpa1 <- gpa %>% filter(Affiliation == "Australia" | Affiliation == "United States")
gpa2 <- gpa %>% filter(Affiliation != "Australia") %>% filter(Affiliation != "United States")

##Plotting
# a <- ggplot(gca, aes(Mapping_affiliation,n)) + 
#   geom_col(aes(fill=Author_gender),position=position_dodge())+
#   facet_grid(rows=vars(First_last), scales="free") +
#   scale_fill_brewer(palette = "Accent") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     axis.title.x=element_blank(),
#     legend.position='bottom') +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# b <- ggplot(gpa, aes(Affiliation,x)) + 
#   geom_col(aes(fill=Gender)) +
#   scale_y_continuous(breaks=seq(0,800,50)) +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     axis.title.x=element_blank(),
#     legend.position='bottom') +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# c <- ggplot(gca2, aes(Mapping_affiliation,n)) + 
#   geom_col(aes(fill=Author_gender),position=position_stack())+
#   facet_grid(rows=vars(First_last), scales="free") +
#   scale_fill_brewer(palette = "Accent") +
#   ylab("") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     axis.title.x=element_blank(),
#     legend.position='bottom') +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# d <- ggplot(gpa2, aes(Affiliation,x)) + 
#   geom_col(aes(fill=Gender)) +
#   scale_y_continuous(breaks=seq(0,800,10)) +
#   ylab("") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     axis.title.x=element_blank(),
#     legend.position='bottom') +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))

# fig_affil <- ggarrange(c,d,
#                        nrow = 2, 
#                        common.legend=TRUE,
#                        legend="bottom"
# ) 

pdf(file="Figure3_gender_affiliation.pdf",width=11,height=8)
ggplot(gpa2, aes(Affiliation,x)) + 
  geom_col(aes(fill=Gender)) +
  scale_y_continuous(breaks=seq(0,800,10)) +
  ylab("Number of articles") +
  xlab("Country of affiliation") +
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
# annotate_figure(fig_affil,
#                 left=text_grob("Number of articles", color="black",rot=90),
#                 bottom=text_grob("Country of affiliation", color="black"))
dev.off()

####### SECTION 3 ########
##Geographical distribution of study countries and marine realms of study

#Clean marine data
m_df <- df2 %>% select(Article.ID,id) %>% distinct()

#Count studies per realm
marine_count <- count(m_df,id)
marine_count$id <- as.character(as.factor(marine_count$id))

##Count studies that are regional (without country information)
no_country <- df2 %>% select(Article.ID,Study_type,Mapping_Country,Study_region) %>% filter(is.na(Mapping_Country))
n_distinct(no_country$Article.ID)

##Count number of studies for all countries and arrange by region
country_count <- matrix(nrow=nrow(country), ncol=2)
rownames(country_count) <- country$Study_country
colnames(country_count) <- c("Study_country", "counts")

df_country <- df2 %>% select(Article.ID,Mapping_Country,Study_type) %>% filter(!is.na(Mapping_Country)) %>% distinct()
n_distinct(df_country$Article.ID)
p <- df_country %>% select(Article.ID,Study_type) %>% distinct() %>% count(Study_type)

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
  geom_map(data=countries_zero, aes(map_id=Code),fill="#f7f7f7",map=map) + 
  expand_limits(x=map$long,y=map$lat) + 
  theme(panel.background = element_rect(fill = "#8abbe6", colour = "#8abbe6"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_equal() +
  theme(legend.position = "bottom")


cd <- cc +
  scale_fill_manual(
    values=c('#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525'),
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

pdf(file="Figure4_GeographicDistribution_StudySites.pdf", width=14, height=10)
plot(cd)
dev.off()

# #Generate Oceania zoom in
# library("sf")
# library("rnaturalearth")
# 
# worldmap <- ne_countries(scale = 'medium', type = 'map_units',
#                          returnclass = 'sf')
# oceania <- st_crop(worldmap, xmin=45,xmax=70,ymin=-223,ymax=15)
# 
# worldmap <- borders("world2", colour="black", fill="black")
# ggplot() +
#   worldmap +
#   theme_bw() +
#   coord_fixed(xlim=c(140,200),ylim=c(-27,15))


#scale_fill_gradient2(low="#deebf7",mid="#9ecae1",high="#08519c",midpoint=(max(countries$counts)/2),limits=c(0,max(countries$counts)))


###FIGURE 5 ######
##Has proportion of in-country author teams changed overtime
oecd <- country %>% select("Study_country","Global") %>% distinct()

geo <- df2 %>% select(Article.ID,Pub_year,Author,Author.Based.In.Country,Prop_overall_authorship,Mapping_affiliation,Mapping_Country) %>% distinct() %>% arrange(Article.ID) %>% filter(!is.na(Author.Based.In.Country)) %>% filter(Author.Based.In.Country != "") %>% arrange(Pub_year) %>% left_join(oecd,by=c("Mapping_Country"="Study_country")) %>% left_join(oecd,by=c("Mapping_affiliation"="Study_country"))
colnames(geo) <- c("Article.ID","Pub_year","Author","Author.Based.In.Country","Prop_overall_authorship","Mapping_affiliation","Mapping_Country","Mapping_OECD","Affiliation_OECD")

#Stats for author teams (in, out, mixed)
geo_stat <- geo %>% select(Article.ID,Author.Based.In.Country) %>% distinct() %>% count(Article.ID,Author.Based.In.Country) %>% select(-n)

g <- unlist(geo_stat$Article.ID) %>% unique()

new_df <- data.frame(Article.ID=as.integer(),Author.Based.In.Country=as.character(),Team=as.character(), stringsAsFactors = FALSE)

for (i in 1:length(g)){
  sub <- geo_stat %>% filter(Article.ID == g[i])
  if (nrow(sub) > 1){
    sub$Team <- c("Mixed")
  } else sub$Team <- sub$Author.Based.In.Country
  
  new_df <- bind_rows(new_df,sub)
}

new_df <- new_df %>% select(-Author.Based.In.Country) %>% distinct() %>% count(Team)

##Fixing author based in country (exact match to each studied country vs. being affiliated with at least one country studied in the paper)

# geo$Author.BasedIn.Country <- c("")
# for (i in 1:nrow(geo)){
#   x <- geo$Mapping_affiliation[i]
#   y <- geo$Mapping_Country[i]
#   if (identical(x,y) == TRUE){
#     geo$Author.BasedIn.Country[i] <- c("Yes")
#   } else geo$Author.BasedIn.Country[i] <- c("No")
# } 

geo$Author.Based.In.Country <- sub("yes","Yes",geo$Author.Based.In.Country)
geo$Author.Based.In.Country <- sub("No ","No",geo$Author.Based.In.Country)

geo_plot <- geo %>% filter(Mapping_OECD == "OECD") %>% select(Article.ID,Pub_year,Author,Author.Based.In.Country,Prop_overall_authorship) %>% distinct() %>% arrange(Pub_year)

gc <- geo_plot %>% select(Article.ID,Pub_year) %>% distinct() %>% count(Pub_year)

geo_y <- aggregate(geo_plot$Prop_overall_authorship, by=list(Year=geo_plot$Pub_year,Geo=geo_plot$Author.Based.In.Country), FUN=sum)

pdf(file="Figure5_in-out-country_over_time_atleastone_OECD.pdf",height=4,width=6)
ggplot(geo_y, aes(Year,x)) + 
  geom_col(aes(fill=Geo),position="fill") +
  scale_x_continuous(breaks=seq(2003,2018,1), name="Publication year") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
  ) +
  theme(
    legend.position='bottom') +
  ylab("Percent of published articles") +
  scale_fill_brewer(palette = "Accent") +
  guides(fill=guide_legend(title="Author based in study country?"),
         guide=guide_legend(
           direction="horizontal",
           #keyheight= unit(2, units="mm"),
           #keywidth = unit(70/length(labels), units="mm"),
           title.position = "top",
           title.hjust = 0.5,
           label.hjust = 1,
           label.position="bottom"
         ))
dev.off()

##Stats on in-country/out-of-country with and without US and Australia studies
geo2 <- geo %>% filter(Mapping_OECD != "OECD") %>% distinct()
# 
gc2 <- geo2 %>% select(Article.ID,Pub_year) %>% distinct() %>% count(Pub_year)
# 
# geo_yy <- aggregate(geo2$Prop_overall_authorship, by=list(Year=geo2$Pub_year,Geo=geo2$Author.BasedIn.Country), FUN=sum)
# 
# pdf(file="Figure5_in-out-country_over_time_noUS-AUS.pdf",height=4,width=6)
# ggplot(geo_yy, aes(Year,x)) + 
#   geom_col(aes(fill=Geo),position="fill") +
#   scale_x_continuous(breaks=seq(2003,2018,1), name="Publication year") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1)
#   ) +
#   theme(
#     legend.position='bottom') +
#   ylab("Percent of published articles") +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Author based in study country?"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# dev.off()

#Have to be based in at least one of study countries to count as "in-country"
geo_plot2 <- geo2 %>% select(Article.ID,Pub_year,Author,Author.Based.In.Country,Prop_overall_authorship) %>% distinct() %>% arrange(Pub_year)

geo_yyy <- aggregate(geo_plot2$Prop_overall_authorship, by=list(Year=geo_plot2$Pub_year,Geo=geo_plot2$Author.Based.In.Country), FUN=sum)

pdf(file="Figure5_in-out-country_over_time_atleastone_nonOECD.pdf",height=4,width=6)
ggplot(geo_yyy, aes(Year,x)) + 
  geom_col(aes(fill=Geo),position="fill") +
  scale_x_continuous(breaks=seq(2003,2018,1), name="Publication year") +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
  ) +
  theme(
    legend.position='bottom') +
  ylab("Percent of published articles") +
  scale_fill_brewer(palette = "Accent") +
  guides(fill=guide_legend(title="Author based in study country?"),
         guide=guide_legend(
           direction="horizontal",
           #keyheight= unit(2, units="mm"),
           #keywidth = unit(70/length(labels), units="mm"),
           title.position = "top",
           title.hjust = 0.5,
           label.hjust = 1,
           label.position="bottom"
         ))
dev.off()

##For studies based in OECD versus non-OECD countries
geo <- arrange(geo,Article.ID)

#How many articles conduced in OECD nations were actually US and Australia?
p <- geo %>% select(Article.ID,Mapping_Country,Mapping_OECD) %>% distinct() 
p2 <- p %>% filter(Mapping_OECD == "OECD") %>% distinct()

pp <- count(p2,Mapping_Country)
n_distinct(p$Article.ID)

ppp <- select(geo,Article.ID,Mapping_OECD) %>% distinct()
n_distinct(ppp$Article.ID)
p4 <- count(ppp,Mapping_OECD)

#How many authors have more than one affiliation in different countries per paper?
q <- geo %>% select(Article.ID,Author,Mapping_affiliation,Affiliation_OECD) %>% distinct() %>% group_by(Article.ID,Author) %>% filter(n() != 1)

n_distinct(q$Author) #Number of authors with more than one affliation listed on a single paper
n_distinct(q$Author)/n_distinct(geo$Author)*100

n_distinct(q$Article.ID) #Number of articles where authors listed more than one affiliation

qq <- q %>% select(-Mapping_affiliation) %>% distinct() %>% group_by(Article.ID,Author) %>% filter(n() != 1)
n_distinct(qq$Author) #Number of authors with multiple affiliations where one is a non-OECD nation
n_distinct(qq$Author)/n_distinct(q$Author)*100

####FIGURE 6#####

#Where people work vs. where they are based

#Refine data
kk <- df2 %>% select(Article.ID,Author,Mapping_affiliation,Mapping_Country,Study_region) %>% distinct()
kk_non <- kk %>% filter(is.na(Mapping_Country))
kk2 <- kk %>% filter(!is.na(Mapping_Country)) %>% filter(!is.na(Mapping_affiliation)) %>% select(-Author) %>% count(Mapping_affiliation,Mapping_Country)

#Summarize dataset
kk2_sub <-kk2[order(kk2$n),]
max <- max(kk2_sub$n)

#Take subset of country data
country$Region2 <- country$Region
country[241,11] <- "United States"
country[14,11] <- "Australia"

coord <- select(country,Study_country,Latitude,Longitude,Region2)

#Connect latitude and longitudes
data_plot <- kk2_sub %>% left_join(coord,by=c("Mapping_affiliation"="Study_country"))
colnames(data_plot) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent")
data_plot <- data_plot %>% left_join(coord,by=c("Mapping_Country"="Study_country"))
colnames(data_plot) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent","travellat","travellon","travelcontinent")

#For sankey diagram
sankey <- data_plot %>% select(Mapping_affiliation,Mapping_Country,homecontinent,travelcontinent,n) %>% distinct()
sankey$Mapping_Country <- sub("^",".",sankey$Mapping_Country)
sankey$travelcontinent <- sub("^",".",sankey$travelcontinent)

#By country
s1 <- sankey %>% select(homecontinent,travelcontinent,n) %>% distinct()
colnames(s1) <- c("source","target","value")

nodes1 <- data.frame(name=c(as.character(s1$source), as.character(s1$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
s1$IDsource=match(s1$source, nodes1$name)-1 
s1$IDtarget=match(s1$target, nodes1$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = s1, Nodes = nodes1,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

##to save you have to export as HTML from R plot viewer --> then open in a browser --> then print as a PDF and then can edit in Illustrator


### FIGURE 7 #####
##Co-author network
##Using degree centrality and betweeness centrality to examine autonomy and influence of authors

##Reshape data into rows of papers and columns of authors
net_df <- df2 %>% select(Article.ID,Author,Mapping_affiliation,Author_order) %>% distinct()
gsgn <- country %>% select(Study_country,Global)
net_df <- net_df %>% left_join(gsgn, by=c("Mapping_affiliation" = "Study_country"))

net_df <- arrange(net_df,Article.ID,Author_order)
f <- net_df %>% filter(Author_order == 1)
ff <- net_df %>% filter(Article.ID %in% f$Article.ID)

##Create edge list
edge <- select(ff, Article.ID,Author,Author_order) %>% distinct() %>% arrange(Article.ID)
e <- spread(edge,Author_order,Author)
e <- select(e,-Article.ID)
rownames(e) <- NULL

write.csv(e,"test_c.csv",row.names=FALSE)

source <- read.csv("test_c.csv",header=F,stringsAsFactors = F)
source <- slice(source,-1)

#test <- read.csv("example.csv",sep=";",header=F)
#test_edge <- Matrix2Edge(test)

edge_list <- Matrix2Edge(source)
edge_list.cleaned <- filter(edge_list, !is.na(a))
edge_list.clean <- filter(edge_list.cleaned, !is.na(b))

#Color by region

colnames(edge_list.cleaned) <- c("source","target")
nodes <- net_df %>% select(Author,Global) %>% distinct()
colnames(nodes) <- c("name","carac")

nodes <- arrange(nodes,name,carac) ##Keeps non-OECD affiliation when dropping below IF author has multiple affiliations on the paper

##PURELY TO TEST - anyone with more than one affiliation - north affiliation is kept
nodes1 = nodes[!duplicated(nodes$name),]
nodes <- nodes1

#calculate network - 5282 authors with 7869 edges (connections)
network <- graph_from_data_frame(d=edge_list.cleaned,vertices=nodes,directed=F)

coul  <- brewer.pal(4, "Set1") 

# Create a vector of color
my_color <- coul[as.numeric(as.factor(V(network)$carac))]

l <- layout.kamada.kawai(network) #explain different graph layouts

# Make the plot
pdf("conetwork_geo_2003.pdf",height=20,width=30)

V(network)$size <- 15*(degree(network, mode="in")/ max(degree(network, mode="in")))
plot(network, vertex.color=my_color, layout=l, vertex.label=NA,edge.color='gray')
legend("bottomleft", legend=levels(as.factor(V(network)$carac))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(0.1, 0.1))

dev.off()

##Alternative - violin plot with time?
prop_aff <- geo %>% select(Article.ID,Pub_year,Author,Affiliation_OECD) %>% distinct()
prop_aff <- prop_aff[!duplicated(prop_aff$Author),]


## FOR FUN STATS

#who is a publishing fiend
author_volume <- df2 %>% select(Article.ID,Author) %>% distinct() %>% count(Author) %>% arrange(desc(n))



#####################################
#####################################
#####################################
#####################################
## PREVIOUS STUFF NOT USED IN PUB BELOW
#####################################
#####################################
#####################################
#####################################



# ##Calculate node centrality measures
# 
# rd.mnet <- degree(network)
# s.mnet <- graph.strength(simplify(network)) # for weighted graph
# 
# hist(rd.mnet)
# hist(s.mnet)
# 
# #Simplify network - 5282 nodes with 7492 edges
# network2 <- simplify(network)
# l.mnet2 <- layout.kamada.kawai(network2)
# 
# l.mnet <- layout.kamada.kawai(network)
# 
# #plot
# plot(network, layout=l.mnet, vertex.label=NA, vertex.size=1, vertex.color='blue')
# plot(network2, layout=l.mnet2, vertex.label=NA, vertex.size=1, vertex.color='red')
# 
# d.mnet <- degree(network2) 
# 
# degree <- d.mnet
# dd.mnet <- degree.distribution(network2) # Compute degree distribution d <- 1:max(d.mnet)-1 ind <- (dd.mnet != 0) a.nn.deg.mnet <- graph.knn(mnet2, V(mnet2))
# 
# 
# 
# 
# 
# 
# 
# 
# mean(d.mnet)
# ###APPROACH 2
# 
# graph=graph.edgelist(as.matrix(edge_list.cleaned),directed=FALSE)
# 
# graph.density(graph)
# 
# degrees <- data.frame(degree = degree(graph),
#                       in_degree = degree(graph, mode = c("in"), loops = FALSE, normalized = FALSE),
#                       out_degree = degree(graph, mode = c("out"), loops = FALSE, normalized = FALSE),
#                       btwn= betweenness(graph, directed = T),
#                       close = closeness(graph, mode = c("all")),
#                       eigen <- evcent(graph)
# )
# 
# #cleaning up the table
# degrees = degrees[,c(1:6)]
# #include this in markdown
# degrees_sorted <- degrees[order(-degrees$degree),] 
# write.csv(degrees_sorted, file = "degrees_sorted_globalns.csv")
# 
# library(gridExtra)
# pdf("degrees_sorted_globalns.pdf", height=15, width=15)
# grid.table(degrees_sorted)
# dev.off()
# 
# cor(degrees_sorted)
# 
# set.seed(12)
# 
# l <- layout_sphere(graph) #explain differnt graph layouts
# 
# # Size of node by in-degree.
# V(graph)$size <- 15*(degree(graph, mode="in")/ max(degree(graph, mode="in")))
# 
# # Size of node label by in-degree.
# V(graph)$label.cex <- ((betweenness(graph, directed = T)+1)/(max(betweenness(graph, directed = T))+1))*2
# 
# # plot the graph
# pdf("network_femalefirst.pdf",height=20,width=20)
# 
# plot(graph, layout=l, edge.arrow.size=.1, edge.curved=F, edge.color="grey")
# 
# dev.off()

# # 
# # ##For connection map
# # 
# # #Create two dataframes - 1) for different affil/study locations and one for the same
# # foreign=as.data.frame(matrix(nrow=1,ncol=9))
# # colnames(foreign) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent","travellat","travellon","travelcontinent")
# # domestic=as.data.frame(matrix(nrow=1,ncol=9))
# # colnames(domestic) <- c("Mapping_affiliation","Mapping_Country","n","homelat","homelon","homecontinent","travellat","travellon","travelcontinent")
# # 
# # for(i in c(1:nrow(data_plot))){
# #   if(data_plot$Mapping_affiliation[i] == data_plot$Mapping_Country[i]){
# #     tmp <- as.data.frame(data_plot[i,])
# #     domestic <- bind_rows(domestic, tmp)
# #   } else if(data_plot$Mapping_affiliation[i] != data_plot$Mapping_Country[i]){
# #     tmp <- as.data.frame(data_plot[i,])
# #     foreign <- bind_rows(foreign,tmp)
# #   }
# # }
# # 
# # summary <- foreign %>% select(-Mapping_affiliation,-Mapping_Country)
# # summary <- arrange(summary,n)
# # summary <- summary[c(2,3,4,5,6,7,1)]
# # summary <- summary %>% filter(!is.na(homelon))
# # 
# # domestic <- domestic %>% select(-Mapping_affiliation,-Mapping_Country)
# # domestic <- arrange(domestic,desc(n))
# # domestic <- domestic[c(2,3,4,5,6,7,1)]
# # domestic <- domestic %>% filter(!is.na(homelon))
# # 
# # # A function that makes a dateframe per connection (we will use these connections to plot each lines)
# # data_for_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, group){
# #   inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
# #   inter=data.frame(inter)
# #   inter$group=NA
# #   diff_of_lon=abs(dep_lon) + abs(arr_lon)
# #   if(diff_of_lon > 180){
# #     inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
# #     inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
# #   }else{
# #     inter$group=as.character(paste(group))
# #   }
# #   return(inter)
# # }
# # 
# # # Complete dataframe for plotting
# # data_ready_plot=data.frame()
# # for(i in c(1:nrow(summary))){
# #   tmp=data_for_connection(summary$homelon[i], summary$homelat[i], summary$travellon[i], summary$travellat[i], i)
# #   tmp$homecontinent=summary$homecontinent[i]
# #   tmp$n=summary$n[i]
# #   data_ready_plot=bind_rows(data_ready_plot, tmp)
# # }
# # data_ready_plot$homecontinent=factor(data_ready_plot$homecontinent, levels=c("East Asia and Pacific","Europe and Central Asia","Latin America and the Caribbean","Middle East and North Africa","North America","South Asia","Sub-Saharan Africa"))
# # #data_ready_plot$homecontinent=factor(data_ready_plot$homecontinent, levels=c("Low-Income","Lower-Middle Income","Upper-Middle Income","High-Income","Not categorized"))
# # 
# # # Plot
# # 
# # worldmap <- map_data ("world")
# # 
# # mybreaks <- c(1,10,50,100,500,1500)
# # mybreaks2 <- c(1,50,150)
# # p <- ggplot(worldmap) +
# #   geom_map(data=worldmap,map=worldmap,aes(x=long,y=lat,map_id=region),col="gray25",fill="black") +
# #   #annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
# #   geom_line(data=data_ready_plot, aes(x=lon, y=lat, group=group, colour=homecontinent, alpha=n), size=0.7) +
# #   geom_point(data=domestic,aes(x=homelon,y=homelat,size=n,colour=homecontinent),alpha=0.6) +
# #   scale_size_continuous(name="Number of authors working in own country",range=c(1,12),breaks=mybreaks) +
# #   scale_alpha_continuous(name="Number of authors working in another country",range=c(0.2,1),breaks=mybreaks2) +
# #   scale_color_brewer(palette="Accent") +
# #   theme_void() +
# #   labs(color="Affiliation of researcher",alpha="Number of authors working in another country") +
# #   theme(
# #     panel.background=element_rect(fill="black","colour"="black"),
# #     legend.text = element_text(color="black",size=17),
# #     legend.margin=margin(0.2,0.2,0.2,0.2,"cm"),
# #     legend.position="bottom",
# #     legend.direction="horizontal",
# #     legend.background=element_rect(fill="white",linetype="solid",color="gray25"),
# #     legend.title=element_text(color="black",size=20,face="bold"),
# #     legend.justification="center"
# #     #panel.background = element_rect(fill = "black", colour = "black"), 
# #     #panel.spacing=unit(c(0,0,0,0), "null"),
# #     #plot.margin=grid::unit(c(0,0,0,0), "cm"),
# #   ) +
# #   guides(linetype = guide_legend(override.aes = list(size = 2))) +
# #   ggplot2::annotate("text", x = -175, y = 80, hjust = 0, size = 11, label = paste("Where coral reef scientists do research"), color = "white") +
# #   ggplot2::annotate("text", x = -175, y = 70, hjust = 0, size = 8, label = paste("Each line represents the contribution of authors from one country to research in another | Increasingly opaque lines represent higher numbers of authors"), color = "white", alpha = 0.5) +
# #   xlim(-180,180) +
# #   ylim(-60,80) +
# #   scale_x_continuous(expand = c(0.006, 0.006)) +
# #   coord_equal() 
# # 
# # # Save at PNG
# # ggsave("WherePeopleDoResearch_byRegion.png", width = 36, height = 15.22, units = "in", dpi = 90)
# # 
# # 
# 
# 
# 
# #Percent first and last authors and other position, male vs. female vs. other
# ratio <- df2 %>% select(Article.ID,Author_gender,First_last) %>% distinct()
# x <- n_distinct(ratio$Article.ID)
# rg <- count(ratio,Author_gender,First_last) %>% filter(First_last != "other")
# 
# ggplot(rg, aes(x=First_last,y=n)) +
#   geom_bar(aes(fill=Author_gender), stat="identity", position=position_dodge()) + 
#   scale_fill_brewer(palette="Accent") +
#   theme_minimal() +
#   theme(
#     axis.ticks.x = element_blank(),
#     legend.position='right') +
#   ylab("Number of articles") +
#   xlab("Author position") +
#   guides(fill=guide_legend(title="Gender"))
# 
# ##By Marine Realm of Study
# #Proportion (overall male vs. female contribution)
# gp_marine <- gender_mf %>% select(Article.ID,Author_gender,Author,id,Prop_authorship) %>% filter(!is.na(id)) %>% filter(id != "Global") %>% filter(id != "Freshwater") %>% distinct()
# gpm <- aggregate(gp_marine$Prop_authorship, by=list(Realm=gp_marine$id,Gender=gp_marine$Author_gender), FUN=sum) %>% mutate(Realm = fct_reorder(Realm, desc(x)))
# 
# #Author position
# gc_marine <- gender_mf %>% select(Article.ID,Author_gender,First_last,id) %>% filter(First_last != "other") %>% filter(!is.na(id)) %>% filter(id != "Global") %>% filter(id != "Freshwater") %>% distinct()
# gcm <- count(gc_marine,id,Author_gender,First_last) %>% mutate(id = fct_reorder(id, desc(n)))
# 
# ##Plotting
# ggplot(gcm, aes(id,n)) + 
#   geom_col(aes(fill=Author_gender),position=position_dodge()) +
#   facet_grid(rows=vars(First_last), scales="free") +
#   ylab("Number of articles") +
#   xlab("Marine realm studied") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     legend.position='bottom') +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# ggplot(gpm, aes(Realm,x)) + 
#   geom_col(aes(fill=Gender)) +
#   scale_y_continuous(breaks=seq(0,800,50), name="Proportion of published articles") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     legend.position='bottom') +
#   xlab("Marine realm studied") +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# 
# 
# ##By Journal
# #Proportion (overall male vs. female contribution)
# gender_mf$Journal[gender_mf$Journal == "PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA"] <- "PNAS"
# gender_mf$Journal[gender_mf$Journal == "PROCEEDINGS OF THE ROYAL SOCIETY B-BIOLOGICAL SCIENCES"] <- "ProcB"
# gender_mf$Journal[gender_mf$Journal == "MARINE ECOLOGY PROGRESS SERIES"] <- "MEPS"
# gender_mf$Journal[gender_mf$Journal == "Plos One"] <- "PLoS One"
# 
# gp_journal <- gender_mf %>% select(Article.ID,Author_gender,Author,Journal,Prop_authorship) %>% distinct()
# gpj <- aggregate(gp_journal$Prop_authorship, by=list(Journal=gp_journal$Journal,Gender=gp_journal$Author_gender), FUN=sum) %>% mutate(Journal = fct_reorder(Journal, desc(x)))
# 
# #Author position
# gc_journal <- gender_mf %>% select(Article.ID,Author_gender,First_last,Journal) %>% filter(First_last != "other") %>% distinct()
# gcj <- count(gc_journal,Journal,Author_gender,First_last) %>% mutate(Journal = fct_reorder(Journal, desc(n)))
# 
# ##Plotting
# ggplot(gcj, aes(Journal,n)) + 
#   geom_col(aes(fill=Author_gender),position=position_dodge()) +
#   facet_grid(rows=vars(First_last), scales="free") +
#   ylab("Number of articles") +
#   xlab("Journal") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     legend.position='bottom') +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# ggplot(gpj, aes(Journal,x)) + 
#   geom_col(aes(fill=Gender)) +
#   scale_y_continuous(breaks=seq(0,1100,50), name="Proportion of published articles") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     legend.position='bottom') +
#   xlab("Journal") +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))





# ##Number of single country publications vs. multiple country publications
# dna <- diversity %>% select(Article.ID,Author,First_last,Mapping_affiliation,Author_gender) %>% distinct()
# dna$num_aff_coun <- c("")
# dna$single <- c("")
# 
# p <- c(1:n)
# id <- unique(dna$Article.ID)
# for (x in p){
#   i <- id[x]
#   sub <- filter(dna, Article.ID == i) %>% distinct()
#   dna$num_aff_coun[dna$Article.ID == i] <- n_distinct(sub$Mapping_affiliation)
#   if (n_distinct(sub$Mapping_affiliation) == 1){
#     dna$single[dna$Article.ID == i] <- c("Yes")
#   } else
#     dna$single[dna$Article.ID == i] <- c("No")
# }
# 
# dna_plot <- dna %>% select(Article.ID,single) %>% distinct() %>% count(single)
# 
# p1 <- ggplot(dna_plot,aes(x="",y=n,fill=single)) +
#   geom_col(width=1) +
#   ylab("Number of articles") +
#   xlab("") +
#   scale_fill_brewer(palette = "Accent",labels=c("International","National")) +
#   guides(fill=guide_legend(title="Country authorship"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# p1 + coord_flip()
# 
# #Proportion of male and female first, last uthors who involved in an international/multiple country collaboration
# 
# dna_plot2 <- dna %>% select(Article.ID,Mapping_affiliation,single,First_last,Author_gender) %>% distinct() %>% count(single,First_last,Author_gender) %>% filter(First_last != "other") %>% filter(First_last != "single") %>% filter(Author_gender != "Not found") %>% filter(Author_gender != "Undetermined")
# 
# p2 <- ggplot(dna_plot2, aes(x=Author_gender,y=n,fill=single)) + 
#   geom_col(position=position_dodge()) +
#   facet_grid(rows=vars(First_last), scales="free") +
#   ylab("Number of articles") +
#   xlab("Gender") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     legend.position='bottom') +
#   scale_fill_brewer(palette = "Accent",labels=c("International","National")) +
#   guides(fill=guide_legend(title=""),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# p2 + coord_flip()
# 
# 
# 
# ##Co-author network
# ##Using degree centrality and betweeness centrality to examine autonomy and influence of authors
# 
# ##Reshape data into rows of papers and columns of authors
# net_df <- df2 %>% select(Article.ID,Author,Author_gender,Mapping_affiliation,Author_order) %>% distinct()
# 
# f <- net_df %>% filter(Author_gender == 'Female' & Author_order == 1)
# ff <- net_df %>% filter(Article.ID %in% f$Article.ID)
# 
# ##Create edge list
# edge <- select(ff, Article.ID,Author,Author_order) %>% distinct() %>% arrange(Article.ID)
# e <- spread(edge,Author_order,Author)
# e <- select(e,-Article.ID)
# rownames(e) <- NULL
# 
# write.csv(e,"coauthor_network_data_all_1031.csv",row.names=FALSE)
# 
# source <- read.csv("coauthor_network_data_all_1031.csv",header=F,stringsAsFactors = F)
# source <- slice(source,-1)
# 
# #test <- read.csv("example.csv",sep=";",header=F)
# #test_edge <- Matrix2Edge(test)
# 
# edge_list <- Matrix2Edge(source)
# edge_list.cleaned <- filter(edge_list, !is.na(a))
# edge_list.clean <- filter(edge_list.cleaned, !is.na(b))
# 
# #Color by reg
# 
# colnames(edge_list.cleaned) <- c("source","target")
# nodes <- net_df %>% select(Author,Author_gender) %>% distinct()
# colnames(nodes) <- c("name","carac")
# 
# #calculate network - 5282 authors with 7869 edges (connections)
# network <- graph_from_data_frame(d=edge_list.cleaned,vertices=nodes,directed=F)
# 
# coul  <- brewer.pal(4, "Set1") 
# 
# # Create a vector of color
# my_color <- coul[as.numeric(as.factor(V(network)$carac))]
# 
# l <- layout.kamada.kawai(network) #explain differnt graph layouts
# 
# # Make the plot
# pdf("full_conetwork.pdf",height=20,width=30)
# 
# V(network)$size <- 15*(degree(network, mode="in")/ max(degree(network, mode="in")))
# plot(network, vertex.color=my_color, layout=l, vertex.label=NA,edge.color='gray')
# legend("bottomleft", legend=levels(as.factor(V(network)$carac))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(0.1, 0.1))
# 
# dev.off()
# 
# ##Calculate node centrality measures
# 
# rd.mnet <- degree(network)
# s.mnet <- graph.strength(simplify(network)) # for weighted graph
# 
# hist(rd.mnet)
# hist(s.mnet)
# 
# #Simplify network - 5282 nodes with 7492 edges
# network2 <- simplify(network)
# l.mnet2 <- layout.kamada.kawai(network2)
# 
# l.mnet <- layout.kamada.kawai(network)
# 
# #plot
# plot(network, layout=l.mnet, vertex.label=NA, vertex.size=1, vertex.color='blue')
# plot(network2, layout=l.mnet2, vertex.label=NA, vertex.size=1, vertex.color='red')
# 
# d.mnet <- degree(network2) 
# 
# degree <- d.mnet
# dd.mnet <- degree.distribution(network2) # Compute degree distribution d <- 1:max(d.mnet)-1 ind <- (dd.mnet != 0) a.nn.deg.mnet <- graph.knn(mnet2, V(mnet2))
# 
# mean(d.mnet)
# 
# ###APPROACH 2
# 
# graph=graph.edgelist(as.matrix(edge_list.cleaned),directed=FALSE)
# 
# graph.density(graph)
# 
# library("igraph")
# 
# degrees <- data.frame(degree = igraph::degree(graph),
#                       in_degree = igraph::degree(graph, mode = c("in"), loops = FALSE, normalized = FALSE),
#                       out_degree = igraph::degree(graph, mode = c("out"), loops = FALSE, normalized = FALSE),
#                       btwn= igraph::betweenness(graph, directed = T),
#                       close = igraph::closeness(graph, mode = c("all")),
#                       eigen <- evcent(graph)
# )
# 
# #cleaning up the table
# degrees = degrees[,c(1:6)]
# #include this in markdown
# degrees_sorted <- degrees[order(-degrees$degree),] 
# write.csv(degrees_sorted, file = "degrees_sorted.csv")
# 
# library(gridExtra)
# pdf("degrees_sorted_gngs.pdf", height=15, width=15)
# grid.table(degrees_sorted)
# dev.off()
# 
# cor(degrees_sorted)
# 
# set.seed(12)
# 
# l <- layout_sphere(graph) #explain differnt graph layouts
# 
# # Size of node by in-degree.
# V(graph)$size <- 15*(degree(graph, mode="in")/ max(degree(graph, mode="in")))
# 
# # Size of node label by in-degree.
# V(graph)$label.cex <- ((betweenness(graph, directed = T)+1)/(max(betweenness(graph, directed = T))+1))*2
# 
# # plot the graph
# pdf("network_gngs.pdf",height=20,width=20)
# 
# plot(graph, layout=l, edge.arrow.size=.1, edge.curved=F, edge.color="grey")
# 
# dev.off()
# 
# #title("Co-Authorship Graph",cex.main=0.8,col.main="black")
# 
# ### B.3. Community detection ----
# fgn = edge.betweenness.community (graph, directed = TRUE, edge.betweenness = TRUE, merges = TRUE,
#                                   bridges = TRUE, modularity = TRUE, membership = TRUE)  ## run Girvan-Newman partitioning
# 
# plot(fgn, graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")  ## plot G-N partitioning
# 
# fwt <- walktrap.community(graph, steps=200,modularity=TRUE) # , labels=TRUE)  ## run random walk partitioning
# 
# plot(fwt, graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")  ## plot R-W partitioning
# 
# flp = label.propagation.community(graph)  ## run label propogation partitioning
# 
# plot(flp, graph, layout=l, edge.arrow.size=.2, edge.curved=T, edge.color="grey")  ## plot L-P partitioning
# 
# #comparing different community dittection algorithms
# compare(fgn, fwt, method= c("nmi"))
# compare(fgn, fwt, method= c("rand"))
# compare(fgn, fwt, method= c("adjusted.rand"))
# 
# compare(fgn, flp, method= c("nmi"))
# compare(fgn, fwt, method= c("rand"))
# compare(fgn, flp, method= c("adjusted.rand"))
# 
# ## get the results in a dataframe
# 
# girvan = data.frame(fgn$membership)
# rw = data.frame(fwt$membership)
# flpm = data.frame(flp$membership)
# 
# degrees <- cbind(degrees, girvan, rw, flpm)

# ##By study type
# #Proportion (overall male vs. female contribution)
# gp_study <- gender_mf %>% select(Article.ID,Author_gender,Author,Study_Type,Prop_authorship) %>% distinct()
# gps <- aggregate(gp_study$Prop_authorship, by=list(Study_Type=gp_study$Study_Type,Gender=gp_study$Author_gender), FUN=sum) %>% mutate(Study_Type = fct_reorder(Study_Type, desc(x)))
# 
# #Author position
# gc_study <- gender_mf %>% select(Article.ID,Author_gender,First_last,Study_Type) %>% filter(First_last != "other") %>% distinct()
# gcs <- count(gc_study,Study_Type,Author_gender,First_last)
# gcs <- rbind(gcs,c("Editorials/Correspondence","Female","single",0))
# gcs <- rbind(gcs,c("Synthesis","Female","single",0))
# gcs <- rbind(gcs,c("Undetermined","Female","single",0))
# gcs$n <- as.integer(gcs$n)
# gcs <- gcs %>% mutate(Study_Type = fct_reorder(Study_Type, desc(n)))
# 
# ##Plotting
# ggplot(gcs, aes(Study_Type,n)) + 
#   geom_col(aes(fill=Author_gender),position="fill") +
#   facet_grid(rows=vars(First_last), scales="free") +
#   ylab("Number of articles") +
#   xlab("Study type") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     legend.position='bottom') +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))
# 
# ggplot(gps, aes(Study_Type,x)) + 
#   geom_col(aes(fill=Gender),position="fill") +
#   theme(
#     axis.text.x=element_text(angle=45,hjust=1),
#     legend.position='bottom') +
#   xlab("Study type") +
#   scale_fill_brewer(palette = "Accent") +
#   guides(fill=guide_legend(title="Gender"),
#          guide=guide_legend(
#            direction="horizontal",
#            #keyheight= unit(2, units="mm"),
#            #keywidth = unit(70/length(labels), units="mm"),
#            title.position = "top",
#            title.hjust = 0.5,
#            label.hjust = 1,
#            label.position="bottom"
#          ))

# ##Times Cited
# #Proportion (overall male vs. female contribution)
# gp_cite <- gender_mf %>% select(Article.ID,Author_gender,Author,Prop_authorship) %>% distinct()
# gpc <- aggregate(gp_cite$Prop_authorship, by=list(Article.ID=gp_cite$Article.ID,Gender=gp_cite$Author_gender), FUN=sum) %>% arrange(Article.ID)
# cite <- gender_mf %>% select(Article.ID,Times.Cited) %>% distinct() 
# gpc <- left_join(gpc,cite,by="Article.ID")
# 
# ##Convert all male author teams to 0% for female
# gpcc <- gpc %>% filter(Gender == "Male" & x == 1.0000000) %>% distinct()
# 
# gpcc$Gender <- gsub("Male","Female",gpcc$Gender)
# gpcc$x <- gsub(1,0,gpcc$x)
# gpcc$x <- as.double(gpcc$x)
# 
# gpc2 <- gpc %>% anti_join(gpcc,by="Article.ID")
# 
# gpc_final <- bind_rows(gpc2,gpcc)
# gpc_final <- arrange(gpc_final,Article.ID)
# gpc_final[2756,4] <- 0
# gpc_final[2757,4] <- 0
# gpc_final <- gpc_final %>% filter(!is.na(Times.Cited))
# gpc_final <- gpc_final %>% filter(Gender != "Male")
# 
# #Average number of citations
# l <- mean(gpc_final$Times.Cited)
# 
# ##Plotting
# 
# pdf("Figure4_citation_gender_proportion.pdf",height=11,width=6)
# ggplot(gpc_final, aes(x=x, y=Times.Cited)) +
#   geom_point(size=1.5, color="#000000", alpha=0.8,stroke=0.5) +
#   geom_hline(yintercept=l, color="orange") +
#   scale_x_continuous(breaks=seq(0,1,0.1),labels=c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")) +
#   scale_y_continuous(breaks=seq(0,2750,250)) +
#   xlab("Proportion of female authorship (per article)") +
#   ylab("Number of citations")
# dev.off()

# ##Proportion of author team and times cited
# tc <- select(df2,Article.ID,Times.Cited) %>% distinct() %>% arrange(Article.ID)
# tc_d <- left_join(dppp,tc,by="Article.ID")
# 
# z <- ggplot(tc_d, aes(x=Yes, y=Times.Cited)) +
#   geom_point(size=2, color="#000000", alpha=0.6,stroke=0.5) +
#   geom_hline(yintercept=mean(tc_d$Times.Cited), color="orange") +
#   scale_x_continuous(breaks=seq(0,1,0.1)) +
#   scale_y_continuous(breaks=seq(0,2600,100)) +
#   xlab("") +
#   ylab("")
# 
# ##Without US and Australia
# tc2 <- select(df2,Article.ID,Times.Cited,Mapping_Country) %>% distinct() %>% arrange(Article.ID) %>% filter(Mapping_Country == "United States") %>% filter(Mapping_Country != "Australia") %>% distinct()
# 
# tcd2 <- left_join(tc2,dppp,by="Article.ID")
# 
# y <- ggplot(tcd2, aes(x=Yes, y=Times.Cited)) +
#   geom_point(size=2, color="#000000", alpha=0.6,stroke=0.5) +
#   geom_hline(yintercept=mean(tc_d$Times.Cited), color="orange") +
#   scale_x_continuous(breaks=seq(0,1,0.1)) +
#   scale_y_continuous(breaks=seq(0,300,100)) +
#   xlab("") +
#   ylab("")
# 
# fig_affil2 <- ggarrange(z,y,
#                         nrow = 2, 
#                         common.legend=TRUE,
#                         legend="bottom"
# ) 
# 
# pdf(file="Figure9_geographicdiversity_authors_citations.pdf",width=11,height=8)
# annotate_figure(fig_affil2,
#                 left=text_grob("Times cited", color="black",rot=90),
#                 bottom=text_grob("Proportion of author team based in study country", color="black"))
# dev.off()

# ##FIGURE 4 (ALTERNATIVE BY MARINE REALM)
# #Summarize and plot marine realms
# #prepare data
# m_lyr <- readOGR(dsn="MEOW-TNC", layer="meow_ecos",verbose=FALSE)
# m_lyr_fortified <- tidy(m_lyr,region="REALM")
# 
# marine_data <- m_lyr_fortified %>% left_join(marine_count, by="id")
# marine_data$n[is.na(marine_data$n)] <- 0
# marine_data_zero <- marine_data %>% filter(n == 0)
# marine_data <- marine_data %>% filter(n != 0)
# 
# ##Plot countries
# map_lyr <- readOGR(
#   dsn="TM_WORLD_BORDERS-0.3",
#   layer="TM_WORLD_BORDERS-0.3",
#   verbose=FALSE
# )
# 
# ggmap <- tidy(map_lyr,region="ISO3")
# 
# pdf(file="Figure1_alternative_marinedistribution.pdf",width=14,height=8)
# ggplot() + 
#   geom_polygon(data=marine_data, aes(fill=n,x=long,y=lat,group=group)) +
#   geom_polygon(data=marine_data_zero, aes(x=long,y=lat,group=group), fill="#7c7c7c") +
#   geom_polygon(data=ggmap, aes(x=long,y=lat,group=group), fill="#ffffff") +
#   scale_fill_gradient2(low="#deebf7",mid="#9ecae1",high="#08519c") +
#   theme_minimal() +
#   theme(
#     #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
#     axis.line = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_line(color = "#ebebe5", size = 0.1),
#     panel.grid.minor = element_blank(),
#     plot.background = element_rect(fill = "#000000", color = NA), 
#     panel.background = element_rect(fill = "#f5f5f2", color = NA), 
#     legend.background = element_rect(fill = "#f5f5f2", color = NA),
#     panel.border = element_blank()
#   )
# dev.off()
# 
# #Summarized in a table
# marine_data_summary <- marine_data %>% select(id, n) %>% distinct() %>% arrange(desc(n))

# ##Affiliation analysis
# ##Refine and clean subset of data
# diversity <- df2 %>% select(Article.ID,Author,Author_order,First_last,Author_gender,Author.Based.In.Country,Author.Based.In.Sovereign,Mapping_Country,Mapping_affiliation,Prop_overall_authorship) %>% distinct()
# diversity$Author.Based.In.Country <- as.character(diversity$Author.Based.In.Country)
# diversity$Author.Based.In.Sovereign <- as.character(diversity$Author.Based.In.Sovereign)
# diversity$Author.Based.In.Country <- gsub("^No ","No",diversity$Author.Based.In.Country)
# diversity$Author.Based.In.Country <- gsub("yes","Yes",diversity$Author.Based.In.Country)
# diversity$Author.Based.In.Country[is.na(diversity$Author.Based.In.Country)] <- "Undetermined"
# diversity$Author.Based.In.Country[diversity$Author.Based.In.Country == ""] <- "Undetermined"
# 
# #How often author(s) were based in study country
# da <- diversity %>% select(Article.ID,Author,Author.Based.In.Country,Mapping_Country) %>% distinct() %>% filter(Author.Based.In.Country != "Undetermined") %>% distinct()
# 
# dac <- count(da,Article.ID,Author.Based.In.Country,Mapping_Country)
# 
# #Proportion of author team  and author position from study country
# #By filtering for only yes in country, should eliminate any authors who have multiple affiliations that are causing a double count in the statistics. Out of country affiliation is dropped (however, still important to see this?)
# dp <- diversity %>% select(Article.ID,Author.Based.In.Country,Author,Prop_overall_authorship) %>% distinct() %>% arrange(Article.ID)
# dpp <- aggregate(dp$Prop_overall_authorship, by=list(Article.ID=dp$Article.ID,InCountry=dp$Author.Based.In.Country), FUN=sum) %>% arrange(Article.ID)
# dppp <- spread(dpp,InCountry,x)
# dppp[is.na(dppp)] <- 0
# dpp_c <- count(dppp,Yes)
# colnames(dpp_c) <- c("Prop.Based.In.Country","n")
# 
# #Without US and Australia
# dp_n <- diversity %>% filter(Mapping_Country != "United States") %>% filter(Mapping_Country != "Australia") %>% select(Article.ID,Author.Based.In.Country,Author,Prop_overall_authorship) %>% distinct() %>% arrange(Article.ID)
# 
# dppn <- aggregate(dp_n$Prop_overall_authorship, by=list(Article.ID=dp_n$Article.ID,InCountry=dp_n$Author.Based.In.Country), FUN=sum) %>% arrange(Article.ID)
# dpppn <- spread(dppn,InCountry,x)
# dpppn[is.na(dpppn)] <- 0
# dpp_cn <- count(dpppn,Yes)
# colnames(dpp_cn) <- c("Prop.Based.In.Country","n")
# 
# pdf(file="Figure8_proportion_in_country.pdf",height=4,width=4)
# 
# # e <- ggplot(dpp_c, aes(x=Prop.Based.In.Country, y=n)) +
# #   geom_point(size=2, color="#000000", alpha=0.6,stroke=0.5) +
# #   scale_x_continuous(breaks=seq(0,1,0.1)) +
# #   scale_y_continuous(breaks=seq(0,800,100)) +
# #   xlab("") +
# #   ylab("")
# 
# f <- ggplot(dpp_cn, aes(x=Prop.Based.In.Country, y=n)) +
#   geom_point(size=2, color="#000000", alpha=0.6,stroke=0.5) +
#   scale_x_continuous(breaks=seq(0,1,0.1)) +
#   scale_y_continuous(breaks=seq(0,400,100)) +
#   xlab("Proportion of author team based in study country") +
#   ylab("Number of articles")
# 
# # fig_affil3 <- ggarrange(e,f,
# #                         nrow = 2, 
# #                         common.legend=TRUE,
# #                         legend="bottom"
# # ) 
# # 
# # annotate_figure(fig_affil3,
# #                 left=text_grob("Number of articles", color="black",rot=90),
# #                 bottom=text_grob("Proportion of author team based in study country", color="black"))
# 
# plot(f)
# dev.off()