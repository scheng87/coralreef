#```{r mar_region_plot, fig.cap='**Figure 3. Distribution of research effort over marine realms**', fig.height=7, fig.width=12, echo=FALSE, results='asis'}
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


#kable(marine_data_summary, caption="Total articles by marine realm")
#```

<!-- # ```{r marine_realm_gender, echo=FALSE} -->
  <!-- # ##By Marine Realm of Study -->
  <!-- # #Proportion (overall male vs. female contribution) -->
  <!-- # gp_marine <- gender_mf %>% select(Article.ID,Author_gender,Author,id,Prop_authorship) %>% filter(!is.na(id)) %>% filter(id != "Global") %>% filter(id != "Freshwater") %>% distinct() -->
  <!-- # gpm <- aggregate(gp_marine$Prop_authorship, by=list(Realm=gp_marine$id,Gender=gp_marine$Author_gender), FUN=sum) %>% mutate(Realm = fct_reorder(Realm, desc(x))) -->
  <!-- #  -->
  <!-- # #Author position -->
  <!-- # gc_marine <- gender_mf %>% select(Article.ID,Author_gender,First_last,id) %>% filter(First_last != "other") %>% filter(!is.na(id)) %>% filter(id != "Global") %>% filter(id != "Freshwater") %>% distinct() -->
  <!-- # gcm <- count(gc_marine,id,Author_gender,First_last) %>% mutate(id = fct_reorder(id, desc(n))) -->
  <!-- #  -->
  <!-- # ##Plotting -->
  <!-- # # ggplot(gcm, aes(id,n)) +  -->
  <!-- # #   geom_col(aes(fill=Author_gender),position=position_dodge()) + -->
  <!-- # #   facet_grid(rows=vars(First_last), scales="free") + -->
  <!-- # #   ylab("Number of articles") + -->
  <!-- # #   xlab("Marine realm studied") + -->
  <!-- # #   theme( -->
  <!-- # #     axis.text.x=element_text(angle=45,hjust=1), -->
  <!-- # #     legend.position='bottom') + -->
  <!-- # #   scale_fill_brewer(palette = "Accent") + -->
  <!-- # #   guides(fill=guide_legend(title="Gender"), -->
  <!-- # #          guide=guide_legend( -->
  <!-- # #            direction="horizontal", -->
  <!-- # #            #keyheight= unit(2, units="mm"), -->
  <!-- # #            #keywidth = unit(70/length(labels), units="mm"), -->
  <!-- # #            title.position = "top", -->
  <!-- # #            title.hjust = 0.5, -->
  <!-- # #            label.hjust = 1, -->
  <!-- # #            label.position="bottom" -->
  <!-- # #          )) -->
  <!-- #  -->
  <!-- # ggplot(gpm, aes(Realm,x)) +  -->
  <!-- #   geom_col(aes(fill=Gender)) + -->
  <!-- #   scale_y_continuous(breaks=seq(0,800,50), name="Proportion of published articles") + -->
  <!-- #   theme( -->
  <!-- #     axis.text.x=element_text(angle=45,hjust=1), -->
  <!-- #     legend.position='bottom') + -->
  <!-- #   xlab("Marine realm studied") + -->
  <!-- #   scale_fill_brewer(palette = "Accent") + -->
  <!-- #   guides(fill=guide_legend(title="Gender"), -->
  <!-- #          guide=guide_legend( -->
  <!-- #            direction="horizontal", -->
  <!-- #            #keyheight= unit(2, units="mm"), -->
  <!-- #            #keywidth = unit(70/length(labels), units="mm"), -->
  <!-- #            title.position = "top", -->
  <!-- #            title.hjust = 0.5, -->
  <!-- #            label.hjust = 1, -->
  <!-- #            label.position="bottom" -->
  <!-- #          )) -->
  <!-- # ``` -->
  
  <!-- # ```{r citation_gender, echo=FALSE, fig.cap='**Figure X.** How often papers authored by different total proportions of gender representation were cited'} -->
  <!-- # ##Times Cited -->
  <!-- # #Proportion (overall male vs. female contribution) -->
  <!-- # gp_cite <- gender_mf %>% select(Article.ID,Author_gender,Author,Prop_authorship) %>% distinct() -->
  <!-- # gpc <- aggregate(gp_cite$Prop_authorship, by=list(Article.ID=gp_cite$Article.ID,Gender=gp_cite$Author_gender), FUN=sum) %>% arrange(Article.ID) -->
  <!-- # cite <- gender_mf %>% select(Article.ID,Times.Cited) %>% distinct()  -->
  <!-- # gpc <- left_join(gpc,cite,by="Article.ID") -->
  <!-- # gpc <- gpc %>% filter(Gender != "Male") -->
  <!-- #  -->
  <!-- # #Author position -->
  <!-- # gc_cite <- gender_mf %>% select(Article.ID,Author_gender,First_last,Times.Cited) %>% filter(First_last != "other") %>% distinct() -->
  <!-- # gcs <- count(gc_cite,Times.Cited,Author_gender,First_last) -->
  <!-- #  -->
  <!-- # ##Plotting -->
  <!-- # ggplot(gpc, aes(x=x, y=Times.Cited)) + -->
  <!-- #   geom_point(size=1.5, color="#7FC97F", alpha=0.6,stroke=0.5) + -->
  <!-- #   scale_x_continuous(breaks=seq(0,1,0.1)) + -->
  <!-- #   scale_y_continuous(breaks=seq(0,2750,250)) + -->
  <!-- #   xlab("Proportion of female authorship (per article)") + -->
  <!-- #   ylab("Number of citations") -->
  <!-- # ``` -->