## Junk script for testing out app components 

dataPresence <- function(df){
  df %>%
    # filter(site == "SERC") %>%
    ggplot(mapping = aes(x = year, y = sensor)) + 
    geom_tile(color = "white") +
    coord_equal() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          # plot.title = element_text(hjust = 0.5, size=24),
          # panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          # legend.position="right", # position of legend or none
          # legend.direction="vertical", # orientation of legend
          legend.title= element_blank(), # no title for legend
          # axis.text.x=element_text(size=10, angle=90, vjust=0.5),
          # axis.text.y=element_text(size=10)
    ) +
    facet_wrap(~site, dir = "v")
}
# dataPresence(key)

trackerFacet <- function(df){
  df %>% 
    dplyr::mutate(siteName = sites[Site]) %>% 
    ggplot(aes(Year, fct_reorder(siteName, desc(siteName))))+
    geom_tile(aes(fill=factor(level)), color="black", size=0.1)+
    scale_fill_manual(values=c("#c6eafb","#0d5388"), na.value = "white", breaks=c(0,1), labels=c(" Level 0", " Level 1"))+
    coord_equal()+
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5, size=24),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position="right", # position of legend or none
          legend.direction="vertical", # orientation of legend
          legend.title= element_blank(), # no title for legend
          axis.text.x=element_text(size=10, angle=90, vjust=0.5),
          axis.text.y=element_text(size=10)
    ) + facet_wrap(~Project, ncol = 10)
}


# plot by project
trackerProject <- function(data, selected_project){
  data %>% 
    mutate(siteName = sites[Site]) %>% 
    filter(Project==selected_project) %>% 
    ggplot(aes(Year, fct_reorder(siteName, desc(siteName))))+
    geom_tile(aes(fill=factor(level)), color="black", size=0.2)+
    scale_fill_manual(values=c("#c6eafb","#0d5388"), na.value = "white", breaks=c(0,1), labels=c(" Level 0", " Level 1"))+
    coord_equal()+
    ggtitle(selected_project)+
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position="right", # position of legend or none
          legend.direction="vertical", # orientation of legend
          legend.title= element_blank(), # no title for legend
          axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16)) 
}

# plot by site
trackerSite <- function(data, selected_site){
  
  uniqueProjects <- data %>% filter(Site==selected_site) %>% filter(!is.na(L0) | !is.na(L1)) %>% pull(Project) %>% unique()
  
  uniqueYears <- data %>% filter(Site==selected_site) %>% filter(!is.na(L0) | !is.na(L1)) %>% pull(Year) %>% unique()
  
  data %>% 
    filter(Site==selected_site) %>% 
    filter(Year %in% uniqueYears) %>% 
    filter(Project %in% uniqueProjects) %>% 
    ggplot(aes(x=Year, y=fct_reorder(Project, desc(Project))))+
    geom_tile(aes(fill=factor(level)), color="black", size=0.2)+
    scale_fill_manual(values=c("#c6eafb","#0d5388"), na.value = "white", breaks=c(0,1), labels=c(" Level 0", " Level 1"))+
    coord_equal(ratio = 1)+
    scale_x_continuous(breaks=uniqueYears)+
    ggtitle(sites[selected_site])+
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5, size=24),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position="none", # position of legend or none
          legend.direction="vertical", # orientation of legend
          legend.title= element_blank(), # no title for legend
          axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16)
    ) 
}