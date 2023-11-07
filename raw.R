library(tidyverse)

library(tidycensus)
library(sf)
thematics<-theme_bw()+theme(legend.position = "top",
                            axis.text = element_text(color="black",size=14),
                            axis.title = element_text(face=2,size=18),
                            plot.title = element_text(face=2,size=20),
                            legend.title = element_text(face=2),
                            plot.caption = element_text(face=1,size=10),
                            panel.border = element_blank(),
                            axis.line = element_line(color="black",linewidth =.25),
                            strip.text = element_text(face=2,size=16),
                            title = element_text(face = 2),
                            panel.grid = element_blank(),
                            strip.background = element_blank(),
                            text = element_text(family = "sans"))
census_api_key("d742d8e5eb30fe5c5289163a40ad46900e073476")

varsOfInt<-load_variables(2020,"dhc")



sexM<-tibble(sex=c("Male"),vCode=as.character(c(str_pad(seq(2,20,1),width = 3,side = "left",pad = 0))))

sexF<-tibble(sex=c("Female"),vCode=as.character(c((seq(106,124,1)))))

sex<-rbind(sexM,sexF)


raceEthni<-tribble(~rGroup,~code,
  "WHITE ALONE, NOT HISPANIC OR LATINO", "I",
  "TWO OR MORE RACES, NOT HISPANIC OR LATINO", "O",
  "SOME OTHER RACE ALONE, NOT HISPANIC OR LATINO", "N",
  "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE, NOT HISPANIC OR LATINO","M",
  "HISPANIC OR LATINO", "H",
  "BLACK OR AFRICAN AMERICAN ALONE, NOT HISPANIC OR LATINO", "J",
  "ASIAN ALONE, NOT HISPANIC OR LATINO","L",
  "AMERICAN INDIAN AND ALASKA NATIVE ALONE, NOT HISPANIC OR LATINO","K"
  )


allRGroupCodes<-crossing(raceEthni,sex) %>% 
  mutate(fullCode=paste0("PCT12",code,"_",vCode,"N"))

getNH<-get_decennial(geography = "tract",
                     variables = allRGroupCodes$fullCode,year = 2020,sumfile = "dhc",geometry = F,state = "NH")


nhBound<-get_decennial(geography = "tract",
                       variables = "PCT12I_002N",year = 2020,sumfile = "dhc",geometry = T,state = "NH") %>% 
  select(GEOID, geometry   )


nh_desc<-getNH %>% left_join(varsOfInt,by=c("variable"="name")) %>% 
  left_join(allRGroupCodes,by=c("variable"="fullCode"))


nh_under_18<-nh_desc %>% 
  filter(!str_detect(variable,"002|106")) %>% 
  group_by(GEOID,NAME,rGroup) %>% 
  summarise(pop_under_18=sum(value)) %>% 
  group_by(GEOID,NAME) %>% 
  mutate(rGroupProp=pop_under_18/sum(pop_under_18),
         totalPop=sum(pop_under_18),
         fillTiny=if_else(pop_under_18==0,1e-70,rGroupProp),
         shannonD=-sum(fillTiny*log(fillTiny)),
         simpsonsD=1-sum(pop_under_18*(pop_under_18-1))/(totalPop*(totalPop-1)))


nhDiv<-nh_under_18 %>% select(GEOID,NAME,shannonD,simpsonsD) %>% distinct() %>% 
  inner_join(nhBound) %>% st_as_sf() %>% 
  st_transform(crs="EPSG:4326") %>% 
  mutate(simpsonsD=replace_na(simpsonsD,0),
         simpsonsD=if_else(GEOID%in%c(33015980011,33011980101),0,simpsonsD))

nhDiv %>% ggplot()+geom_sf(aes(fill=simpsonsD,geometry=geometry),color="black",inherit.aes = T)+
  scale_fill_viridis_c()+thematics


library(leaflet)

palCols <- colorNumeric( palette="viridis", domain=nhDiv$simpsonsD, na.color=NA,reverse = F,alpha = T)


leaflet(nhDiv) %>% addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(stroke = T, smoothFactor = 0.2, fillOpacity = .8,weight = 1,
              fillColor  = ~palCols(simpsonsD),color = "black",
              popup = paste0("Tract: ",nhDiv$NAME,"<br>Index Value:",round(nhDiv$simpsonsD,3))) %>% 
  addLegend( pal=palCols,values =nhDiv$simpsonsD , opacity=0.9, title = "<b style=\"color:black;\">Population Diversity <br>Simpson's Diversity Index</b>", position = "bottomleft")


fullUnder18<-nh_under_18 %>% distinct() %>% 
  inner_join(nhBound) %>% st_as_sf() %>% 
  st_transform(crs="EPSG:4326") %>% 
  select(-fillTiny) %>% 
  mutate(simpsonsD=if_else(GEOID%in%c(33015980011,33011980101),0,simpsonsD))


fullUnder18 %>% filter(rGroup=="BLACK OR AFRICAN AMERICAN ALONE, NOT HISPANIC OR LATINO") %>% ggplot()+geom_sf(aes(fill=rGroupProp,geometry=geometry),color="black",inherit.aes = T)+
  scale_fill_viridis_c()+facet_wrap(~rGroup)


write_sf(fullUnder18,"allTractsFull.shp")


