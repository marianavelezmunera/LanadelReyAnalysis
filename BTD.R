# BTD
tabla_btd<-subset(lanadata,Album=="Born to Die")[,-2] %>%
  gt()%>%
  cols_label(Canción="Canción",Puntaje="Puntaje",Skip="Skip",Meidentifico="Me identifico")%>%
  opt_all_caps()%>%
  opt_table_font(font = list(google_font("Bebas Neue"),default_fonts())) %>%
  tab_style(
    style = cell_borders(
      sides = c("left","right"),
      weight = px(2),color="black"
    ),
    locations = cells_body()
  )%>%
  tab_style(
    style = cell_borders(
      sides=c("bottom","top"),color="transparent"),
    locations = cells_body()
  ) %>%
  tab_options(table_body.border.bottom.color = "black") %>%
  tab_options(column_labels.font.size = px(15)) %>%
  cols_align("center")%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "all", weight = px(2)),
      #Make text bold
      cell_text(weight = "bold")
    ))%>%
  gt::tab_header(title = add_text_img("",url=info_completa$url[8],height = 60))%>%
  tab_style(
    locations = cells_title("title"),
    style = list(
      cell_borders(sides="all"))) %>%
  tab_style(
    locations = cells_title("title"),
    style = list(
      cell_borders(sides=c("top","right","left"),color="transparent")
    )
  )%>%
  tab_options(table.border.top.style = "hidden")%>%
  tab_style(
    style = cell_fill(color = "#6B90BF"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Video Games"))
tabla_btd
gtsave_extra(tabla_btd,"tabla_BTD.png")

# Pie charts

cringe_btd<-nrow(subset(lanadata,Album=="Born to Die"&Letra=="CRINGE"))
master_btd<-nrow(subset(lanadata,Album=="Born to Die"&Letra=="MASTERPIECE"))
normal_btd<-nrow(subset(lanadata,Album=="Born to Die"&Letra=="NORMAL"))
total_btd<-nrow(subset(lanadata,Album=="Born to Die"))
percentage_cringe_btd<-cringe_btd/total_btd
percentage_master_btd<-master_btd/total_btd
percentage_normal_btd<-normal_btd/total_btd

ymax<-cumsum(c(percentage_cringe_btd,percentage_master_btd,percentage_normal_btd))
ymin<-c(0,head(ymax,n=-1))

data_btd<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_btd,percentage_master_btd,percentage_normal_btd),ymax,ymin)

labelposition<-(ymax+ymin)/2 #label positions in the pie chart
#There were no masterpieces in btdut, so I subseted the data to avoid adding a master box in the legend when there weren't any masterpieces

#Pie chart

pie_btd<-ggplot(data_btd,aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition,label=c("33%","40%","27%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#6B90BF","#D9D6D2","#592722"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_btd

bar_btd<-ggplot(data = subset(skips,album=="Born to Die"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#6B90BF","#D9D6D2"))+
  theme(axis.title = element_text(family = "Bebas Neue",face = "bold",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_btd
btd_stats<-bar_btd+pie_btd #combined plot 
ggsave("btd_stats.png",plot=btd_stats)#save plot 
