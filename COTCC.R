#Chemtrails over the Country Club


tabla_cotcc<-subset(lanadata,Album=="Chemtrails over the Country Club")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[5],height = 80))%>%
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
    style = cell_fill(color = "#BFBFBF"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Wild at Heart"))
tabla_cotcc
gtsave_extra(tabla_cotcc,"tabla_cotcc.png")

# Pie charts

master_cotcc<-nrow(subset(lanadata,Album=="Chemtrails over the Country Club"&Letra=="MASTERPIECE"))
normal_cotcc<-nrow(subset(lanadata,Album=="Chemtrails over the Country Club"&Letra=="NORMAL"))
total_cotcc<-nrow(subset(lanadata,Album=="Chemtrails over the Country Club"))
percentage_master_cotcc<-master_cotcc/total_cotcc
percentage_normal_cotcc<-normal_cotcc/total_cotcc
ymax_cotcc<-cumsum(c(percentage_master_cotcc,percentage_normal_cotcc))
ymin_cotcc<-c(0,head(ymax_cotcc,n=-1))

data_cotcc<-data.frame(letra=c("master","normal"),percentage=c(percentage_master_cotcc,percentage_normal_cotcc),ymax_cotcc,ymin_cotcc)


labelposition_cotcc<-(ymax_cotcc+ymin_cotcc)/2

#Pie chart

pie_cotcc<-ggplot(data_cotcc,aes(ymax=ymax_cotcc,ymin=ymin_cotcc,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_cotcc,label=c("45%","55%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#BFBFBF","#F2F2F2"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_cotcc


bar_cotcc<-ggplot(data = subset(skips,album=="Chemtrails over the Country Club"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,2))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#BFBFBF","#F2F2F2"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_cotcc
cotcc_stats<-bar_cotcc+pie_cotcc #combined plot 
ggsave("cotcc_stats.png",plot=cotcc_stats)#save plot 
