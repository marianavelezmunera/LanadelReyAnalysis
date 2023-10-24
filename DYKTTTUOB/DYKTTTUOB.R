#DYKTTTUOB


tabla_DYKTTTUOB<-subset(lanadata,Album=="DYKTTTUOB")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[9],height = 80))%>%
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
    style = cell_fill(color = "#F2DB94"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Paris, Texas (ft SYML)"))
tabla_DYKTTTUOB
gtsave_extra(tabla_DYKTTTUOB,"tabla_DYKTTTUOB.png")

# Pie charts

master_DYKTTTUOB<-nrow(subset(lanadata,Album=="DYKTTTUOB"&Letra=="MASTERPIECE"))
normal_DYKTTTUOB<-nrow(subset(lanadata,Album=="DYKTTTUOB"&Letra=="NORMAL"))
total_DYKTTTUOB<-nrow(subset(lanadata,Album=="DYKTTTUOB"))
percentage_master_DYKTTTUOB<-master_DYKTTTUOB/total_DYKTTTUOB
percentage_normal_DYKTTTUOB<-normal_DYKTTTUOB/total_DYKTTTUOB
ymax_DYKTTTUOB<-cumsum(c(percentage_master_DYKTTTUOB,percentage_normal_DYKTTTUOB))
ymin_DYKTTTUOB<-c(0,head(ymax_DYKTTTUOB,n=-1))

data_DYKTTTUOB<-data.frame(letra=c("master","normal"),percentage=c(percentage_master_DYKTTTUOB,percentage_normal_DYKTTTUOB),ymax_DYKTTTUOB,ymin_DYKTTTUOB)


labelposition_DYKTTTUOB<-(ymax_DYKTTTUOB+ymin_DYKTTTUOB)/2

#Pie chart

pie_DYKTTTUOB<-ggplot(data_DYKTTTUOB,aes(ymax=ymax_DYKTTTUOB,ymin=ymin_DYKTTTUOB,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_DYKTTTUOB,label=c("25%","75%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#F2DB94","#9CA0A6"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_DYKTTTUOB


bar_DYKTTTUOB<-ggplot(data = subset(skips,album=="DYKTTTUOB"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#F2DB94","#9CA0A6"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_DYKTTTUOB
DYKTTTUOB_stats<-bar_DYKTTTUOB+pie_DYKTTTUOB #combined plot 
ggsave("DYKTTTUOB_stats.png",plot=DYKTTTUOB_stats)#save plot 
