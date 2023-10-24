#Ultraviolence

tabla_ultra<-subset(lanadata,Album=="Ultraviolence")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[7],height = 60))%>%
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
    style = cell_fill(color = "#89888C"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Old Money"))
tabla_ultra
gtsave_extra(tabla_ultra,"tabla_ultra.png")

# Pie charts

master_ultra<-nrow(subset(lanadata,Album=="Ultraviolence"&Letra=="MASTERPIECE"))
normal_ultra<-nrow(subset(lanadata,Album=="Ultraviolence"&Letra=="NORMAL"))
total_ultra<-nrow(subset(lanadata,Album=="Ultraviolence"))
percentage_master_ultra<-master_ultra/total_ultra
percentage_normal_ultra<-normal_ultra/total_ultra
ymax_ultra<-cumsum(c(percentage_master_ultra,percentage_normal_ultra))
ymin_ultra<-c(0,head(ymax_ultra,n=-1))

data_ultra<-data.frame(letra=c("master","normal"),percentage=c(percentage_master_ultra,percentage_normal_ultra),ymax_ultra,ymin_ultra)


labelposition_ultra<-(ymax_ultra+ymin_ultra)/2

#Pie chart

pie_ultra<-ggplot(data_ultra,aes(ymax=ymax_ultra,ymin=ymin_ultra,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_ultra,label=c("21%","79%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#89888C","#D9D9D9"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_ultra

bar_ultra<-ggplot(data = subset(skips,album=="Ultraviolence"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,3))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#89888C","#D9D9D9"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_ultra
ultra_stats<-bar_ultra+pie_ultra #combined plot 
ggsave("ultra_stats.png",plot=ultra_stats)#save plot 
