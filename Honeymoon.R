#Honeymoon


tabla_honey<-subset(lanadata,Album=="Honeymoon")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[4],height = 60))%>%
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
    style = cell_fill(color = "#C2E5F2"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Terrence Loves You"))
tabla_honey
gtsave_extra(tabla_honey,"tabla_honey.png")

# Pie charts

master_honey<-nrow(subset(lanadata,Album=="Honeymoon"&Letra=="MASTERPIECE"))
normal_honey<-nrow(subset(lanadata,Album=="Honeymoon"&Letra=="NORMAL"))
total_honey<-nrow(subset(lanadata,Album=="Honeymoon"))
percentage_master_honey<-master_honey/total_honey
percentage_normal_honey<-normal_honey/total_honey
ymax_honey<-cumsum(c(percentage_master_honey,percentage_normal_honey))
ymin_honey<-c(0,head(ymax_honey,n=-1))

data_honey<-data.frame(letra=c("master","normal"),percentage=c(percentage_master_honey,percentage_normal_honey),ymax_honey,ymin_honey)


labelposition_honey<-(ymax_honey+ymin_honey)/2

#Pie chart

pie_honey<-ggplot(data_honey,aes(ymax=ymax_honey,ymin=ymin_honey,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_honey,label=c("57%","43%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#C2E5F2","#1C4AA6"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_honey


bar_honey<-ggplot(data = subset(skips,album=="Honeymoon"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,2))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#C2E5F2","#1C4AA6"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_honey
honey_stats<-bar_honey+pie_honey #combined plot 
ggsave("honey_stats.png",plot=honey_stats)#save plot 
