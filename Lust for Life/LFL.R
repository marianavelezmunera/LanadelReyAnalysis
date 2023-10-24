#Lust for Life


tabla_lfl<-subset(lanadata,Album=="Lust for Life")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[3],height = 100))%>%
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
    style = cell_fill(color = "#B7D2DD"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Tomorrow Never Came (ft Sean Ono Lennon)"))
tabla_lfl
gtsave_extra(tabla_lfl,"tabla_lfl.png")

# Pie charts

master_lfl<-nrow(subset(lanadata,Album=="Lust for Life"&Letra=="MASTERPIECE"))
normal_lfl<-nrow(subset(lanadata,Album=="Lust for Life"&Letra=="NORMAL"))
total_lfl<-nrow(subset(lanadata,Album=="Lust for Life"))
percentage_master_lfl<-master_lfl/total_lfl
percentage_normal_lfl<-normal_lfl/total_lfl
ymax_lfl<-cumsum(c(percentage_master_lfl,percentage_normal_lfl))
ymin_lfl<-c(0,head(ymax_lfl,n=-1))

data_lfl<-data.frame(letra=c("master","normal"),percentage=c(percentage_master_lfl,percentage_normal_lfl),ymax_lfl,ymin_lfl)


labelposition_lfl<-(ymax_lfl+ymin_lfl)/2

#Pie chart

pie_lfl<-ggplot(data_lfl,aes(ymax=ymax_lfl,ymin=ymin_lfl,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_lfl,label=c("44%","56%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#B7D2DD","#8C1B2F"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_lfl


bar_lfl<-ggplot(data = subset(skips,album=="Lust for Life"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,2))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#B7D2DD","#8C1B2F"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_lfl
lfl_stats<-bar_lfl+pie_lfl #combined plot 
ggsave("lfl_stats.png",plot=lfl_stats)#save plot 
