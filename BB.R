#Blue Banisters


tabla_bb<-subset(lanadata,Album=="Blue Banisters")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[6],height = 30))%>%
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
    style = cell_fill(color = "#8C5C4A"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="If You Lie Down with Me"))
tabla_bb
gtsave_extra(tabla_bb,"tabla_bb.png")

# Pie charts

master_bb<-nrow(subset(lanadata,Album=="Blue Banisters"&Letra=="MASTERPIECE"))
normal_bb<-nrow(subset(lanadata,Album=="Blue Banisters"&Letra=="NORMAL"))
total_bb<-nrow(subset(lanadata,Album=="Blue Banisters"))
percentage_master_bb<-master_bb/total_bb
percentage_normal_bb<-normal_bb/total_bb
ymax_bb<-cumsum(c(percentage_master_bb,percentage_normal_bb))
ymin_bb<-c(0,head(ymax_bb,n=-1))

data_bb<-data.frame(letra=c("master","normal"),percentage=c(percentage_master_bb,percentage_normal_bb),ymax_bb,ymin_bb)


labelposition_bb<-(ymax_bb+ymin_bb)/2

#Pie chart

pie_bb<-ggplot(data_bb,aes(ymax=ymax_bb,ymin=ymin_bb,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_bb,label=c("53%","47%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#8C5C4A","#D3D7B6"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_bb


bar_bb<-ggplot(data = subset(skips,album=="Blue Banisters"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,2))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#8C5C4A","#D3D7B6"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_bb
bb_stats<-bar_bb+pie_bb #combined plot 
ggsave("bb_stats.png",plot=bb_stats)#save plot 
