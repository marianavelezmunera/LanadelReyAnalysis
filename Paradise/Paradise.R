# Paradise
tabla_paradise<-subset(lanadata,Album=="Paradise")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[2],height = 60))%>%
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
    style = cell_fill(color = "#B3984B"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="Bel Air"))
tabla_paradise
gtsave_extra(tabla_paradise,"tabla_paradise.png")

# Pie charts

cringe_para<-nrow(subset(lanadata,Album=="Paradise"&Letra=="CRINGE"))
master_para<-nrow(subset(lanadata,Album=="Paradise"&Letra=="MASTERPIECE"))
normal_para<-nrow(subset(lanadata,Album=="Paradise"&Letra=="NORMAL"))
total_para<-nrow(subset(lanadata,Album=="Paradise"))
percentage_cringe_para<-cringe_para/total_para
percentage_master_para<-master_para/total_para
percentage_normal_para<-normal_para/total_para
ymax_para<-cumsum(c(percentage_cringe_para,percentage_master_para,percentage_normal_para))
ymin_para<-c(0,head(ymax_para,n=-1))

data_para<-data.frame(letra=c("cringe","master","normal"),percentage=c(percentage_cringe_para,percentage_master_para,percentage_normal_para),ymax_para,ymin_para)

labelposition_para<-(ymax_para+ymin_para)/2

#Pie chart

pie_para<-ggplot(data_para,aes(ymax=ymax_para,ymin=ymin_para,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_para,label=c("11%","56%","33%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#B3984B","#6D848C","#D9B779"),labels=c("Cringe","Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_para

percentage_normal_para

bar_paradise<-ggplot(data = subset(skips,album=="Paradise"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,2))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#B3984B","#6D848C"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_paradise
paradise_stats<-bar_paradise+pie_para #combined plot 
ggsave("paradise_stats.png",plot=paradise_stats)#save plot 
