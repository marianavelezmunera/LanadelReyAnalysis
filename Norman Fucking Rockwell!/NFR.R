#Norman Fucking Rockwell!


tabla_nfr<-subset(lanadata,Album=="Norman Fucking Rockwell!")[,-2] %>%
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
  gt::tab_header(title = add_text_img("",url=info_completa$url[1],height = 100))%>%
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
    style = cell_fill(color = "#A3D930"),
    locations = cells_body(
      columns = everything(),
      rows = Canción=="The Greatest"))
tabla_nfr
gtsave_extra(tabla_nfr,"tabla_nfr.png")

# Pie charts

master_nfr<-nrow(subset(lanadata,Album=="Norman Fucking Rockwell!"&Letra=="MASTERPIECE"))
normal_nfr<-nrow(subset(lanadata,Album=="Norman Fucking Rockwell!"&Letra=="NORMAL"))
total_nfr<-nrow(subset(lanadata,Album=="Norman Fucking Rockwell!"))
percentage_master_nfr<-master_nfr/total_nfr
percentage_normal_nfr<-normal_nfr/total_nfr
ymax_nfr<-cumsum(c(percentage_master_nfr,percentage_normal_nfr))
ymin_nfr<-c(0,head(ymax_nfr,n=-1))

data_nfr<-data.frame(letra=c("master","normal"),percentage=c(percentage_master_nfr,percentage_normal_nfr),ymax_nfr,ymin_nfr)


labelposition_nfr<-(ymax_nfr+ymin_nfr)/2

#Pie chart

pie_nfr<-ggplot(data_nfr,aes(ymax=ymax_nfr,ymin=ymin_nfr,xmax=4,xmin=3,fill=letra))+
  geom_rect(colour="black")+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.text = element_text(size=20,family="Bebas Neue"))+
  geom_text(x=3.5,size=8,aes(y=labelposition_nfr,label=c("79%","21%"),family="Bebas Neue"))+
  scale_fill_manual(values=c("#A3D930","#D79936"),labels=c("Masterpiece","Normal"),name="LA LETRA ES:")+
  theme(legend.title = element_text(family="Bebas Neue",size = 24))
pie_nfr


bar_nfr<-ggplot(data = subset(skips,album=="Norman Fucking Rockwell!"),aes(x=skips,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black",width = 0.25)+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  xlab("¿SKIP?")+
  scale_y_continuous(breaks = seq(0,15,2))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#A3D930","#D79936"))+
  theme(axis.title = element_text(family = "Bebas Neue",size = 24))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(aspect.ratio = 1)
bar_nfr
nfr_stats<-bar_nfr+pie_nfr #combined plot 
ggsave("nfr_stats.png",plot=nfr_stats)#save plot 
