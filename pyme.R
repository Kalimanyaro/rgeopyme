##################################################
## Proyecto: Analsis datos Pymes
## Objetivo Script: Procesar con Coogle Maps API las direcciones de los clientes
## Fecha: junio 2017
## Autor: Juan Fernando Correa
##################################################
#rm(list=ls(all=TRUE))

# LIbrerias ----
library(tidyverse)
library(ggmap)
library(stringr)
library(lubridate)
library(viridis)
geocodeQueryCheck()

# types of output
# geocode("parque del perro Cali Colombia", output = "latlona")
# geocode("parque del perro Cali Colombia", output = "more")
# geocode("calle 3 oeste #35-33 cali colombia", output = "more")
# str(geocode("parque del perro Cali Colombia", output = "all"))


# cargar datos ----


ventas_area_orf <- read_csv("./data/ventas_area-orf.csv",
col_types = cols(FECHA = col_date(format = "%Y-%m-%d")))


# inspeccionar datos 
errores<-c("<U+00B4>","<U+00D1>", "<U+00D2>",  "<U+00C1>", "<U+00BA>","<U+00CD>", "<U+00C9>", "<U+00DA>","<U+00A5>")
cambios<-c("'","Ñ","O","A","O","I","E","U","Ñ")

# ventas_area_orf$CLIENTE %>% class()

#Pruebas de deteccion
# str_detect(ventas_area_orf$CLIENTE,coll("<U+00B4>")) %>% ventas_area_orf[.,]
# str_detect(ventas_area_orf$CLIENTE,"<U\\+00D1>") %>% ventas_area_orf[.,]
# str_detect(ventas_area_orf$CLIENTE,"") %>% ventas_area_orf[.,]
# 
# ventas_area_orf$DIRECCION %>%
#   tolower() %>% 
#   str_detect("apto|piso") %>% 
#   ventas_area_orf[.,"DIRECCION"]


# Crear varibles para modificaciones
ventas_area_orf$cliente_mod<-ventas_area_orf$CLIENTE
ventas_area_orf$dir_mod<-ventas_area_orf$DIRECCION
ventas_area_orf$ciudad_mod<-ventas_area_orf$CIUDAD
# Hacer sustituciones de los carateres extraños
# sustituir_chars<-function(ventas,err, sus){
for(i in 1:length(cambios)){
  print(errores[i])
  ventas_area_orf$cliente_mod<-str_replace_all(ventas_area_orf$cliente_mod,coll(errores[i]),cambios[i])
  ventas_area_orf$dir_mod<-str_replace_all(ventas_area_orf$dir_mod,coll(errores[i]),cambios[i]) 
  ventas_area_orf$ciudad_mod<-str_replace_all(ventas_area_orf$ciudad_mod,coll(errores[i]),cambios[i])
}  
# }
ventas_area_orf$ciudad_mod<-ventas_area_orf$ciudad_mod %>% tolower()


# modificaciones a dir para mejorar geocodificacion
ventas_area_orf$dir_mod %>% tolower() %>% 
  str_replace_all(coll("of.215 cencar"),"cencar") %>%
  str_replace_all("dappa","dapa") %>%
  str_replace_all("int\\.|int\\.\\d","") %>%
  str_replace_all("centro cial\\.|c\\.cial\\.","centro comercial ") %>%
  str_replace_all("urbanizacion mameyal","") %>%
  str_replace_all("sexta|6ta\\."," 6 ") %>%
  str_replace_all("2da\\.|2da "," 2 ") %>%
  str_replace_all("3ra\\.|3ra "," 3ra ") %>%
  str_replace_all(coll("treminal "),"terminal ") %>%
  str_replace_all(coll("of.814 holguines trade center"),"holguines trade center") %>%
  str_replace_all("kil\\.|kim\\.|kilometro\\.|km |kilom\\.|kilometro","km. ") %>%
  str_split_fixed("apto|apto |piso|casa|apt|ap |parcel|local|oficina|of.|b/|l\\.\\d\\d|manz\\.|t\\.|xd",2) %>% .[,1] %>% 
  str_replace_all("av\\.|avda\\.|av ","avenida ") %>%
  str_replace_all("cl\\.|cl ","calle ") %>%
  str_replace_all("kra\\.|cra\\.|cra","carrera ") %>%
  str_replace_all("no\\.|no\\:|no |n<o\\.|noo","# ") %>%
  str_replace_all("nt\\.|nt |norte norte|norte "," nte ") %>%
  str_replace_all("\\s+"," ") %>% str_trim()->ventas_area_orf$dir_mod2
  # %>% 
  # unique() %>% sort()

# ventas_area_orf$dir_mod2 %>% is.na() %>% ventas_area_orf$dir_mod2[.]
# ventas_area_orf %>% drop_na(dir_mod) %>% nrow()
# ventas_area_orf %>% nrow()

#eliminamos ventas sin direccion para el analisis espacial. Es prudente
#revisar los montos de esas ventas y verificar si se puede completar esa info
ventas_area_geo<-ventas_area_orf %>% drop_na(dir_mod) %>% select(id_venta,cliente_mod,
                                                                  tipo_cliente=`TIPO DE CLIENTE`,
                                                                  dir_mod,
                                                                  dir_mod2,
                                                                  ciudad_mod,
                                                                  fecha=FECHA,
                                                                  vendedor=VENDEDOR,
                                                                  valor_factura=`VR TOTAL`,
                                                                  descuento=`VR DESCUENTO`,
                                                                  iva=`VR IVA`)


# Estadisticas por cliente
ventas_area_geo %>% mutate(address_str=paste(dir_mod2,ciudad_mod,"colombia",sep=" , "))%>%
  group_by(cliente_mod,tipo_cliente,dir_mod2,ciudad_mod,address_str) %>%
  summarize(n=n(),
            total_ventas=sum(valor_factura),
            total_descuento=sum(descuento),
            total_iva=sum(iva)) %>% 
  arrange(desc(total_ventas)) -> clientes_dir


summary(clientes_dir)
head(clientes_dir)

# ventas por cliente geocodificadas
clientes_dir%>% as.data.frame()%>% 
mutate_geocode(address_str,output = "latlona")->clientes_geocoded 

clientes_geocoded %>% summary()

#ventas individuales geocodificadas
ventas_area_geocoded<-ventas_area_geo %>% 
  inner_join(select(clientes_geocoded,
                    cliente_mod:address_str,
                    lon:address),
             by = c("cliente_mod","dir_mod2","ciudad_mod","tipo_cliente")) 

valle<-c(-77.3616,2.4957,-75.5132,4.4507)
valle[1]

ventas_area_geocoded%>%
  filter(ciudad_mod=="cali" & str_detect(address,"cali")) %>% 
  filter(valle[1] <= lon & lon <= valle[3],
         valle[2] <= lat & lat <=  valle[4]) ->ventas_cali

#estadisticas por cliente_tipo

# graficas estadisticas

ggplot(clientes_dir, aes(x=total_ventas))+geom_histogram(bins = 100) 

clientes_dir %>% 
  select(cliente_mod,ciudad_mod,total_ventas)%>%head(70) %>%
ggplot( aes(x= reorder( cliente_mod,-total_ventas),total_ventas, fill=tipo_cliente))+
  geom_bar(stat = "identity")+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Top 70 clientes volumen de ventas")

ggplot(ventas_area_geo, aes(x=valor_factura))+geom_histogram(bins = 100)

ventas_area_geo %>% filter(valor_factura > 1000000) %>% .$cliente_mod %>% unique()

ggplot(ventas_area_geo, aes(x=tipo_cliente,y=valor_factura))+
  geom_boxplot() + coord_flip() 
ggplot(ventas_area_geo, aes(x=tipo_cliente,y=valor_factura, color= tipo_cliente))+
geom_jitter(position = position_jitter(0.2),alpha=0.2)+ 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "blue")+
  coord_flip()+ scale_color_brewer(palette="Dark2") +theme_minimal()

ggplot(ventas_area_geo, aes(x=tipo_cliente,y=valor_factura, color= tipo_cliente))+
  geom_jitter(position = position_jitter(0.2),alpha=0.2)+ 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "blue")+
  coord_flip()+ 
  scale_color_brewer(palette="Dark2") +
  theme_minimal()+
  facet_wrap(~ciudad_mod,ncol = 3)

#en el tiempo

ggplot(ventas_area_geo,aes(x=fecha,y=valor_factura,color=tipo_cliente))+
  geom_point(alpha=0.5,size=1)+
  facet_wrap(~tipo_cliente)

ventas_area_geo
ventas_area_geo$sdia<- wday(ventas_area_geo$fecha, label = T)
ventas_area_geo$sem<-week(ventas_area_geo$fecha)
ventas_area_geo$ano<-year(ventas_area_geo$fecha)

ventas_area_geo$sdia %>% levels() 
ventas_fecha$fecha %>% day()

ventas_fecha_tipo_ciudad<-ventas_area_geo %>% 
  group_by(tipo_cliente,fecha) %>%
  summarise(n=n(),ventas=sum(valor_factura)) %>%
  mutate(sdia=wday(fecha, label = T),sem=week(fecha),ano=year(fecha))

ventas_fecha<-ventas_area_geo %>% 
  group_by(fecha) %>%
  summarise(n=n(),ventas=sum(valor_factura)) %>%
  mutate(sdia=wday(fecha, label = T),sem=week(fecha),ano=year(fecha))

ggplot(ventas_fecha_tipo_ciudad) + 
  geom_tile(aes(x=sem, y=sdia, fill = ventas), colour = "white") + 
  scale_fill_viridis(direction = -1,option = "D")+
  theme_grey()+
  scale_x_continuous(expand=c(0.01,0.01)) + 
  # scale_fill_gradientn(colours = c("#D61818","#FFAE63","#FFFFBD","#B5E384"), na.value='transparent')+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),         # remove axis titles
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), # remove gridlines
        legend.title=element_blank(),                                       # remove legend title
        axis.text.x=element_blank(), axis.ticks.x=element_blank()           # remove x-axis labels and ticks
  )+coord_equal()+
  facet_grid(ano~tipo_cliente)

ggplot(ventas_fecha) + 
  geom_tile(aes(x=sem, y=sdia, fill = ventas), colour = "white") + 
  scale_fill_viridis(direction = -1,option = "D")+
  theme_grey()+
  scale_x_continuous(expand=c(0.01,0.01)) + 
  # scale_fill_gradientn(colours = c("#D61818","#FFAE63","#FFFFBD","#B5E384"), na.value='transparent')+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),         # remove axis titles
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), # remove gridlines
        legend.title=element_blank(),                                       # remove legend title
        axis.text.x=element_blank(), axis.ticks.x=element_blank()           # remove x-axis labels and ticks
  )+coord_equal()+
  facet_wrap(~ano,ncol = 1)

week2 <- function(d, year) { 
  # If January 1st is a Sunday, my weeks will start from 1 instead of 0 like the rest of them. 
  nyd <- as.Date(ISOdate(year, 1, 1))
  # So if that's the case, subtract 1. 
  as.integer(format(d, '%U')) - ifelse(wday(nyd) == 1, 1, 0)
}

calendar_tetris_data <- function(date_min, date_max) {
  
  start <- as.Date(ISOdate(year(date_min),1,1))
  end <- as.Date(ISOdate(year(date_max), 12, 31))
  
  all.dates <- start + 0:as.integer(end - start, units='days')
  
  data.frame(date=all.dates) %>% as_tibble() %>% 
    mutate(
      wday=wday(date),
      sday=wday(date,label = T),
      year=year(date),
      month=month(date),
      week=week2(date,year),
      day=day(date),
      # (a) put vertical lines to the left of the first week of each month
      x=ifelse(day <= 7, week - 0.5, NA),
      ymin=ifelse(day <= 7, wday - 0.5, NA),
      ymax=ifelse(day <= 7, wday + 0.5, NA),
      # (b) put a horizontal line at the bottom of the first of each month
      y=ifelse(day == 1, wday - 0.5, NA),
      xmin=ifelse(day == 1, week - 0.5, NA),
      xmax=ifelse(day == 1, week + 0.5, NA),
      # (c) in december, put vertical lines to the right of the last week
      dec.x=ifelse(month==12 & day >= 25, week + 0.5, NA),
      dec.ymin=ifelse(month==12 & day >= 25, wday - 0.5, NA),
      dec.ymax=ifelse(month==12 & day >= 25, wday + 0.5, NA),
      # (d) put a horizontal line at the top of New Years Eve
      nye.y=ifelse(month==12 & day == 31, wday + 0.5, NA),
      nye.xmin=ifelse(month==12 & day == 31, week - 0.5, NA),
      nye.xmax=ifelse(month==12 & day == 31, week + 0.5, NA),
      # (e) put the first letter of the month on the first day
      month.x=ifelse(day == 1, week, NA),
      month.y=ifelse(day == 1, sday, NA),
      month.l=ifelse(day == 1, substr(format(date, '%B'), 1, 3), NA)
    )
}


#calgrid<-calendar_tetris_data(min(ventas_fecha$fecha), max(ventas_fecha$fecha))
# calendar_tetris_data(min(ventas_fecha$fecha), max(ventas_fecha$fecha))%>% 
#   left_join(ventas_fecha,by=c("date"="fecha")) %>% View

calendar_tetris_data(min(ventas_fecha$fecha), max(ventas_fecha$fecha))%>% 
  left_join(ventas_fecha,by=c("date"="fecha")) %>% 
  ggplot() + 
  geom_tile(aes(x=week, y=sday, fill = ventas), colour = "grey") + 
  #scale_fill_gradientn(colours = c("#D61818","#FFAE63","#FFFFBD","#B5E384"), na.value='transparent') +
  scale_fill_viridis(na.value="transparent", option = "viridis", direction = -1)+
  geom_segment(aes(x=x, xend=x, y=ymin, yend=ymax)) +                       # (a)
  geom_segment(aes(x=xmin, xend=xmax, y=y, yend=y)) +                       # (b)
  geom_segment(aes(x=dec.x, xend=dec.x, y=dec.ymin, yend=dec.ymax)) +       # (c)
  geom_segment(aes(x=nye.xmin, xend=nye.xmax, y=nye.y, yend=nye.y)) +       # (d)
  geom_segment(x=-0.5, xend=51.5, y=7.5, yend=7.5) +                        # put a line along the top
  geom_segment(x=0.5, xend=52.5, y=0.5, yend=0.5) +                         # put a line along the bottom
  geom_text(aes(x=month.x, y=month.y, label=month.l), hjust=0.25) +         # (e)
  scale_x_continuous(expand=c(0.01,0.01)) +                                 # remove excessive left+right padding 
  coord_equal()+theme_classic()+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),         # remove axis titles
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), # remove gridlines 
        legend.title=element_blank(),                                       # remove legend title
        axis.text.x=element_blank(), axis.ticks.x=element_blank()           # remove x-axis labels and ticks
  ) + 
  facet_wrap(~ year, ncol = 1) 


# Graficar datos de clientes geocodificados
clientes_geocoded %>% 
qmplot(data=. ,lon, lat, maptype = "toner-lite", color = "red",size=total_ventas)


# Graficar ventas Cali


ventas_cali %>% 
  qmplot(data= .,lon, lat, maptype = "toner-lite", color = tipo_cliente) +
  facet_wrap(~tipo_cliente)

ventas_cali %>% 
  qmplot(data= .,lon, lat, maptype = "toner-lite", geom="blank")+
  facet_wrap(~tipo_cliente)


ventas_cali %>% 
  qmplot(data= .,lon, lat, maptype = "toner-lite", geom="blank")+ 
  #geom_density2d(data =ventas_cali ,aes(x = lon, y = lat), size = 0.3) +
  stat_density2d(data = ventas_cali,  
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_viridis(option = "plasma", direction = 1) + 
  scale_alpha(range = c(0.1, 0.4), guide = FALSE)+
  facet_wrap(~tipo_cliente)

  qmplot(data= ventas_cali,lon, lat, maptype = "toner-lite", geom="blank") +
  geom_point(alpha=0.3,color="turquoise3",aes(size=valor_factura,color=tipo_cliente))
  
  
  qmplot(data= ventas_cali,lon, lat, maptype = "toner-lite", geom="blank") +
    geom_point(alpha=0.3,aes(size=valor_factura,color=tipo_cliente))+
    facet_wrap(~tipo_cliente)



