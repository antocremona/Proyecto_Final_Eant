#Datasets de seguridad

#Dataset SUACI
df=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/sistema-unico-de-atencion-ciudadana/sistema-unico-de-atencion-ciudadana-2020.csv', 
            stringsAsFactors = F, 
            encoding = 'UTF-8')

#DF SUACI refinado
df0=df %>% 
  filter(fecha_ingreso>='2020-03-20'),
lat!='na',
long!='na',
str_detect(concepto,'CORONA') | str_detect(concepto,'MOSQUITO'))

df0=df %>% 
  filter(fecha_ingreso>='2020-03-20',
         lat!='na',
         long!='na',
         str_detect(concepto,'CORONA') | str_detect(concepto,'MOSQUITO'))

#dataset de tobilleras electrónicas
df_tobilleras_raw=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/tobilleras/tobilleras_limpio.csv',
                       stringsAsFactors = F,
                       encoding = 'UTF-8')

#tabla pivot de las tobilleras
df_tobilleras = df_tobilleras_raw %>% 
  mutate(Year=substr(año_mes_oficio,1,4))

rpivotTable(df_tobilleras, rows="categoria", col="estado_tobillera", aggregatorName="Count", vals="caso_id")

#dataset áreas de protección familiar
df_areaspf=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/areas-proteccion-familiar-policia-de-la-ciudad/areas-proteccion-familiar-policia-de-la-ciudad.csv',
                    stringsAsFactors = F,
                    encoding = 'UTF-8')

#mapa de las áreas de protección
df_areaspf %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles('Thunderforest.SpinalMap') %>%
  setView(lng = -58.3772316,
          lat = -34.6131516,
          zoom = 13) %>%
  addMarkers(lng = ~long,
             lat = ~lat) %>% 
  addCircleMarkers(lng = ~long,
                   lat = ~lat,
                   popup = ~ htmlEscape(nombre_comisaria),
                   clusterOptions = markerClusterOptions())




df2 <- df_mapa_all %>% 
  group_by(anio,
           tipo_delito,
           subtipo_delito) %>% 
  summarise(cantidad = n())

df1 <- head(df_mapa_all) %>%
  mutate(anio = as.numeric(format.Date(fecha, '%Y')))
  group_by(anio) %>% 
  count(tipo_delito,subtipo_delito)

df2 %>% 
  ggplot(aes(x=Delito, fill = Delito)) + 
  geom_bar(show.legend = T) +
  facet_wrap(~Año) +
  
  