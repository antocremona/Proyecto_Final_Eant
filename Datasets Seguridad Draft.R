#Datasets de seguridad

#dataset de tobilleras electr�nicas
df_tobilleras_raw=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/tobilleras/tobilleras_limpio.csv',
                       stringsAsFactors = F,
                       encoding = 'UTF-8')

#tabla pivot de las tobilleras
df_tobilleras = df_tobilleras_raw %>% 
  mutate(Year=substr(a�o_mes_oficio,1,4))

rpivotTable(df_tobilleras, rows="categoria", col="estado_tobillera", aggregatorName="Count", vals="caso_id")

#dataset �reas de protecci�n familiar
df_areaspf=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/areas-proteccion-familiar-policia-de-la-ciudad/areas-proteccion-familiar-policia-de-la-ciudad.csv',
                    stringsAsFactors = F,
                    encoding = 'UTF-8')

#mapa de las �reas de protecci�n
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
                   popup = ~ htmlEscape(nombre_comisaria))


#mapa del delito 2019
'http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv'

#mapa del delito 2018
'http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv'

#mapa del delito 2017
'http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv'

#mapa del delito 2016
'http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2016.csv'