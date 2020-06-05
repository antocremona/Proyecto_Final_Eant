#Paquetes a importar
library(shiny)
library(DT)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(shinythemes)
library(leaflet)
library(htmltools)
library(rlang)
library(readxl)
library(stringr)
library(devtools)
library(rpivotTable)
library(shinycustomloader)

library(shinydashboard)

withLoader(plotOutput("distPlot"), type="html", loader='loader5')

library(shinycustomloader)


#Dataframe del mapa del delito
df_mapa_all= rbind(df_mapa1=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv',
                                     stringsAsFactors = F,
                                     encoding = 'UTF-8'),
                   df_mapa2=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv',
                                     stringsAsFactors = F,
                                     encoding = 'UTF-8'),
                   df_mapa3=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv',
                                     stringsAsFactors = F,
                                     encoding = 'UTF-8'),
                   df_mapa4=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2016.csv',
                                     stringsAsFactors = F,
                                     encoding = 'UTF-8')) %>% 
  mutate(fecha = as.Date(fecha),
         anio = as.numeric(format.Date(fecha, '%Y')))

#Dataframe de homicidios
df1 <- df_mapa_all %>% 
  filter(is.na(df_mapa_all$lat) == F,
         is.na(df_mapa_all$long) == F,
         str_detect(df_mapa_all$tipo_delito, 'Homicidio'))

#Resumen del mapa
df2 <- df_mapa_all %>% 
  group_by(anio,
           tipo_delito,
           subtipo_delito) %>% 
  summarise(cantidad = n())

#dataset de tobilleras electrónicas
df_tobilleras=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/tobilleras/tobilleras_limpio.csv',
                       stringsAsFactors = F,
                       encoding = 'UTF-8') %>% 
  mutate(Year=substr(año_mes_oficio,1,4))

#Shiny App
ui = fluidPage(theme = shinytheme('darkly'), 
               h1 (img 
                   (src='https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSDilWTQk6Twojy1Iy1K_FIbcmEjBVMH-RF_yYqZU8QOj_RA4uo&usqp=CAU',
                     height='50%', width='50%')), #imagen Eant
  br(),
  p('Proyecto Final - ', strong('EANT')),
  tabsetPanel(
    tabPanel('Información de delitos en CABA',
             navlistPanel(
               tabPanel('Listado de delitos',
                        br(),
                        selectInput(inputId = 'Tipo_delito',
                                    label = 'Tipo de delito',
                                    choices = unique(df_mapa_all$tipo_delito)),
                        dataTableOutput(outputId = 'Table_Tipo')
               ),
               tabPanel('Mapa de Homicidios en 2019',
                        br(),
                        leafletOutput(outputId = 'Mapa_delitos1')))
    ), #Fin Tab tipos de delitos
    tabPanel('Tobilleras Electrónicas',
             br(),
             fluidRow(
               column(2),
               column(8,
                      rpivotTableOutput(outputId = 'RPivot_Tobilleras')
               ),
               column(2)
             )) #Fin Tabla pivot Tobilleras
  ))

server = function(input, output){
  output$Table_Tipo=renderDataTable({
    df2 <- df_mapa_all %>% 
      group_by(anio,
               tipo_delito,
               subtipo_delito) %>% 
      summarise(cantidad = n())
  })
  
  output$Mapa_delitos1=renderLeaflet({
    df1 %>%
      filter(df1$anio==2019) %>% 
      leaflet() %>%
      addTiles() %>%
      addProviderTiles('Thunderforest.SpinalMap') %>%
      setView(lng = -58.3772316,
              lat = -34.6131516,
              zoom = 13) %>%
      addMarkers(lng = ~long,
                 lat = ~lat,
                 popup = ~ htmlEscape(paste(tipo_delito,' - ',subtipo_delito))) %>% 
      addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       color =ifelse(df1$subtipo_delito!='Doloso',"purple","red"),
                       clusterOptions = markerClusterOptions())
  })
  
  output$RPivot_Tobilleras=renderRpivotTable({
    rpivotTable(df_tobilleras, rows="categoria", col="estado_tobillera", aggregatorName="Count", vals="caso_id")
  })
}
shinyApp(ui=ui, server=server)