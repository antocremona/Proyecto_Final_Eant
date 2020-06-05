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
                                     encoding = 'UTF-8'))

#Dataframe editado
df0 = df_mapa_all %>% 
  mutate(fecha = as.Date(df_mapa_all$fecha),
         Año = as.numeric(format.Date(df_mapa_all$fecha, '%Y'))) %>% 
  select(-id,
         fecha, 
         Año,
         Horario = franja_horaria,
         Delito = tipo_delito,
         Subtipo = subtipo_delito,
         lat,
         long,
         Comuna = comuna,
         Barrio = barrio)

rm(df_mapa_all)

#Dataframe de homicidios
df1 <- df0 %>% 
  filter(is.na(df0$lat) == F,
         is.na(df0$long) == F,
         str_detect(df0$Delito, 'Homicidio'))

#Resumen del mapa
df2 <- df0 %>% 
  group_by(Año,
           Delito,
           Subtipo,
           Comuna,
           Barrio) %>% 
  summarise(cantidad = n())

#dataset de tobilleras electrónicas
df_tobilleras=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/tobilleras/tobilleras_limpio.csv',
                       stringsAsFactors = F,
                       encoding = 'UTF-8') %>% 
  mutate(Año= as.numeric(substr(año_mes_oficio,1,4))) %>% 
  select(-caso_id)


#Shiny App
ui = fluidPage(theme = shinytheme('superhero'), 
               h1 (img 
                   (src='https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSDilWTQk6Twojy1Iy1K_FIbcmEjBVMH-RF_yYqZU8QOj_RA4uo&usqp=CAU',
                     height='50%', width='50%')), #imagen Eant
  br(),
  p('Proyecto Final - ', strong('EANT'),' - Tema: Seguridad'),
  tabsetPanel(
    tabPanel('Descripción del proyecto',
             br(),
             column(12,
                    p('En esta aplicación Shiny se podrán observar diferentes variables de relevancia con respecto a la seguridad en la Ciudad Autónoma de Buenos Aires'),
                    )),
    tabPanel('Información de delitos en CABA',
             navlistPanel(
               tabPanel('Detalle de delitos en CABA',
                        br(),
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
             )), #Fin Tabla pivot Tobilleras
    tabPanel('Gráficos comparativos',
             navlistPanel(
               tabPanel('Delitos por modalidad',
                        br(),
                        plotOutput(outputId = 'G1')
               ),
               tabPanel('Delitos por Comuna',
                        br(),
                        plotOutput(outputId = 'G2')))
    ), #Fin Tab gráficos
    tabPanel("About us",
             br(),
             column(2),
             column(10,
                    p('Fuente de datos:'),
                    tags$div(tags$ul(
                      tags$li(tags$a(href='https://data.buenosaires.gob.ar/dataset/mapa-del-delito','Mapa del delito')),
                      tags$li(tags$a(href='https://data.buenosaires.gob.ar/dataset/tobilleras-electronicas','Tobilleras electrónicas'))
                    )),
                    p('Estos datos se generaron a partir de los siguientes paquetes:'),
                    tags$div(tags$ul(
                      tags$li('shiny'),
                      tags$li('DT'),
                      tags$li('tidyverse'),
                      tags$li('shinythemes'),
                      tags$li('leaflet'),
                      tags$li('rlang'),
                      tags$li('readxl'),
                      tags$li('rpivotTable'),
                      tags$li('devtools'),
                      tags$li('stringr')
                    )),
                    br(),
                    p('Creado por Antonella Cremona en el marco del curso Data Analytics con R - Mayo de 2020'),
                    p('Código disponible en: ',tags$a(href='https://github.com/antocremona/Proyecto_Final_Eant.git','Github'))),
             column(2))
  ))

server = function(input, output){
  output$Table_Tipo=renderDataTable({
    df2 <- df0 %>% 
      group_by(Año,
               Delito,
               Subtipo,
               Comuna,
               Barrio) %>% 
      summarise(cantidad = n())
  })
  
  output$Mapa_delitos1=renderLeaflet({
    df1 %>%
      filter(df1$Año==2019) %>% 
      leaflet() %>%
      addTiles() %>%
      addProviderTiles('Thunderforest.SpinalMap') %>%
      setView(lng = -58.3772316,
              lat = -34.6131516,
              zoom = 13) %>%
      addMarkers(lng = ~long,
                 lat = ~lat,
                 popup = ~ htmlEscape(paste(Delito,' - ',Subtipo))) %>% 
      addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       color =ifelse(df1$Subtipo!='Doloso',"purple","red"),
                       clusterOptions = markerClusterOptions())
  })
  
  output$RPivot_Tobilleras=renderRpivotTable({
    rpivotTable(df_tobilleras, rows="categoria", col="estado_tobillera", aggregatorName="Count", vals="caso_id")
  })
  
  output$G1=renderPlot({
    df2 %>% 
      mutate(Crimen = paste(Delito,' - ',Subtipo)) %>% 
      ggplot(aes(x=Crimen, y=cantidad, fill = Crimen)) + 
      geom_col(show.legend = F) +
      facet_wrap(~Año)+
      coord_flip()+
      theme_bw()
  })
  
  output$G2=renderPlot({
    df2 %>% 
      mutate(Crimen = paste(Delito,' - ',Subtipo)) %>%
      filter(is.na(Comuna)==F) %>% 
      ggplot(aes(x=Comuna, y=cantidad, fill = Crimen)) + 
      geom_col(show.legend = T) +
      facet_wrap(~Año)+
      coord_flip()+
      theme_bw()
})
}

shinyApp(ui=ui, server=server)