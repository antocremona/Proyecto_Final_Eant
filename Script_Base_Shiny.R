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

#Dataset SUACI
df=read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/sistema-unico-de-atencion-ciudadana/sistema-unico-de-atencion-ciudadana-2020.csv', 
            stringsAsFactors = F, 
            encoding = 'UTF-8')

#DF SUACI refinado
df0=df %>% 
  filter(fecha_ingreso>='2020-03-20',
         lat!='na',
         long!='na',
         str_detect(concepto,'CORONA') | str_detect(concepto,'MOSQUITO'))


#Shiny App
ui = fluidPage(
  h1 (
    img (src='https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSDilWTQk6Twojy1Iy1K_FIbcmEjBVMH-RF_yYqZU8QOj_RA4uo&usqp=CAU',
         height='50%', width='50%')), #imagen Eant
  br(),
  p('Proyecto Final - ', strong('EANT')),
  tabsetPanel(
    tabPanel('Sistema Unico de Atención Ciudadana',
             navlistPanel(
               tabPanel('Contactos durante la cuarentena',
                        br(),
                        selectInput(inputId = 'Concepto',
                                    label = 'Concepto',
                                    choices = unique(df0$concepto)),
                        dataTableOutput(outputId = 'Table_Concepto')
               ),
               tabPanel('Mapa de los contactos',
                        br(),
                        leafletOutput(outputId = 'Mapa_Concepto')))
    ), #Fin Tabla categoria
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
  output$Table_Concepto=renderDataTable({
    df0%>%select(categoria,subcategoria,concepto,domicilio_barrio)
  })
  
  output$Mapa_Concepto=renderLeaflet({
    df0 %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles('Thunderforest.SpinalMap') %>%
      setView(lng = -58.3772316,
              lat = -34.6131516,
              zoom = 13) %>%
      addMarkers(lng = ~long,
                 lat = ~lat,
                 popup = ~ htmlEscape(concepto)) %>% 
      addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       color = ifelse(str_detect(df0$concepto,'MOSQUITO'),'purple','green'))
  })
  
  output$RPivot_Tobilleras=renderRpivotTable({
    rpivotTable(df_tobilleras, rows="categoria", col="estado_tobillera", aggregatorName="Count", vals="caso_id")
  })
}
shinyApp(ui=ui, server=server)