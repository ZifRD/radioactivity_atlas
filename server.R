#Author: Zaporozhtsev I.F.
#Created: 2019-2020

#### INI libraries ####
library(leaflet)
library(htmlwidgets)
library(openxlsx)
library(sf)
library(mapedit)
library(mapview)
library(leafpm)
library(officer)
library(flextable)
library(readxl)

# By default the file size limit is 5MB. Here limit is 70MB.
options(shiny.maxRequestSize = 70*1024^2)
options(max.print=1000)
memory.size(max = FALSE)

Sys.setlocale("LC_ALL", "Russian")

#### END libraries ####
shinyServer(function(input, output, session){

#### INI ANA_STREQ On Load ####
# ini reactives
withProgress(message = 'Загрузка данных из базы...', value = 0,
             rv <- reactiveValues(
               regions = dbGetQuery(conn, paste0("SELECT name FROM regions order by region_id;")),
               ships = dbGetQuery(conn, paste0("SELECT name FROM ships order by ship_id;")),
               pollutants = dbGetQuery(conn, paste0("SELECT name FROM pollutants order by pollutant_id;")),
               objects = c("вода","грунт","биота"),
               stations = dbGetQuery(conn, paste0("SELECT station_id as stid,exp_id as eid,lat,lon FROM stations;")),
               years = 1991:2019
             )
)
ana_streq_mainDF<-reactiveVal(NULL)
ana_streq_mapfilteredDF<-reactiveVal(NULL) 
ana_streq_markerstate<-reactiveVal("notselected")

observe({  #not reactive - for "selected"value render of ana_streq_datatable is prohibited
  switch(ana_streq_markerstate(),
         "notselected" = {
           ana_streq_mapfilteredDF(ana_streq_mainDF())
           shinyjs::hide("ana_streq_abs_stationAB") 
           shinyjs::hide("ana_streq_abs_expAB")
           shinyjs::hide("ana_streq_abs_reqAB")
           isolate(ana_streq_expstidlist(NULL))
           isolate(ana_streq_expstid0(NULL))
           }, #delault
         
         "selected"= {
           shinyjs::show("ana_streq_abs_stationAB") 
           shinyjs::show("ana_streq_abs_expAB")
           shinyjs::hide("ana_streq_abs_reqAB")
           }, 
         "station" = {
           ana_streq_mapfilteredDF(ana_streq_mainDF() %>% 
             filter(lat == ana_streq_selectedMarkerLat() & lon == ana_streq_selectedMarkerLon()))
           shinyjs::hide("ana_streq_abs_stationAB") 
           shinyjs::show("ana_streq_abs_expAB")
           shinyjs::show("ana_streq_abs_reqAB")
           isolate(ana_streq_expstidlist(NULL))
           isolate(ana_streq_expstid0(NULL))
           },
         "expedition" = {
           shinyjs::show("ana_streq_abs_stationAB") 
           shinyjs::hide("ana_streq_abs_expAB")
           shinyjs::show("ana_streq_abs_reqAB")
           }
  )
})

ana_streq_stationsDF <-reactive({
  if(!is.null(ana_streq_mainDF()))
  {
    used <- unique(ana_streq_mainDF()$stid)
    rv$stations %>% subset(stid %in% used)
  } else {return(NULL)}
})
# end reactives

# Names ending with 0 are in global.R
observe({
  regs <- c("Все", rv$regions$name)
  updateSelectInput(session, "ana_streq_regionSI", choices = regs, selected = regionName0)
  ships <- c("Все", rv$ships$name)
  updateSelectInput(session, "ana_streq_shipSI", choices = ships, selected = shipName0)
  updateSelectInput(session, "ana_upd_shipsSI", choices = ships, selected = shipName0)
  polls <- c("Все", rv$pollutants$name)
  updateSelectInput(session, "ana_streq_pollSI", choices = polls, selected = pollName0)
  updateSelectInput(session, "ana_streq_objectSI", choices = rv$objects, selected = objectName0)
  ana_streq_mainDF(getPoints0());
})

ana_streq_dfToDT <- function(dataset)
{
  if(is.null(dataset)) {return()}
  dataset <- dataset %>% tibble::add_column(Action = paste0('<a class="ana-streq-gomap" href="" data-lat="', 
                                                dataset$lat, '" data-long="', dataset$lon,'"><i class="fa fa-crosshairs"></i></a>'),.before="date1")
  action <- DT::dataTableAjax(session, dataset)
  return(DT::datatable(dataset,
                   colnames=c('Карта' = 'Action',
                              "Дата1" = 'date1',
                              'Дата2' = 'date2',
                              'Дата3' = 'date3',
                              'Год' = 'year',
                              'Судно' = 'shipname',
                              'Район' ='regname',
                              'Подрайон' = 'regcom',
                              'Широта' = 'lat',
                              'Долгота' = 'lon',
                              'Поллютант' = 'polname',
                              'Знак' = 'polsign',
                              'Значение' = 'polval',
                              'Погрешность' = 'polerr'
                              ),
                   options = list(language = list(url = "Russian.json"),ajax = list(url = action),
                                  scrollY = 500, scroller = TRUE, scrollX = T,pageLength = 100), escape = FALSE))
}

output$ana_streq_datatable <- DT::renderDataTable({
  ana_streq_dfToDT(ana_streq_mapfilteredDF()[,-c(5,11)])
})
#### END ANA_STREQ On Load ####

# ANA_STREQ Leaflet (ana_streq_mainDF + ana_streq_hover_coordinates)
output$ana_streq_map <- renderLeaflet({
  top=70.4;
  bottom=66.05;
  right=42.05;
  left=27.5;
  
  leaflet(data = ana_streq_stationsDF(),options = leafletOptions(minZoom = 2,maxZoom = 13))%>%fitBounds(right,bottom,left,top)%>%
    addTiles() %>% addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap")  %>%
    addMarkers(
      lat = ~lat,
      lng = ~lon,
      icon = ordinaryIcon,
      layerId = ~as.character(stid)
    ) %>%
    onRender(
      "function(el,x){
         this.on('mousemove', function(e) {
           var lat = e.latlng.lat;
           var lng = e.latlng.lng;
           var coord = [lat, lng];
           Shiny.setInputValue('ana_streq_hover_coordinates', coord)
         });
         this.on('mouseout', function(e) {
            Shiny.setInputValue('ana_streq_hover_coordinates', null)
         })
      }"
    )
}); 

#### INI ANA_STREQ Sidebar Run ####
observeEvent(input$ana_streq_runB, {
  ana_rerunBaseSampleConstruction()
})

ana_rerunBaseSampleConstruction <- function()
{
  ana_streq_mainDF(getPoints(input$ana_streq_objectSI,input$ana_streqdateRI[1],input$ana_streqdateRI[2],
                             input$ana_streq_regionSI,input$ana_streq_shipSI,input$ana_streq_pollSI))
  ana_streq_markerstate("notselected")
}
#### END ANA_STREQ Sidebar Run ####

# ANA_STREQ Map hover info (ana_streq_hover_coordinates)
output$ana_streq_hoverLatLonTO <- renderText({
  if(is.null(input$ana_streq_hover_coordinates)) {
    "Курсор вне карты"
  } else {
    paste0("Lat: ", input$ana_streq_hover_coordinates[1], 
           " Long: ", input$ana_streq_hover_coordinates[2])
  }
})

#### INI ANA_STREQ Marker selection ####
# ini reactives
ana_streq_selectedMarkerLat <- reactiveVal(NULL)
ana_streq_selectedMarkerLon <- reactiveVal(NULL)
ana_streq_prevSelectedMarkerID <- reactiveVal(NULL)
ana_streq_expstidlist <- reactiveVal(NULL)
ana_streq_expstid0 <- reactiveVal(NULL)
# end reactives

generatePopup <- function(dataSet)
{
  return (paste0("<b>Широта: </b>",dataSet$lat,"° с.ш.","<br><b>Долгота: </b>",dataSet$lon,"° в.д.","<br>"))
}

ordinaryIcon <- makeIcon(iconUrl = "www/mark.png",iconWidth = 20, iconHeight = 25)
selectedIcon <- makeIcon(iconUrl = "www/bouy.png",iconWidth = 20, iconHeight = 25)
expeditionIcon <- makeIcon(iconUrl = "www/exp.png",iconWidth = 20, iconHeight = 20)

observe({
  click<-input$ana_streq_map_marker_click
  if(is.null(click)){
    return()
  }
  markersIndex <- click$id
  localPrevSelected = isolate(ana_streq_prevSelectedMarkerID())
  
  proxy <- leafletProxy("ana_streq_map")
  if(!is.null(localPrevSelected))
  {
    prevMarkerData <- isolate(ana_streq_stationsDF()[ana_streq_stationsDF()[,"stid"] == localPrevSelected,])
    previcon = NULL
    if(isolate(ana_streq_markerstate()) == "expedition" & 
                                  isolate(markersIndex %in% ana_streq_expstidlist()))
    { previcon = expeditionIcon }
    else {previcon = ordinaryIcon}
    
    proxy %>% removeMarker(layerId = localPrevSelected) %>% 
      clearPopups() %>%
      addMarkers(lat = prevMarkerData$lat,
                 lng = prevMarkerData$lon,
                 layerId = as.character(localPrevSelected),
                 icon = previcon)
  }
  chosenMarkerData <- isolate(ana_streq_stationsDF()[ana_streq_stationsDF()[,"stid"] == markersIndex,])
  proxy %>% removeMarker(layerId = markersIndex)
  proxy %>% addMarkers(lat = chosenMarkerData$lat,
                       lng = chosenMarkerData$lon,
                       layerId = as.character(markersIndex),
                       icon = selectedIcon) %>% 
  addPopups(lat = chosenMarkerData$lat,lng = chosenMarkerData$lon,popup = generatePopup(chosenMarkerData))
  isolate(ana_streq_prevSelectedMarkerID(markersIndex))
  
  ana_streq_selectedMarkerLon(chosenMarkerData$lon)
  ana_streq_selectedMarkerLat(chosenMarkerData$lat)
  
  if(isolate(ana_streq_markerstate()) == "notselected") {
    isolate(ana_streq_markerstate("selected"))
  } 
})

# ini abs panel part
output$ana_streq_abs_markerlatlonTO <- renderText(
  if(ana_streq_markerstate() == 'notselected') {paste0("Нажмите на маркер для активации дополнительных возможностей")}
  else paste0("Широта: ",ana_streq_selectedMarkerLat(),"° с.ш., долгота: ",ana_streq_selectedMarkerLon(),"° в.д."))

observeEvent(input$ana_streq_abs_stationAB,{
  ana_streq_removeExpMarkers()
  ana_streq_markerstate("station")
})

ana_streq_removeExpMarkers<-function(){
  if(!is.null(ana_streq_expstid0()))
  {
    tempdf <- ana_streq_stationsDF() %>% subset(stid %in% ana_streq_expstidlist())
    proxy <- leafletProxy("ana_streq_map")
    proxy %>% removeMarker(layerId = ana_streq_expstidlist())
    proxy %>% addMarkers(lat = tempdf$lat,
                         lng = tempdf$lon,
                         layerId = as.character(ana_streq_expstidlist()),
                         icon = ordinaryIcon)
  } 
}

observeEvent(input$ana_streq_abs_expAB,{
  ana_streq_markerstate("expedition")
  ana_streq_mapfilteredDF(ana_streq_mainDF() %>% group_by(expid) %>% 
                            filter(any(lat == ana_streq_selectedMarkerLat() & lon == ana_streq_selectedMarkerLon())))
  tempeid <- as.list(ana_streq_stationsDF() %>% subset(stid == ana_streq_prevSelectedMarkerID()) %>% select(eid))
  ana_streq_expstid0(ana_streq_prevSelectedMarkerID())
  
  tempdfff <- ana_streq_stationsDF() %>% subset(eid %in% tempeid) %>% select(stid)
  ana_streq_expstidlist(tempdfff$stid)
  tempdf <- ana_streq_stationsDF() %>% subset(stid %in% ana_streq_expstidlist())
 
  proxy <- leafletProxy("ana_streq_map")
  proxy %>% removeMarker(layerId = ana_streq_expstidlist())
  proxy %>% addMarkers(lat = tempdf$lat,
                       lng = tempdf$lon,
                       layerId = as.character(ana_streq_expstidlist()),
                       icon = expeditionIcon)
 
})
observeEvent(input$ana_streq_abs_reqAB,{
  ana_streq_removeExpMarkers()
  ana_streq_markerstate("notselected")
})
# end abs panel part

output$ana_streq_clickedmarkerstateTO <- renderText({
  switch(ana_streq_markerstate(),
         "notselected" = {"Режим базовой выборки"},
         "selected"={"Маркер выбран"},
         "station" = {"Выбрана станция"},
         "expedition" = {
           lat <- ana_streq_stationsDF()[ana_streq_stationsDF()[,"stid"] == ana_streq_expstid0(),]$lat
           lon <- ana_streq_stationsDF()[ana_streq_stationsDF()[,"stid"] == ana_streq_expstid0(),]$lon
           paste0("Выбрана экспедиция, построенная для станции (",lat,",",lon,")")})
})

#### END ANA_STREQ Marker selection ####

#### INI ANA_STREQ JS From table to Leaflet map ####
observe({
  if (is.null(input$ana_streq_gotomappoint))
    return()
  isolate({
    map <- leafletProxy("ana_streq_map")
    map %>% clearPopups()
    dist <- 0.25
    pointID <- input$ana_streq_gotomappoint$pointID
    lat <- input$ana_streq_gotomappoint$lat
    lng <- input$ana_streq_gotomappoint$lng
    map %>% addPopups(lng, lat, "Выбранная станция!\n Клик для продолжения...") %>% 
      fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  })
})
#### END ANA_STREQ JS From table to Leaflet map ####

#### INI ANA_STREQ Download Handler ####
output$ana_streq_exportCSV <- downloadHandler(
  filename = "Standard_request.csv",
  content = function(file) {
    write.csv(ana_streq_mapfilteredDF(), file, row.names = FALSE)
  }
)
#openxlsx
output$ana_streq_exportXLS <- downloadHandler(
  filename = "Standard_request.xlsx",
  content = function(file) {
    tempdf <-ana_streq_mapfilteredDF()
    names(tempdf) <- c("Дата1",'Дата2','Дата3','Год',"ID экспедиции",
                     'Судно','Район','Подрайон','Широта','Долгота',"ID станции",
                     'Поллютант','Знак','Значение','Погрешность')
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Лист1")
    caption <- "Данные для записи"
    writeData(wb, sheet = 1, x = caption, startCol = 1, startRow = 1)
    hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=8,
                      fontName="Arial Narrow", fgFill = "#4F80BD")
    writeData(wb, sheet = 1, x = tempdf[,-c(5,11)], 
              startCol = 1, startRow = 3,headerStyle = hs, borders = "columns")
    saveWorkbook(wb, file)
  }
)

output$ana_streq_exportDOC <- downloadHandler(
  filename = "Standard_request.docx",
  content = function(file) {
    tempdf <-ana_streq_mapfilteredDF()
    names(tempdf) <- c("Дата1",'Дата2','Дата3','Год',"ID экспедиции",
                       'Судно','Район','Подрайон','Широта','Долгота',"ID станции",
                       'Поллютант','Знак','Значение','Погрешность')
    my_doc <- read_docx() 
    styles_info(my_doc)
    ft <- flextable::flextable(tempdf[,-c(5,11)])
    ft <- flextable::fontsize(ft, size = 8, part = "all")
    ft <- flextable::border(ft, border = fp_border(color = "blue"))
    my_doc <- my_doc %>% 
      body_add_par("Здесь какой-то заголовок", style = "Normal") %>% 
      body_add_par("И какой-то комментарий", style = "Normal") %>% 
      flextable::body_add_flextable(value = ft) %>%
      body_end_section_landscape() 
    print(my_doc, file)
  }
)
#### END ANA_STREQ Download Handler ####

#-----------------------------------------------------------------------

# ANA_EXCEL On Load
observe({
  switch(input$tabset,
         "1" = {shinyalert("Уведомление", "Демонстрационная версия программы не отображает реальных измеренных значений, представлен ограниченный функционал, см. код на github (https://github.com/ZifRD/radioactivity_atlas)!", type = "warning")},
         "2" = {},
         "3" = {},
         "4" = {},
         "5" = { shinyalert("Редактирование БД - раздел в разработке", "Раздел имеет ограниченную функциональность в демоверсии, см. код на github!", type = "warning")},
         "6"= { shinyalert("Анализ данных - раздел в разработке", "Раздел имеет ограниченную функциональность в демоверсии, см. код на github!", type = "warning")})
})

#### INI ANA_EXCEL Render MultiObject ####
observeEvent(input$ana_excel_sortablecollistAB, {
  shinyjs::toggle(id = "ana_excel_sortablePanel", anim = T,animType = "slide", time = 0.3)
})
observeEvent(input$ana_excel_hideBottom, {
  shinyjs::hide(id = "ana_excel_sortablePanel", anim = T,animType = "slide", time = 0.3)
})

ana_excel_sortableorderednameList<-reactiveVal(
  c("date1","date2","date3","year","shipname","regname","regcom","lat","lon","polname","polsign","polval","polerr")
)

ana_excel_justOpenedList<-reactiveVal(
  c("date1","date2","date3","year","shipname","regname","regcom","lat","lon","polname","polsign","polval","polerr")
)

output$ana_excel_multiobject <- renderUI({
  print("Render!")
  uiList <- list()
  for (v in  ana_excel_justOpenedList()) {
    switch(v,
           "date1" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Дата1","Дата сбора проб"))},
           "date2" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Дата2","Дата начала экспедиции"))},
           "date3" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Дата3","Дата окончания экспедиции"))},
           "year" = {uiList <- append(uiList,ana_excel_createcomplexWPFunc(v,"Год","Год сбора проб",rv$years))},
           "shipname" = {uiList <- append(uiList,ana_excel_createcomplexWPFunc(v,"Судно","Судно, с которого отбирали пробы (\"нет\" - для береговой экспедиции)",rv$ships$name))},
           "regname" = {uiList <- append(uiList,ana_excel_createcomplexWPFunc(v,"Район","Район взятия пробы",rv$regions$name))},
           "regcom" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Комментарий","Комментарий к району работ (обычно подрайон)"))},
           "lat" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Широта","Широта станции, на которой получена проба"))},
           "lon" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Долгота","Долгота станции, на которой получена проба"))},
           "polname" = {uiList <- append(uiList,ana_excel_createcomplexWPFunc(v,"Поллютант","Поллютант, который содержится в пробе",rv$pollutants$name))},
           "polsign" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Знак","Знак равно или меньше"))},
           "polval" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Значение","Измеренное значение"))},
           "polerr" = {uiList <- append(uiList,ana_excel_createsimpleWPFunc(v,"Погрешность","Погрешность измерения"))}
    )
  }
  for (i in 1:length(uiList)) {
    uiList[i] <- uiList[i]$children
  }
 
  jqui_sortable(div(id = 'ana_excel_sortablecollistJQ',uiList))
})

ana_excel_createsimpleWPFunc <- function(v,name,helptext){
  return(tags$div(wellPanel(id=paste0("ana_excel_",v,"P"),
                            div(style="display: inline-block;  width: 10px;",
                                checkboxInput(paste0("ana_excel_",v,"Chk"), label = NULL, value = TRUE)),
                            div(style="display: inline-block;  width: 150px;",
                                textInput(paste0("ana_excel_",v,"TI"), label = NULL, value = name)),
                            div(style="display: inline-block;",helpText(helptext)), 
                            style = "padding: 1px;",
                            bsTooltip(id = paste0("ana_excel_",v,"TI"), 
                                      title = paste0("Введите короткое имя столбца (\"",name,"\" по умолчанию)"),
                                      placement ="right",trigger = "hover"))))
}

ana_excel_createcomplexWPFunc <- function(v,name,helptext,vals){
  return(tags$div(wellPanel(id=paste0("ana_excel_",v,"P"),
                            div(style="display: inline-block;  width: 10px;",
                                checkboxInput(paste0("ana_excel_",v,"Chk"), label = NULL, value = TRUE)),
                            div(style="display: inline-block;  width: 150px;",
                                textInput(paste0("ana_excel_",v,"TI"), label = NULL, value = name)),
                            div(style="display: inline-block;  width: 50px;",
                                shinyWidgets::dropdownButton(inputId = paste0("ana_excel_",v,"DDB"),icon=icon("tasks"),
                                                             checkboxGroupInput(inputId=paste0("ana_excel_",v,"CHGI"), label = toupper(name), choices = vals, selected = vals)
                                )),
                            div(style="display: inline-block;  width: 50px;",
                                shinyWidgets::materialSwitch(inputId = paste0("ana_excel_",v,"SW"),label = "ВСЕ",value = TRUE,status = "warning")
                            ),
                            div(style="display: inline-block;",helpText(helptext)), 
                            style = "padding: 1px;",
                            bsTooltip(id = paste0("ana_excel_",v,"TI"), 
                                      title = paste0("Введите короткое имя столбца (\"",name,"\" по умолчанию)"),
                                      placement ="right",trigger = "hover"))))
}
#### END ANA_EXCEL Render MultiObject ####

#### INI ANA_EXCEL UI base interaction####
# CHGI #
#in this observer value SW is updated and isolate it in order to not call this observer ones more
observe({
  if(is.null(input$ana_excel_yearCHGI) & is.null(isolate(input$ana_excel_yearSW))) {return()}
  if(is.null(input$ana_excel_yearCHGI))
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_yearSW",value = FALSE)
  if(length(input$ana_excel_yearCHGI) == length(rv$years))
  {
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_yearSW",value = TRUE)
  }
  else shinyWidgets::updateMaterialSwitch(session,"ana_excel_yearSW",value = FALSE)
})

observe({
  if(is.null(input$ana_excel_shipnameCHGI) & is.null(isolate(input$ana_excel_shipnameSW))) {return()}
  if(is.null(input$ana_excel_shipnameCHGI))
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_shipnameSW",value = FALSE)
  if(length(input$ana_excel_shipnameCHGI) == length(rv$ships$name))
  {
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_shipnameSW",value = TRUE)
  }
  else shinyWidgets::updateMaterialSwitch(session,"ana_excel_shipnameSW",value = FALSE)
})

observe({
  if(is.null(input$ana_excel_regnameCHGI) & is.null(isolate(input$ana_excel_regnameSW))) {return()}
  if(is.null(input$ana_excel_regnameCHGI))
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_regnameSW",value = FALSE)
  if(length(input$ana_excel_regnameCHGI) == length(rv$regions$name))
  {
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_regnameSW",value = TRUE)
  }
  else shinyWidgets::updateMaterialSwitch(session,"ana_excel_regnameSW",value = FALSE)
})

observe({
  if(is.null(input$ana_excel_polnameCHGI) & is.null(isolate(input$ana_excel_polnameSW))) {return()}
  updateSelectInput(session,"ana_excel_FpolnameSI",choices = input$ana_excel_polnameCHGI, selected = input$ana_excel_polnameCHGI[1])
  
  if(is.null(input$ana_excel_polnameCHGI))
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_polnameSW",value = FALSE)
  if(length(input$ana_excel_polnameCHGI) == length(rv$pollutants$name))
  {
    shinyWidgets::updateMaterialSwitch(session,"ana_excel_polnameSW",value = TRUE)
  }
  else shinyWidgets::updateMaterialSwitch(session,"ana_excel_polnameSW",value = FALSE)
})


# SW #
observe({
  if(is.null(input$ana_excel_yearSW)) {return()}
  if(input$ana_excel_yearSW) {
    if(length(isolate(input$ana_excel_yearCHGI)) != length(rv$years))
      updateCheckboxGroupInput(session,"ana_excel_yearCHGI",choices = rv$years,selected = rv$years)
  }
  else {
    #not update "always" because of loaded SW=FALSe and CHGI list from file
    if(isolate(length(input$ana_excel_yearCHGI)) == length(rv$years))
    updateCheckboxGroupInput(session,"ana_excel_yearCHGI",choices = rv$years,selected = NULL)
  }
})

observe({
  if(is.null(input$ana_excel_shipnameSW)) {return()}
  if(input$ana_excel_shipnameSW) {
    if(length(isolate(input$ana_excel_shipnameCHGI)) != length(rv$ships$name))
      updateCheckboxGroupInput(session,"ana_excel_shipnameCHGI",choices = rv$ships$name,selected = rv$ships$name)
  }
  else {
    if(isolate(length(input$ana_excel_shipnameCHGI)) == length(rv$ships$name))
    updateCheckboxGroupInput(session,"ana_excel_shipnameCHGI",choices = rv$ships$name,selected = NULL)}
})

observe({
  if(is.null(input$ana_excel_regnameSW)) {return()}
  if(input$ana_excel_regnameSW) {
    if(length(isolate(input$ana_excel_regnameCHGI)) != length(rv$regions$name))
     updateCheckboxGroupInput(session,"ana_excel_regnameCHGI",choices = rv$regions$name,selected = rv$regions$name)
  }
  else {
    if(isolate(length(input$ana_excel_regnameCHGI)) == length(rv$regions$name))
      updateCheckboxGroupInput(session,"ana_excel_regnameCHGI",choices = rv$regions$name,selected = NULL)}
})

observe({
  if(is.null(input$ana_excel_polnameSW)) {return()}
  if(input$ana_excel_polnameSW) {
    if(length(isolate(input$ana_excel_polnameCHGI)) != length(rv$pollutants$name))
      updateCheckboxGroupInput(session,"ana_excel_polnameCHGI",choices = rv$pollutants$name,selected = rv$pollutants$name)
  }
  else {
    if(isolate(length(input$ana_excel_polnameCHGI)) == length(rv$pollutants$name))
      updateCheckboxGroupInput(session,"ana_excel_polnameCHGI",choices = rv$pollutants$name,selected = NULL)}
})

# Chk #
observe({
  if(is.null(input$ana_excel_yearChk)) {return()}
  if(input$ana_excel_yearChk){
    shinyjs::enable(id = "ana_excel_yearDDB")
    shinyjs::show(id = "ana_excel_yearSW")
  }
  else{
    shinyjs::disable(id = "ana_excel_yearDDB")
    shinyjs::hide(id = "ana_excel_yearSW")
    }
})
observe({
  if(is.null(input$ana_excel_shipnameChk)) {return()}
  if(input$ana_excel_shipnameChk){
    shinyjs::enable(id = "ana_excel_shipnameDDB")
    shinyjs::show(id = "ana_excel_shipnameSW")
  }
  else{
    shinyjs::disable(id = "ana_excel_shipnameDDB")
    shinyjs::hide(id = "ana_excel_shipnameSW")
  }
})
observe({
  if(is.null(input$ana_excel_regnameChk)) {return()}
  if(input$ana_excel_regnameChk){
    shinyjs::enable(id = "ana_excel_regnameDDB")
    shinyjs::show(id = "ana_excel_regnameSW")
  }
  else{
    shinyjs::disable(id = "ana_excel_regnameDDB")
    shinyjs::hide(id = "ana_excel_regnameSW")
  }
})
observe({
  if(is.null(input$ana_excel_polnameChk)) {return()}
  if(input$ana_excel_polnameChk){
    shinyjs::enable(id = "ana_excel_polnameDDB")
    shinyjs::show(id = "ana_excel_polnameSW")
  }
  else{
    shinyjs::disable(id = "ana_excel_polnameDDB")
    shinyjs::hide(id = "ana_excel_polnameSW")
  }
})
#### END ANA_EXCEL UI base interaction####

#### INI ANA_EXCEL Open and Save options####

observeEvent(input$ana_excel_opendefaultTop,{ana_excel_open("ana_excel_default.txt")})
observeEvent(input$ana_excel_opendefaultBottom,{ana_excel_open("ana_excel_default.txt")})
observeEvent(input$ana_excel_openTop,{ana_excel_open("ana_excel.txt")})
observeEvent(input$ana_excel_openBottom,{ana_excel_open("ana_excel.txt")})
observeEvent(input$ana_excel_saveTop,{ana_excel_save()})
observeEvent(input$ana_excel_saveBottom,{ana_excel_save()})

ana_excel_open <- function(filename){
  fconn <- file(filename,open="r")
  updateRadioButtons(session,"ana_excel_objectRB",
                     choices = list("вода", "грунт", "биота"),selected = readLines(fconn, n = 1, warn = FALSE),inline=T)
  # If not use this line and load file several times consequently Multi render will not be called 
  # Reactive val is not changed. But NULL changes it!
  ana_excel_sortableorderednameList(NULL)
  ana_excel_sortableorderednameList(unlist(strsplit(readLines(fconn, n = 1)," ")))
  ana_excel_justOpenedList(ana_excel_sortableorderednameList())
  while (length(oneLine <- readLines(fconn, n = 1, warn = FALSE)) > 0)
  {
    slist <- unlist(strsplit(oneLine, ","))
    name <- slist[1]
    updateCheckboxInput(session,paste0("ana_excel_",name,"Chk"),label = NULL,value = as.logical(slist[2]))
    updateTextInput(session,paste0("ana_excel_",name,"TI"),value = slist[3])
    if((slist[2] == TRUE) & name %in% c("year","shipname","polname","regname"))
    {
      oneLine <- readLines(fconn, n = 1)
      if(oneLine == "Все")
      {
        switch(name,
               "year" = {updateCheckboxGroupInput(session,"ana_excel_yearCHGI",label = toupper(slist[3]),choices = rv$years,selected = rv$years)},
               "regname" ={updateCheckboxGroupInput(session,"ana_excel_regnameCHGI",label = toupper(slist[3]),choices = rv$regions$name,selected = rv$regions$name)},
               "shipname" = {updateCheckboxGroupInput(session,"ana_excel_shipnameCHGI",label = toupper(slist[3]),choices = rv$ships$name,selected=rv$ships$name)},
               "polname" = {updateCheckboxGroupInput(session,"ana_excel_polnameCHGI",label = toupper(slist[3]),choices = rv$pollutants$name,selected = rv$pollutants$name)}
        )
      }
      else
      {
        lines <- readLines(fconn, n = as.integer(oneLine), warn = FALSE)
        mylist <- switch(name, "year" = rv$years,"shipname" = rv$ships$name,"regname" = rv$regions$name,"polname" = rv$pollutants$name)
        result <- NULL
        for (v in lines) {
          if(v %in% mylist) result <- c(result,v) 
        }
        switch(name,
               "year" = {updateCheckboxGroupInput(session,"ana_excel_yearCHGI",label = toupper(slist[3]),choices = rv$years,selected = result)},
               "regname" ={updateCheckboxGroupInput(session,"ana_excel_regnameCHGI",label = toupper(slist[3]),choices = rv$regions$name,selected = result)},
               "shipname" = {updateCheckboxGroupInput(session,"ana_excel_shipnameCHGI",label = toupper(slist[3]), choices = rv$ships$name,selected=result)},
               "polname" = {updateCheckboxGroupInput(session,"ana_excel_polnameCHGI",label = toupper(slist[3]),choices = rv$pollutants$name,selected = result)}
        )
      }
    }
  }
  close(fconn)
}

ana_excel_save <- function(){
  mylist <- input$ana_excel_sortablecollistJQ_order$id
  mylist <- mylist[mylist != ""]
  mylist <- unlist(lapply(mylist, function(v) substr(v,11,nchar(v)-1)))
  sink("ana_excel.txt")
  cat(paste0(input$ana_excel_objectRB,"\n"))
  cat(mylist)
  cat("\n")
  for (v in mylist) {
    cat(paste0(v,",",input[[paste0("ana_excel_",v,"Chk")]],",",input[[paste0("ana_excel_",v,"TI")]],"\n"))
    if(v %in% c("year","shipname","polname","regname"))
    {
      ana_excel_writegross(v)
    }
  }
  sink()
}
ana_excel_writegross <- function(v){
  if(input[[paste0("ana_excel_",v,"Chk")]])
  {
    if(input[[paste0("ana_excel_",v,"SW")]]) cat("Все\n")
    else
    {
      templist <- input[[paste0("ana_excel_",v,"CHGI")]]
      cat(length(templist))
      cat("\n")
      for (v in templist)
      {
        cat(v)
        cat("\n")
      }
    }
  }
}
#### END ANA_EXCEL Open and Save options####

#### INI ANA_EXCEL Pollutants values filter ####
ana_excel_constraintsList <- reactiveVal(NULL)

observe({
  updateActionButton(session, "ana_excel_FaddfilterAB", 
                     label = paste0("≤ Ограничить ",input$ana_excel_FpolnameSI," ≤"))
})

observeEvent(input$ana_excel_FaddfilterAB,{
  mylist <- c(input$ana_excel_FpolnameSI,
              as.numeric(input$ana_excel_FlowerTI),
              as.numeric(input$ana_excel_FupperTI))
  ana_excel_constraintsList(append(ana_excel_constraintsList(),list(mylist)))
})

observeEvent(input$ana_excel_FremoveallAB,{
  ana_excel_constraintsList(NULL)
})

observeEvent(input$ana_excel_FremoveAB,{
  ana_excel_constraintsList(head(ana_excel_constraintsList(),-1))
})

output$ana_excel_FreportHO <- renderUI({
  filtersUsed <- NULL
  for (v in ana_excel_constraintsList()) {
    filtersUsed <- append(filtersUsed, paste0("Применить фильтр: ",v[2]," ≤ ",v[1]," ≤ ",v[3]))
  }
  if(is.null(filtersUsed)) HTML(paste0("Фильтры не заданы"))
  else HTML(paste(filtersUsed, collapse = "<br>"))
})

ana_excel_generateConstraintFunc <- function(mylist){
  tableName <- "";
  if (input$ana_excel_objectRB == "вода") tableName <- "w_samples";
  if (input$ana_excel_objectRB == "грунт") tableName <- "s_samples";
  if (input$ana_excel_objectRB == "биота") tableName <- "b_samples";
  constraint <- paste0("pollutants.name = '",mylist[1],
                       "' AND ",tableName,".val >= ",mylist[2]," AND ",
                       tableName,".val <= ",mylist[3])
  return(constraint)
}
#### END ANA_EXCEL Pollutants values filter ####

#### INI ANA_EXCEL Render Data table ####
observeEvent(input$ana_excel_confirmsortablecollistAB,{
  ana_excel_DF(ana_excel_getDataFunc())
  tempchecks <- NULL
  tempnames <- NULL
  for(v in ana_excel_sortableorderednameList())
  {
    if(input[[paste0("ana_excel_",v,"Chk")]] == TRUE){
      tempchecks <- c(tempchecks,v)
      tempnames <- c(tempnames,input[[paste0("ana_excel_",v,"TI")]])
    }
  }
  names(tempchecks) <- tempnames
  ana_excel_tablechecks(tempchecks)
})

ana_excel_DF <- reactiveVal(NULL)
ana_excel_tablechecks <- reactiveVal(NULL)

output$ana_excel_datatable <- DT::renderDataTable({
  ana_excel_dfToDT(ana_excel_DF())
})
ana_excel_dfToDT <- function(dataset)
{
  if(is.null(dataset)) {return()}
  return(DT::datatable(dataset,
      colnames=ana_excel_tablechecks(),
      options = list(language = list(url = "Russian.json"),scrollY = 500, scroller = TRUE,scrollX = T,pageLength = 100)))}

ana_excel_getDataFunc <- function(){
  mylist <- input$ana_excel_sortablecollistJQ_order$id
  mylist <- mylist[mylist != ""]
  mylist <- unlist(lapply(mylist, function(v) substr(v,11,nchar(v)-1)))
  ana_excel_sortableorderednameList(mylist)
  
  
  for (v in c("year","shipname","regname","polname")) {
    if(input[[paste0("ana_excel_",v,"Chk")]] & is.null(input[[paste0("ana_excel_",v,"CHGI")]]))
    {
      shinyalert("Ошибка настроек фильтра столбца", 
                             paste0("Выберите хотя бы 1 пункт из списка \"",
                                    toupper(input[[paste0("ana_excel_",v,"TI")]]),"\""), type = "error")
      return(NULL)
    }
  }
  tableName <- "";
  if (input$ana_excel_objectRB == "вода") tableName <- "w_samples";
  if (input$ana_excel_objectRB == "грунт") tableName <- "s_samples";
  if (input$ana_excel_objectRB == "биота") tableName <- "b_samples";
  
  query <- paste0("SELECT ")
  checks <- NULL
  for(v in ana_excel_sortableorderednameList())
  {
    if(input[[paste0("ana_excel_",v,"Chk")]]) checks <- c(checks,v)
  }
  for (v in checks) {
    tv <- switch(v,"date1" = "stations.st_date",
                 "date2" = "expeditions.start_date",
                 "date3" = "expeditions.stop_date",
                 "year" = "EXTRACT(YEAR FROM to_date(expeditions.start_date,'dd/mm/yyyy'))",
                 "shipname" = "ships.name",
                 "regname" = "regions.name",
                 "regcom" = "stations.reg_comment",
                 "lat" = "stations.lat",
                 "lon" = "stations.lon",
                 "polname" = "pollutants.name",
                 "polsign" = paste0(tableName,".c"),
                 "polval" = paste0(tableName,".val"),
                 "polerr" = paste0(tableName,".err")
    )
    query <- paste0(query,tv," as ",v,",")
  }
  #-1 because comma is the last character
  query <- substr(query,1,nchar(query)-1)
  query <- paste0(query,
                  " FROM stations
                  INNER JOIN regions ON stations.region_id=regions.region_id
                  INNER JOIN expeditions ON expeditions.exp_id = stations.exp_id
                  INNER JOIN ships ON expeditions.ship_id=ships.ship_id
                  INNER JOIN ",tableName," ON stations.station_id=",tableName,".station_id
                  INNER JOIN pollutants ON pollutants.pollutant_id=",tableName,".pollutant_id ")
  query <- paste0(query,"WHERE ")
 
  for (v in checks) {
      result = ""
      if(v == "year") {result <- ana_excel_queryPartFilterFunc(v,"EXTRACT(YEAR FROM to_date(expeditions.start_date,'dd/mm/yyyy'))")}
      if(v == "shipname") {result <- ana_excel_queryPartFilterFunc(v,"ships.name")}
      if(v == "regname")  {result <- ana_excel_queryPartFilterFunc(v,"regions.name")}
      if(v == "polname") {result <- ana_excel_queryPartFilterFunc(v,"pollutants.name")}
      if(result != "") query <- paste0(query,result," AND ")
  }

  if(!is.null(ana_excel_constraintsList()))
  {
    query <- paste0(query,"(")
    mylist <- ana_excel_constraintsList()
    for (i in 1:length(mylist)) {
      query <- paste0(query,ana_excel_generateConstraintFunc(unlist(mylist[i]))," OR ")
    }
    query <- paste0(query,ana_excel_generateConstraintFunc(unlist(mylist[i])))
    query <- paste0(query,")")
  }
  
  if(substr(query,start = nchar(query)-3,stop=nchar(query)) == "AND ")
  {query <- substr(query,1,nchar(query)-5)}
  
  if(substr(query,start = nchar(query)-5,stop=nchar(query)) == "WHERE ")
  {query <- substr(query,1,nchar(query)-7)}
  query <- paste0(query,";")
  return(dbGetQuery(conn, query))
}

ana_excel_queryPartFilterFunc <- function(v,tv)
{
  a = toString(input[[paste0("ana_excel_",v,"CHGI")]])
  a = gsub(', ', "','", a);
  if(!grepl(",",a)) return(paste0(tv," = '",a,"' "))
  else 
  {
    if(!input[[paste0("ana_excel_",v,"SW")]]) return(paste0(tv," IN ('",a,"')"))
  }
  return("")
}
#### END ANA_EXCEL Render Data table ####

#### INI ANA_EXCEL Download Handler ####
output$ana_excel_exportCSV <- downloadHandler(
  filename = "Standard_request.csv",
  content = function(file) {
    write.csv(ana_excel_DF(), file, row.names = FALSE)
  }
)
#openxlsx
output$ana_excel_exportXLS <- downloadHandler(
  filename = "Standard_request.xlsx",
  content = function(file) {
    tempdf <-ana_excel_DF()
    names(tempdf) <- names(ana_excel_tablechecks())
  
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Лист1")
    caption <- "Данные для записи"
    writeData(wb, sheet = 1, x = caption, startCol = 1, startRow = 1)
    hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=8,
                      fontName="Arial Narrow", fgFill = "#4F80BD")
    writeData(wb, sheet = 1, x = tempdf[,-c(5,11)], 
              startCol = 1, startRow = 3,headerStyle = hs, borders = "columns")
    saveWorkbook(wb, file)
  }
)

output$ana_excel_exportDOC <- downloadHandler(
  filename = "Standard_request.docx",
  content = function(file) {
    tempdf <-ana_excel_DF()
    names(tempdf) <- names(ana_excel_tablechecks())
   
    my_doc <- read_docx() 
    styles_info(my_doc)
    ft <- flextable::flextable(tempdf[,-c(5,11)])
    ft <- flextable::fontsize(ft, size = 8, part = "all")
    ft <- flextable::border(ft, border = fp_border(color = "blue"))
    my_doc <- my_doc %>% 
      body_add_par("Здесь какой-то заголовок", style = "Normal") %>% 
      body_add_par("И какой-то комментарий", style = "Normal") %>% 
      flextable::body_add_flextable(value = ft) %>%
      body_end_section_landscape() 
    print(my_doc, file)
  }
)
#### END ANA_EXCEL Download Handler ####

#-----------------------------------------------------------------------
#### INI ANA_POLY Load text files ####
ana_poly_polysf <- reactiveVal(NULL)

ana_poly_oneText <- reactiveVal(paste0("Образец! Файл содержит одну строку пар (lat,lon):<br><pre>{(69.1,33.5), (69.2,33.7), ..., (69.05,33.4)}</pre>"))
ana_poly_csvText <- reactiveVal(paste0("Образец! Файл содержит строки из пар (lat,lon):<br><pre>","lat,lon","<br>","69.1,33.5","<br>","69.2,33.7","<br>","...","<br>","69.05,33.4</pre>"))
output$ana_poly_oneContentHO <- renderUI(
  HTML(ana_poly_oneText())
)
output$ana_poly_csvContentHO <- renderUI(
  HTML(ana_poly_csvText())
)

ns <- NS("ana_poly_map")
edits <- callModule(
  editMod,
  leafmap = leaflet(options = leafletOptions(minZoom = 2,maxZoom = 13))%>%fitBounds(42.05,66.05,27.5,70.4)%>%
    addTiles() %>%addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
    addDrawToolbar(polylineOptions = FALSE,
                   polygonOptions = FALSE,
                   circleOptions = FALSE,
                   rectangleOptions = FALSE,
                   markerOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   editOptions = FALSE,
                   singleFeature = TRUE),
  id = "ana_poly_map"
)

observe({
  #if changed ana_poly_polysf()
  #makes ana_streq_mainDF() to change
  if(is.null(ana_poly_polysf())) return();
  
  ana_rerunBaseSampleConstruction()
  
  pointsfrommainDF <- isolate(ana_streq_mainDF()) %>% select("stid","lat","lon")
  pointsfrommainDF <- pointsfrommainDF %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  reslt <- pointsfrommainDF %>% mutate(res = sapply(st_intersects(pointsfrommainDF,ana_poly_polysf()), function(z) if (length(z)==0) NA_integer_ else z[1]))
  intersectedstids <- unique(na.omit(reslt)[["stid"]])
  isolate(ana_streq_mainDF(ana_streq_mainDF() %>% subset(stid %in% intersectedstids)))
  
  leafletProxy(ns("map")) %>%
    removeDrawToolbar() %>%
    removeShape(layerId = "ana_poly_polyid") %>%
    addPolygons(layerId = "ana_poly_polyid", data = ana_poly_polysf(), color = "blue", weight = 4,#poly%>% slice(1:nrow(poly))
                smoothFactor = 0.5,opacity = 1.0,
                fillOpacity = 0.5,fillColor = 'red')
})

ana_poly_deleteDrawPanelFunc <- function(){
  edits <- callModule(
  editMod,
  leafmap = leaflet(options = leafletOptions(minZoom = 2,maxZoom = 13))%>%fitBounds(42.05,66.05,27.5,70.4)%>%
    addTiles() %>%addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
    addDrawToolbar(polylineOptions = FALSE,
                   polygonOptions = FALSE,
                   circleOptions = FALSE,
                   rectangleOptions = FALSE,
                   markerOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   editOptions = FALSE,
                   singleFeature = TRUE),
  id = "ana_poly_map")
}

observe({
  if(is.null(input$ana_poly_oneLoadFI)) return()
  ana_poly_deleteDrawPanelFunc()
  infile <- input$ana_poly_oneLoadFI
  con <- file(infile$datapath, "r");
  line <- readLines(con, n = 1);
  ana_poly_oneText(paste0("<pre>",line,"</pre>"))
  s1 <- chartr('(){}[]','      ', line); #Replace with whitespace
  #Turn the cleaned string into a two column matix
  m1 <- matrix(as.numeric(strsplit(s1,",")[[1]]),ncol=2,byrow=TRUE);
  m1[,c(1,2)] <- m1[,c(2,1)]
  m1 <- rbind(m1, c(m1[1,1],m1[1,2])) #add last to close poly 
  ana_poly_polysf(st_sf(st_sfc(st_polygon(list(m1))), crs = 4326, ID= "poly1"))
  close(con)
})

observe({
  if(is.null(input$ana_poly_csvLoadFI)) return()
  ana_poly_deleteDrawPanelFunc()
  infile <- input$ana_poly_csvLoadFI
  df <- read.csv(file = infile$datapath, header=TRUE, sep=",")
  colnames(df)[1] <- "Lat";
  colnames(df)[2] <- "Long";
  df <- df[,c("Long", "Lat")]  #swap columns
  con <- file(infile$datapath, "r");
  lines <- readLines(con);
  ana_poly_csvText(paste0("<pre>",paste(lines, collapse = '<br>'),"</pre>"))
  df <- rbind(df, df[1,])# You need first to close your polygon 
  ana_poly_polysf(st_sf(st_sfc(st_polygon(list(as.matrix(df)))), crs = 4326, ID= "poly1"))
  close(con)
})
#### END ANA_POLY Load text files ####

#### INI ANA_POLY Draw on map ####
observeEvent(input$ana_poly_clicksAB,{
  
  edits <- callModule(
    editMod,
    leafmap = leaflet(options = leafletOptions(minZoom = 2,maxZoom = 13))%>%fitBounds(42.05,66.05,27.5,70.4)%>%
      addTiles() %>%addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addDrawToolbar(polylineOptions = FALSE,
                     polygonOptions = drawPolygonOptions(repeatMode = TRUE),
                     circleOptions = FALSE,
                     rectangleOptions = FALSE,
                     markerOptions = FALSE,
                     circleMarkerOptions = FALSE,
                     editOptions = FALSE,
                     singleFeature = TRUE),
    id = "ana_poly_map"
  )
})

observeEvent(input$ana_poly_acceptclicksAB,{
  geom <- edits()$finished
  geom <- as.list(geom$geometry)
  ana_poly_polysf(geom[length(geom)])
  ana_poly_deleteDrawPanelFunc()
})

#### END ANA_POLY Draw on map ####

#### INI ANA_POLY Load SHP ####
ana_poly_polygon <- reactiveVal(NULL)

ana_poly_uploadedShape <- reactive({
  shpdf <- input$ana_poly_shpLoadFI
  tempdirname <- dirname(shpdf$datapath[1])
  for(i in 1:nrow(shpdf)){
    file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
  }
  map <- readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
  print(map)
  leafletProxy("ana_poly_map") %>%
    removeShape(layerId = "ana_poly_polyid") %>%
    addPolygons(layerId = "ana_poly_polyid", data = bord, color = "blue", weight = 4, 
                smoothFactor = 0.5,opacity = 1.0, 
                fillOpacity = 0.5,fillColor = 'red')
})

#### END ANA_POLY Load SHP ####

#--------------------------------------------------------------------------------------------------------------------------------------------

#### INI ANA_UPD Load Lists ####

output$ana_upd_objsDT <- DT::renderDataTable({})

#ships
output$ana_upd_shipsDT <- DT::renderDataTable({
  datatable(data.frame(col=rv$ships$name),rownames = FALSE,options = list(
    headerCallback = JS(
      "function(thead, data, start, end, display){",
      "  $(thead).remove();",
      "}"),dom = 't',pageLength = length(rv$ships$name),scrollY = 500, scroller = TRUE
  ),editable = 'cell',selection = 'none')})

observeEvent(input$ana_upd_shipsDT_cell_edit, {
  info = input$ana_upd_shipsDT_cell_edit
  i = info$row
  j = info$col
  v = info$value
  shinyalert(
    title = "Запрос на редактирование БД",
    text = "Значение элемента списка отредактировано! Изменить значение в этом списке и в БД? [Недоступно в ДЕМОВЕРСИИ, см. код на github]",
    closeOnEsc = FALSE, closeOnClickOutside = FALSE,
    html = FALSE, type = "warning", showConfirmButton = TRUE, showCancelButton = TRUE,
    confirmButtonText = "Да", confirmButtonCol = "#AEDEF4", cancelButtonText = "Нет",
    animation = TRUE, callbackR = function(x) {
      if(x == TRUE){
        #Disabled in demo
        #dbSendQuery(conn, paste0("UPDATE ships Set name = '",v,"' where name = '",rv$ships$name[i],"';"))
        #rv$ships <- dbGetQuery(conn, "SELECT name FROM ships order by ship_id;")
      }
      else{
        replaceData(dataTableProxy('ana_upd_shipsDT'), data.frame(col=rv$ships$name),rownames = FALSE)
      }
    }
  )
})

#regions
output$ana_upd_regsDT <- DT::renderDataTable({
  datatable(data.frame(col=rv$regions$name),rownames = FALSE,options = list(
    headerCallback = JS(
      "function(thead, data, start, end, display){",
      "  $(thead).remove();",
      "}"),dom = 't',pageLength = length(rv$regions$name),scrollY = 500, scroller = TRUE
  ),editable = 'cell',selection = 'none')})

observeEvent(input$ana_upd_regsDT_cell_edit, {
  info = input$ana_upd_regsDT_cell_edit
  i = info$row
  j = info$col
  v = info$value
  shinyalert(
    title = "Запрос на редактирование БД",
    text = "Значение элемента списка отредактировано! Изменить значение в этом списке и в БД? [Недоступно в ДЕМОВЕРСИИ, см. код на github]",
    closeOnEsc = FALSE, closeOnClickOutside = FALSE,
    html = FALSE, type = "warning", showConfirmButton = TRUE, showCancelButton = TRUE,
    confirmButtonText = "Да", confirmButtonCol = "#AEDEF4", cancelButtonText = "Нет",
    animation = TRUE, callbackR = function(x) {
      if(x == TRUE){
        #Disabled in demo
        #dbSendQuery(conn, paste0("UPDATE regions Set name = '",v,"' where name = '",rv$regions$name[i],"';"))
        #rv$ships <- dbGetQuery(conn, "SELECT name FROM regions order by region_id;")
      }
      else{
        replaceData(dataTableProxy('ana_upd_regsDT'), data.frame(col=rv$regions$name),rownames = FALSE)
      }
    }
  )
})

#pollutants
output$ana_upd_pollsDT <- DT::renderDataTable({
  datatable(data.frame(col=rv$pollutants$name),rownames = FALSE,options = list(
    headerCallback = JS(
      "function(thead, data, start, end, display){",
      "  $(thead).remove();",
      "}"),dom = 't',pageLength = length(rv$pollutants$name),scrollY = 500, scroller = TRUE
  ),editable = 'cell',selection = 'none')})

observeEvent(input$ana_upd_pollsDT_cell_edit, {
  info = input$ana_upd_pollsDT_cell_edit
  i = info$row
  j = info$col
  v = info$value
  shinyalert(
    title = "Запрос на редактирование БД",
    text = "Значение элемента списка отредактировано! Изменить значение в этом списке и в БД?  [Недоступно в ДЕМОВЕРСИИ, см. код на github]",
    closeOnEsc = FALSE, closeOnClickOutside = FALSE,
    html = FALSE, type = "warning", showConfirmButton = TRUE, showCancelButton = TRUE,
    confirmButtonText = "Да", confirmButtonCol = "#AEDEF4", cancelButtonText = "Нет",
    animation = TRUE, callbackR = function(x) {
      if(x == TRUE){
        #Disabled in demo
        #dbSendQuery(conn, paste0("UPDATE pollutants Set name = '",v,"' where name = '",rv$pollutants$name[i],"';"))
        #rv$ships <- dbGetQuery(conn, "SELECT name FROM pollutants order by pollutant_id;")
      }
      else{
        replaceData(dataTableProxy('ana_upd_pollsDT'), data.frame(col=rv$pollutants$name),rownames = FALSE)
      }
    }
  )
})
  
observeEvent(input$ana_upd_addshipAB, {
  shinyalert("Отказ в доступе", "Недоступно в ДЕМОВЕРСИИ, см. код на github!", type = "warning")
  #Disabled in demo
  #dbSendQuery(conn,"SELECT setval('ships_ship_id_seq', (SELECT MAX(ship_id) from ships));")
  #dbSendQuery(conn, paste0("insert into ships(name) values ('",input$ana_upd_addshipTI,"');"))
  #rv$ships <- dbGetQuery(conn, "SELECT name FROM ships order by ship_id;")
})

observeEvent(input$ana_upd_addregAB, {
  shinyalert("Отказ в доступе", "Недоступно в ДЕМОВЕРСИИ, см. код на github!", type = "warning")
  #Disabled in demo
  #dbSendQuery(conn,"SELECT setval('regions_region_id_seq', (SELECT MAX(region_id) from regions));")
  #dbSendQuery(conn, paste0("insert into regions(name) values ('",input$ana_upd_addregTI,"');"))
  #rv$regions <- dbGetQuery(conn, "SELECT name FROM regions order by region_id;")
})

observeEvent(input$ana_upd_addpollAB, {
  shinyalert("Отказ в доступе", "Недоступно в ДЕМОВЕРСИИ, см. код на github!", type = "warning")
  #Disabled in demo
  #dbSendQuery(conn,"SELECT setval('pollutants_pollutant_id_seq', (SELECT MAX(pollutant_id) from pollutants));")
  #dbSendQuery(conn, paste0("insert into pollutants(name) values ('",input$ana_upd_addpollTI,"');"))
  #rv$pollutants <- dbGetQuery(conn, "SELECT name FROM pollutants order by pollutant_id;")
})

#### END ANA_UPD Load Lists ####

#### INI ANA_UPD Restore defaul DB ####

observeEvent(input$ana_upd_restoreAB,{
  #UNDER DEVELOPMENT
  shinyalert("Отказ в доступе", "Недоступно в ДЕМОВЕРСИИ, см. код на github!", type = "warning")
  
  #Disabled in demo
  # query1=paste0("DELETE FROM regions;");
  # dbSendQuery(conn, query1);
  # query1=paste0("DELETE FROM ships;");
  # dbSendQuery(conn, query1);
  # query1=paste0("DELETE FROM pollutants;");
  # dbSendQuery(conn, query1);
  # 
  # print("ships")
  # print(dbGetQuery(conn,"Select count(*) from ships;"))
  # print("regions")
  # print(dbGetQuery(conn,"Select count(*) from regions;"))
  # print("pollutants")
  # print(dbGetQuery(conn,"Select count(*) from pollutants;"))
  # print("expeditions")
  # print(dbGetQuery(conn,"Select count(*) from expeditions;"))
  # print("stations")
  # print(dbGetQuery(conn,"Select count(*) from stations;"))
  # 
  # print("b_samples")
  # print(dbGetQuery(conn,"Select count(*) from b_samples;"))
  # print("s_samples")
  # print(dbGetQuery(conn,"Select count(*) from s_samples;"))
  # print("w_samples")
  # print(dbGetQuery(conn,"Select count(*) from w_samples;"))
  # 
  # ana_upd_local_ships <- read.csv("restoredata/Ships.csv",sep=",")
  # ana_upd_local_regions <- read.csv("restoredata/Regions.csv",sep=",")
  # ana_upd_local_exps <- read.csv("restoredata/Exps5.csv",sep=",")
  # ana_upd_local_stations <- read.csv("restoredata/Stations.csv",sep=",")
  # 
  # ana_upd_local_pollutants <- read.csv("restoredata/Pollutants.csv",sep=",")
  # ana_upd_local_bs <- read.csv("restoredata/b_samples.csv",sep=",")
  # ana_upd_local_ws <- read.csv("restoredata/w_samples.csv",sep=",")
  # ana_upd_local_ss <- read.csv("restoredata/s_samples.csv",sep=",")
  # 
  # print("...Restore DB...")
  # 
  # dbWriteTable(conn, "ships", ana_upd_local_ships,append = TRUE)
  # print("ships")
  # print(dbGetQuery(conn,"Select count(*) from ships;"))
  # 
  # dbWriteTable(conn, "regions", ana_upd_local_regions,append = TRUE)
  # print("regions")
  # print(dbGetQuery(conn,"Select count(*) from regions;"))
  # 
  # dbWriteTable(conn, "pollutants", ana_upd_local_pollutants,append = TRUE)
  # print("pollutants")
  # print(dbGetQuery(conn,"Select count(*) from pollutants;"))
  # 
  # dbWriteTable(conn, "expeditions", ana_upd_local_exps,append = TRUE)
  # print("expeditions")
  # print(dbGetQuery(conn,"Select count(*) from expeditions;"))
  # 
  # print("stations")
  # dbWriteTable(conn, "stations", ana_upd_local_stations,append = TRUE)
  # print(dbGetQuery(conn,"Select count(*) from stations;"))
  # 
  # print("b_samples")
  # dbWriteTable(conn, "b_samples", ana_upd_local_bs,append = TRUE)
  # print(dbGetQuery(conn,"Select count(*) from b_samples;"))
  # 
  # print("s_samples")
  # dbWriteTable(conn, "s_samples", ana_upd_local_ss,append = TRUE)
  # print(dbGetQuery(conn,"Select count(*) from s_samples;"))
  # 
  # print("w_samples")
  # dbWriteTable(conn, "w_samples", ana_upd_local_ws,append = TRUE)
  # print(dbGetQuery(conn,"Select count(*) from w_samples;"))
})
#### END ANA_UPD Restore defaul DB ####

#### INI ANA_UPD Show Expeditions ####
ana_upd_allexpsDF <- reactiveVal(NULL)

output$ana_upd_allexpsDT <- DT::renderDataTable(datatable(ana_upd_allexpsDF(),rownames= FALSE))

observeEvent(input$ana_upd_getallexpsAB,{
  query1=paste0("SELECT * FROM (SELECT expeditions.exp_id,ships.name,expeditions.start_date,expeditions.stop_date 
                FROM expeditions INNER JOIN ships ON expeditions.ship_id = ships.ship_id) as local 
                ORDER BY TO_DATE(local.start_date, 'DD/MM/YYYY') DESC;");
  res1 <- dbGetQuery(conn, query1);
  res1 = as.data.frame(res1);
  ana_upd_allexpsDF(res1)
 
})

#### END ANA_UPD Show Expeditions ####

#### INI ANA_UPD Delete expedition ####
observeEvent(input$ana_upd_delexpAB,{
  shinyalert("Отказ в доступе", "Недоступно в ДЕМОВЕРСИИ, см. код на github!", type = "warning")
  
  #Disabled in demo
  # query1=paste0("DELETE FROM expeditions WHERE exp_id = ",input$ana_upd_delexpTI,";");
  # print(query1)
  # dbSendQuery(conn, query1);
})
#### END ANA_UPD Delete expedition ####

#### INI ANA_UPD Load data from xls/xlsx ####
#UNDER DEVELOPMENT - use only sample xlsx (table_to_update_DB.xls)

ana_upd_loadedDF <- reactiveVal(NULL)
ana_upd_xlsFileName <- reactiveVal(NULL)
ana_upd_prevrecordregion <- reactiveVal(NULL)

output$ana_upd_loadedDT <- DT::renderDataTable(
    {
      if(is.null(ana_upd_loadedDF())) return()
      ana_upd_loadedDF() %>%  datatable(
        
        colnames=c('Шифр' = '...2',
                   'Объект' = '...5',
                   'Район' = '...7',
                   'Комментарий' = '...8',
                   'Дата' = '...10',
                   '№' = '...11',
                   'Ш DDDD' = '...12',
                   'Д DDDD' = '...13',
                   'Широта' = '...14',
                   'Долгота' = '...15'
        ),
      options=list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(visible=FALSE, targets=1:38+38)),language = list(url = "Russian.json"),
                   scrollY = 500, scroller = TRUE, scrollX = T,pageLength = 100)
    ) %>% 
      formatStyle(
        columns = 1:38, 
        valueColumns = 1:38+39, 
        border = '1px solid #a8ca2e',
        backgroundColor = styleEqual(c(1,0,2), c("red", "#CCF8AD","#FF9473")) #ccffcc"))
)})

observe({
  if(is.null(input$ana_upd_loadxlsFI)) return()
  ana_upd_xlsFileName(input$ana_upd_loadxlsFI)
})


observeEvent(input$ana_upd_submitAB,{
  # if(is.null(input$ana_upd_loadxlsFI)) {
  #   shinyalert("Файл БД (Excel) не выбран или не может быть прочитан", 
  #                                                  paste0("Выберите файл БД, а если он выбран, то проверьте правильность структуры (последовательность столбцов)"),
  #                                                         type = "error")
  #   return()
  # }
  

  ana_upd_xlsFileName("table_to_update_DB.xls") # UNDER DEVELOPMENT
  
  ana_upd_loadedxlsx <- read_excel(ana_upd_xlsFileName(),col_names = FALSE,col_types=c(rep("text",each=9),"date",rep("text",each=39)))
  ana_upd_loadedxlsx <- ana_upd_loadedxlsx %>% select("...2","...5","...7","...8","...10":"...15","...19":"...47")
  ana_upd_loadedxlsx <- ana_upd_loadedxlsx %>% subset(...2 %in% c("19001":"19033"))
  
  #-desc, +asc
  ana_upd_loadedxlsx = ana_upd_loadedxlsx %>% arrange(...2) #...10,...14,...15)  
  df <- ana_upd_loadedxlsx
  m <- matrix(0, ncol = ncol(df)-1, nrow = nrow(df))
  
  ana_upd_startDate = as.POSIXct(input$ana_upd_datesFromTI,format="%d/%m/%Y")
  ana_upd_endDate = as.POSIXct(input$ana_upd_datesToTI,format="%d/%m/%Y")

  ana_upd_prevrecordregion(ana_upd_loadedxlsx$...7[1])
  tempreg <- ana_upd_prevrecordregion()
  bord <- readOGR(paste0("./seas/",tempreg),"sea")
  lons <- bord@polygons[[1]]@Polygons[[1]]@coords[,1]
  lats <- bord@polygons[[1]]@Polygons[[1]]@coords[,2]
  
  for (i in 1:nrow(ana_upd_loadedxlsx)){
    print(ana_upd_loadedxlsx$...8[i])
    ## check region name
    tempreg = ana_upd_loadedxlsx$...7[i]
    if (!(tempreg %in% rv$regions$name))
    {
      m[i,1] <- 2
      m[i,3] <- 1
    }
    else{
      #№ check coordinates
      if(tempreg != ana_upd_prevrecordregion()){
        bord <- readOGR(paste0("./seas/",tempreg),"sea")
        lons <- bord@polygons[[1]]@Polygons[[1]]@coords[,1]
        lats <- bord@polygons[[1]]@Polygons[[1]]@coords[,2]
      }
      tval = sp::point.in.polygon(point.x = as.numeric(ana_upd_loadedxlsx$...14[i]),
                                   point.y = as.numeric(ana_upd_loadedxlsx$...15[i]),
                                   pol.x = lats,
                                   pol.y = lons,
                                   mode.checked = TRUE)
      if(tval == 0){
        m[i,1] <- 2
        m[i,7] <- 1
        m[i,9] <- 1
      }
    }
    #№ check dates 
    tempdate = as.POSIXct(ana_upd_loadedxlsx$...10[i])
    print(tempdate)
    if(tempdate > ana_upd_endDate | tempdate < ana_upd_startDate){
      print("False")
      m[i,1] <- 2
      m[i,5] <- 1
    }
  }
  ana_upd_loadedDF(df %>% cbind(m))
})
#### END ANA_UPD Load data from xls/xlsx ####

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#### INI ANA_ANA Display data ####
#UNDER DEVELOPMENT
output$ana_ana_expCountTO <- renderText("130")
  #as.integer(dbGetQuery(conn, "select count(*) from expeditions;")$count)
output$ana_ana_landExpCountTO <- renderText("18")
  #as.integer(dbGetQuery(conn, "select count(*) from expeditions where ship_id = 26;")$count)
output$ana_ana_seaExpCountTO <- renderText("112") 
  #as.integer(dbGetQuery(conn, "select count(*) from expeditions where ship_id <> 26;")$count))
output$ana_ana_nordExpCountTO <- renderText("98") 
  #as.integer(dbGetQuery(conn, "select count(distinct exp_id) from (select expeditions.exp_id from expeditions inner join stations ON expeditions.exp_id = stations.exp_id where stations.region_id not in (2,8,11)) as vals;")$count))
output$ana_ana_southExpCountTO <- renderText("32")
  #as.integer(dbGetQuery(conn, "select count(distinct exp_id) from (select expeditions.exp_id from expeditions inner join stations ON expeditions.exp_id = stations.exp_id where stations.region_id in (2,8,11)) as vals;")$count))
output$ana_ana_stCountTO <- renderText("2323")
 # as.integer(dbGetQuery(conn, "select count(*) from stations;")$count))
output$ana_ana_totalMeaCountTO <- renderText("7646")
 # as.integer(dbGetQuery(conn, "select count(*) from b_samples;")$count)+
#  as.integer(dbGetQuery(conn, "select count(*) from w_samples;")$count)+
 # as.integer(dbGetQuery(conn, "select count(*) from s_samples;")$count)

output$ana_ana_bMeaCountTO <- renderText("1159")
 # as.integer(dbGetQuery(conn, "select count(*) from b_samples;")$count))
output$ana_ana_sMeaCountTO <- renderText("5004")
#  as.integer(dbGetQuery(conn, "select count(*) from s_samples;")$count))
output$ana_ana_wMeaCountTO <- renderText("1483")
 # as.integer(dbGetQuery(conn, "select count(*) from w_samples;")$count))
output$p <- renderPlotly({
  df <- dbGetQuery(conn, "select w_samples.sample_id as id,stations.region_id as reg,w_samples.val as val from w_samples inner join stations on w_samples.station_id = stations.station_id where (w_samples.pollutant_id = 1) and (stations.region_id in (1,3,4));") 

  df$reg[df$reg == 1] <- "Баренцево"
  df$reg[df$reg == 3] <- "Карское"
  df$reg[df$reg == 4] <- "Лаптевых"
  df$reg <- as.factor(df$reg)
  
  f <- list(family = "Courier New, monospace",size = 18,color = "black")
  x <- list(title = "<b>Исследуемое море</b>",titlefont = f)
  y <- list(title = "<b>Значение концентрации</b>",titlefont = f)
  plot_ly(df,x =~reg,y = ~val, color = ~reg, type = "box") %>%
    layout(title = "<br><b>Концентрации Cs-137 в воде морей Арктики</b>",xaxis = x, yaxis = y)
})

#### END ANA_ANA Display data ####

}) # end of server