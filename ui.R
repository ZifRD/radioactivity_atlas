#Author: Zaporozhtsev I.F.
#Created: 2019-2020

#### Libraries ####

library(shiny)
library(shinyjs)
library(shinyjqui)
library(shinythemes)
library(shinyBS)
library(htmlwidgets)
library(leaflet)
library(rmarkdown)
library(knitr)
library(maptools)
library(mapview)
library(mapedit)
library(dplyr)
library(xts)
library(ggplot2)
library(dygraphs)
library(RColorBrewer)
library(rgeos)
library(mapproj)
library(rgdal)
library(DT)
library(leaflet.extras)
library(shinyalert)
library(shinyWidgets)
library(plotly)
#### end Libraries ####

shinyUI(fluidPage(useShinyjs(),useShinyalert(),
  setBackgroundColor(
    color = c("#AFCDE7", "#081A66"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Атлас радиационного загрязнения северных и южных морей России (ДЕМО)", windowTitle = "MMBIRadio2019"),
  includeCSS('./www/styles.css'),
  sidebarLayout(fluid = TRUE,
  sidebarPanel(width = 2,
#### INI sidebarPanel ####
      tags$div(id="ANA_SIDEPANELBLOCKST",
           h4("Стандартный запрос"),
           p("Выберите период работ,\n район, судно, объект и поллютант. Для района, судна и поллютанта доступен вариант \"Все\"."),
         
           dateRangeInput("ana_streqdateRI", "Период", start="1991-01-01", end="2019-01-01", format = "dd/mm/yyyy", separator = " до "),
           verticalLayout(selectInput(inputId="ana_streq_regionSI",label = "Район",choices = c(""), selected = ""),
            selectInput(inputId="ana_streq_shipSI",label = "Судно",choices = c(""), selected = ""),
            selectInput(inputId="ana_streq_objectSI",label = "Объект",choices = c(""), selected = ""),
            selectInput(inputId="ana_streq_pollSI",label = "Поллютант",choices = c(""), selected = ""),
            div(style="margin:20px;",fluidRow(column(12,actionButton("ana_streq_runB","Построить выборку", width="100%")))),
            div(style="margin:20px;",tags$img(src="mmbi.png",height="90%",width="90%")))
)
#### END sidebarPanel ####
  ),

#######################
# INI mainPanel
#######################
  mainPanel(width = 10,
          tags$style(HTML("
                  .tabbable > .nav > li > a                  {font-weight: bold; background-color: #F5F5F5;  color:black}
                  .tabbable > .nav > li[class=active]    > a {background-color: gold; color:black}
                  ")),
    tabsetPanel(id = "tabset",
    # Стандартный запрос ----      
      tabPanel("Стандартный запрос",value="1",
               column(12,style="background-color:#D3E4F3",
               tags$head(
                 includeScript("./www/gomap.js")
               ),
               fluidRow(column(12, verbatimTextOutput("staticdate1"))),
               fluidRow(column(12, leafletOutput("ana_streq_map"),
                            absolutePanel(right = 15, top = 5, left = "60%", 
                                  class = "panel panel-default",
                                 textOutput("ana_streq_abs_markerlatlonTO"),
                                 hidden(actionButton("ana_streq_abs_stationAB",label = "Перейти к станции")),
                                 hidden(actionButton("ana_streq_abs_expAB",label = "Перейти к экспедиции")),
                                 hidden(actionButton("ana_streq_abs_reqAB",label = "Перейти к базовой выборке"))
                            )
                        )),
               br(),
               fluidRow(column(12,verbatimTextOutput('ana_streq_hoverLatLonTO'))),
               fluidRow(column(12,verbatimTextOutput('ana_streq_clickedmarkerstateTO'))),
               downloadButton('ana_streq_exportCSV', 'Экспорт в CSV'),
               downloadButton('ana_streq_exportXLS', 'Экспорт в MS Excel'),
               downloadButton('ana_streq_exportDOC', 'Экспорт в MS Word'),
               br(),br(),
               fluidRow(column(12,div( dataTableOutput('ana_streq_datatable'),  style = "font-size:80%")))
      )),
   # Запрос по схеме Excel ---- 
      tabPanel("Запрос по схеме Excel",value="3",
               column(12,style="background-color:#D3E4F3",
			         fluidRow(column(12,
			           radioButtons("ana_excel_objectRB", label = "Фильтр объектов исследования",
			                      choices = list("вода", "грунт", "биота"), 
			                      selected = "вода", inline=T)
			         )),
			         fluidRow(column(12,
			                         actionButton(inputId = "ana_excel_sortablecollistAB",label = "Свернуть/развернуть панель выбора столбцов для отображения и фильтров по значениям"),
			                         actionButton(inputId = "ana_excel_confirmsortablecollistAB",label = "Принять настройки"),
			                         shinyjs::hidden(div(id="ana_excel_sortablePanel",
			                             hr(),
			                             actionButton("ana_excel_openTop","Загрузить последние сохраненные настройки"),
			                             actionButton("ana_excel_saveTop","Сохранить настройки"),
			                             actionButton("ana_excel_opendefaultTop","Загрузить настройки \"По умолчанию\""),br(),
			                             tags$style(HTML(".ui-sortable {
                                      width: 1200px !important;
                			             } ")),
                                   uiOutput('ana_excel_multiobject'),
			                             actionButton("ana_excel_openBottom","Загрузить последние сохраненные настройки"),
			                             actionButton("ana_excel_saveBottom","Сохранить настройки"),
			                             actionButton("ana_excel_opendefaultBottom","Загрузить настройки \"По умолчанию\""),
			                             actionButton("ana_excel_hideBottom","Свернуть панель"),
			                             hr()
			                         ))
			         )),
               fluidRow(column(5,selectInput("ana_excel_FpolnameSI","",choices = NULL),
                                 actionButton("ana_excel_FremoveallAB","Удалить все фильтры",width="100%"),
                                 actionButton("ana_excel_FremoveAB","Удалить последний фильтр",width="100%")),
                        column(7,
                               column(3,div(style="padding-top: 0px;", textInput("ana_excel_FlowerTI","",value="0",width="100%"))),
                               column(6,div(style="padding-top: 17px;",actionButton("ana_excel_FaddfilterAB","≤ Ограничить ≤",width="100%"))),
                               column(3,div(style="padding-top: 0px;",textInput("ana_excel_FupperTI","",value="10",width="100%"))),
                               htmlOutput("ana_excel_FreportHO")
                        )
               ),
			         fluidRow(column(12, DTOutput("ana_excel_datatable"))),
			         downloadButton('ana_excel_exportCSV', 'Экспорт в CSV'),
			         downloadButton('ana_excel_exportXLS', 'Экспорт в MS Excel'),
			         downloadButton('ana_excel_exportDOC', 'Экспорт в MS Word')
      )),
    # Запрос по области ---- 
      tabPanel("Запрос по области",value="4",
               column(12,style="background-color:#D3E4F3",
               h4("Загрузка текстового файла c координатами одного полигона"),
               fluidRow(column(3,fileInput("ana_poly_oneLoadFI","Загрузить файл со строкой",buttonLabel = "Обзор",placeholder = "Файл не выбран")
                        ),
                        column(9,htmlOutput("ana_poly_oneContentHO",style='margin-top:25px'))),
               div(style="height: 2px; border: 1px solid black;"),br(),  
               fluidRow(column(3,fileInput("ana_poly_csvLoadFI","Загрузить файл формата CSV",buttonLabel = "Обзор",placeholder = "Файл не выбран")
                       ),
                        column(9,htmlOutput("ana_poly_csvContentHO",style='margin-top:25px'))),
               div(style="height: 2px; border: 1px solid black;"),br(),  
               fluidRow(
                 div(style="display: inline-block;",fileInput("ana_poly_serverLoadFI",
                                                              "Выбрать шейпфайл из каталога на сервере",buttonLabel = "Обзор",placeholder = "Файл не выбран")),
                 div(style="display: inline-block;",fileInput("ana_poly_shpLoadFI",
                                                              "Импортировать шейпфайл",buttonLabel = "Обзор",placeholder = "Файл не выбран"))),
               div(style="height: 2px; border: 1px solid black;"),br(),  
               fluidRow(
                 h4("Построение полигона вручную на интерактивной карте"),
                 div(style="display: inline-block;",actionButton("ana_poly_clicksAB","Задать вершины полигона кликами мыши на карте")),
                 div(style="display: inline-block;",actionButton("ana_poly_acceptclicksAB","Использовать построенный полигон")),
                 div(style="display: inline-block;",actionButton("ana_poly_removeAB","Удалить полигон")),
                 helpText("Нажмите на кнопку на карте, постройте полигон, нажмите Использовать построенный полигон, перейдите на вкладку Стандартный запрос. Если фильтр слева указывает море, которому соответствует полигон, то метки будут отображены на карте стандартного запроса.")
               ),
               br(),br(),
               div(style="height: 2px; border: 1px solid black;"),br(),  
               fluidRow(column(12, editModUI("ana_poly_map")))
    )),
   
   tabPanel("Редактирование БД",value="5",
            
            column(12,style="background-color:#D3E4F3",
                   
                   tabsetPanel(id = "tabset_upd",       
                      tabPanel("Обновление списков",value="1",
                               column(3,h4("Объекты"),
                                      DTOutput("ana_upd_objsDT"),
                                      textInput("ana_upd_addnobjTI", label = 'Название', value = ""),
                                      textInput("ana_upd_adduobjTI", label = 'Ед. измерения', value = ""),
                                      checkboxInput("ana_upd_addobjhoriChk", label = "Горизонт", value = TRUE),
                                      checkboxInput("ana_upd_addobjdepChk", label = "Глубина", value = TRUE),
                                      checkboxInput("ana_upd_addobjlayChk", label = "Слой", value = TRUE),
                                      actionButton("ana_upd_addobjAB","Добавить")),
                               column(3,h4("Суда"),
                                      DTOutput("ana_upd_shipsDT"),
                                      textInput("ana_upd_addshipTI", label = "Название", value = ""),
                                      actionButton("ana_upd_addshipAB","Добавить")),
                               column(3,h4("Поллютанты"),
                                      DTOutput("ana_upd_pollsDT"),
                                      textInput("ana_upd_addpollTI", label = "Название", value = ""),
                                      actionButton("ana_upd_addpollAB","Добавить")),
                               column(3,h4("Районы"),
                                      DTOutput("ana_upd_regssDT"),
                                      textInput("ana_upd_addregTI", label = "Название", value = ""),
                                      actionButton("ana_upd_addregAB","Добавить"))
                      ),
                      tabPanel("Добавление экспедиции",value="2",
                        fluidRow(column(12,actionButton("ana_upd_restoreAB","Восстановить БД по умолчанию"))),
                        fluidRow(column(12,actionButton("ana_upd_getallexpsAB","Список экспедиций"))),
                        fluidRow(column(12, DTOutput("ana_upd_allexpsDT"))),
                        fluidRow(column(12,div(style="display: inline-block; ",actionButton("ana_upd_delexpAB","Удалить экспедицию")),
                        div(style="display: inline-block; width:50px; ", textInput("ana_upd_delexpTI", label = NULL, value = "-1")))),
                       
                        fluidRow(column(12,
                          div(style="display: inline-block; width:350px;", strong("Название экспедиции (необязательное поле):")),
                          div(style="display: inline-block; width:700px;",textInput("ana_upd_expNameTI",label = NULL,value = "")))),
                        
                        fluidRow(column(12,selectInput("ana_upd_shipsSI","Выберите судно",choices = c(""),selected=""))),
                        fluidRow(column(12,
                        div(style="display: inline-block; width:130px;", p("Добавить судно:")),
                        div(style="display: inline-block; width:400px;",textInput("ana_upd_shipTI",label = NULL,value = "")))),
                        
                        fluidRow(column(12,strong("Введите даты начала и окончания экспедиции"))),
                        fluidRow(column(12,div(style="display: inline-block; width:10px; ",p("c")),
                        div(style="display: inline-block; width:200px; ", textInput("ana_upd_datesFromTI", label = NULL, value = "03/04/2019")),
                        div(style="display: inline-block; width:20px; ",p("по")),
                        div(style="display: inline-block; width:200px; ", textInput("ana_upd_datesToTI", label = NULL, value = "21/04/2019")))), 
                       
                        fluidRow(column(12,fileInput("ana_upd_loadxlsFI","Открыть xls/xlsx",buttonLabel = "Обзор",placeholder = "Файл не выбран"))),
                        fluidRow(column(12,div(style="display: inline-block; width:280px; ",strong("Загрузить записи из файла с номерами")),
                        div(style="display: inline-block; width:10px; ",p("c")),
                        div(style="display: inline-block; width:100px; ", textInput("ana_upd_recFromTI", label = NULL, value = "19001")),
                        div(style="display: inline-block; width:20px; ",p("по")),
                        div(style="display: inline-block; width:100px; ", textInput("ana_upd_recToTI", label = NULL, value = "19033")))), 
                        
                        fluidRow(column(12,actionButton("ana_upd_submitAB","Загрузить данные в БД"))),
                        br(),div(style="height: 2px; border: 1px solid black;"),br(),
                        fluidRow(column(12, DTOutput("ana_upd_loadedDT")))
                      )
            
                   )
            )),
   tabPanel("Анализ данных",value="6",column(8,style="background-color:#D3E4F3",
            fluidRow(column(12,
                            div(style="display: inline-block; width:253px;", strong("Общее количество экспедиций: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_expCountTO")))
            ),
            fluidRow(column(12,
                           div(style="display: inline-block; width:90px;", strong(" ")),
                           div(style="display: inline-block; width:160px;", strong("из них наземных: ")),
                           div(style="display: inline-block;",textOutput("ana_ana_landExpCountTO")))
            ),
            fluidRow(column(12,
                            div(style="display: inline-block; width:90px;", strong(" ")),
                            div(style="display: inline-block; width:160px;", strong("из них морских: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_seaExpCountTO")))
            ),
            fluidRow(column(12,
                            div(style="display: inline-block; width:90px;", strong(" ")),
                            div(style="display: inline-block; width:160px;", strong("из них в Арктике: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_nordExpCountTO")))
            ),
            fluidRow(column(12,
                            div(style="display: inline-block; width:90px;", strong(" ")),
                            div(style="display: inline-block; width:160px;", strong("из них на юге России: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_southExpCountTO")))
            ),
            ######
            
            fluidRow(column(12,
                            div(style="display: inline-block; width:253px;", strong("Общее количество станций: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_stCountTO")))
            ),

            ######
            fluidRow(column(12,
                            div(style="display: inline-block; width:253px;", strong("Общее число измерений: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_totalMeaCountTO")))
            ),
            fluidRow(column(12,
                            div(style="display: inline-block; width:90px;", strong(" ")),
                            div(style="display: inline-block; width:160px;", strong("из них для биоты: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_bMeaCountTO")))
            ),
            fluidRow(column(12,
                            div(style="display: inline-block; width:90px;", strong(" ")),
                            div(style="display: inline-block; width:160px;", strong("из них для воды: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_wMeaCountTO")))
            ),
            fluidRow(column(12,
                            div(style="display: inline-block; width:90px;", strong(" ")),
                            div(style="display: inline-block; width:160px;", strong("из них для грунта: ")),
                            div(style="display: inline-block;",textOutput("ana_ana_sMeaCountTO")))
            ),
            div(style="height: 2px; border: 1px solid black;"),br(),
            fluidRow(column(8,plotlyOutput(outputId = "p")))
   )),
   tabPanel("Справка",value="7",
            column(12,style="background-color:#D3E4F3",tags$iframe(src="manual.pdf", width="100%", height="600px", scrolling="auto")
   )),
   tabPanel("О программе",value="8",column(12, style="background-color:#D3E4F3",
                                           column(8,includeMarkdown("./www/helpabout.md")),column(4,tags$img(src="mmbi.png",height="100%",width="100%")))
    )
  )                


#######################
# END mainPanel
#######################
) #mainPanel   
) #sidebarLayout closed
#) #tabpanel of navbar
#) #navbar
))