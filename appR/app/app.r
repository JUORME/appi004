#####################################################################################
#  Proyecto D365 FO                                                                 #
#  Extracción de datos Stock Insuficiente                                           #
#  Autor: Junior T. Ortiz Mejia                                                     #
#  Fecha: 08/03/2021                                                                #                                                                              
#####################################################################################

options(encoding = "utf-8")
options(shiny.maxRequestSize = 30*1024^2)
options(warn=-1)

ent <- 1
if(ent == 1){
	pathglo <- "D:/github/appi005/appR"
}else {
	pathglo <- "/srv/shiny-server/appi001/appR"
}


library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinyjs)

library(httr)    
library(rjson)  
library(jsonlite)
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(stringr)


shinyApp(
	ui = fluidPage( theme = shinytheme("lumen"), useShinyjs(),useShinydashboard(),
			list(
				tags$head(HTML('<link rel="icon", href="http://181.65.149.162:4001/app014/img/cropped-fvtrujll02-32x32.png", type="image/png">')),
				tags$style(HTML("
						.navbar {left: -20px; }
						.navbar-default .navbar-brand { color: #FFF;
														front-size: 16px;
														background-color: #E1120B ;}
					"))
			),

			shinythemes::themeSelector(), #seleccionar themas libreria shinythemes
			navbarPage( "Modulo Análisis de Productos - Trujillo investment 2021",
				tabPanel("Precios de Costo",
					sidebarPanel( style='margin-left:-10',

						actionButton("idBtn1","Precios de coste", class = "btn-success"),
						downloadButton('idBtn2','Download', class = "btn-info"),
						textOutput("selected_var")
					),
					valueBoxOutput("progressBox1",width = 2),valueBoxOutput("progressBox2",width = 2),
					fluidRow(
						column(5,
						)
					),					
					fluidRow(
						column(5,
							tabPanel("Tabla de Productos",DT::dataTableOutput('table0.output'),style = 'font-size:90%')
						)
					),
					column(6,
		            htmlOutput("text3"),
		            tabPanel("Kardex Detallado", DT::dataTableOutput(outputId = 'table2.output'),style = "font-size:95%;top:10px;" )
		          )
				)
			)
		),	
	server <- function(input, output,session ){

		 global_dataread1 <- reactiveValues(dataread1 = NULL)

		shinyjs::hide("idBtn2")

		# source(paste(pathglo,"/functions/ultima_compra.r",sep=""))
		# data <- ultimacompra()

		data <- iris
		fechaid <- paste("Actualizado al ",Sys.Date(),sep="")

		observeEvent(input$idBtn1,{

			output$table0.output <- DT::renderDataTable({DT::datatable(data)}) 
			output$selected_var <- renderText({fechaid})
			shinyjs::show("idBtn2")

			output$progressBox1 <- renderValueBox({
                valueBox(
                  #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
                  paste0(nrow(data)), "Casos Observados", icon = icon("list"),
                  color = 'green'
                )
			})
			output$progressBox2 <- renderValueBox({
                valueBox(
                  #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
                  paste0(nrow(data)), "Productos Comprometidos", icon = icon("list"),
                  color = 'yellow'
                )
             })

			output$idBtn2 <- downloadHandler(

				filename = function(){
					paste("Extractos-",Sys.Date(),".xlsx",sep="")
				},
				content = function(file) {
					write.xlsx(data,file,row.names=TRUE)
				})


         	})


        observeEvent(input$table1.output_rows_selected, {


            #ALGORITMO PARA MOSTRAR VISTA PREVIA DEL ARCHIVO EXCEL

             selectedrowindex <- input$table2.output_rows_selected
             det<-global_dataread1$dataread1[selectedrowindex,]
             #det<-as.data.frame(det[1,3])

             source(paste(pathglo,"/scripts/read_data2b.r",sep=""))
             #source(paste(pathglo,"/scripts/read_data2.r",sep=""))
             global_dataread2$dataread2<-read_data2(ent,det)

             # mat <- global_dataread2$dataread2
             # if (sum(mat$Alert)>0) {
             #     output$text3 <- renderText({ paste("Validador en Tiempo Real desde Dynamics 365: ","<font color=\"#FF0000\"><b> Este producto PRESENTA NEGATIVOS EN EL KARDEX</b></font>") })
             # } else {
             #     output$text3 <- renderText({ paste("Validador en Tiempo Real desde Dynamics 365: ","<font color=\"#45C777\"><b> Este producto ha sido REPARADO EN LAS ULTIMAS HORAS</b></font>") })
             # }



             output$table2.output <- DT::renderDataTable({

                DT::datatable(global_dataread2$dataread2,rownames = FALSE,options =
                    list(pageLength = 20,lengthMenu
                      = list(c(20,50, 100, 300, -1), list('20', '50', '100','300', 'All')), paging = T)) %>%
                        formatStyle(
                          'Alert',
                          #transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
                          backgroundColor = styleEqual(
                            c(0,1), c('lightgreen', '#FC8591')
                          )
                        )
             })
		
 		})
