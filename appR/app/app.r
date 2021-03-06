#####################################################################################
#  Proyecto D365 FO                                                                 #
#  Extracción de datos Stock Insuficiente                                           #
#  Autor: Junior T. Ortiz Mejia                                                     #
#  Fecha: 08/03/2021                                                                #                                                                              
#####################################################################################

options(encoding = "utf-8")
options(shiny.maxRequestSize = 30*1024^2)
options(warn=-1)

ent <- 2
if(ent == 1){
	pathglo <- "D:/github/appi010/appR"
}else {
	pathglo <- "/srv/shiny-server/appi010/appR"
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
			navbarPage( "Modulo Análisis de Productos - Trujillo Investment 2021",
				tabPanel("Precios de Costo",
					sidebarPanel( style='margin-left:-10',

						actionButton("idBtn1","Precios de coste", class = "btn-danger"),
						downloadButton('idBtn2','Download', class = "btn-success"),
						textOutput("selected_var")

					),
						valueBoxOutput("progressBox1",width = 2),
						valueBoxOutput("progressBox2",width = 2),
					fluidRow(
						column(5
						)
					),					
					fluidRow(
						column(6,
							tabPanel("Tabla de Productos",DT::dataTableOutput('table0.output'),style = 'font-size:90%')
						),

						column(5,
							textOutput('infor'),
				            tabPanel("Tabla de Facturas", DT::dataTableOutput('table1.output'),style = "font-size:95%" )
	          				)
					),
				)
			)
		),	
	server <- function(input, output,session ){
		
		shinyjs::hide("idBtn2")
	# Carga del algoritmo del ultimo comprobante regustrado
		source(paste(pathglo,"/functions/ultima_compra.r",sep=""))
		data <- ultimacompra()

	# Algoritmo com caracteristicas reactivas
		Product <- reactive({data})

	# Descripción para archivo de descarga
		fechaid <- paste("Actualizado al ",Sys.Date(),sep="")

	# Progreso de avance 
		progre1 <- subset(data, data[,8] == 1)
		progre2 <- subset(data, data[,4] == 0)

		
	# Observe event del Btn 01 - Calcular los precios de costo
		observeEvent(input$idBtn1,{

			output$table0.output <- DT::renderDataTable({

				DT::datatable(Product(),selection = "single", 
					options = list(pageLength = 15,
									autoWidth = TRUE,
									filter = "top"))
				}) 

		# Visualizar la fecha de consulta
			output$selected_var <- renderText({fechaid})

		# Visualizar el KPI
			output$progressBox1 <- renderValueBox({
                valueBox(
                  paste0(nrow(progre1)), "Productos Observados", icon = icon("list"),
                  color = 'green'
                )
			})
			output$progressBox2 <- renderValueBox({
                valueBox(
                  paste0(nrow(progre2)), "Productos con Costo Cero", icon = icon("list"),
                  color = 'yellow'
                )
             })

		# Muestra el segundo btn de Download
			shinyjs::show("idBtn2")
		
		# Funacion para descargar el archivo data 
			output$idBtn2 <- downloadHandler(

				filename = function(){
					paste("Precios de Costo Al",Sys.Date(),".xlsx",sep="")
				},
				content = function(file) {
					write.xlsx(data,file,row.names=TRUE)
				})

         	})


	# Funcion que permite visualizar la segunda tabla seleccionando una fila de la tabla0
        observeEvent(input$table0.output_rows_selected, {
			 	row_count <- input$table0.output_rows_selected
			 	dataselec <- Product()[row_count,]

        		output$infor <- renderText({
					 
					paste("Producto: ",dataselec[,1],dataselec[,2],sep = " ")
	        	})

	        	output$table1.output <- DT::renderDataTable({

	        		dataselec1 <- read.csv(paste(pathglo,"/upload/alldata.csv",sep = ""),sep=",")
	        		dataselec1.1 <- dataselec1[,c(1,2,4,5,6,7,9)]
	        		dataselec2 <- subset(dataselec1.1, ItemId == as.numeric(dataselec[,1]) )

	        		names(dataselec2) <- c("IdCompra","Factura","Cod","UniCompra","UniPrecio","FechaEmision","Moneda")

					DT::datatable(dataselec2,selection = 'single', 
						options = list(pageLength = 10,
									autoWidth = TRUE,
									filter = "top"))
	        	})


	    })

})