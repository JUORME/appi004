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
							htmlOutput('infor'),
				            tabPanel("Tabla de Facturas", DT::dataTableOutput('table1.output'),style = "font-size:95%" )
	          				)
					),
				)
			)
		),	
	server <- function(input, output,session ){
		# data <- iris
		source(paste(pathglo,"/functions/ultima_compra.r",sep=""))
		data <- ultimacompra()

		Product <- reactive({data})


		fechaid <- paste("Actualizado al ",Sys.Date(),sep="")

		progre1 <- subset(data, Val == 1)
		progre2 <- subset(data, UnitCost == 0)

		shinyjs::hide("idBtn2")

		observeEvent(input$idBtn1,{

			output$table0.output <- DT::renderDataTable({

				DT::datatable(Product(),selection = 'single', 
					options = list(pageLength = 15,
									autoWidth = TRUE,
									filter = "top"))
				}) 

			output$selected_var <- renderText({fechaid})

			

			output$progressBox1 <- renderValueBox({
                valueBox(
                  #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
                  paste0(nrow(progre1)), "Productos Observados", icon = icon("list"),
                  color = 'green'
                )
			})
			output$progressBox2 <- renderValueBox({
                valueBox(
                  #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
                  paste0(nrow(progre2)), "Productos con Costo Cero", icon = icon("list"),
                  color = 'yellow'
                )
             })

			shinyjs::show("idBtn2")
			 
			output$idBtn2 <- downloadHandler(

				filename = function(){
					paste("Precios de Costo Al",Sys.Date(),".xlsx",sep="")
				},
				content = function(file) {
					write.xlsx(data,file,row.names=TRUE)
				})

         	})


        observeEvent(input$table0.output_rows_selected, {
			 	row_count <- input$table0.output_rows_selected
			 	dataselec <- Product()[row_count,]

        		output$infor <- renderText({
				 # 	cat('Item Seleccionado: ')
					# cat(dataselec[,1]  )
					# cat(dataselec[,2] )
					 
					paste("Producto: ","<font color=\"#45C777\"><b>",dataselec[,1], " " ,dataselec[,2] ,"</b></font>",sep = "")
	        	})

	        	output$table1.output <- DT::renderDataTable({

	        		dataselec1 <- read.csv(paste(pathglo,"/upload/alldata.csv",sep = ""),sep=",")
	        		dataselec1.1 <- dataselec1[,c(1,2,4,5,6,7)]
	        		dataselec2 <- subset(dataselec1.1, ItemId == as.numeric(dataselec[,1]) )

					DT::datatable(dataselec2,selection = 'single', 
						options = list(pageLength = 10,
									autoWidth = TRUE,
									filter = "top"))
	        	})


	    })

})