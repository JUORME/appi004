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
library(shinythemes)
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
	ui = fluidPage( theme = shinytheme("lumen"), useShinyjs(),

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
				tabPanel("Precios de costo",
					sidebarPanel( style='margin-left:-10',

						actionButton("idBtn1","Precios de coste", class = "btn-success"),
						br(),
						downloadButton('idBtn2','Download', class = "btn-info"),
						textOutput("selected_var")
					),
					fluidRow(
						column(6,
							)
						),
					fluidRow(
						column(7,
							tabPanel("Tabla Inventario insuficiente",DT::dataTableOutput('table0.output'),style = 'font-size:90%')
						)
					)
				)
			)
		),	
	server <- function(input, output){

		shinyjs::hide("idBtn2")

		source(paste(pathglo,"/functions/extractos.r",sep=""))
		data <- extrac()

		fechaid <- paste("Actualizado al ",Sys.Date(),sep="")

		observeEvent(input$idBtn1,{

			output$table0.output <- DT::renderDataTable({DT::datatable(data)}) 
			output$selected_var <- renderText({fechaid})
			shinyjs::show("idBtn2")

		})

			output$idBtn2 <- downloadHandler(

				filename = function(){
					paste("Extractos-",Sys.Date(),".xlsx",sep="")
				},
				content = function(file) {
					write.xlsx(data,file,row.names=TRUE)
				})


		
 	}
)
