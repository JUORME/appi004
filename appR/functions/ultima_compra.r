#####################################################################################
#  Proyecto D365 FO                                                                 #
#  Extraccion de precios de coste de ultima compra                                  #
#  Autor: Junior T. Ortiz Mejia                                                     #
#  Fecha: 22/03/2021                                                                #                                                                              
#####################################################################################






#Fijar el la ruta de trabajo

ent <- 1
if(ent == 1){
	pathglo <- "D:/github/appi005/appR/functions"
}else {
	pathglo <- "/srv/shiny-server/appi001/appR/functions"
}


setwd(pathglo)


#Blibliotecas requeridas
	library(httr)    
	library(rjson)  
	library(jsonlite)
	library(dplyr)
	library(openxlsx)
	library(data.table)
	library(bit64)
	library(stringr)

#Conexion a D365FO TRUJILLO INVESTMENT
	body <- list(grant_type = "client_credentials", client_id = "7cb678f1-2bc4-4456-a590-f7216b23dd88",
	client_secret = "~9X1PR2etZ1sHw1tsv-15Y.pD3F_RaTCxG", resource = "https://mistr.operations.dynamics.com")

	response <-POST("https://login.microsoftonline.com/ceb88b8e-4e6a-4561-a112-5cf771712517/oauth2/token",add_headers("Cookie: x-ms-gateway-slice=prod; stsservicecookie=ests; fpc=AqQZzzXZjstDgAtb0IfeeFZVotOLAQAAANAmrtYOAAAA"), body=body,encode = "form")

	datatoken <-fromJSON(content(response,type="text")) %>% as.data.frame
	tok_type<-as.character(datatoken[1,1])
	tok<-as.character(datatoken[1,7])
	token <- paste(tok_type," ",tok,"",sep="")


# Uso de la funcion para extraer datos con el Entity proporcionado	
	fi <- "2021-02-27"
	source("algoritmo_count.r")
	url1 <- paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesEntities/$count?$filter=CreatedDateTimeInvoice%20le%20",fi,"",sep="")
	count<-get_count_url(url1,token)

	lote<-10000
	source("genera_url.r")
	urldata1<-paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesEntities?$skip=rvar_s&$top=rvar_t&$select=PurchId,InvoiceId,PackingSlipId,PurchId,ItemId,PurchUnit,PurchPrice,InvoiceDocumentDate,CreatedDateTimeInvoice",sep="")
	vec1<-genera_url(count,lote,urldata1)

	source("function_get_collect.r")
	prodprice <- get_records_url(vec1,token)
	head(prodprice)


	prodprice$InvoiceDocumentDate <- as.character(as.POSIXct(prodprice$InvoiceDocumentDate, format="%Y-%m-%d",tz="UTC"))
	#prodprice$CreatedDateTimeInvoice <- as.character(as.POSIXct(prodprice$CreatedDateTimeInvoice, format="%Y-%m-%d %H:%M:%S",tz="UTC"))

# Opciones de Filtros

	source("conver_unid.r")
	p2 <- prodprice %>%
			group_by(ItemId)  %>% 
			filter(PurchPrice != 0.0000) %>%
			filter(InvoiceDocumentDate == max(InvoiceDocumentDate)) %>%
			mutate(NumUnit = conver_unidades(PurchUnit),
					UnitPrice =round(PurchPrice / as.numeric(NumUnit),2)) %>%
			filter(UnitPrice == max(UnitPrice)) %>%
			filter(CreatedDateTimeInvoice == max(CreatedDateTimeInvoice))


	p3 <- as.data.frame(p2)

# Extracción de precios de productos emitidos

	source("function_get_collect.r")
	emitprice <- get_records_url("https://mistr.operations.dynamics.com/data/ReleasedDistinctProducts?$select=ItemNumber,InventoryUnitSymbol,UnitCost",token)
	head(emitprice)

	q1 <- as.data.frame(emitprice)

	q1[,3] <- round(q1[,3],2)


#Unir los dos data frames

	u1 <- merge(q1 , p3 ,by.x="ItemNumber", by.y="ItemId", all.x=TRUE)

	u1 %>% mutate(diff = ifelse(UnitPrice == 0 , 0, 1))

# source("functions/function_get_collect.r")
# 	data1f_collect <- get_records_url("https://mistr.operations.dynamics.com/data/ReleasedProductCreationsV2?$select=ItemNumber,PurchaseUnitSymbol,PurchUnit,InventQty,ItemId,PurchPrice,",token)
# 	head(data1f_collect)



# ##################################################### 
# #           VALIDACION DE CASOS EXTREMOS            #        
# #####################################################

# productos <- prodprice

# source("functions/conver_unid.r")
# productos1 <- productos %>%
# 				filter(PurchPrice != 0.0000) %>%
# 				mutate(NumUnit = conver_unidades(PurchUnit),
# 					UnitPrice =round(PurchPrice / as.numeric(NumUnit),2))



# val <- read.csv("Validacion.csv", header = TRUE , sep = ";")

# val2 <- val %>% filter( InventoryUnitSymbol != "U")

# val3 <- merge(val2 , productos1 ,by.x="ItemNumber", by.y="ItemId", all.x = TRUE)


# fwrite(val3, "mergeval.csv", sep= ",")