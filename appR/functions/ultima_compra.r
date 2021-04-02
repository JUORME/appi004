#####################################################################################
#  Proyecto D365 FO                                                                 #
#  Extraccion de precios de coste de ultima compra                                  #
#  Autor: Junior T. Ortiz Mejia                                                     #
#  Fecha: 22/03/2021                                                                #                                                                              
#####################################################################################

ultimacompra <- function (){

#Fijar el la ruta de trabajo

ent <- 1
if(ent == 1){
	pathglo <- "D:/github/appi010/appR/functions"
}else {
	pathglo <- "/srv/shiny-server/appi010/appR/functions"
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
	fi <- "2021-02-28"
	source("algoritmo_count.r")
	# url1 <- paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesEntities/$count?$filter=CreatedDateTimeInvoice%20le%20",fi,"",sep="") #URl para data 01
	url1 <- paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesEntities/$count?$filter=CreatedDateTimeInvoice%20ge%20",fi,"",sep="")
	count<-get_count_url(url1,token)

	lote<-10000
	source("genera_url.r")
	# urldata1<-paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesEntities?$skip=rvar_s&$top=rvar_t&$filter=CreatedDateTimeInvoice%20le%20",fi,"%20&$select=PurchId,InvoiceId,PackingSlipId,PurchId,ItemId,PurchUnit,PurchPrice,InvoiceDocumentDate,CreatedDateTimeInvoice,CurrencyCode_W",sep="")
	urldata1<-paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesEntities?$skip=rvar_s&$top=rvar_t&$filter=CreatedDateTimeInvoice%20ge%20",fi,"%20&$select=PurchId,InvoiceId,PackingSlipId,PurchId,ItemId,PurchUnit,PurchPrice,InvoiceDocumentDate,CreatedDateTimeInvoice,CurrencyCode_W",sep="")
	vec1<-genera_url(count,lote,urldata1)

	source("function_get_collect.r")
	prodprice <- get_records_url(vec1,token)
	#head(prodprice)


	# Guardar data1
	# fwrite(prodprice,"../upload/data1.csv", sep=",")

	data2 <- as.data.frame(prodprice)

	data1 <- read.csv("../upload/data1.csv", sep=",")

	alldata <- rbind(data1,data2)

	alldata$InvoiceDocumentDate <- as.character(as.POSIXct(alldata$InvoiceDocumentDate, format="%Y-%m-%d",tz="UTC"))
	#prodprice$CreatedDateTimeInvoice <- as.character(as.POSIXct(prodprice$CreatedDateTimeInvoice, format="%Y-%m-%d %H:%M:%S",tz="UTC"))

	# Tipos de Cambio
	source("function_get_collect.r")
	tc <- get_records_url("https://mistr.operations.dynamics.com/data/ExchangeRates?$filter=RateTypeName%20eq%20%27Venta%27&$select=RateTypeName,FromCurrency,StartDate,Rate",token)
	# head(tc)

	tc1 <- tc
	tc1$StartDate <- as.character(as.POSIXct(tc$StartDate, format="%Y-%m-%d",tz="UTC"))
	tc1 <- arrange(tc1,StartDate)





	fwrite(alldata,"../upload/alldata.csv", sep= ",")	

# Opciones de Filtros

	source("conver_unid.r")
	p2 <- alldata %>%
			group_by(ItemId)  %>% 
			filter(PurchPrice != 0.0000) %>%
			filter(InvoiceDocumentDate == max(InvoiceDocumentDate)) %>%
			mutate(NumUnit = conver_unidades(PurchUnit),
					UnitPrice =round(PurchPrice / as.numeric(NumUnit),3)) %>%
			filter(UnitPrice == max(UnitPrice)) %>%
			filter(CreatedDateTimeInvoice == max(as.character(CreatedDateTimeInvoice)))


	p3 <- as.data.frame(p2)

# Extracción de precios de coste de productos emitidos

	source("function_get_collect.r")
	emitprice <- get_records_url("https://mistr.operations.dynamics.com/data/ReleasedDistinctProducts?$select=ItemNumber,InventoryUnitSymbol,UnitCost,ProductType",token)
	# head(emitprice)

	q1 <- as.data.frame(filter(emitprice, ProductType == "Item"))

	#q1[,3] <- signif(q1[,3],3)

# Extracción de nombre de los productos
	# Descarga de datos 
	# source("function_get_collect.r")
	# nameprod <- get_records_url("https://mistr.operations.dynamics.com/data/AllProducts?$select=ProductNumber,ProductName",token)
	# head(nameprod)

	# prod <- nameprod
	# fwrite(prod,"../upload/nameprod.csv", sep = ",")

#carga de datos 
	nameprod <- read.csv("../upload/nameprod.csv",sep = ",")



#Unir los dos data frames

	u1 <- merge(q1 , p3 ,by.x = "ItemNumber", by.y="ItemId", all.x=TRUE)
	u1$Diff <- u1$UnitCost - u1$UnitPrice
	u1$Diff <- round(u1$Diff,3)
	u1$Val <- ifelse(u1$Diff < -0.1 | u1$Diff > 0.1 , 1, 0)

	u2 <- merge(u1,nameprod, by.x="ItemNumber", by.y="ProductNumber", all.x=TRUE)



	u3 <- u2 %>% 
			select(ItemNumber,ProductName,InventoryUnitSymbol,UnitCost,UnitPrice,CurrencyCode_W,Diff,Val)


	names(u3) <- c("Cod","Descripcion","InvUnit","EmitCost","UltimFac","Moneda","Diff","Val")		

	return(u3)

}