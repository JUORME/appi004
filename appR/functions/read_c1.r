read_c1 <- function (ent,det) {

	#Entorno Windows o Linux 
	ent<-1

	ent <- 1
	if(ent == 1){
		pathglo <- "D:/github/appi005/appR"
	}else {
		pathglo <- "/srv/shiny-server/appi001/appR"
	}

    
     pro <- as.character(det[1,1])

     datapre <- read.csv(paste(pathglo,"/upload/alldata.csv",sep = ""),sep=",")

    datapre1 <- datapre %>% 
     			filter(ItemId == pro)

     e_ent_1 <- setDT(datapre1)

	return(e_ent_1)

}
