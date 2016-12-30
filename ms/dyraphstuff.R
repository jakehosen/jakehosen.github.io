library(dygraphs)
library(data.table)
library(gdata)
library(ggplot2)
library(gridExtra)
library(car)
library(pracma)
library(zoo)
library(reshape2)
library(scales)
library(dplyr)
library(zoo)
library(rmarkdown)

setwd("~/Box Sync/MacroSystems/Data/Sonde_Data/Provisional/")

dyCrosshair <- function(dygraph, 
                        direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("examples/plugins/crosshair.js", 
                       package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

theme_ts_space<-theme_grey() +
		theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill="white", colour="black", size=2),
		legend.key       = element_blank(),
		legend.text      = element_text(size=10),
		legend.title     = element_text(size=10),
		axis.text.x = element_text(size=10,colour="black",hjust=1,angle=45),
		axis.text.y = element_text(size=10,colour="black",vjust=.3),
		axis.title.x = element_text(size=10),
		axis.title.y = element_text(size=10),
#		legend.position  = c(0.8,0.8)
		legend.position  = "right"
		)


#function that imports sonde data and merges it with discharge data
sonde_q <- function(site_id){
unio_sonde<-read.csv(paste("~/Box Sync/MacroSystems/Data/Sonde_Data/Provisional/Sonde_",site_id,"_Provisional.csv",sep=""),header=TRUE,stringsAsFactors=FALSE)
site_dis<-read.csv(paste("~/Box Sync/MacroSystems/Data/USGS_Data/Discharge_Instantaneous/",site_id,"_usgs_q_data.csv",sep=""),header=TRUE,stringsAsFactors=FALSE)
unio_sonde$datetime<-paste(unio_sonde$DATE,unio_sonde$TIME,sep=" ")

#numchar<-nchar(unio_sonde$datetime[1])

#if(numchar==19){
#	unio_sonde$dtp <- round(strptime(unio_sonde$datetime, format="%Y-%m-%d %H:%M:%S",tz="EST"),"mins")
#} else {
#	unio_sonde$dtp <- round(strptime(unio_sonde$datetime, format="%m/%d/%y %H:%M:%S",tz="EST"),"mins")
#}

unio_sonde$dtp <- as.POSIXct(round(strptime(unio_sonde$dtp, format="%m/%d/%y %H:%M:%S",tz="EST"),"mins"),tz="EST")

#turbn<-sum(names(unio_sonde)=="Turb_NTU")
#turbn0<-sum(names(unio_sonde)=="TurbDig_NTU")
#turbn1<-turbn0+turbn
#if(turbn1==2){
#unio_sonde$Turb_NTU<-rowSums(unio_sonde[,c("TurbDig_NTU","Turb_NTU")],na.rm=TRUE)
#}
#if(turbn0==1 & turbn1!=2){
	names(unio_sonde)<-gsub("TurbDig_NTU","Turb_NTU",names(unio_sonde))
#}
unio_sonde$Turb_NTU[unio_sonde$Turb_NTU<0]<-0


unio_sonde<-unio_sonde[order(unio_sonde$dtp),]
unio_sonde<-unio_sonde[!is.na(unio_sonde$dtp),]

site_dis$tz_cd2<-site_dis$tz_cd
site_dis$tz_cd2<-gsub("EST", "-0500",site_dis$tz_cd2)
site_dis$tz_cd2<-gsub("EDT", "-0400",site_dis$tz_cd2)
site_dis$dateTime<-gsub("T", " ",site_dis$dateTime)
site_dis$dateTime<-gsub(":00.000", " ",site_dis$dateTime)
site_dis$dateTime<-gsub("-04:00", "-0400",site_dis$dateTime)
site_dis$dateTime<-gsub("-05:00", "-0500",site_dis$dateTime)
site_dis$dateTime<-gsub("-06:00", "-0600",site_dis$dateTime)
site_dis$dateTime<-gsub("-07:00", "-0700",site_dis$dateTime)
#site_dis$datetime<-paste(site_dis$dateTime, site_dis$tz_cd2,sep=" ")

#strptime(site_dis$dateTime, format="%m/%d/%y %H:%M")
site_dis$dtp <- strptime(site_dis$dateTime, format="%Y-%m-%d %H:%M %z",tz="EST")
site_dis<-site_dis[order(site_dis$dtp),]
site_dis<-site_dis[!is.na(site_dis$dtp),]

#site_dis$dtp <- strptime(site_dis$dateTime, format="%Y-%m-%d %H:%M")
#unio_sonde$dtp<-as.POSIXct(unio_sonde$dtp)
site_dis$dtp<-as.POSIXct(site_dis$dtp,tz="EST")
all_sonde<-merge(unio_sonde,site_dis,all.x=TRUE,by="dtp")
all_sonde$dtp_sonde<-all_sonde$dtp
#all_sonde<-subset(all_sonde, as.numeric(Temp_deg_C)>=0 & as.numeric(CDOM_ug_l)>25 & as.numeric(SpCond_uS_cm)>5)
all_sonde<-unique(all_sonde)
return(all_sonde)
}

sonde_imp <- function(site_id){
unio_sonde<-read.csv(paste("~/Box Sync/MacroSystems/Data/Sonde_Data/Provisional/provisional_corrections/",site_id,"_sonde_corrected.csv",sep=""),header=TRUE,stringsAsFactors=FALSE)
unio_sonde$dtp <- as.POSIXct(round(strptime(unio_sonde$dtp, format="%m/%d/%y %H:%M:%S",tz="EST"),"mins"),tz="EST")
return(unio_sonde)
}

sonde_q_noq <- function(site_id){
unio_sonde<-read.csv(paste("~/Box Sync/MacroSystems/Data/Sonde_Data/Provisional/Sonde_",site_id,"_Provisional.csv",sep=""),header=TRUE,stringsAsFactors=FALSE)
unio_sonde$datetime<-paste(unio_sonde$DATE,unio_sonde$TIME,sep=" ")

numchar<-nchar(unio_sonde$datetime[1])


#if(numchar==19){
#	unio_sonde$dtp <- round(strptime(unio_sonde$datetime, format="%Y-%m-%d %H:%M:%S",tz="EST"),"mins")
#} else {
#	unio_sonde$dtp <- round(strptime(unio_sonde$datetime, format="%m/%d/%y %H:%M:%S",tz="EST"),"mins")
#}

unio_sonde$dtp <- as.POSIXct(round(strptime(unio_sonde$dtp, format="%m/%d/%y %H:%M:%S",tz="EST"),"mins"),tz="EST")


#turbn<-sum(names(unio_sonde)=="Turb_NTU")
#turbn0<-sum(names(unio_sonde)=="TurbDig_NTU")
#turbn1<-turbn0+turbn
#if(turbn1==2){
#unio_sonde$Turb_NTU<-rowSums(unio_sonde[,c("TurbDig_NTU","Turb_NTU")],na.rm=TRUE)
#}
#if(turbn0==1 & turbn1!=2){
	names(unio_sonde)<-gsub("TurbDig_NTU","Turb_NTU",names(unio_sonde))
#}
unio_sonde$Turb_NTU[unio_sonde$Turb_NTU<0]<-0


unio_sonde<-unio_sonde[order(unio_sonde$dtp),]
unio_sonde<-unio_sonde[!is.na(unio_sonde$dtp),]

#site_dis$dtp <- strptime(site_dis$dateTime, format="%Y-%m-%d %H:%M")
unio_sonde$dtp<-as.POSIXct(unio_sonde$dtp)
all_sonde<-unio_sonde
#all_sonde<-merge(unio_sonde,site_dis,all.x=TRUE,by="dtp")
all_sonde$dtp_sonde<-all_sonde$dtp
#all_sonde<-subset(all_sonde, Temp_deg_C>=0 & CDOM_ug_l>25 & SpCond_uS_cm>5)
all_sonde<-unique(all_sonde)
return(all_sonde)
}

farm_sonde<-sonde_q("FARM")
farm_sonde2<-sonde_imp("FARM")
farm_cdom2<-zoo(farm_sonde2[,c("fdom2_qsu_temp","fdom2_turb_ife")],farm_sonde$dtp)
farm_cdom1<-zoo(farm_sonde2[1:100,c("fdom_qsu_temp","fdom_turb_qsu","fdom_turb_ife")],as.POSIXct(farm_sonde2$dtp_sonde,format="%Y-%m-%d %H:%M",tz="EST")[1:100])
farm_temp<-zoo(farm_sonde[,c("Temp_deg_C")],farm_sonde$dtp)
farm_cond<-zoo(farm_sonde[,c("SpCond_uS_cm")],farm_sonde$dtp)
farm_turb<-zoo(farm_sonde[,c("Turb_NTU")],farm_sonde$dtp)
farm_dis<-zoo(farm_sonde[,c("X_00060_00011")],farm_sonde$dtp)
farm_do<-zoo(farm_sonde[,c("HDO_mg_l")],farm_sonde$dtp)
dygraph(farm_cdom1,group="farm")  %>% 
  dyRangeSelector()  %>% 
  dyVisibility(visibility=c(TRUE,TRUE, TRUE))  
dygraph(farm_cdom,group="farm")  %>% 
    dyRangeSelector()  %>% 
	 dyOptions(drawGrid = FALSE)

dygraph(farm_temp,group="farm")  %>% 
  dyRangeSelector()
dygraph(farm_cond,group="farm")  %>% 
    dyRangeSelector()    %>% 
	dySeries("V1", label = "Female")
dygraph(farm_turb,group="farm")  %>% 
    dyRangeSelector()
dygraph(farm_dis,group="farm")  %>% 
   dyRangeSelector()
dygraph(farm_do,group="farm")  %>% 
   dyRangeSelector()


   library(dygraphs)

   lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)

   dyVisibility <- function (dygraph, visibility = TRUE){
     dygraph$x$attrs$visibility <- visibility
     dygraph
   }

  dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
     dyVisibility(visibility=c(TRUE,FALSE, TRUE))


render("~/Documents/jakehosen.github.io/ms/index.Rmd")
render("~/Documents/jakehosen.github.io/ms/radio_test.Rmd")

xt <- xts(x = df$price, order.by = time)
farm_sonde$dtp2<-ts(as.POSIXct(farm_sonde$dtp))
test<-zoo(farm_sonde$Turb_NTU,farm_sonde$dtp)
xts(farm_sonde,order.by="dtp2")

library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(test)

dygraph(ldeaths, main = "All", group = "lung-deaths")
dygraph(mdeaths, main = "Male", group = "lung-deaths")
dygraph(fdeaths, main = "Female", group = "lung-deaths")

dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)
  
  
  
  
  
5:30:00.000Z","2015-07-09T05:45:00.000Z","2015-07-09T06:00:00.000Z","2015-07-0
farm_sonde2$dt<-paste(farm_sonde2$DATE," ",farm_sonde2$TIME, sep="")

write.csv(farm_sonde2[,c("dt","Temp_deg_C","pH_units","SpCond_uS_cm","HDO_mg_l","fdom_qsu_temp","fdom_turb_ife","CDOM_qsu","X_00060_00011")],file="farm_sonde_dygraph.csv",row.names=FALSE)

x<-paste("\"",farm_sonde2$dt,", ",farm_sonde2$Temp_deg_C,", ",farm_sonde2$pH_units,", ",farm_sonde2$SpCond_uS_cm,", ",farm_sonde2$HDO_mg_l,", ",farm_sonde2$fdom_qsu_temp,", ",farm_sonde2$fdom_turb_ife,", ",farm_sonde2$CDOM_qsu,"\\n\"+\n",sep="")
cat("	<!DOCTYPE html>
	<html>
	  <head>
	    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=EmulateIE7; IE=EmulateIE9\">
	    <title>visibility</title>
	    <!--[if IE]>
	    <script type=\"text/javascript\" src=\"../excanvas.js\"></script>
	    <![endif]-->
	    <!--
	    For production (minified) code, use:
	    <script type=\"text/javascript\" src=\"dygraph-combined.js\"></script>
	    -->
	    <script type=\"text/javascript\" src=\"dygraph-combined-dev.js\"></script>

	    <script type=\"text/javascript\" src=\"data.js\"></script>
	  </head>
	  <body>
	    <h3>Click the check boxes to toggle series visibility</h3>
	    <div id=\"div_g\" style=\"width:600px; height:300px;\"></div>

	    <p><b>Show Series:</b></p>
	    <p>
	      <input type=checkbox id=\"0\" onClick=\"change(this)\">
	      <label for=\"0\"> Temperature</label><br/>
	      <input type=checkbox id=\"1\" checked onClick=\"change(this)\">
	      <label for=\"1\"> pH</label><br/>
	      <input type=checkbox id=\"2\" checked onClick=\"change(this)\">
	      <label for=\"2\"> Conductivity</label><br/>
	      <input type=checkbox id=\"3\" onClick=\"change(this)\">
	      <label for=\"3\"> DO</label><br/>
	      <input type=checkbox id=\"4\" checked onClick=\"change(this)\">
	      <label for=\"4\"> Temp-Cor fDOM (QSU)</label><br/>
	      <input type=checkbox id=\"5\" checked onClick=\"change(this)\">
	      <label for=\"5\"> Full-Cor fDOM (QSU)</label><br/>	
	      <input type=checkbox id=\"6\" checked onClick=\"change(this)\">
	      <label for=\"6\"> fDOM (QSU)</label><br/>
	    </p>

	    <p>g.visibility() = <span id=\"visibility\"></span></p>


	    <script type=\"text/javascript\">
	      g = new Dygraph(
	            document.getElementById(\"div_g\"),\n
\"Date,Temp,pH,Conductivity,DO_mgl,fdom_qsu_temp,fdom_turb_ife,fDOM_QSU\\n\"+\n			
			",x,"\n
		  
			    {
			                 visibility: [false, false, false,false,false,false,false]
			               }
			             );
			         setStatus();

			         function setStatus() {
			           document.getElementById(\"visibility\").innerHTML =
			             g.visibility().toString();
			         }

			         function change(el) {
			           g.setVisibility(parseInt(el.id), el.checked);
			           setStatus();
			         }
			       </script>

			     </body>
			   </html>",file="test.html")