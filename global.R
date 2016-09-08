usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("rJava")
usePackage("shiny")
usePackage("shinydashboard")
usePackage("plyr")
usePackage("dplyr")
usePackage("ggplot2")
usePackage("XLConnect")
usePackage("readxl")
usePackage("xlsx")
usePackage("png")
usePackage("leaflet")
usePackage("htmlwidgets")
usePackage("htmltools")
usePackage("DT")
usePackage("rpivotTable")
usePackage("knitr")
usePackage("plotrix")
usePackage("plotly")
usePackage("dygraphs")
usePackage("formattable")
usePackage("data.table")
usePackage("rAmCharts")
usePackage("datasets")
usePackage("RColorBrewer")
usePackage("devtools")
usePackage("Quandl")
usePackage("portfolio")
usePackage("treemap")
usePackage("googleVis")


# library(rJava)
# library(shiny)
# library(plyr)
# library(dplyr)
# library(shiny)
# library(shinydashboard)
# library(ggplot2)
# library(XLConnect)
# library(readxl)
# library(xlsx)
# library(png)
# library(leaflet)
# library(htmlwidgets)
# library(htmltools)
# library(DT)
# library(rpivotTable)
# library(knitr)
# library(plotrix)
# library(plotly)
# library(dygraphs)
# library(formattable)
# library(data.table)
# library(ggplot2)
# library(rAmCharts)
# library(datasets)
# library(RColorBrewer)
# library(devtools)
# library(Quandl)
# library(portfolio)
# library(treemap)
# library(googleVis)
# library(raster)
# library(rgdal)
#devtools::install_github('rstudio/DT')
Datafeed <- read.xlsx2("Datasets/GT22016_DataFeedProfile_10July2016_V2.xlsx",
                       sheetIndex = 2, startRow = 2, stringsAsFactors = F, header = T)
Implementation <- read.xlsx2("Datasets/The Network Participant Onboarding Status - July.xlsm",
                             sheetIndex = 1, startRow = 7, stringsAsFactors = F, header = T)
Workflow <- read.xlsx2("Datasets/Assessment Tracker080916.xlsx",
                       sheetIndex = 1, startRow = 1, endRow = 168, stringsAsFactors = F, header = T, colClasses = c(rep("character", 6), rep("Date",8)))
WorkflowEMR <- read.xlsx2("Datasets/Copy of Assessment Tracker 08042016 DA.xlsx",
                          sheetIndex = 1, startRow = 1,stringsAsFactors = F, header = T)


GeoRecruitment <- read.xlsx2("Datasets/Updated Facility Crosswalk 2016-7-28.xlsx",
                          sheetIndex = 1, startRow = 1, stringsAsFactors = F, header = T, colClasses = c(rep("character",27), rep("numeric",2)))
Recruitment <- read.xlsx2("Datasets/AzHeC Programs Overview 08_01_16.xlsx",
                             sheetIndex = 1, startRow = 1, stringsAsFactors = F, header = T)

city <- as.character(GeoRecruitment$City)
cityU <- unique(city)
impl.colnames <- names(Implementation)
df <- data.frame(lon = GeoRecruitment$Longitude[!is.na(GeoRecruitment$Longitude)], lat = GeoRecruitment$Latitude[!is.na(GeoRecruitment$Latitude)], label = GeoRecruitment$Participant[!is.na(GeoRecruitment$Latitude)])
#setwd("U:/ads/Rscripts")

# Recruitment dataset
targetCnt <- length(Recruitment$Organization)
Recruitment$Current.Participant[Recruitment$Current.Participant==""] <- NA
participantCnt <- length(Recruitment$Current.Participant[!is.na(Recruitment$Current.Participant)])

particpantType <- Recruitment %>% dplyr::group_by(Type) %>% dplyr::summarise(Count_t = n())
particpantType <- particpantType[-1,]
particpantType$ID<-seq.int(nrow(particpantType))
particpantType$Category<- c("Community", "Physical", "Behavioral", "Behavioral","Behavioral","Behavioral", "Community", "Community", "Community", 
                             "Physical","Physical","Physical","Physical","Physical","Physical","Community")


#Workflow Dataset - Date Selection
handoffDate <- as.POSIXlt(Workflow$Hand.off)
inviteDate <- as.POSIXlt(Workflow$Invite.Date)
visitDate <- as.POSIXlt(Workflow$Visit.Date)
draftsentDate <- as.POSIXlt(Workflow$Draft.Sent.with.PNP.and.Portal.Forms)
draftcompleteDate <- as.POSIXlt(Workflow$Draft.Complete)
finalDate <- as.POSIXlt(Workflow$Final.Draft.Complete.Including.Portal)
invite_hand <- as.numeric(difftime(inviteDate,handoffDate, units = "days"))
visit_invite <- as.numeric(difftime(visitDate,inviteDate, units = "days"))
draftsent_visit <- as.numeric(difftime(draftsentDate, visitDate, units = "days"))
draftcomp_draftsent <- as.numeric(difftime(draftcompleteDate, draftsentDate, units = "days"))
final_draftcomp <- as.numeric(difftime(finalDate,draftcompleteDate, units = "days"))
handoffCnt <- length(as.numeric(Workflow$Hand.off[!is.na(Workflow$Hand.off)]))
draftcomCnt <- length(as.numeric(Workflow$Draft.Complete[!is.na(Workflow$Draft.Complete)]))
finalCnt <- length(as.numeric(Workflow$Final.Draft.Complete.Including.Portal[!is.na(Workflow$Final.Draft.Complete.Including.Portal)]))
inviteCnt <- length(as.numeric(Workflow$Invite.Date[!is.na(Workflow$Invite.Date)]))
visitCnt <- length(as.numeric(Workflow$Visit.Date[!is.na(Workflow$Visit.Date)]))
draftsentCnt <- length(as.numeric(Workflow$Draft.Sent.with.PNP.and.Portal.Forms[!is.na(Workflow$Draft.Sent.with.PNP.and.Portal.Forms)]))
duration <- matrix(data = c(invite_hand/inviteCnt, visit_invite/visitCnt, draftsent_visit/draftsentCnt, draftcomp_draftsent/draftcomCnt, final_draftcomp/finalCnt), ncol = 5)
colnames(duration) <- c("Recruitment Hand-off->Invite", "Invite->Visit", "Visit->Draft sent","Draft sent->Draft complete","Draft complete->Hand-off Implementation")
tot_duration <- colSums(duration, na.rm = T) %>% as.table() %>% t() %>% as.data.frame()
colnames(tot_duration) <- c("Type","Transition Stage", "Total Days")
tot_duration$`Total Days` <- as.numeric(paste(tot_duration$`Total Days`))

handoffCntP <- formattable::percent(handoffCnt/participantCnt,2)
draftcomCntP <- formattable::percent(draftcomCnt/participantCnt,2)
finalCntP <- formattable::percent(finalCnt/participantCnt,2)
#correct the foll calculation to unique org id
draftcomOrgID <- as.data.frame(Workflow$OrganizationID[!is.na(Workflow$Draft.Complete)])
colnames(draftcomOrgID)  <- "DraftCompletedOrganizationID"
OrgdraftcomCnt <- length(unique(draftcomOrgID$DraftCompletedOrganizationID))

EMRrecord <- table(Workflow$EMR)  #used for EMR classification

#Workflow Dataset - Timeseries

timelength_handoff <- Workflow %>% dplyr::mutate(Month_Handoff = month(handoffDate), 
                                                 Year_Handoff = year(handoffDate))%>%
  dplyr::group_by(Month_Handoff, Year_Handoff)%>%
  dplyr::summarize(Count_ho = n())

timelength_draftcomp <- Workflow %>% dplyr::mutate(Month_Draftcomp = month(draftcompleteDate), 
                                                   Year_Draftcomp = year(draftcompleteDate))%>%
  dplyr::group_by(Month_Draftcomp, Year_Draftcomp)%>%
  dplyr::summarize(Count_dc = n())

timelength_final <- Workflow %>% mutate(Month_Final = month(finalDate), 
                                        Year_Final = year(finalDate)) %>%
  dplyr::group_by(Month_Final, Year_Final)%>%
  dplyr::summarize(Count_f = n())

combined_timelength <- c("timelength_handoff", "timelength_draftcomp", "timelength_final") %>% as.data.frame()

p <- ggplot() + 
  geom_line(data = timelength_handoff, aes(x = Month_Handoff, y = Count_ho, color = "Recruitment Handoff")) +
  geom_line(data = timelength_draftcomp, aes(x = Month_Draftcomp, y = Count_dc, color = "Draft Complete"))  +
  geom_line(data = timelength_final, aes(x = Month_Final, y = Count_f, color = "Handoff to Implementation"))  +
  
  labs(color = "Legend")+
  #scale_x_discrete(limit = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                #   labels = c("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  coord_cartesian(ylim = c(0,30))+
  xlab('Month') +
  ylab('Count')

#Remove Outlier

p1 <- ggplot() + 
  geom_bar(data = Workflow, aes(x = as.Date(Hand.off), color = "Recruitment Handoff"), na.rm = T) +
  geom_bar(data = Workflow, aes(x = as.Date(Draft.Complete), color = "Draft Complete"), na.rm = T)  +
  geom_bar(data = Workflow, aes(x = as.Date(Final.Draft.Complete.Including.Portal), color = "Handoff to Implementation"), na.rm = T)  +
  scale_x_date(limits = as.Date(c('2016-01-04','2016-31-7')))+
  
  labs(color = "Legend")+
  
  xlab('Month') +
  ylab('Count')

#DYgraph

Workflow$Hand.off <- as.Date(Workflow$Hand.off)
Workflow$Draft.Complete <- as.Date(Workflow$Hand.off)
Workflow$Final.Draft.Complete.Including.Portal <- as.Date(Workflow$Final.Draft.Complete.Including.Portal)

# Workflowxts <- xts(Workflow$Hand.off,Workflow$Draft.Complete, Workflow$Final.Draft.Complete.Including.Portal)
# timelength_handoffxts <- xts(timelength_handoff) 
# 
# dygraph(Workflowxts, main = "Process Progress") %>%
#   dySeries("V1", label = "Unemployment (%)") %>%
#   dyRangeSelector(strokeColor = "darkred", fillColor = "darkred") %>%
#   dyOptions(colors = c("darkred", "darkred")) 

# Workflow EMR/EHR
EMRrecordCnt <-  Workflow %>% dplyr::group_by(EMR)%>% dplyr::summarise(Count_f = n()) 
EMR1Count <- EMRrecordCnt %>% dplyr::filter(Count_f == 1)
EMRrecordCnt <- EMRrecordCnt %>% dplyr::filter(Count_f > 1)
n <- nrow(EMRrecordCnt)
EMR1C <- sum(EMR1Count[,2])
EMRrecordCnt[n+1,] <- c("EMR1Count", EMR1C)
EMRrecordCnt$Count_f <- as.numeric(paste(EMRrecordCnt$Count_f))
EMRrecordCnt <- EMRrecordCnt[sort.list(EMRrecordCnt$Count_f, decreasing = T, na.last = T),]
                  
# EMRcorrectCnt <- WorkflowEMR %>% dplyr::group_by(EMR)%>% dplyr::summarise(count_f1 = n())


#GeoRecruitment Dataset - Facilitycross walk geomapping coordinates
df <- data.frame(lon = GeoRecruitment$Longitude[!is.na(GeoRecruitment$Longitude)], lat = GeoRecruitment$Latitude[!is.na(GeoRecruitment$Latitude)], label = GeoRecruitment$Participant[!is.na(GeoRecruitment$Latitude)])

inboundSite <- dplyr::filter(GeoRecruitment, Operational.Inbound.Data.Feeds == "Y")
#inboundSite <- data.frame(GeoRecruitment, GeoRecruitment$Operational.Inbound.Data.Feeds == "Y")
inboundSiteDF <- data.frame(lon = inboundSite$Longitude[!is.na(inboundSite$Longitude)], lat = inboundSite$Latitude[!is.na(inboundSite$Latitude)], 
                            label = inboundSite$Participant[!is.na(inboundSite$Latitude)] )

outboundSite <- dplyr::filter(GeoRecruitment, (Operational.Query.Response.Installations == "Y") |
                                              (GeoRecruitment$Operational.Portal.Installations == "Y") |
                                              (GeoRecruitment$Operational.Query.Response.Installations == "Y") |
                                              (GeoRecruitment$Operational.Alerts..Installations == "Y" )|
                                              (GeoRecruitment$Operational.Direct.Email.Installations == "Y") |
                                              (GeoRecruitment$Operational.Crisis.Portal.Installations == "Y"))
outboundSiteDF <-    data.frame(lon = outboundSite$Longitude[!is.na(outboundSite$Longitude)], lat = outboundSite$Latitude[!is.na(outboundSite$Latitude)], 
                                label = outboundSite$Participant[!is.na(outboundSite$Latitude)] )  

Operational.Query.Response.InstallationsSite <- dplyr::filter(GeoRecruitment, Operational.Query.Response.Installations == "Y")
Operational.Query.Response.InstallationsDF <- data.frame(lon = Operational.Query.Response.InstallationsSite$Longitude[!is.na(Operational.Query.Response.InstallationsSite$Longitude)], lat = Operational.Query.Response.InstallationsSite$Latitude[!is.na(Operational.Query.Response.InstallationsSite$Latitude)], 
                            label = Operational.Query.Response.InstallationsSite$Participant[!is.na(Operational.Query.Response.InstallationsSite$Latitude)] )

Operational.Portal.InstallationsSite <- dplyr::filter(GeoRecruitment, Operational.Portal.Installations == "Y")
Operational.Portal.InstallationsDF <- data.frame(lon = Operational.Portal.InstallationsSite$Longitude[!is.na(Operational.Portal.InstallationsSite$Longitude)], lat = Operational.Portal.InstallationsSite$Latitude[!is.na(Operational.Portal.InstallationsSite$Latitude)], 
                                                         label = Operational.Portal.InstallationsSite$Participant[!is.na(Operational.Portal.InstallationsSite$Latitude)] )

Operational.Alerts..InstallationsSite <- dplyr::filter(GeoRecruitment, Operational.Alerts..Installations == "Y")
Operational.Alerts..InstallationsDF <- data.frame(lon = Operational.Alerts..InstallationsSite$Longitude[!is.na(Operational.Alerts..InstallationsSite$Longitude)], lat = Operational.Alerts..InstallationsSite$Latitude[!is.na(Operational.Alerts..InstallationsSite$Latitude)], 
                                                 label = Operational.Alerts..InstallationsSite[!is.na(Operational.Alerts..InstallationsSite$Latitude)] )

Operational.Direct.Email.InstallationsSite <- dplyr::filter(GeoRecruitment, Operational.Direct.Email.Installations == "Y")
Operational.Direct.Email.InstallationsDF <- data.frame(lon = Operational.Direct.Email.InstallationsSite$Longitude[!is.na(Operational.Direct.Email.InstallationsSite$Longitude)], lat = Operational.Direct.Email.InstallationsSite$Latitude[!is.na(Operational.Direct.Email.InstallationsSite$Latitude)], 
                                                  label = Operational.Direct.Email.InstallationsSite$Participant[!is.na(Operational.Direct.Email.InstallationsSite$Latitude)] )

Operational.Crisis.Portal.InstallationsSite <- dplyr::filter(GeoRecruitment, Operational.Crisis.Portal.Installations == "Y")
Operational.Crisis.Portal.InstallationsDF <- data.frame(lon = Operational.Crisis.Portal.InstallationsSite$Longitude[!is.na(Operational.Crisis.Portal.InstallationsSite$Longitude)], lat = Operational.Crisis.Portal.InstallationsSite$Latitude[!is.na(Operational.Crisis.Portal.InstallationsSite  $Latitude)], 
                                                       label = Operational.Crisis.Portal.InstallationsSite$Participant[!is.na(Operational.Crisis.Portal.InstallationsSite$Latitude)] )


markerIcons <- iconList(violet = makeIcon("/markers/violetmarker.png", iconWidth = 24, iconHeight =32),
                        orange = makeIcon("/markers/orangemarker.png", iconWidth = 24, iconHeight =32),
                        red = makeIcon("/markers/redmarker.png", iconWidth = 24, iconHeight =32),
                        green = makeIcon("/markers/greenmarker.png", iconWidth = 24, iconHeight =32))

#Implementation Dataset - Division of Operational, Underway and Planning

inbOperationalCnt <- length(Implementation$X24...Operational.Inbound.Data.Feeds[Implementation$X24...Operational.Inbound.Data.Feeds == 'Operational'])
inbUnderwayCnt <- length(Implementation$X24...Operational.Inbound.Data.Feeds[Implementation$X24...Operational.Inbound.Data.Feeds == 'Underway'])
inbPlanningCnt <- length(Implementation$X24...Operational.Inbound.Data.Feeds[Implementation$X24...Operational.Inbound.Data.Feeds == 'Planning'])
inb <- matrix(data = c(inbOperationalCnt,inbUnderwayCnt, inbPlanningCnt), ncol = 3)
colnames(inb) <- c("Operational","Underway","Planning")
impParticipantCnt <- length(Implementation$Participant.Name)
inbOperationalP <- formattable::percent(inbOperationalCnt/impParticipantCnt,2)
inbUnderwayP <- formattable::percent(inbUnderwayCnt/impParticipantCnt,2)
inbPlanningP <- formattable::percent(inbPlanningCnt/impParticipantCnt,2)
inbP <- matrix(c(inbOperationalP, inbUnderwayP, inbPlanningP), ncol = 3)
inbCntP <- formattable::percent(impParticipantCnt/participantCnt,2)


cnt <- plyr::count(Implementation$Participant.Name[Implementation$X24...Operational.Inbound.Data.Feeds == "Operational"|
                                                     Implementation$X3...Operational.Query.Response.Installations== "Operational"|
                                                     Implementation$X24...Operational.Portal.Installations== "Operational"|
                                                     Implementation$X17...Operational.Alerts..Installations == "Operational"|
                                                     Implementation$X15...Operational.Direct.Email.Installations == "Operational"|
                                                     Implementation$X0...Operational.Crisis.Portal.Installations == "Operational"])
impOperationalCnt <- sum(cnt[2])
impOperationalP <- formattable::percent(impOperationalCnt/participantCnt,2)

cntUnique <- plyr::count(Implementation$OrganizationID[Implementation$X24...Operational.Inbound.Data.Feeds == "Operational"|
                                                     Implementation$X3...Operational.Query.Response.Installations== "Operational"|
                                                     Implementation$X24...Operational.Portal.Installations== "Operational"|
                                                     Implementation$X17...Operational.Alerts..Installations == "Operational"|
                                                     Implementation$X15...Operational.Direct.Email.Installations == "Operational"|
                                                     Implementation$X0...Operational.Crisis.Portal.Installations == "Operational"])
colnames(inbP) <- c("Operational%","Underway%","Planning%")

ImpName <- as.data.frame(Implementation$OrganizationID[Implementation$X24...Operational.Inbound.Data.Feeds == "Operational"|
                                                 Implementation$X3...Operational.Query.Response.Installations== "Operational"|
                                                 Implementation$X24...Operational.Portal.Installations== "Operational"|
                                                 Implementation$X17...Operational.Alerts..Installations == "Operational"|
                                                 Implementation$X15...Operational.Direct.Email.Installations == "Operational"|
                                                 Implementation$X0...Operational.Crisis.Portal.Installations == "Operational"])


colnames(ImpName) <- "OperationalOrganizationID"

OrgimpOpertionalCnt <- length(unique(ImpName$OperationalOrganizationID))
OrgimpCnt <- length(unique(Implementation$OrganizationID))
OrgimpCntP <- formattable::percent(OrgimpCnt/participantCnt,2)

#Implementation Dataset - Division of Project Managers
PM <- Implementation$Project.Manager
div <- Implementation %>% mutate(PM = PM)%>%
  dplyr::group_by(PM)%>%
  dplyr::summarise(count = n())

#Datafeed Audit

totDatafeed <- length(Datafeed$Server[Datafeed$Server != ""])
ADT <- length(Datafeed$Patient.Info..PID.[Datafeed$Patient.Info..PID. == "Y"])
LAB <- length(Datafeed$LAB[Datafeed$LAB == "Y"])
RAD <- length(Datafeed$Rad[Datafeed$Rad == "Y"])
TRANS <- length(Datafeed$TRAN[Datafeed$TRAN == "Y"])

ADTp <- formattable::percent(ADT/totDatafeed,2)
LABp <- formattable::percent(LAB/totDatafeed,2)
RADp <- formattable::percent(RAD/totDatafeed,2)
TRANSp <- formattable::percent(TRANS/totDatafeed,2)


Percentage <- c(ADTp, LABp, RADp, TRANSp)
value <- (as.numeric(sub("%","",Percentage)))
description <- c("ADT %" , "LAB %", "TRANS %", "RAD %")
ChannelsP <- cbind(description, value) %>% as.data.frame()
ChannelsP$value <- (as.numeric(paste(ChannelsP$value)))

#as.numeric(paste())


dataAudit <- subset(Datafeed, , c(Organization, City, Channels.., Patient.Info..PID.,
                                LAB, TRAN, Rad, Path, MicroBiology, SIU, CVX))
names(dataAudit) <- c("Organization", "City", "Channels", "ADT", "LAB", "TRANS", "RAD", "PATH",
                      "Micro Bio", "SIU", "CVX")
# Summary

processCnt <-  c(participantCnt,  OrgdraftcomCnt, OrgimpOpertionalCnt)
processName <- c("Recruited Participants", "Workflow Completed", "Onboarded Participants")
processDF <- cbind(processName, processCnt) %>% as.data.frame()
processDF$processCnt <- as.numeric(paste(processDF$processCnt))

# Dummy timeseries

lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)



  