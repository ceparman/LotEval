

library(CoreAPI)
library(dplyr)
library(shiny)
library(ggplot2)
library(gridExtra)
library(RGraphics)


## function call
## ${servletUrl}?cmd=iframe&iframeSrc=https://ngsanalytics.shinyapps.io/ARUP/?exptBarcode=${entityBarcode}



#Global Variables

exptBarcode <- ""
exptID <-""
experiment <-list()
specData<-data.frame()
dsdev<-as.numeric()
dave<-as.numeric()
dcv <- as.numeric()
dmin <- as.numeric()
dmax <- as.numeric()
min_ctl<-as.numeric()
max_ctl<-as.numeric()
dobs <- as.numeric()
plt <- list()
plotdata <- data.frame()
lot <-""
function(input, output,session) {
  
  
  output$expt <- renderText({
    
    exptBarcode <<- parseQueryString(session$clientData$url_search)$exptBarcode
    
    paste("Processing Experiment", exptBarcode)
    
  })
  
  # output$message <- renderText({
  #   
  #   inFile <- input$file1
  #   
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   filePath <<- inFile$datapath
  #   filename <<- inFile$name
  #   paste0("file '", inFile$name, ", loaded")
  #   
  # })
  # 
  # 
  
  ###################### 
  
  
  output$plot<-renderPlot({
    
   
    output$message <- renderText({ "Loading Lot Data"})
    
    tenant<-CoreAPI::coreAPI("account-BA.json")
    
    tenant<-CoreAPI::authBasic(tenant,useVerbose = TRUE)$coreApi
    
    
    
    #Get the Experiment info
    
    
    #Get the Experiment info
    
    
    experiment<<-CoreAPI::getEntityByBarcode(tenant,"NEW CONTROL LOT EVALUATION RUN"
                                             ,exptBarcode )
    
    
    exptsamples<-CoreAPI::getExperimentSamples(tenant,entityType = "NEW CONTROL LOT EVALUATION RUN",
                                               barcode = exptBarcode)$entity$experimentSamples
    
    esbarcode <- unlist(lapply(exptsamples,function(x) x$barcode))
    
    lot <<-exptsamples[[1]]$sampleEntityBarcode
    
    logout<- CoreAPI::logOut(tenant)
    
    #get assay data
    
    specData <- unlist(
      as.numeric(lapply(exptsamples, function(x) 
        x$experimentData$SPECIMEN_DATA$stringData )
      ))
    
    
   
    #Reduce Data 
    
    dsdev<<-sd(specData)
    dave<<-mean(specData)
    dcv <<- dsdev/dave * 100
    dmin <<- min(specData)
    dmax <<- max(specData)
    dobs <<- length(specData)
    min_ctl<<-dave-(2*dsdev)
    max_ctl<<-dave+(2*dsdev)
    
    plotdata<<-data.frame(index=1:length(specData), specData = specData, z = round((specData - dave)/dsdev,2))     
    
   plt1<-ggplot2::ggplot(plotdata,aes(x=index,y=z))
   plt1<-plt1+geom_point()

     plt1 <- plt1 + coord_cartesian(ylim = c(-3, 3))+ylab("SD INDEX")
 
    
     plt1 <- plt1 + scale_x_continuous(breaks = 1:length(specData)) + xlab("Sample Index")
    
    plt1<-plt1+geom_segment(aes(x=0,xend=dobs,y=1,yend=1),color="green",linetype="dotdash")  
    plt1<-plt1+geom_segment(aes(x=0,xend=dobs,y=-1,yend=-1),color="green",linetype="dotdash")
    plt1<-plt1+geom_segment(aes(x=0,xend=dobs,y=2,yend=2),color="red",linetype="dashed")  
    plt1<-plt1+geom_segment(aes(x=0,xend=dobs,y=-2,yend=-2),color="red",linetype="dashed")
    plt1<-plt1+geom_segment(aes(x=0,xend=dobs,y=3,yend=3),color="darkred",linetype="dashed")  
    plt1<-plt1+geom_segment(aes(x=0,xend=dobs,y=-3,yend=-3),color="darkred",linetype="dashed")
    
    plt1<<-plt1+geom_segment(aes(x=0,xend=dobs,y=0,yend=0),color="black")
    
    plot(plt1)  
      
    })  
    
  output$exptDetail<-renderTable(
  
   expr = matrix(byrow=TRUE, data=
                  c("Assay:", experiment$entity$values$EVALUATION_ASSAY$stringData,"       ",
                    "Vendor:",  experiment$entity$values$VENDOR$stringData,
                    "Analyte:",  experiment$entity$values$ANALYTE$stringData,"       ",
                    "Date Rec./Made:",  experiment$entity$values$DATE_REC__MADE$stringData,
                    "Control Level:",  experiment$entity$values$CONTROL_LEVEL$stringData,"       ",
                    "Expiration Date:",  experiment$entity$values$EXPIRATION_DATE$stringData,
                    "Lot #:",  lot,"       ",
                    "In Use:",  experiment$entity$values$IN_USE$stringData),
                nrow=4,ncol=5),
     colnames = FALSE
    
  )
  
  output$exptResults<-renderTable(
    
    expr = matrix(byrow=TRUE, data=
                      c("Average:", round(dave,3),
                      "Standard Deviation:",round(dsdev,3),
                      "CV%",round(dcv,3)),
                  nrow=3,ncol=2),
    colnames = FALSE
    ,digits = 3
  ) 
    
    
  observeEvent(input$report,{
    
    output$message <- renderText({paste0("Generating PDF Report")})  
    
 #   build grob for pdf, already have plt1 object
   
    
    
    title<-textGrob("New Control Lot Evaluation",gp=gpar(fontsize=20))
    
    hm<- matrix(byrow=TRUE, data=
                  c("Assay:", experiment$entity$values$EVALUATION_ASSAY$stringData,"       ",
                    "Vendor",  experiment$entity$values$VENDOR$stringData,
                    "Analyte",  experiment$entity$values$ANALYTE$stringData,"       ",
                    "Date Rec./Made",  experiment$entity$values$DATE_REC__MADE$stringData,
                    "Control Level",  experiment$entity$values$CONTROL_LEVEL$stringData,"       ",
                    "Expiration Date",  experiment$entity$values$EXPIRATION_DATE$stringData,
                    "Lot #",  lot,"       ",
                    "In Use",  experiment$entity$values$IN_USE$stringData),
                nrow=4,ncol=5)
    
    
    header  <-tableGrob(hm)
    
    
    tt <- ttheme_default(colnames = c("n","Result"))
    
    tbldata <-data.frame( n = plotdata$index, Result = plotdata$specData)
    
    tbl <- tableGrob(tbldata, rows=NULL, theme=tt)
    

    texttable1 <- paste0( "New Lot Precision Statistics \n",
                          "MEAN: ", round(mean(plotdata$specData),2),
                         "     SD: ", round(sd(plotdata$specData),2),
                         "     CV: ", round(sd(plotdata$specData)/mean(plotdata$specData)*100,2),
                         "%",
                         "     MIN: ", round(min(plotdata$specData),2),
                         "     MAX: ", round(max(plotdata$specData),2),
                         "\n",
                         "2 SD Range:  ", round(mean(plotdata$specData)-sd(plotdata$specData),2),
                         "   -   ", round(mean(plotdata$specData)+sd(plotdata$specData),2)
                         )
    
    t1 <- textGrob(texttable1,vjust = "bottom")
    
   
    
    
    
    a<-data.frame("New Lot" = c(round(mean(plotdata$specData),2),
                                round(sd(plotdata$specData),2),
                                round(sd(plotdata$specData)/
                                        mean(plotdata$specData),2)),
                  "Current Lot" = c(13.75,1.38,10.04),
                  "Diff " = c(0.12,0.03,0.30)
    )
    
    row.names(a) <- c("Mean","SD","CV %")
    
    
    gt<-tableGrob(a)
    
    
    nr<-textGrob("NEW RANGE  11.17 - 16.57")
    
    
    ft<-textGrob(paste("Notes: \n", experiment$entity$values$NOTES$stringData))
    
    
    #arrange output 
    
    gs<-list(title,header,tbl,plt1,t1,gt,ft,nr)
    
    lay <- rbind(c(1,1,1),
   
                 c(2,2,2),
                 c(3,4,4),
                 c(3,4,4),
                 c(3,4,4),
                 c(3,4,4),
                 c(3,5,5),
                 c(3,6,6),
            
                 c(3,8,8),
                 c(7,7,7)
    )
    
    
#create PDF    
    
    pdf("report.pdf",height= 11, width = 8,colormodel ="srgb" )
    grid.arrange(grobs = gs, layout_matrix = lay)
    dev.off()
    
#save plot to experiment
    
    
    tenant<-CoreAPI::coreAPI("account-BA.json")
    
    tenant<-CoreAPI::authBasic(tenant)$coreApi
    filePath <- "report.pdf"
    filename <- "report.pdf"
    
    save <- CoreAPI::attachFile(coreApi = tenant,barcode =  exptBarcode, filename = filename,
                                filepath = filePath  )  
    
    logout<- CoreAPI::logOut(tenant)
 
     #need to add delete pdf file  
    
  #  file.remove("report.pdf")
    
    output$message <- renderText({paste0("PDF Report Saved to Experiment")})  
    
     })
  

 
  
  ############################ 
  
  output$return <- renderUI({
    
    tenant<-CoreAPI::coreAPI("account-BA.json")
    
    tenant<-CoreAPI::authBasic(tenant)$coreApi
    
    #Get the Experiment info
    
    
    experiment<-CoreAPI::getEntityByBarcode(tenant,"NEW CONTROL LOT EVALUATION RUN",exptBarcode )
    
    id<-experiment$entity$entityId
    
    logout<- CoreAPI::logOut(tenant)
    
    
    shiny::a(h4("Return to Experiment", class = "btn btn-default action-button",
                style = "fontweight:600"), target = "_parent",
             href = paste0(tenant$scheme,"://",tenant$coreUrl,"/",tenant$TenantShortName,
                           "/corelims?cmd=get&entityType","=NEW%20CONTROL%20LOT%20EVALUATION%20RUN&entityId=",
                           id)
          
    )
    
  })
  
  observeEvent(input$return, {js$closeWindow()
    stopApp()
  })
  
  
  
}