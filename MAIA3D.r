####################################################################
## MAIA Microperimetry volume code - creates a 3D hill of         ## 
## vision with an associated volumetric measure                   ##                
##                                                                ##
## Use of the following code in publications is permitted on      ## 
## condition that a citation of the authors journal submission    ##
## (https://doi.org/10.1167/tvst.10.7.12) is provided:            ##
##                                                                ##
## "Microperimetry hill of vision and volumetric measures of      ##
## retinal sensitivity Amandeep Singh Josan, Thomas M W Buckley,  ##
## Laura J Wood, Jasleen K Jolly, Jasmina Cehajic-Kapetanovic     ##
## and Robert E MacLaren"                                         ##
##                                                                ##                            
## please also cite the appropriate                               ##
## packages contained within this code (ggplot2,rgl,fields).      ##
## This program is intended for research use only. Clinical       ##
## decisions should not be made based on information generated    ##
## using this program.                                            ##        
####################################################################

rm(list = ls(all = TRUE))
library(ggplot2)
library(plyr)
library(concaveman)
library(pracma)
library(devtools)
library(ggalt)
library(dplyr)
library(fields)
library(plot3D)
library(misc3d)
library(rgl)
library(ggpmisc)
library(reshape2)
library(openxlsx)
library(filenamer)
library(data.table)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(rsconnect)



#============= shiny ui ==============
ui = fluidPage(
    sidebarLayout(
      sidebarPanel(fileInput("file1", "Choose txt File",
                  accept = c("text",".txt")),
      sidebarPanel("please wait after upload"),
      tags$hr(),
        ),
      mainPanel(
          verbatimTextOutput("text"),
          column(width=8, align="right",
          rglwidgetOutput("plot_3D_tpsvol", 
                          width = 600, height = 600))
      
      )))
#================ end ui ====================
  
#============= shiny server =================
  server <- function(input, output) {
    output$text <- renderText({
      paste("3D MAIA","Please cite journal submission \"Microperimetry hill of vision and volumetric measures of retinal sensitivity\"", 
            "by Josan et al. (https://doi.org/10.1167/tvst.10.7.12)",
            "rotate plot with mouse and zoom with scroll wheel ---- (intended for research use only)", sep="\n")
    })
        output$plot_3D_tpsvol <- renderRglwidget({
            rgl.open(useNULL=T)
#============ end shiny server ==========    
      
          
        
        
        
        
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)

      ######  use this to find the string containing the headers for the dataframe of interest
      text <- readLines(inFile$datapath)
      rownumber <- grep("^ID\tx_deg\ty_deg", text)
      rownumber = rownumber-1
      
      #### read from shiny input .txt file
      data0 <- read.table(inFile$datapath, header = T, stringsAsFactors = F, skip=rownumber)

      data <- data0[,c("ID","x_deg","y_deg","Threshold")]
      colnames(data) <- c("ID", "x","y","thresh")
      #### remove blindspot test
      data <- data[!data$ID==0,]
      #### flip y-coords to match maia output (retinal space as opposed to projected space)
      data$y <- data$y*(-1)
      #### order the rows
      data <- data[with(data, order((data$y), (data$x))), ]


      #-------------------------------------------------------------

      #############################################################
      ############# specify coordinates of interest ###############
      #############################################################
      xmin <- min(data$x)-2
      xmax <- max(data$x)+2
      ymin <- min(data$y)-2
      ymax <- max(data$y)+2
      coord <- coord_cartesian(xlim=c(xmin,xmax), ylim=c(ymin,ymax))
      #############################################################
      ########### segemented analysis #############################
      ### use eccentricity to specify regions
      #data <- mutate(data, eccen = sqrt((x^2)+(y^2)))
      #data <- data[data$eccen<90,]  # consider total field
      #data <- data[data$eccen<=8,] # central field only
      #data <- data[data$y>0,]    # superior field only etc...
      #data <-data[-c(5)]
      #write.xlsx (x = as.data.frame(alldata), file = "testdata.xlsx")
      ##############################################################
      ##############################################################
      ## change MAIA definitions of not seen <0db to 0db and seen at
      ## brightest 0db to a small no. <- will alter area slightly
      data$thresh[data$thresh==0] <- 0.1
      data$thresh[data$thresh==-1] <- 0
      meanthresh <- mean(data$thresh)
      meanthresh = formatC(meanthresh, digits = 1, format = "f")
      ### calculated flawed MAIA mean threshold output
      data1 <- data0[,c("ID","x_deg","y_deg","Threshold")]
      colnames(data1) <- c("ID", "x","y","thresh")
      data1 <- data1[!data1$ID==0,]   ## removes blindspot for mean threshold calc
      MAIAmeanthresh <- mean(data1$thresh)
      MAIAmeanthresh[MAIAmeanthresh<0] <- 0  ## if resulting MS is <0 report back as MS=0dB
      MAIAmeanthresh = formatC(MAIAmeanthresh, digits = 1,
                               format = "f")   ## report to 1 decimal place

      #central4 <- rbind.data.frame(data0[1:4,])
      #MAIAmeanthresh_central4 <- mean(central4$thresh)

      #############################################################
      ###### colour scheme and legend breaks to match MAIA ########
      #############################################################
      palette = c("#000000","#4F1B87",
                  "#A10974","#A30A5E","#B30B43","#D6083C","#FA1B23","#F7131B",
                  "#FF3037","#F73B3E","#F54A20","#F75128","#F75931","#ED582F",
                  "#ED5B32","#EB5426","#F56231","#FF722B","#F08827","#FF9C38",
                  "#FFA442","#FFAB4A","#FFC04A","#FFFF4A","#E1FF4A","#C7FF57",
                  "#BDFF42","#B3FA2F","#9FFA2F","#76FA2F","#52ED2B","#2ED622",
                  "#29CC21","#26C720","#24BF1F","#20B51D","#139911","#0B8A0B","#09010d"
      )
      #-------------------------------------------------------------
      #-------------------------------------------------------------
      b = c(0,0.09,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)
      lab = c("<0","0","1","2","3","4","5","6","7","8","9","10","11","12",
              "13","14","15","16","17","18","19","20","21","22","23",
              "24","25","26","27","28","29","30","31","32","33","34",
              "35","36")

      ### factorise the thresholds to categorise into legend values
      data$thresh_f <- cut(data$thresh, breaks = b, right = FALSE)
      my_breaks <- levels(data$thresh_f)
      levels(data$thresh_f)[levels(data$thresh_f)=="0.09"] <- "0" #this doesn't seem to do anything??

      #===========================================================
      #--------------- pointmap Plot Theme -----------------------
      theme <- theme_bw()+theme(
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 8),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        legend.key.size = unit(0.2, "cm"),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=9, hjust=0.95,
                                  margin = margin(b = -10))
      )+ theme(aspect.ratio=1)
      #===========================================================

      #### create convex hull just for visualisation purpose - Fields does this automatically
      hull <- data %>%
        slice(chull(x, y))
      ######################## PLOTS ##############################
      #############################################################
      #####################################
      ##### print pointmap fields plot
      #####################################
      #jpeg(filename = "plot-%d.jpeg",
      #     width=3000, height=2000, res = 350)
      pointmap <-  ggplot(data, aes(x=x, y=y, colour=thresh_f)) +
        geom_point(size=1.5) +
        #  geom_point(data = hull, col="darkblue", fill=NA)+  ## visualise convex/concave hull points
        geom_polygon(data = hull, col="darkblue", fill=NA)+ ## visualise convex/concave hull line
        scale_colour_manual(values=palette, lab=lab, drop=F,
                            name="[dB]")+
        geom_text(aes(label=thresh), hjust=-0.8, vjust=-0.1,
                  size=1.9, col="black")+
        labs(x = "x (degrees)", y = "y (degrees)")+
        guides(col = guide_legend(override.aes = list(shape = 15,
                                                      size = 3.5)))+
        theme +
        coord
      pointmap
      #dev.off()


      ##################################
      ##### generate heatmap and 3D map
      ##################################

      #########################################################
      #------------- TPS interpolation below ---------------
      #----------------------------------------------------------
      ###########################################################
      ############### Interpolate with TPS ######################
      ####### use Thin Plate Spline from Fields package #########
      nx<-400
      ny<-400
      df <- nrow(data1)
      tps_int <- fields::Tps(data.frame(data$x,data$y),
                             data$thresh, m=2, df=df)
      #                      data$thresh, theta = 2500) # cool graphics # use fastTps
      tps <- predictSurface(tps_int, nx=nx, ny=ny)
      ### following 3 lines remove possible artifacts
      tps$z[tps$z<0] <- 0
      tps$z[tps$z==0] <- 1e-04
      tps$z[tps$z>36] <- 36
      #---------------------
      dftps <- reshape2::melt(tps$z, na.rm = T)
      names(dftps) <- c("x", "y", "thresh")
      ##### factorize dataframe tps to create breaks to make like Octopus
      dftps$thresh_ftps <- cut(dftps$thresh, breaks = b, right = F)
      my_breaks <- levels(dftps$thresh_ftps)
      dftps$x <- tps$x[dftps$x]
      dftps$y <- tps$y[dftps$y]
      plot_tps <- ggplot(dftps, aes(x, y, z = thresh_ftps)) +
        geom_raster(aes(fill = thresh_ftps)) +
        scale_fill_manual(breaks=my_breaks, values=palette,
                          labels=lab, drop=F, name="[dB]") +
        theme +
        coord
      ########################
      ### vol under 3d surface
      ########################
      xlim <- range(tps$x)
      ylim <- range(tps$y)
      ## the size of each grid cell (a rectangular cell) is:
      cell_size <- (diff(xlim)/nx) * (diff(ylim)/ny)
      ## can convert units by changing z below (e.g. multiple by steradians), atm have dB-degrees^2
      z_tps <- tps$z
      norm <- sum(z_tps, na.rm=T) * cell_size
      ## your integrand
      integrand_tps <- z_tps
      ## get numerical integral by summation:
      volume_tps <- sum(integrand_tps, na.rm=T) * cell_size
      volume_tps = formatC(volume_tps, digits = 2, format = "f")
      #-----------------------------------------------------------


      #===========================================================
      ############ 2d heatmap plot with MS value ########################
      #============================================================
      #jpeg(filename = "plot-%d.jpeg",
      #    width=3000, height=2000, res = 600)
      #print(plot_tps)+labs(x = "x (degrees)", y = "y (degrees)")+
      #    ggtitle(paste("MAIA MS =",MAIAmeanthresh,"(dB)"))
      #dev.off()
      #============================================================
      #################### 3d plot ################################
      col_tps <- palette[cut(tps$z, breaks = b)]   #factorise the different colours on the 3D plot
      plot_3D_tpsvol <-  rgl::persp3d(tps$x,tps$y,tps$z, color=col_tps,
                                      xlim = c(xmin,xmax), ylim = c(ymin,ymax), zlim = c(0,40),
                                      xlab ="",ylab ="", zlab ="", axes=F, specular="gray60",
                                      sub="", main="", alpha = 1,
                                      aspect = c(100, 100, 40))  # changes axis aspect ratios

      ### create matrix which dictates the initial viewpoint
      #### view for pathology composite
      customview1 = matrix(c(0.951,-0.235,0.202,0,0.044,0.747,0.662,0,-0.306,-0.621,0.720,0,0,0,0,1), # the data elements
                           nrow = 4,             # number of rows
                           ncol= 4,             # number of columns
                           byrow = TRUE)
      #### view for nature RPGR case
      customview2 = matrix(c(1.014,0.193,0.027,0,-0.018,0.814,0.552,0,0.104,-0.547,0.832,0,0,0,0,1), # the data elements
                           nrow = 4,             # number of rows
                           ncol= 4,             # number of columns
                           byrow = TRUE)
      view3d(userMatrix=customview1, zoom=1.15)
      #view3d(theta = 0, phi = 0)    # change initial angle of 3D plot
      axes3d(c('x--','y--','z-+'))    # change position of 3D axis (back or front)
      title3d(xlab = "x (degrees)", line=2, cex=1.0)
      title3d(ylab = "y (degrees)", line=3, cex=1.0)
      mtext3d("[dB]", "z-+", line = 3, cex=0.9)
      par3d(windowRect = c(0, 31, 769, 679))   # change scale of display window for 3D plot

      #userMatrix<-par3d()$userMatrix
      #windowRect<-par3d()$windowRect

      #### print volume calculated onto plot title separately
      rgl::bgplot3d({
        plot.new()
        title(main = paste("Vol =",volume_tps, "(dB-degrees^2)",
                           "\n MAIA MS =",MAIAmeanthresh,"dB"),
              line=-4, cex.main=1.8)
      })


      ##############################
      ### save a snapshot ##########
      #rgl.snapshot("fig4.4.png")

      ##############################
      ### save interactive HTML page
      ##############################
      #browseURL(paste("file://", writeWebGL(dir=file.path("C:/Users/ajosan/Desktop/R_scripts/MAIA3d_px_examples"), width=500), sep=""))

      par3d(mouseMode = "trackball")
      rglwidget()
  })
    }
  shinyApp(ui, server)
#################### End of program #############################
#############################################################
#############################################################
