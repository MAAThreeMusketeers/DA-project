############################################################
#	IMAGE RECONSTRUCTION WITH PRINCIPAL COMPONENTS ANALYSIS  #
# Shiny dashboard                                          #
############################################################ 
#   Final Project                                          #
#   Descriptive Analytics                                  #
#   Fall semester 2015/2016                                #
#   Master in Advanced Analytics                           #
############################################################
#   Carolina Duarte                                        #
#   Beata Babiakova                                        #
#   Andras Kolbert                                         #
############################################################

#deploy the app
#shinyapps::deployApp('C:/Users/closer/Documents/GitHub/DA-project/SHINY')


############################################
##### SETTINGS #####

#install.packages("shiny")
#install.packages("shinydashboard")

library(shiny)
library(shinydashboard)
library(jpeg)


# Set Working Directory# ###SET TO YOUR  OWN GITHUB DIRECTORY!!!###
#setwd("C:\\Users\\-Andris\\Documents\\GitHub\\DA-project\\ToDeliver")
#setwd("C:\\Users\\closer\\Documents\\GitHub\\DA-project\\ToDeliver")
setwd("C:\\Users\\Carolina\\Documents\\GitHub\\DA-project\\ToDeliver")


############################################
##### USER INTERFACE #####


## app.R ##
ui <- dashboardPage(
  ##### HEADER #####
  dashboardHeader(title = "Image reconstruction with Principal Component Analysis"),
  
  ##### SIDEBAR #####
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Picture decomposition",
               tabName = "home", 
               badgeLabel = "pic",
               badgeColor ="maroon",
               icon = icon("line-chart")),
      
      menuItem("Choose image",
               icon = icon("picture",
               lib = "glyphicon"),
               selectInput("image",
               label = "Choose image",
               choices = c("Lisbon" = "lisbon", "Mona Lisa" = "monalisa", "User Input" = "user"))),
      
      menuItem("Choose number of PCs",
               icon=icon("arrows-h"),
               sliderInput(inputId = "numPCs", 
               label = "Choose number of PCs", 
               value = 1, min = 1, max = 100)),
      
      menuItem("Upload your own picture",
               icon = icon("cloud-upload", lib = "glyphicon"),
               fileInput("upload", 
               label = 'Select an Image',
               multiple = FALSE,
               accept=c('image/jpeg'))),
      
      menuItem("Choose correlation or covariance",
               icon = icon("cog", lib = "glyphicon"),
               selectInput("CorCov",
               label = "Choose correlation or covariance",
               choices = c("Correlation" = "cor","Covariance" = "cov"),multiple = FALSE)),
      
      menuItem("Choose Red Green or Blue",
               icon = icon("adjust", lib = "glyphicon"),
               selectInput("RGB",
               label = "Choose the component of RGB",
               choices = c("Red" = "r", "Green" = "g", "Blue" = "b"),multiple = FALSE)),
      
      menuItem("Scree plot", 
               tabName = "scree", 
               badgeLabel = "1. step", 
               badgeColor ="green",
               icon = icon("line-chart")),
      
      menuItem("Explained Variance", 
               tabName = "tableExplained", 
               badgeLabel = "2.step", 
               badgeColor ="green", 
               icon = icon("anchor")),
      
      menuItem("Loadings", 
               tabName = "loadings", 
               badgeLabel = "3. step", 
               badgeColor ="green", 
               icon = icon("th")),
      
      menuItem("BiPlot", 
               tabName = "biplot", 
               badgeLabel = "4. step", 
               badgeColor ="green", 
               icon = icon("anchor")),
      
      menuItem("About",
               tabName = "About",
               icon = icon("users"))
      
    )#end sidebarMenu
    
  ),#end of dashboardSidebar
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "Image decomposition", 
                    width = 12,
                    solidHeader = TRUE, 
                    status = "primary",
                    imageOutput("result"), 
                    height = 800)
              )
      ),
      
      tabItem(tabName = "scree",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "Scree plot",
                    width = 12,
                    solidHeader = TRUE, 
                    status = "primary",
                    plotOutput("screeplot", click = "plot_click"), 
                    verbatimTextOutput("info"),
                    #plotOutput(point), #trying to get a red point...
                    "The scree plot is a useful visual aid for determining an 
                    appropriate number of principal components. The scree plot 
                    graphs the eigenvalue against the component number. 
                    To determine the appropriate number of components, we look for 
                    an elbow in the scree plot. The component number is taken to 
                    be the point at which the remaining eigenvalues are relatively 
                    small and all about the same size.")
                )
      ),
      
      tabItem(tabName = "loadings",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "Loadings",
                    width = 12,
                    solidHeader = TRUE, 
                    status = "primary",
                    tableOutput("loadings"),
                    "The Loading Plot is a plot of the relationship between 
                    original variables and subspace dimensions. PC loadings 
                    measure the importance of each variable in accounting for the
                    variability in the PC. It is possible to interpret the first 
                    few PCs in terms of 'overall' effect or a 'contrast' between 
                    groups of variables based on the structures of PC loadings.")
                )
      ),
      
      tabItem(tabName = "biplot",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "Bi-Plot",
                    width = 12,
                    solidHeader = TRUE, 
                    status = "primary",
                    plotOutput("PC12plot"), 
                    "BiPlot:
                    The bi-plot shows both the loadings and the scores for two 
                    selected components in parallel. Bi-plot display is a 
                    visualization technique for investigating the inter-relationships 
                    between the observations and variables in multivariate data. 
                    In PCA, relationships between PC scores and PCA loadings 
                    associated with any two PCs can be illustrated in a bi-plot 
                    display.")
                )
      ),
      
      tabItem(tabName = "tableExplained",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "Explained Variance",
                    width = 12,
                    solidHeader = TRUE, 
                    status = "primary",
                    tableOutput("table_exp"), 
                    "Explained variance"),
                fluidRow(
                  box(title = "Kaiser Criterion",
                      width = 6,
                      solidHeader = TRUE, 
                      status = "primary",
                      verbatimTextOutput("kaiser")),
                  box(title = "Pearson Criterion",
                      width = 6,
                      solidHeader = TRUE, 
                      status = "primary",
                      verbatimTextOutput("pearson"))
                  
                ) #end of inside ("criteria") fluidRow
              ) #end of outside ("explained variance") fluidRow
       ), #end of "tableExplained" tabItem 
      
      tabItem(tabName = "About",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "About the project", 
                    width = 8,
                    height =550,
                    solidHeader = TRUE, 
                    status = "primary",
                    p(HTML('<img src="http://www.novaims.unl.pt/images/Logo.png" alt="NOVA IMS University" padding hspace="40"/>'),align ="center"),
                    p("This app was developed by Andras Kolbert, Bea Babiak and Carolina Duarte, candidates of Master in Advanced Analytics at Information Management School, NOVA University of Lisbon, Portugal. It is part of the final project of the Descriptive Analytics course (professor Jorge Mendes). The purpose is to present image reconstruction with Principal Component Analysis (PCA) as well as visually show and explain the PCA method."),
                    p("Fall semester, December 2015.")
                ),
                box(title = "About the Team", 
                    width = 4,
                    height =550,
                    solidHeader = TRUE, 
                    status = "primary",
                    p(HTML('<head>
                           <script src="//platform.linkedin.com/in.js" type="text/javascript"></script>
                           <script type="IN/MemberProfile" data-id="https://www.linkedin.com/in/beababiak" data-format="inline" data-related="false"></script>
                           </head>'),align = "center"),
                    p(HTML('<head>
                           <script src="//platform.linkedin.com/in.js" type="text/javascript"></script>
                           <script type="IN/MemberProfile" data-id="https://www.linkedin.com/in/carolinalmeidaduarte" data-format="inline" data-related="false"></script>
                           </head>'),align = "center"),
                    p(HTML('<head>
                           <script src="//platform.linkedin.com/in.js" type="text/javascript"></script>
                           <script type="IN/MemberProfile" data-id="https://www.linkedin.com/in/andraskolbert" data-format="inline" data-related="false"></script>
                           </head>'),align = "center")
                    )
                    )
                    )
      
                    )#end of tabItems
    
  )#end of dashboardBody
  
)#end of dashboardPage
  


############################################
##### SERVER #####

server <- function(input, output,session) {
  options(shiny.maxRequestSize = 5*1024^2)
  
  #copy the file into the pic's folder, save as user.jpg
  observe({
    if (is.null(input$upload)) return()
    file.remove(paste(getwd(),"pics","UserInput.jpg", sep="/"))
    file.copy(input$upload$datapath, paste(getwd(),"pics","UserInput.jpg", sep="/"))
  })
  
  #Dropdown menu decides which input to load
  
  #Reads the filenames into a variable
  picfiles <- grep('.jpg', list.files(path = paste(getwd(),"pics", sep="/")), 
                   value=TRUE)
  
  #Rtrail - eliminating .jpg
  picnames <- gsub(".jpg", "", picfiles)
  
  #Creating named list for the dropdown menu
  ls_choices.list <- as.list(picfiles) 
  names(ls_choices.list) <- picnames
  ls_choices.list
  
  
  #output for the app:
  # Calculate image with k PCs
  output$result <- renderImage({
    #load to "var" the path to the user-chosen input image
    var <-switch(input$image,
                 "lisbon" = "pics/LisbonInput.jpg",
                 "monalisa" = "pics/MonaLisaInput.jpg",
                 "user" = "pics/UserInput.jpg"
    )
    
    
    # loading picture previously chosen
    original=readJPEG(var)
    
    #chosen picture into the form of a 3D matrix (each pixel has 3 values: RGB)
    #(or, in other words, 3 matrices: 1 for each of the R, G and B component)  
    R=original[,,1]
    G=original[,,2]
    B=original[,,3]
    
    #Updating the displayed maximum number of PCs to use
    updateSliderInput(session, "numPCs", max =dim(original)[2])
    
    #Number of principal components to use (coming from the input slider):
    k = input$numPCs
    
    #####
    #Compute a correlation or covariance matrix and its eigen vectors 
    #(for all R, G and B - to be used in further calculations)
    #Red
    r1 <- switch(input$CorCov,
                 "cor" = cor(R),
                 "cov" = cov(R)
    )
    g1=eigen(r1)
    Rv=g1$vectors[,1:k]
    
    #Green
    r2 <- switch(input$CorCov,
                 "cor" = cor(G),
                 "cov" = cov(G)
    )
    g2=eigen(r2)
    Gv=g2$vectors[,1:k]
    
    
    #Blue
    r3 <-switch(input$CorCov,
                "cor" = cor(B),
                "cov" = cov(B)
    )
    g3=eigen(r3)
    Bv=g3$vectors[,1:k]
    
    
    #a temp file to save the output
    outfile <- tempfile(fileext = ".jpg")
    
    #initialize to a 3D matrix of the correct size
    img=original 
    
    img[,,1]=R%*%Rv%*%t(Rv)
    img[,,2]=G%*%Gv%*%t(Gv)
    img[,,3]=B%*%Bv%*%t(Bv)
    
    writeJPEG(img, target = outfile)
    #return a list containing information about the image
    list(src = outfile,
         contentType = "image/jpeg")
    
    
  })# end of output$result
  
  
  
  #output for the app:
  #Explained Variance Table
  output$table_exp <- renderTable({
    #load to "var" the path to the user-chosen input image
    var <-switch(input$image,
                 "lisbon" = "pics/LisbonInput.jpg",
                 "monalisa" = "pics/MonaLisaInput.jpg",
                 "user" = "pics/UserInput.jpg"
    )
    
    # loading picture previously chosen
    original=readJPEG(var)
    
    #chosen picture into the form of a 3D matrix (each pixel has 3 values: RGB)
    #(or, in other words, 3 matrices: 1 for each of the R, G and B component)  
    R=original[,,1]
    G=original[,,2]
    B=original[,,3]
    
    #output theory for R,G or B (1 of them, depending on the input)
    A <- switch(input$RGB,
                "r" = R,
                "g" = G,
                "b" = B)
    
    g <- switch(input$CorCov,
                "cor" = eigen(cor(A)),
                "cov" = eigen(cov(A)))
    
    
    # -1- perentage of total variation explained by the components (first 15)
    perc_exp<-g$values/sum(g$values)
    
    cum_exp<-c(perc_exp[1], sum(perc_exp[1:2]), sum(perc_exp[1:3]), 
               sum(perc_exp[1:4]),sum(perc_exp[1:5]),sum(perc_exp[1:6]),
               sum(perc_exp[1:7]),sum(perc_exp[1:8]),sum(perc_exp[1:9]),
               sum(perc_exp[1:10]),sum(perc_exp[1:11]),sum(perc_exp[1:12]),
               sum(perc_exp[1:13]),sum(perc_exp[1:14]),sum(perc_exp[1:15]),
               sum(perc_exp[1:16]),sum(perc_exp[1:17]),sum(perc_exp[1:18]),
               sum(perc_exp[1:19]),sum(perc_exp[1:20]),sum(perc_exp[1:21]),
               sum(perc_exp[1:22]),sum(perc_exp[1:23]),sum(perc_exp[1:24]),
               sum(perc_exp[1:25]))
    
    table_exp<-cbind("Eigen Values"=g$values[1:25], "Variance Explained"=perc_exp[1:25], 
                     "Cummulated Variance Explained"=cum_exp)
    
    #output for the app:
    #Kaiser criterion
    output$kaiser <- renderText({
      
      #the number of Principal Components that should be considered: factors with 
      #eigenvalues greater than 1
      Kaiser <- sum(table_exp[,1] > sum(g$values)/ncol(A))
      paste0("According to Kaiser, we should keep the principal components with eigenvalues greater than the average. In our case the number of principal components to keep by the Kaiser criterion is ",
             Kaiser,
             ".")
    })
    
    #output for the app:
    #Pearson criterion
    output$pearson <- renderText({
      
      #accept PC until when cumulated variance explained is above 0.8, included
      Pearson <- sum(table_exp[,3] <= 0.8)+1
      paste0("Regarding the Pearson's criterion, we have enough principal components when the cumulative  variance explained by them is more than 80%. In this case the number of principal components to keep by Pearson criterion is ",
             Pearson,
             ".")
    })
    
    table_exp
    
  }) # end of output$table_exp
  
  
  
  #output for the app:
  # Screeplot to see the elbow
  output$screeplot <- renderPlot({
    #load to "var" the path to the user-chosen input image
    var <-switch(input$image,
                 "lisbon" = "pics/LisbonInput.jpg",
                 "monalisa" = "pics/MonaLisaInput.jpg",
                 "user" = "pics/UserInput.jpg"
    )
    
    # loading picture previously chosen
    original=readJPEG(var)
    
    #chosen picture into the form of a 3D matrix (each pixel has 3 values: RGB)
    #(or, in other words, 3 matrices: 1 for each of the R, G and B component)  
    R=original[,,1]
    G=original[,,2]
    B=original[,,3]
    
    #output theory for R,G or B (1 of them, depending on the input)
    A <- switch(input$RGB,
                "r" = R,
                "g" = G,
                "b" = B)
    
    g <- switch(input$CorCov,
                "cor" = eigen(cor(A)),
                "cov" = eigen(cov(A)))
    
    
    lambda=g$values
    
    plot(y=lambda,
         x=1:length(lambda),
         log="x",
         type="b",
         las=1,
         ylab="Eigenvalues",
         xlab="Index (log scale)")
    
    output$info <- renderText({
      paste0("Number of PCs chosen by scree plot: ", 
             round(as.numeric(input$plot_click$x)))
    })
    
  })# end of output$screeplot
  
  
  
  #output for the app:
  #Table with Loadings
  output$loadings <- renderTable({
    #load to "var" the path to the user-chosen input image
    var <-switch(input$image,
                 "lisbon" = "pics/LisbonInput.jpg",
                 "monalisa" = "pics/MonaLisaInput.jpg",
                 "user" = "pics/UserInput.jpg"
    )
    
    # loading picture previously chosen
    original=readJPEG(var)
    
    #chosen picture into the form of a 3D matrix (each pixel has 3 values: RGB)
    #(or, in other words, 3 matrices: 1 for each of the R, G and B component)  
    R=original[,,1]
    G=original[,,2]
    B=original[,,3]
    
    
    #output theory for R,G or B (1 of them, depending on the input)
    A <- switch(input$RGB,
                "r" = R,
                "g" = G,
                "b" = B)
    
    g <- switch(input$CorCov,
                "cor" = eigen(cor(A)),
                "cov" = eigen(cov(A)))
    

    #15 rows, 10 columns, just a sample!
    A.std<-scale(A, center=TRUE, scale=TRUE)
    scores<-A.std%*%g$vectors
    
    loadings<-cor(scores, A.std)
    loadings[1:15,1:10]
    
    
  })
  
  
  
  #output for the app:
  # Scatterplot of the scores PC1 versus PC2
  output$PC12plot <- renderPlot({
    #load to "var" the path to the user-chosen input image
    var <-switch(input$image,
                 "lisbon" = "pics/LisbonInput.jpg",
                 "monalisa" = "pics/MonaLisaInput.jpg",
                 "user" = "pics/UserInput.jpg"
    )
    
    # loading picture previously chosen
    original=readJPEG(var)
    
    #chosen picture into the form of a 3D matrix (each pixel has 3 values: RGB)
    #(or, in other words, 3 matrices: 1 for each of the R, G and B component)  
    R=original[,,1]
    G=original[,,2]
    B=original[,,3]
  
    
    #output theory for R,G or B (1 of them, depending on the input)
    A <- switch(input$RGB,
                "r" = R,
                "g" = G,
                "b" = B)
    
    g <- switch(input$CorCov,
                "cor" = eigen(cor(A)),
                "cov" = eigen(cov(A)))
    
    
    A.std<-scale(A, center=TRUE, scale=TRUE)
    scores<-A.std%*%g$vectors
    plot(scores[,1:2])
    abline(h=0); 
    abline(v=0)
  })
  
  
}#end of server

shinyApp(ui = ui, server = server)

