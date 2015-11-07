############################################################
#	IMPLEMENTING PROJECT IN SHINY				     #
#	last: bea, 20151106					     #
############################################################

#observations: 
#1
#if we use pictures with a lot of blank space 
#(say whole "rows" or "columns" empty, the app gives error "infinite or missing values, standard deviation is zero")

#2
#Carolina maybe??
#we should check the max possible PCs (number of "variables") and use that as the maximum on the slider, would be great! (you can get to the original picture!)

############
#tutorial used to create this App: http://shiny.rstudio.com/tutorial/

############
#note: there is a computer maintaining the shiny app in the background and it our case right now it is
#YOUR computer and it uses an R session and now it is YOUR session, so your R is busy
#with maintaining the app. If you want to do something else (run the sample code in this file),
#you have to close the app by clicking the STOP button (kill the app). then you can run the code

############
#you can do this later, it is for putting the App online, I have done it and runs fine (but takes quite some time)

#create an account on shinyapps.io and follow their instructions!! 

#-------------------IMPORTANT------------------#
#after finishing the project we will deploy it again:
#shinyapps::deployApp('C:/Users/beyka/Desktop/MAA projects/DA-Project/DA-project/SHINY')

############
library(shiny)

############################################
#what the app will actually do:

ui <- fluidPage(

	titlePanel("Image reconstruction with PCA"),

#INPUTS
#interesting inputs for our project to be discovered: fileInput(to upload your own pic), selectInput: for choosing pic)(

#a slider to choose number of principal components
	sliderInput(
		inputId = "numPCs", 
		label = "Choose number of PCs", 
		value = 40, min = 1, max = 100), #here we should have MAX set to the number of possible PCs of each specific picture (should read it each time)

###THIS DOES NOT WORK YET, TO BE ADJUSTED!###

#this does not generate error but apparently does not work yet:
#a select list to choose a pic
	selectInput(
		InputId = "image",
		label = "Choose image",
		choices = c("Lisbon" = "lisbon", "Mona Lisa" = "monalisa"),
		multiple = FALSE),

#a select list to choose between correlation and covariance
#	selectInput(
#		InputId = "CorCov",
#		label = "Choose correlation or covariance",
#		choices = c("Correlation" = "cor", "Covariance" = "cov"),
#		multiple = FALSE),

##if possible, it would be good to add an option to upload your own picture!


#OUTPUTS
#interesting outputs for our project: dataTableOutput(): an interactive table, imageOutput(), plotOutput(), textOutput()
	imageOutput("result")
)

#############################################

# Set Working Directory# ###SET TO YOUR  OWN GITHUB DIRECTORY!!!###
#setwd("C:\\Users\\beyka\\Desktop\\MAA projects\\DA-Project\\DA-project\\SHINY")


# (Install jpeg library previously)
# Load library and read the file

# TIP: if the readJPEG does not work for you maybe your file is stored as .jpg, even though it is a jpeg. you can check it with this command:
# file.info(list.files(getwd(),full.names=TRUE))


library(jpeg)


#############################################
#assemble your input values into output values:

server <- function(input, output) {

if (input$image == "monalisa") then { 	original = readJPEG("LisbonInput.jpg")}
if (input$image == "lisbon") then { 	original = readJPEG("MonaLisaInput.jpg")}

# Do the same process separatly for each channel
	R=original[,,1]
	G=original[,,2]
	B=original[,,3]


#output function depends on the input
#render function makes the output
	output$result <- renderImage({ 

# Compute a correlation or covariance matrix
# and its eigen vectors

#Number of principal components to use:
		k = input$numPCs

### Red
		r=cov(R)
		#r=cor(R)
		g=eigen(r)
		Rv=g$vectors[,1:k]


###Green
		#if (input$CorCov == "cor") 
		r=cov(G)
		#r=cor(G)}
		g=eigen(r)
		Gv=g$vectors[,1:k]


###Blue
		r=cov(B)
		#r=cor(B)}
		g=eigen(r)
		Bv=g$vectors[,1:k]


#a temp file to save the output.
		outfile <- tempfile(fileext = ".jpg")

# just an easy way to initialize it to a 3D matrix of the correct size
		img=original 

		img[,,1]=R%*%Rv%*%t(Rv)
		img[,,2]=G%*%Gv%*%t(Gv)
		img[,,3]=B%*%Bv%*%t(Bv)

		writeJPEG(img, target = outfile)

#return a list containing information about the image

		list(src = outfile,
			contentType = "image/jpeg",
			alt = "This is alternate text") #I dont know yet what this does but might be useful later
	})
}

shinyApp(ui = ui, server = server)

