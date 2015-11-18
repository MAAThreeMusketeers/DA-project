# Set Working Directory
setwd("C:\\Users\\closer\\Documents\\Projecto\\DA-project")


# (Install jpeg library previously)
# Load library and read the file

library(jpeg)
original = readJPEG("MonaLisaInput.jpg")

# The image is viewed as a 3D array of dimensions:
dim(original)


# That means to each pixel corresponds 3 values, that
# determine the color (RGB)
# RGB channels can have values between 0 and 255 or 
# between 0 and 1 (which is the case)

# To do the analysis we need a matrix with the same number
# of pixels but with just one value per pixel
# To do that we can use just one channel or combine the 3

# Lets start with gray images.A possible way of converting it:

#gray = (original[,,1]+original[,,2]+original[,,3])/3

##################################################
#MISSING DATA CHALLENGE
##################################################

#insert missing values (NA) 
#(check http://www.r-bloggers.com/r-na-vs-null/)

original[60:75, 60:65, 1:3] <- NA
writeJPEG(original,"MissingValuesOutput1.jpg")

gray = (original[,,1]+original[,,2]+original[,,3])/3


#checking
if(sum(is.na(gray))>0) {
  library(mice)
  imp <- mice(gray, m=1, maxit=1, printFlag=TRUE)
  Datimp <- complete(imp, "long", include=TRUE)  
  gray2 <- Datimp[145:288, 1:120]
  gray <- sapply(gray2[1:144, 3:120], as.numeric)
}



writeJPEG(gray,"MissingValuesOutput2.jpg")



# Do the analysis using the correlation or the covariance
# matrix. Just change the definition of r

#r=cov(gray)   
r=cor(gray) 

g=eigen(r)
v=g$vectors


# Do the scree plot (using log scale just to see it better)

lambda=g$values

plot(y=lambda,
     x=1:length(lambda),
     log="x",
     type="b",
     las=1,ylab="Eigenvalues",xlab="Index (log scale)")


# Reconstruct the original image

img=gray%*%v%*%t(v)
writeJPEG(img,"original.jpeg")


# Reconstruct the image with just k pcs
k=60
v=g$vectors[,1:k]
img=gray%*%v%*%t(v)
name=paste(k,"PCs.jpeg")
writeJPEG(img,name)

# If you want to see how a particular component looks like
#k=1
#v=g$vectors[,k]
#img=gray%*%v%*%t(v)
#name=paste(k,"PC.jpeg")
#writeJPEG(img,name)

