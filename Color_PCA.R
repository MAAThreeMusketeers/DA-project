# Set Working Directory
setwd("C:/Users/Carolina/Desktop")


# (Install jpeg library previously)
# Load library and read the file

library(jpeg)
original = readJPEG("monalisa.jpeg")

# Do the same process separatly for each channel

R=original[,,1]
G=original[,,2]
B=original[,,3]


# Compute a correlation or covariance matrix
# and its eigen vectors

#Number of principal components to use:
k=10

### Red

#r=cov(R)
r=cor(R)
g=eigen(r)
Rv=g$vectors[,1:k]


###Green

#r=cov(G)
r=cor(G)
g=eigen(r)
Gv=g$vectors[,1:k]


###Blue

#r=cov(B)
r=cor(B)
g=eigen(r)
Bv=g$vectors[,1:k]


# Reconstruct the original image

# just an easy way to initialize it to a 3D matrix of the correct size
img=original 

img[,,1]=R%*%Rv%*%t(Rv)
img[,,2]=G%*%Gv%*%t(Gv)
img[,,3]=B%*%Bv%*%t(Bv)
name=paste(k,"PCsRGB.jpeg")
writeJPEG(img,name)
