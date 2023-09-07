wd = "/Users/Xiao/Desktop/FaceData/CroppedYale/train"
setwd(wd)
library(pixmap)
library(MASS)
f.names = list.files(path = ".", pattern = ".pgm")

data = array(0/0, dim=c(length(f.names), 192*168))
for (i in 1:length(f.names)){
  test = read.pnm(f.names[i])
  test = matrix(test@grey,ncol=1)
  data[i,] = t( test )
}
plot(read.pnm(f.names[2]))
# plot the mean face:
average = colMeans(data)
average.m = matrix(average, nrow=192)
image.template = read.pnm(f.names[1])
average.face = image.template
average.face@grey = average.m
plot(average.face)



# take the mean
for (i in 1:ncol(data)){
  data[,i] = (data[,i] - mean(data[,i]))
}

# svd:
K = 20
U = svd(t(data))$u
U = U[,1:K]

# recover:
image = image.template
tmp = matrix( U %*%  ginv(U) %*% matrix(image@grey,ncol=1), nrow = 192)
tmp[which(tmp<0)] = 0
tmp[which(tmp>1)] = 1
image@grey =   tmp
plot(image)

# PCA classification
U = svd(t(data))$u
U = U[,1:3]
data.Z = data %*% U
plotPCA(data.Z, 5, text=rownames(data))


# -------------------
# Test
wd = "/Users/Xiao/Desktop/FaceData/CroppedYale/test"
setwd(wd)
f.names.test = list.files(path = ".", pattern = ".pgm")
data.test = array(0/0, dim=c(length(f.names.test), 192*168))
for (i in 1:length(f.names.test)){
  test = read.pnm(f.names.test[i])
  test = matrix(test@grey,ncol=1)
  data.test[i,] = t( test )
}

for (i in 1:ncol(data.test)){
  data.test[,i] = (data.test[,i] - average[i]) 
}


# recover:
U = svd(t(data))$u
U = U[,1:10]
image = read.pnm(f.names.test[1])
tmp = matrix( U %*%  ginv(U) %*% matrix(image@grey,ncol=1), nrow = 192)
tmp[which(tmp<0)] = 0
image@grey =   tmp
plot(image)


U = svd(t(data))$u
U = U[,1:3]
data.Z.test = data.test %*% U
groups = c(1,1,2,2,2)

plot3d(data.Z.test, col=groups, type="s", size=1, axes=F)
axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
text3d(data.Z.test,texts=c("1","1","2","2","2"),cex=1)
grid3d("x")
grid3d("y")
grid3d("z")


