source("idt-function.R")
library(data.tree)

idtTree=Node$new(1, dat = data, depth=1, bname=1, ids=c(0), Leaf=FALSE, class=NULL)
cluster11 = idtTree$AddChild(2, depth=2, bname=c(1,2), ids=c(1), Leaf=FALSE, class=NULL)
cluster12 = idtTree$AddChild(3, depth=2)
cluster21 = cluster11$AddChild(4, depth=3)
plot(idtTree)


feat=iris[,1:4]
feat=as.data.frame(feat)

# first we do PCA on the data, note that feat.pca$x is the data after pca (transformed data)
feat.pca=prcomp(feat, retx=TRUE, ceter=TRUE)

#we select the 2nd elbow as the dimension for clustering, also get the scree plot
dim=getElbows(feat.pca$sdev)[2]
print(dim)
#we do clustering on the data feat.pca$x[,1:dim]
feat_clusted = Mclust(feat.pca$x[,1:dim], G=1:2, modelNames=c("EEE","EEV","VEV","VVV"))

#plot the clusted data
plot(feat_clusted, what="classification")

# print out the labels after clustering
#print(feat_clusted$classification)

#reattached the labels to the feat_pca, prepare for break-up by label
feat.pca_newdatawithlabel=cbind(feat.pca$x, feat_clusted$classification)

#make sure the resulted data is a dataframe
feat.pca_newdatawithlabel=as.data.frame(feat.pca_newdatawithlabel)

#we split the data accorading to the lables
feat2_withlabel = split(feat.pca_newdatawithlabel , f = feat.pca_newdatawithlabel[,ncol(feat.pca_newdatawithlabel)])


#return a list of subclusters
print(feat2_withlabel)


splittedfeat=list()
labelindex=1
for (i in feat2_withlabel){
  rownames=rownames(i)
  print(nrow(feat[rownames,]))
  newlabel=rep(labelindex, nrow(feat[rownames,]))
  subcluster_with_label=cbind(feat[rownames,], newlabel)
  labelindex=labelindex+1
  #print(feat[rownames,])
  splittedfeat[[length(splittedfeat)+1]]=subcluster_with_label
}

print(splittedfeat)

#mylist[[length(mylist)+1]] <- obj



