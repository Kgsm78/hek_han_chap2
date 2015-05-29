
########2.42 Proximity for nominal attributes
#This function calculates proximity between the elements of a vector---either nominal or numeric

##function to calculate dissimilarity matrix for a vector
dissimvect <- function(x) {    #x is a vector with either nominal or numeric data
        dismat<-matrix(nrow=length(x),ncol =length(x))
        for (i in 1:length(x)) {
                for (j in 1:length(x)) {
                        ifelse((x[i]==x[j]),      
                               dismat[i,j]<-0,
                               dismat[i,j]<-1)
                }
                
        }
        print("Dissimilarity matrix")
        print(dismat)
        simmat<-matrix(nrow=length(x),ncol =length(x))
        for (i in 1:length(x)) {
                for (j in 1:length(x)) {
                        simmat[i,j]<-1-dismat[i,j]}}
        print("Similarity matrix")
        simmat
}

#for example:
table22<-read.csv("C:/Users/Holly/Desktop/R/table22.csv")  #your filepath here
vector1<-table22$test_1    #nominal values in this vector
dissimvect(vector1)  #dissimilarity matrix corrspnds to example 2.17


##############2.43 Proximity for Binary attibutes  Jaccard coeficient
table24<-read.csv("C:/Users/Holly/Desktop/R/table24.csv")   #put your filepath heare
nogen<-table24[,3:8]     #these are the binary attributes
#install and load vegan package
install.packages("vegan")
library(vegan)
#use vegdist function
vegdist(nogen,method="jaccard")  #results correspond to top ob page 72


##############2.44 Minkowski distance for numeric data
tableex<-read.csv("C:/Users/Holly/Desktop/R/minktable.csv") #your filehpath
nolab<-tableex[,2:5]  #This are the numeric attributes

#dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dist(nolab, method = "euclidean")
dist(nolab, method = "manhattan")
dist(nolab,method = "minkowski", p =1)  #p=1 yields Manhattan
dist(nolab,method = "minkowski", p =2)    #p=2 yields Euclidean
#supremum
dist(nolab, method = "maximum")
#'supremum'--of all the attributes for any given row in relation to another row, 
#'what is maximum difference'between values of the attribute?
#'
################2.46  Dissimilarity for Attributes of Mixed Types
table22<-read.csv("C:/Users/Holly/Desktop/R/table22.csv")
table22
install.packages("cluster")
library("cluster")
test<-table22[,c(2,4,6)]  #these are the normalized attributes
daisy(test)#the results correspond to table at top of page 77

##################2.47 cosine similarity
x<-c(5,0,3,0,2,0,0,2,0,0)
y<-c(3,0,2,0,1,1,0,1,0,1)
matrix<-rbind(x,y)
matrix
install.packages('proxy') 
library('proxy') # Library of similarity/dissimilarity measures for 'dist()'
answer1<-dist(matrix, method="cosine")
answer2<-1-answer1
answer2

cosineDist <- function(x){
        as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}

1-cosineDist(matrix)



###tanimoto/jaccard
dist(matrix, method="jaccard")
#  3 attributes with common values among 10 attributes = .33333