
## read in the data sets 
pml_train<-read.csv("pml-training.csv")
pml_test<-read.csv("pml-testing.csv")

a<-which(colSums(is.na(pml_test))==0) ## get all the columns that have ZERO NAs from the test dataset 


################################
#GEt values from an atomic vector
#################################

getValuesFromAnAtomicVector <- function(list) {

  ll<-c()
  
  for(i in 1:length(list)){    
    #print(i)  
    tmp<-getElement(a,i)
    ll<-append(ll,tmp)
  }
  
  ll
  
}


columnsWithNoNAs<-getValuesFromAnAtomicVector(a) # this is all the columns that have ZERO NAs

pml_train_NoNAs<-pml_train[,columnsWithNoNAs] ## create a new train dataset with just the columns that have ZERO NAs from the test dataset


################################
# Find Factors in DF
#################################

FindFactorsInDF <- function(df = data.frame()) {
  
  ndf<-names(df)
  index<-c()
  
  for(i in 1:length(ndf)){
    col.name <- ndf[i]
    #print(col.name)
    #print(class(df[,1]))
    if (class(df[,i])=="factor"){
      index<-append(index,i)
    }
  }
  index #return index of variables that are factors  
}


FindFactorsInDF(pml_train_NoNAs) ## Find Factors in DF 

#these are:
#user_name 
#cvtd_timestamp
#new_window
#classe ## but don't want to delete this 

FindFactorsInDF(pml_train_NoNAs) ## Find Factors in DF 
#[1]  2  5  6 60 
pml_train_NoNAs[,2]<-NULL ## remove columns that are factors
FindFactorsInDF(pml_train_NoNAs) ## Find Factors in DF 
#[1]  4  5 59
pml_train_NoNAs[,4]<-NULL ## remove columns that are vectors
FindFactorsInDF(pml_train_NoNAs) ## Find Factors in DF 
#[1]  4 58
pml_train_NoNAs[,4]<-NULL ## remove columns that are vectors
FindFactorsInDF(pml_train_NoNAs) ## Find Factors in DF
#[1] 57


library(caret) ## need this for train function 

mf1<-train(classe ~.,data=pml_train_NoNAs, method="lda") ## build my model

mf1  # show my model this is 99% whcih suggests overfitting 

################################
# Predict Model for project giving it model and testdata fro prediction
#################################

predicMLProject<- function(df = data.frame(),mf) {  ##mf will be model fit to use for prediction
  
  nr<- nrow(df) # get number of rows # it will be 1 row per test/prediction
  pl<-c() # prediction_list containg all the predictions per row 
  for(i in 1:nr){
    #print(i)
    nn<-predict(mf,df[i,])
    nn<-as.character(nn)
    pl<-append(pl,nn)
  }
  
  pl
}


predicMLProject(pml_test,mf1)
#[1] "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A" "A"

mf1
#Linear Discriminant Analysis 

#19622 samples
#56 predictor
#5 classes: 'A', 'B', 'C', 'D', 'E' 

#No pre-processing
#Resampling: Bootstrapped (25 reps) 

#Summary of sample sizes: 19622, 19622, 19622, 19622, 19622, 19622, ... 

#Resampling results

#Accuracy   Kappa      Accuracy SD   Kappa SD    
#0.9999335  0.9999158  8.121178e-05  0.0001027739


plot(pml_train_NoNAs[,1], ylab=names(pml_train_NoNAs)[1]) ## a plot of variable X this could be removed because of its pattern 

#a qplot of the variable X in relation to classe
qplot(classe,pml_train_NoNAs[,1],color=classe,data=pml_train_NoNAs,xlab="classe",ylab=names(pml_train_NoNAs[1]))

#then I decided to remove a further 3 columns 
#X
#raw_timestamp_part_1
#raw_timestamp_part_2


pml_train_NoNAs_mf2<-pml_train_NoNAs
> dim(pml_train_NoNAs_mf2)
#[1] 19622    57

dim(pml_train_NoNAs_mf2)
#[1] 19622    57 
pml_train_NoNAs_mf2[,1]<-NULL
dim(pml_train_NoNAs_mf2)
#[1] 19622    56
pml_train_NoNAs_mf2[,1]<-NULL
dim(pml_train_NoNAs_mf2)
#[1] 19622    55
pml_train_NoNAs_mf2[,1]<-NULL
dim(pml_train_NoNAs_mf2)
#[1] 19622    54

# create my second model
mf2<-train(classe ~.,data=pml_train_NoNAs_mf2, method="lda")

mf2
#Linear Discriminant Analysis 

#19622 samples
#53 predictor
#5 classes: 'A', 'B', 'C', 'D', 'E' 

#No pre-processing
#Resampling: Bootstrapped (25 reps) 

#Summary of sample sizes: 19622, 19622, 19622, 19622, 19622, 19622, ... 

#Resampling results

#Accuracy   Kappa      Accuracy SD  Kappa SD  
#0.7140877  0.6381531  0.00532862   0.00652724



 
predicMLProject(pml_test,mf2)
#[1] "B" "A" "B" "A" "A" "E" "D" "D" "A" "A" "D" "A" "E" "A" "B" "A" "A" "B" "B" "B"

#this takes a while
mf3<-train(classe ~.,data=pml_train_NoNAs_mf2, method="rf")
#Loading required package: randomForest
#randomForest 4.6-10
#Type rfNews() to see new features/changes/bug fixes.
mf3
#Random Forest 

#19622 samples
#53 predictor
#5 classes: 'A', 'B', 'C', 'D', 'E' 

#No pre-processing
#Resampling: Bootstrapped (25 reps) 

#Summary of sample sizes: 19622, 19622, 19622, 19622, 19622, 19622, ... 

#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa      Accuracy SD   Kappa SD   
#2    0.9950220  0.9937043  0.0013603270  0.001720807
#27    0.9977092  0.9971029  0.0006661504  0.000843213
#53    0.9947202  0.9933219  0.0021087307  0.002671587

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 27. 


predicMLProject(pml_test,mf3)
#[1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A" "B" "B" "B"  # mf3 answers
ans<-predicMLProject(pml_test,mf3)

##ans[c(3,8,11,12,15,16)]
#[1] "B" "B" "B" "C" "E" "E"
#ans_orig_4wrong[c(3,8,11,12,15,16)]
#[1] "B" "D" "D" "A" "B" "A" ## answers I got wrong last time; there was 6 in total; notice that no.3 is still the same  

#############
##Function to write out my 20 files 
#############

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(ans)  # write my 20 new files 






#############
##Rough Work
#############

plotAllVariables <- function(df = data.frame()) {
  
  ndf<-names(df)
  r<- 3  ## how many rows
  c<- 3   ## how many columns 
  
  par(mfrow=c(3,3))
  
  for(i in 1:length(ndf)){
    col.name <- ndf[i]
    print(col.name)
    plot(df[,i], ylab=ndf[i]) 
  }
  
}


plotAllVariablesProject <- function(df = data.frame()) {
  
  ndf<-names(df)
  #r<- 3  ## how many rows
  #c<- 3   ## how many columns 
  
  #par(mfrow=c(3,3))
  par(ask = TRUE)
  #The standard way to pause between plots is to set 'par(ask = TRUE)'
  #before your first plot, which will then prompt you to continue after
  #each new plot. See ?par for more information.
  
  for(i in 1:length(ndf)){
    col.name <- ndf[i]
    print(paste(i,": ", col.name))
    #plot(df[,57],df[,i], ylab=ndf[i]) 
    
    #Option1 to plot
    #plot(df[,i], ylab=names(df)[i])
    #Option2 to plot
    print(qplot(classe,df[,i],color=classe,data=df,xlab="classe",ylab=names(df[i])) ## need to use print here for qplot
  }
  
}


## replace all the values "#DIV/0!" with NA using this function 
fix_missing <- function(x) {
  x[x == "#DIV/0!"] <- NA
  x
}


FindFactorsInDF <- function(df = data.frame()) {
  
  ndf<-names(df)
  r<- 3  ## how many rows
  c<- 3   ## how many columns 
  index<-c()
  
  for(i in 1:length(ndf)){
    col.name <- ndf[i]
    #print(col.name)
    #print(class(df[,1]))
    if (class(df[,i])=="factor"){
      index<-append(index,i)
    }
  }
  index #return index of variables that are factors  
}

## this might be wrong as the index would need to change each time a column was deleted 
DropColumnsFromDF <- function(df = data.frame(),index) {
  
  print(dim(df))
  for(i in index){
    print(i)
    df[,i]<- NULL
  }
  print(dim(df))
  df # return the df with columns removed 
}



makeAllColumnsnumericDF <- function(df = data.frame()) {
  
  print(dim(df))
  for(i in 1:ncol(df)){
    print(i)
    if(class(df[,i])=="integer"){
      df[,i]<- as.numeric(df[,i])
    }
  }
  print(dim(df))
  df # return the df with columns removed 
}


## want to plot all the data this is an example
library(datasets); data(mtcars); require(stats); require(graphics)
pairs(mtcars, panel = panel.smooth, main = "mtcars data", col = 3)

require(stats); require(graphics)
pairs(pml, panel = panel.smooth, main = "mtcars data", col = 3)


####
Want a funtion that will give the output of my predict function 
I will give it the df to predict 
####
> class(predict(mf,pml_test[2,]))
[1] "factor"
> nn<-predict(mf,pml_test[2,])
> nn
[1] A
Levels: A B C D E
> nn2<-as.character(nn)
> nn2
[1] "A"






####
Prediction Assignment Submission: Instructions Help

Please apply the machine learning algorithm you built to each of the 20 test cases in the testing 
data set. For more information and instructions on how to build your model see the prediction 
assignment writeup. For each test case you should submit a text file with a single capital 
letter (A, B, C, D, or E) corresponding to your prediction for the corresponding problem in the 
test data set. You get 1 point for each correct answer. You may submit up to 2 times for 
each problem. I know it is a lot of files to submit. It may be helpful to use the following 
function to create the files. If you have a character vector with your 20 predictions in order
for the 20 problems. So something like (note these are not the right answers!):
  
  answers = rep("A", 20)

then you can load this function by copying and pasting it into R:
  
  
  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }

then create a folder where you want the files to be written. Set that to be your working directory and run:
  
  
  pml_write_files(answers)

and it will create one file for each submission. Note: if you use this script, please make sure the files that get written out have one character each with your prediction for the corresponding problem ID. I have noticed the script produces strange results if the answers variable is not a character vector. 


class(pml_train3$user_name)=="factor"



