mini <- FALSE

#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 1000
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

# Find r and s indices:
if((sim_num-sim_start)%%50==0) {s_index<-(sim_num-sim_start)/50
                                r_index<-50} 
if((sim_num-sim_start)%%50!=0) {s_index<-ceiling((sim_num-sim_start)/50)
      r_index<-sim_num-sim_start-(s_index-1)*50}
#============================== Run the simulation study ==============================#

# Load packages:
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)

# I/O specifications:
datapath <- "/home/pdbaines/data"
outpath <- "output/"

# mini or full?
if (mini){
  rootfilename <- "blb_lin_reg_mini"
} else {
  rootfilename <- "blb_lin_reg_data"
}

# Filenames:
# Set up I/O stuff:
# Attach big.matrix :
# Remaining BLB specs:
# Extract the subset:
# Reset simulation seed:
# Bootstrap dataset:
# Fit lm:
# Output file:
# Save estimates to file:

# file specs:
infilename <- paste0(rootfilename,".txt")
backingfilename <- paste0(rootfilename,".bin")
descriptorfilename <- paste0(rootfilename,".desc")

# Set up I/O stuff:
infile <- paste(datapath,infilename,sep="/")
backingfile <- paste(datapath,backingfilename,sep="/")
descriptorfile <- paste(datapath,descriptorfilename,sep="/")

cat("Running read.big.matrix (i.e., creating file-backed matrix)...\n")

##
#
# Prepare filebacked big matrix file:
# 
# Note: 
# You only need to do this once, to create the .bin and .desc
# files. For the homework, these have already been 
# created for you, so you can just skip to the 
# attach.big.matrix portion below.
#
##
verbose<-TRUE
# Attach big.matrix :
if (verbose){
  cat("Attaching big.matrix...\n")
}
dat <- attach.big.matrix(dget(descriptorfile),backingpath=datapath)


n<-nrow(dat)
gamma<-0.7
b<-floor(n^gamma)

set.seed(s_index)
index<-sample(1:n,b,replace=TRUE)
subsample<-data.frame(dat[index,])

set.seed(sim_num)
index_Bp<-rmultinom(1,n,prob=rep(1/b,b))# this is a bx1 vector indicating the number of occurence of each of the b data points
betahat <- lm(X1001~.-1,data=subsample,weights=index_Bp)$coef #each column is a set of parameter est. and there are r columns.
#Store the est in a pxr matrix

outfile=paste0("output/","coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt")
write.table(betahat, file=outfile)