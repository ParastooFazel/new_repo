library(oligo)
library(ff)
source("./AUC_TNO/MedianImpute.R")
load(file="./Converted_CELfiles/fRMA_data_oligo_core.RData")

# Some samples were discarded.
# I need to organize the samples and impute the discarded ones!
# For this, I need to know how the samples are organised
#
# Week5 Day2:  OGTT T0, T30, T60, T90, T120
# Week18 Day2: OGTT T0, T30, T60, T90, T120
# Week5 Day3:  MMT T0, T60, T120, T240, T360, T480
# Week18 Day3: MMT T0, T60, T120, T240, T360, T480

# The patient identifier is utilized in the samplename, so we need to get all patient IDs to check wheter all timepoints are there for all patients!
doSplit <- function(sampleName)
{
  x.split <- strsplit(sampleName, "-", fixed=TRUE)[[1]][1]
  return(strsplit(x.split, "_", fixed=TRUE)[[1]][2])
}
PatientIDs <- lapply(sampleNames(eset), doSplit)
PatientIDs <- unique(unlist(PatientIDs))

# Get all OGTT and MMT samples
OGTT_Samples <- grep("D2", sampleNames(eset))
OGTT_Patient_IDs <- lapply(sampleNames(eset)[OGTT_Samples], doSplit)
OGTT_Patient_IDs <- unique(unlist(OGTT_Patient_IDs))

# Different time points OGTT Week 5
OGTT.W5.D2.T0 <- sprintf("%s-W5D2T0", OGTT_Patient_IDs)
OGTT.W5.D2.T0.Hits <- lapply(OGTT.W5.D2.T0, grep, sampleNames(eset)[OGTT_Samples])
OGTT.W5.D2.T0.lenght <- lapply(OGTT.W5.D2.T0.Hits, length)
OGTT.W5.D2.T0.lenght <- unlist(OGTT.W5.D2.T0.lenght)
which(OGTT.W5.D2.T0.lenght == 0) # 42: one missing sample!

OGTT.W5.D2.T0.Samples.index <- !logical(length=length(OGTT.W5.D2.T0.lenght))
OGTT.W5.D2.T0.Samples.index[which(OGTT.W5.D2.T0.lenght == 0)] <- FALSE

OGTT.W5.D2.T0.Samples <- matrix(NA, ncol=1, nrow=length(OGTT.W5.D2.T0.lenght))
OGTT.W5.D2.T0.Samples[OGTT.W5.D2.T0.Samples.index] <- sampleNames(eset)[OGTT_Samples][unlist(OGTT.W5.D2.T0.Hits)]

x.OGTT.W5.D2.T0 <- matrix(NA, nrow=length(OGTT.W5.D2.T0.Samples), ncol=dim(exprs(eset))[1])
x.OGTT.W5.D2.T0[OGTT.W5.D2.T0.Samples.index,] <- t(exprs(eset)[,OGTT.W5.D2.T0.Samples[!is.na(OGTT.W5.D2.T0.Samples)]])
colnames(x.OGTT.W5.D2.T0) <- featureNames(eset)
x.OGTT.W5.D2.T0 <- cbind(matrix(0, ncol=1, nrow=length(OGTT.W5.D2.T0.lenght)), x.OGTT.W5.D2.T0)
colnames(x.OGTT.W5.D2.T0)[1] <- "t"
rownames(x.OGTT.W5.D2.T0) <- sprintf("%s-W5D2T0", OGTT_Patient_IDs)

# Now repeat this for T30...
OGTT.W5.D2.T30 <- sprintf("%s-W5D2T30", OGTT_Patient_IDs)
OGTT.W5.D2.T30.Hits <- lapply(OGTT.W5.D2.T30, grep, sampleNames(eset)[OGTT_Samples])
OGTT.W5.D2.T30.lenght <- lapply(OGTT.W5.D2.T30.Hits, length)
OGTT.W5.D2.T30.lenght <- unlist(OGTT.W5.D2.T30.lenght)
which(OGTT.W5.D2.T30.lenght == 0)

OGTT.W5.D2.T30.Samples.index <- !logical(length=length(OGTT.W5.D2.T30.lenght))
OGTT.W5.D2.T30.Samples.index[which(OGTT.W5.D2.T30.lenght == 0)] <- FALSE

OGTT.W5.D2.T30.Samples <- matrix(NA, ncol=1, nrow=length(OGTT.W5.D2.T30.lenght))
OGTT.W5.D2.T30.Samples[OGTT.W5.D2.T30.Samples.index] <- sampleNames(eset)[OGTT_Samples][unlist(OGTT.W5.D2.T30.Hits)]

x.OGTT.W5.D2.T30 <- matrix(NA, nrow=length(OGTT.W5.D2.T30.Samples), ncol=dim(exprs(eset))[1])
x.OGTT.W5.D2.T30[OGTT.W5.D2.T30.Samples.index,] <- t(exprs(eset)[,OGTT.W5.D2.T30.Samples[!is.na(OGTT.W5.D2.T30.Samples)]])
colnames(x.OGTT.W5.D2.T30) <- featureNames(eset)
x.OGTT.W5.D2.T30 <- cbind(matrix(30, ncol=1, nrow=length(OGTT.W5.D2.T30.lenght)), x.OGTT.W5.D2.T30)
colnames(x.OGTT.W5.D2.T30)[1] <- "t"
rownames(x.OGTT.W5.D2.T30) <- sprintf("%s-W5D2T30", OGTT_Patient_IDs)

