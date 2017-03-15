Do_Clustering <- function(x.norm)
{
  if (class(x.norm) == "data.frame")
  {
    require(affy)
    x.norm <- new("ExpressionSet", exprs = as.matrix(x.norm))
  }

  if (class(x.norm)[1] != "ExpressionSet")
    stop("Error! Input must be an ExpressionSet!\nExecution is halted!\n")

  # Load the required library
  require(stats)

  # Calculate the Pearson correlation distance
  x.dist <- as.dist(1 - cor(exprs(x.norm), use = "all.obs", method="pearson"))

  # Apply average linkage clustering
  x.hc <- hclust(x.dist, method="ward")

  # Create the dendrogram
  Current_Date <- as.character(Sys.Date())
  Current_Date <- gsub("-", "_", Current_Date)
  if (length(sampleNames(x.norm)) < 50)
  {
    QC.cex = 1.0
  } else {
    QC.cex = .7/(length(sampleNames(x.norm))/ 50)
  }

  cat("Making the screen image of the plot...\n");
  plot(x.hc, main=sprintf("Dendrogram using %s clustering on the %s distance measure. Date: %s.", "Ward", "Pearson", date()), ylab="Distance measure", xlab="Samples", cex=QC.cex)

  cat("Writing the pdf version of the dendrogram...\n"); 
  PDF_Report_File <- sprintf("./Dendrogram_%s_%s_%s.pdf", "Pearson", "Ward", Current_Date)
  if (is.na(file.info(PDF_Report_File)$size) == FALSE)
  {
    ANSWER <- readline("Warning! File exists! Overwrite (y/n)? ")
    if (substr(ANSWER, 1, 1) == "y")
    {
      pdf(file=PDF_Report_File, title=sprintf("Dendrogram (%s)", date()), paper="special", width=11.68, height=8.26)
      plot(x.hc, main=sprintf("Dendrogram using %s clustering on the %s distance measure. Date: %s.", "Ward", "Pearson", date()), ylab="Distance measure", xlab="Samples", cex=QC.cex)
      dev.off()
    } else {
      cat("Skipped saving pdf-file!\n")
    }
  } else {
    pdf(file=PDF_Report_File, title=sprintf("Dendrogram (%s)", date()), paper="a4r")
    plot(x.hc, main=sprintf("Dendrogram using %s clustering on the %s distance measure. Date: %s.", "Ward", "Pearson", date()), ylab="Distance measure", xlab="Samples", cex=QC.cex)
    dev.off()
  }
}

# x <- read.delim(file="/tmp/hookdata_filtered_Individual_Experiments.txt", row.names=1)
# Do_Clustering(x)


