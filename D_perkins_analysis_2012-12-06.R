# script from DPerkins 2023-12-06

rm(list = ls()) # clear history


########################################################
## Packages and libraries                              #
########################################################


# Function to Install and Load R Packages
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages);
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}

# Specify the list of required packages to be installed and load    
Required_Packages=c("readxl", "remotes");

# Call the Function
Install_And_Load(Required_Packages);

# install latest sizeSpectra package from github  
remotes::install_github("andrew-edwards/sizeSpectra")
library(sizeSpectra)


########################################################
# SET PATH TO THE DATA ON YOUR DEVICE     
########################################################

aFile<-file.choose() # SET YOUR DATA PATH 
data.path = dirname(aFile)
setwd(data.path)   

#################################################
# ADD LIST OF ALL FILE NAMES TO BE ANALYISID HERE
#################################################

#all.file.names <- list.files(path = data.path, pattern = "*.xlsx",recursive = FALSE)
all.file.names <- "UK_data.xlsx" # USING THIS AS A TEST DATASET

################################################################################################
# Run loop that calls each data file, creates subsets for each site, and fits and plots MLEbin
################################################################################################


for (fff in 1:length(all.file.names)){
  print(paste("file running ", fff, " of ", length(all.file.names)))
  current.file <- all.file.names[fff]
  df  <- read_excel(paste(data.path, current.file ,sep="/")) # individual file 
  
  uniq <- unique(unlist(df$site)) # list of unique sites
  #num.reps <- length(uniq) # number of unique sites
  
  # dummy summary file with output
  s.out <- data.frame(site=unique(df$site), min.x = NA, max.x = NA, n = NA, b = NA, MLE.b =NA, MLE.b.l.ci = NA, MLE.b.u.ci =NA)
  
  for (iii in 1:length(uniq)){
    print(paste("site running ", iii, " of ", length(uniq)))
    site.df <- subset(df, site == uniq[iii])
    
    invert.df <- subset(site.df, organism_group != "Fish") # ONLY USEING INVERT DATA FOR THE MOMENT - NEED TO UPDATE TO BE ABLE TO SELECT IMVERTS, FISH, INVERTS + FISH
    
    
    sizes <- invert.df$body_mass
    s.out$min.x[iii] <- min(sizes)
    s.out$max.x[iii] <- max(sizes)
    s.out$n[iii] <- length(sizes)
    s.out$biomass[iii] <- sum(sizes) # RELATIVE BIOMASS, NEED TO UPDATE TO ACCOUNT FOR SAMPLE AREA
    
    
    # Binn the data for plotting purposes
    x.binned <- binData(x = sizes,
                        binWidth = "2k")  # 1, 2, 5 or "2k"
    
    
    #head(x.binned$indiv)     # Individual x values and which bin they are assigned to
    #x.binned$binVals         # Bins and the counts in each bin (with extra columns  that we won't need here).
    
    #Now fit the PLB distribution using the MLEbin method.
    #The fitting function takes the binned data as two vectors (the bin breaks and the counts in each bin):
    
    num.bins <- nrow(x.binned$binVals)
    
    # bin breaks are the minima plus the max of the final bin:
    binBreaks <- c(dplyr::pull(x.binned$binVals, binMin),
                   dplyr::pull(x.binned$binVals, binMax)[num.bins])
    
    binCounts <- dplyr::pull(x.binned$binVals, binCount)
    
    
    MLEbin.res <-  calcLike(negLL.fn = negLL.PLB.binned,
                            p = -1.5,
                            w = binBreaks,
                            d = binCounts,
                            J = length(binCounts),   # = num.bins
                            vecDiff = 1)             # increase this if hit a bound
    
    s.out$MLE.b[iii] <- MLEbin.res$MLE
    s.out$MLE.b.l.ci[iii] <-MLEbin.res$conf[1]
    s.out$MLE.b.u.ci[iii] <- MLEbin.res$conf[2]
    
    
    #The `NA/Inf replaced by maximum positive value` warnings are fine - the
    #likelihood function is blowing up in a very unlikely region of parameter
    #space.
    
    
    
    fig.path <- file.path("/Users/dansnewmac/Desktop/Figures/")
    
    pdf(paste(fig.path, uniq[iii], '.pdf', sep = ''), width = 5, height = 5)
    
    # wrap figure function with try catch as sometimes does not work
    fitMLE <- tryCatch(
      {
        
        LBN_bin_plot(binValsTibble = x.binned$binVals,
                     b.MLE = MLEbin.res$MLE,
                     b.confMin = MLEbin.res$conf[1],
                     b.confMax = MLEbin.res$conf[2],
                     #leg.text = "(c)",
                     xLab = expression(paste("Body mass ", italic(x), "(mg)")),
                     log.xy = "xy",
                     plot.binned.fitted = TRUE)
        
        title(uniq[iii])
        
        
      },
      error=function(cond)
      {
        message("MLEbin figure did not work")
      })
    
    dev.off()
    
    
  }
  
  write.csv(s.out,  paste( current.file),  row.names = FALSE)
  
}



