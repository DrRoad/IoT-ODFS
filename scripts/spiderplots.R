#### Load/install required packages -----------
installed <- function(pkg) {
  is.element(pkg, installed.packages()[,1])
}

if (!installed("gtools")) { install.packages("gtools") }
if (!installed("ggplot2")) { install.packages("ggplot2") }
if (!installed("RColorBrewer")) { install.packages("RColorBrewer") }
if (!installed("fmsb")) { install.packages("fmsb") }

library(gtools)
library(ggplot2)
library(RColorBrewer)
library(fmsb)

#### Functions --------------------------------

radar <- function(df, query) {
  df <- subset(df,
               select = c(Q, Throughput, ResultSize, NormTriples, UM, KGSize.mb.))
  colnames(df) <- c("Configuration", "Throughput", "Result Size", "Norm Triples", "Used Memory", "KG Size")
  df <- data.frame(df[,-1],
                   row.names = df[,1])
  
  # Scale all data to 0-100
  df <- data.frame(lapply(df, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE) / 100)))
  
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  radardata <- rbind(rep(100,5), rep(0,5), df)
  
  colors_border = c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.7,0.5,0.1,0.9))
  colors_in = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4))
  radarchart(radardata,
             title = query,
             axistype = 4,
             calcex = 0.9,
             # custom polygon
             pcol = colors_border, pfcol = colors_in, plwd = 2, plty = 1,
             # custom the grid
             cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.8,
             # custom labels
             vlcex = 0.9, palcex = 0.9)
  legend(x = 0.7, y = 1.4,
         legend = c("full", "onDS", "onDSF"),
         bty = "n", pch = 20, col = colors_in,
         cex = 0.9, pt.cex = 2)
}

allradar <- function(folder) {
  files <- list.files(path = folder, pattern = "*.tsv", full.names = T, recursive = FALSE)
  # sort file names
  files <- mixedsort(sort(files))
  
  # create empty list
  lst <- vector("list", length(files))
  # read files in to list
  for(i in 1:length(files)) {
    lst[[i]] <- read.csv(files[i], header = TRUE, sep = "\t", as.is = TRUE)
  }

  new <- lapply(lst, function(x) {
    x$Throughput <- x$ResultSize / x$QETime.ms. * 1000
    x$NormTriples <- x$KGTriples / max(x$KGTriples, na.rm = TRUE)
    return(x)
  })

  for (i in 1:length(new)) {
    print(new[[i]])
    radar(new[[i]], paste0("Q", toString(i)))
  }
}

#### Main code --------------------------------

par(mar = c(0, 0, 1.0, 0)) # decrease default margin
layout(matrix(1:10, nrow = 2, byrow = T)) # draw 10 plots
allradar("20minutes")
dev.print(pdf, file = "20minutes.pdf", width = 9, height = 3)

par(mar = c(0, 0, 1.0, 0)) # decrease default margin
layout(matrix(1:10, nrow = 2, byrow = T)) # draw 10 plots
allradar("1hour")
dev.print(pdf, file = "1hour.pdf", width = 9, height = 3)

dev.off()