#
# R script
# PCA of spectral reflectance data from PolyPen (exemplary for GitHub)
#
# Created by Christopher Haase on 18th December 2022.
# E-mail: christopher.haase@ag.uni-giessen.de / christopher.haase93@gmail.com



###
# Set working directory:
###

setwd("~/Downloads/Master Nutzpflanzenwissenschaften/5. Semester (SS 2022)/Masterarbeit/Messungen/2022-03-29/PCA")


###
# Read data:
###

reflection_03_29 <- read.table("2022-03-29-transmittance-PCA.csv",
                               header=T,
                               sep=";",
                               dec=",",
                               #row.names = 1,
                               stringsAsFactors=T)


###
# for further workflow, assign current data file to "reflection":
###

reflection <- reflection_03_29


###
# Data preparation - Use all commands and have a look at the data.frame afterwards
# because the formatting is important, especially the usage of "columnnames"
# (done automatically in most cases) and "rownames" (has to be adapted individually)!
# Other commands, like sorting, are optional but help understanding the data.
###


###
# Remove "X" from wavelengths:
###

colnames(reflection) <- c(gsub("[X]", "", colnames(reflection)))


###
# Remove all rows of Leaf "2" (or "1") ...
###

updatedReflection1 <- reflection[reflection$Leaf != "1", ]


###
# ... or, when using both leaves: Simply assign reflection to updatedReflection1:
###

updatedReflection1 <- reflection


###
# Calculate means per plot (mean of 3 plants and 2 leafs (or 1 leaf) each):
###

updatedReflection1 <- aggregate(.~Plot, updatedReflection1, mean, na.rm = T, na.action = na.pass)
# https://stackoverflow.com/questions/16844613/aggregate-methods-treat-missing-values-na-differently


###
# Remove unnecessary columns (Index, Leaf and Date):
###

updatedReflection1 <- subset(updatedReflection1, select = -c(Index, Leaf, Date) )


###
# Give Replication, Fertiliser and Level columns a unique tag, then rename Level:
###

updatedReflection1$Replication <- paste("R", updatedReflection1$Replication, sep = "")
updatedReflection1$Fertiliser <- paste("F", updatedReflection1$Fertiliser, sep = "")
updatedReflection1$Level <- paste("L", updatedReflection1$Level, sep = "")

updatedReflection1$Level[updatedReflection1$Level == "L1"] <- "I"
updatedReflection1$Level[updatedReflection1$Level == "L2"] <- "II"
updatedReflection1$Level[updatedReflection1$Level == "L3"] <- "III"


###
# Rename "F1"-"F5" to "none", "PK", "NP", "NK" and "NPK"
###

updatedReflection1$Fertiliser[updatedReflection1$Fertiliser == "F1"] <- "none"
updatedReflection1$Fertiliser[updatedReflection1$Fertiliser == "F2"] <- "PK"
updatedReflection1$Fertiliser[updatedReflection1$Fertiliser == "F3"] <- "NP"
updatedReflection1$Fertiliser[updatedReflection1$Fertiliser == "F4"] <- "NK"
updatedReflection1$Fertiliser[updatedReflection1$Fertiliser == "F5"] <- "NPK"


###
# Make Replication, Fertiliser and Level factors again:
###

updatedReflection1$Replication <- factor(updatedReflection1$Replication)
updatedReflection1$Fertiliser <- factor(updatedReflection1$Fertiliser)
updatedReflection1$Level <- factor(updatedReflection1$Level)


###
# Order by Replication:
###

updatedReflection1 <- updatedReflection1[order(updatedReflection1$Replication),]


###
# Create new column as a unique combination of "Fertiliser" and "Level" and make
# it a factor with factor levels; then reorder the data frame:
###

updatedReflection1$F_L <- factor(paste(updatedReflection1$Fertiliser,
                                       updatedReflection1$Level, sep = "-"))
levels(updatedReflection1$F_L) # show factor levels of new factor "Fertiliser_Level"
updatedReflection1 <- updatedReflection1[,c(261, 1:260)] # move "Fertiliser_Level" to the first column


###
# Remove unnecessary columns:
###

updatedReflection1 <- subset(updatedReflection1, select = -c(Plot, Replication, Fertiliser))


###
# Move Level to the last column (used for grouping (habillage) later on))
# Habillage can be used to specify the factor variable for coloring the individuals by groups.
# https://f0nzie.github.io/machine_learning_compilation/detailed-study-of-principal-component-analysis.html

updatedReflection1 <- updatedReflection1[,c(1, 3:258, 2)]


###
# Calculate means for same Fertiliser and Level combinations and make Level a factor again:
###

updatedReflection1 <- aggregate(.~F_L, updatedReflection1, mean)
updatedReflection1$Level <- factor(updatedReflection1$Level)


###
# Make F_L rownames:
###

rownames(updatedReflection1) <- updatedReflection1[,1]
updatedReflection1[,1] <- NULL


### End of data preparation ###
### Now: creating graphs ###


###
# Load required packages first (or install them if needed):
###

#install.packages("ggplot2")
library(ggplot2)
#install.packages("factoextra")
library(factoextra)
#install.packages("FactoMineR")
library(FactoMineR)

# As a data.frame should be used for PCA(), make Level a factor with a label and roman numerals (again):
###

updatedReflection1$Level <- paste("Level", updatedReflection1$Level, sep = "-")

updatedReflection1$Level[updatedReflection1$Level == "Level-1"] <- "Fertilising level I"
updatedReflection1$Level[updatedReflection1$Level == "Level-2"] <- "Fertilising level II"
updatedReflection1$Level[updatedReflection1$Level == "Level-3"] <- "Fertilising level III"

updatedReflection1$Level <- factor(updatedReflection1$Level)


###
# Do actual PCA:
###

fmr.pca <- PCA(updatedReflection1,
               ncp = 15, # number of dimensions kept in the results (by default 5)
               scale.unit = T,
               #quanti.sup = NULL,
               quali.sup = 257, # column "Level" from data.frame
               graph = F) # set to "T" if graphs should be displayed


###
# Visualise eigenvalues (scree plot) -> shows the percentage of variances
# explained by each principal component
# https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/eigenvalue
###

fviz_eig(fmr.pca,
         geom = "bar",
         ncp = 15,
         addlabels = T,
         xlab = "Principal component", ylab = "Percent Variation" # my own axis labels similar to "old" scree plot labels
)


###
# Graph of individuals (the principal components (15 Fertiliser-Level combinations in my case)):
# (classic PCA plot with factoextra)
###

fmr.pca.var.percentage.PC1 <- round(fmr.pca$eig[1,2], 1) # (rounded to one decimal place)
fmr.pca.var.percentage.PC1
fmr.pca.var.percentage.PC2 <- round(fmr.pca$eig[2,2], 1) # (rounded to one decimal place)
fmr.pca.var.percentage.PC2

fviz_pca_ind(fmr.pca,
             col.ind = "cos2", # "Color for individuals", colored by the quality of representation of the variable on the PC = "cos2" = how well is a variable/individual represented in a PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # avoid text overlapping
             #select.ind = list(cos2 = 20), # be careful: also changes the position of individuals
             #habillage = 257 # can only be used, if col.ind and gradient.cols are NULL -> decide, what to use
) + labs(title ="PCA",
         x = paste("PC1 - ", fmr.pca.var.percentage.PC1, "%", sep=""),
         y = paste("PC2 - ", fmr.pca.var.percentage.PC2, "%", sep=""))

# https://www.rdocumentation.org/packages/ggplot2/versions/3.3.6/topics/labs
# http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining


###
# Optional, but used in my version, to reduce the number of wavelengths (variables) drawn in the plots:
###

# Getting loading scores and rank them -> using loading scores to determine which variables
# (here wavelengths) have the largest effect on where samples are plotted in the PCA plot
# (there are loading scores for each PC) + reducing the number of wavelengths (variables) drawn in the plots

# PCA() has not output with loading scores, so they have to be calculated manually,
# which is done here after this: https://groups.google.com/g/factominer-users/c/BRN8jRm-_EM

loadings <- sweep(fmr.pca$var$coord,
                  MARGIN = 2,
                  STATS = sqrt(fmr.pca$eig[1:14,1]),
                  FUN = "/")

loading_scores_fmr.pca_PC1 <- loadings[,1]

loading_scores_fmr.pca_PC2 <- loadings[,2]


###
# for synoptic Excel-table - magnitudes (!) ranked from high to low
###

wavelength_score_ranked_PC1_abs <- sort(abs(loading_scores_fmr.pca_PC1), decreasing = TRUE) # sort the magnitudes of the loading scores, from high to low
names(wavelength_score_ranked_PC1_abs[1:10])

# get original loadings of these most important variables/wavelengths
loading_scores_fmr.pca_PC1[names(wavelength_score_ranked_PC1_abs[1:10])]

wavelength_score_ranked_PC2_abs <- sort(abs(loading_scores_fmr.pca_PC2), decreasing = TRUE) # sort the magnitudes of the loading scores, from high to low
names(wavelength_score_ranked_PC2_abs[1:10])

# get original loadings of these most important variables/wavelengths
loading_scores_fmr.pca_PC2[names(wavelength_score_ranked_PC2_abs[1:10])]


###
# Preparation for further loading extractions and biplot
###

wavelength_score_ranked_lowToHighPC1 <- sort(loading_scores_fmr.pca_PC1, decreasing=FALSE)

wavelength_score_ranked_highToLowPC1 <- sort(loading_scores_fmr.pca_PC1, decreasing=TRUE)

wavelength_score_ranked_lowToHighPC2 <- sort(loading_scores_fmr.pca_PC2, decreasing=FALSE)

wavelength_score_ranked_highToLowPC2 <- sort(loading_scores_fmr.pca_PC2, decreasing=TRUE)


###
# Further loading extractions
###

# get original loadings of these most important variables/wavelengths
loading_scores_fmr.pca_PC1[names(wavelength_score_ranked_lowToHighPC1[1:10])]

# get original loadings of these most important variables/wavelengths
loading_scores_fmr.pca_PC1[names(wavelength_score_ranked_highToLowPC1[1:10])]

# get original loadings of these most important variables/wavelengths
loading_scores_fmr.pca_PC2[names(wavelength_score_ranked_lowToHighPC2[1:10])]

# get original loadings of these most important variables/wavelengths
loading_scores_fmr.pca_PC2[names(wavelength_score_ranked_highToLowPC2[1:10])]


###
# For biplot - not using magnitudes, but extracting "most negative" and highest positive values for both PC1 and PC2
###

top_2_wavelengthsPC1_lowToHigh <- names(wavelength_score_ranked_lowToHighPC1[1:2])

top_2_wavelengthsPC1_highToLow <- names(wavelength_score_ranked_highToLowPC1[1:2])

top_2_wavelengthsPC2_lowToHigh <- names(wavelength_score_ranked_lowToHighPC2[1:2])

top_2_wavelengthsPC2_highToLow <- names(wavelength_score_ranked_highToLowPC2[1:2])


###
# Graph of variables (wavelengths in this case):
###

# Optional: increasing max.overlaps of ggrepel, to show all labels:
options(ggrepel.max.overlaps = Inf)

fviz_pca_var(fmr.pca,
             repel = TRUE,
             col.var = "contrib", # or "cos2"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             select.var = list(name = c(top_2_wavelengthsPC1_lowToHigh, top_2_wavelengthsPC1_highToLow, top_2_wavelengthsPC2_lowToHigh, top_2_wavelengthsPC2_highToLow)), # only show the top 8 wavelengths for PC1 and PC2 from fmr.pca
             #select.var = list(cos2 = 10), # only the top 10 active variables with the highest cos2 are plotted
             alpha.var = "cos2", # changes transparency by cos2 values
)


###
# Background information:
###

# understanding of cos2 and contrib: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# contrib seems more reasonable to me right now
# cos2 and contrib can both not reduce the number of variables drawn (knowledge right now)
# number of variables drawn can be reduced by selecting only the 15 most important variables for both PC1 and PC1 and
# can also be reduced by using select.var


###
# The graph to use in the final work (Biplot with ellipses for grouping variable):
###

# if not calculated before:

fmr.pca.var.percentage.PC1 <- round(fmr.pca$eig[1,2], 1) # rounded to one decimal place, same as prcomp()
fmr.pca.var.percentage.PC1
fmr.pca.var.percentage.PC2 <- round(fmr.pca$eig[2,2], 1) # rounded to one decimal place, same as prcomp()
fmr.pca.var.percentage.PC2

# then, create biplot:

fviz_pca_biplot(fmr.pca,
                geom.ind = c("point", "text"), # or just one of them
                geom.var = c("arrow", "text"), # or just one of them (decide, if arrows should be labeled or not - at the moment decided not to because they overlap strongly)
                fill.ind = updatedReflection1$Level, # Level has to be a factor
                col.ind = "black",
                pointshape = 21, pointsize = 2, labelsize = 3,
                palette = "jco",
                #alpha.var = "contrib",
                col.var = "contrib",
                gradient.cols = "RdYlBu",
                legend.title = list(fill = "Level", color = "Contrib", alpha = "Contrib"),
                #label = "var", # A text specifying the elements to be labelled. Default value is "all". 
                #habillage = 257, # column 257 in updatedReflection1
                #select.var = list(contrib = 20),
                select.var = list(name = c(top_2_wavelengthsPC1_lowToHigh, top_2_wavelengthsPC1_highToLow, top_2_wavelengthsPC2_lowToHigh, top_2_wavelengthsPC2_highToLow)),
                addEllipses = TRUE
                #,ellipse.level = 0.66
                ,ellipse.type = "confidence" #"t" #"euclid" #"norm" #"convex"
                ,title = "" #"PCA-Biplot of means of the 15 fertiliser-level-combinations (both leaves) (29 March 2022)" # adjust leaf and date
                ,font.main = 12
                ,xlab = paste("PC1 (", fmr.pca.var.percentage.PC1, " %)", sep="")
                ,ylab = paste("PC2 (", fmr.pca.var.percentage.PC2, " %)", sep="")
                ,repel = TRUE
)
# graphical adjustments can be done according to: https://rdrr.io/cran/ggpubr/man/ggpar.html


###
# The current biplot can be saved as a graphics-file. It will be saved to the current working directory.
###

ggsave(
  "MS-Word-2022-03-29-PCA-Biplot-bothLeaves.png", # adjust leaf and date
  plot = last_plot(),
  #device = png,
  path = NULL,
  scale = 0.8, # 1 is default, below 1 scales up, above 1 scales down
  width = 2480,
  height = 2480,
  units = "px",
  dpi = 300
  ,bg = "white"
)
# https://ggplot2.tidyverse.org/reference/ggsave.html