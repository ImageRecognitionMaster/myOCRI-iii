## ==================================================================================== ##
# SOCRISP ShinyApp is design for clinician to run Oral Cancer Risk Index on DNA-image 
# cytometry measurement obtained from exfoliative cells
# 
# Copyright (C) 2017  Yicheng Li
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# You may contact the author of this code, Yicheng Li, at <yli100@uncc.edu>
## ==================================================================================== ##

library(shiny) #devtools::install_github("rstudio/shiny"); devtools::install_github("rstudio/shinyapps")
library(reshape2)
library(ggplot2)
library(ggthemes)
library(gplots)
library(ggvis)
library(dplyr)
library(tidyr)
library(DT) #devtools::install_github('ramnathv/htmlwidgets'); devtools::install_github('rstudio/DT')
library(RColorBrewer)
library(pheatmap)
library(shinyBS)
library(plotly)
library(markdown)
library(NMF)
library(scales)
library(heatmaply)
library(readr)

##================================================================================##


#troubleshooting
if(FALSE) {
  seqdata <- read.csv("../data/_00872872.csv",stringsAsFactors = FALSE)
#  load('data/mousecounts_example_analysis_results.RData')
#  load('data/mousecounts_example_analyzed.RData') #example_data_results
 
}
