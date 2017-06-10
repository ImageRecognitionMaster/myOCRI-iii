
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
## 
## 
# ui.R
source("helperFuncs/helpers.R")

customHeaderPanel <- function(title,windowTitle=title){
  tagList(
    tags$head(
      tags$title(windowTitle),
      tags$link(rel="stylesheet", type="text/css",
                href="app.css"),
      tags$h1(a(href="https://sites.google.com/site/thechenlabatnccu/"))
    )
  )
}


tagList(
  tags$head(
    tags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
    tags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}")
  ),
  navbarPage(
    
    theme = "bootstrap.min.united.updated.css",
    #United theme from http://bootswatch.com/
    #customHeaderPanel(title="SOCRISP: Shiny Oral Cancer Risk Prediction"),#img(src="KCardio_CMYK_4C_pos_small.jpg",height=50,width= 92,align="right")	,
    title = "SOCRISP: Shiny Oral Cancer Risk Prediction",
    source("ui-intro-tab.R",local=TRUE)$value,
    source("ui-inputData-tab.R",local=TRUE)$value,
    source("ui-pdf-demo.R",local=TRUE)$value,
    source("ui-tab-terms.R",local=TRUE)$value,
    
    #end definitions of tabs, now footer
    ## ==================================================================================== ##
    ## FOOTER
    ## ==================================================================================== ##              
    footer=p(hr(),p("ShinyApp created by ", strong("{Yicheng Li + Xiaoxin Chen}")," of ",align="center",width=4),
             p(("Department of Mathematics and Statistics, University of North Carolina at Charlotte"),align="center",width=4),
             p(("Cancer Research Program, North Carolina Central University"),align="center",width=4),
             p(("Copyright (C) 2017, code licensed under GPLv3"),align="center",width=4),
             #    p(a("Liu, Y et al (2015) `Quantitative risk stratification of oral leukoplakia with exfoliative cytology` PloSOne.     doi: 10.1371/journal.pone.0126760.",href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0126760"),align="center",width=4),
             p(a("Liu, Y et al (2017) `Quantitative prediction of oral cancer risk in patients with oral leukoplakia` Oncotarget.  doi: 10.18632/oncotarget.17550.",href="http://www.impactjournals.com/oncotarget/index.php?journal=oncotarget&page=article&op=view&path%5B%5D=17550&path%5B%5D=56152"),align="center",width=4)
             
    ),
    
    ## ==================================================================================== ##
    ## end
    ## ==================================================================================== ## 
    tags$head(includeScript("google-analytics.js"))
  ) #end navbarpage
) #end taglist

