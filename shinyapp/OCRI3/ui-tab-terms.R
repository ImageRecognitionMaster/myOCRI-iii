
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

tabPanel("Terms & Conditions",
         fluidRow(
           column(4,wellPanel(
             h4("Shinyapps.io Terms & Conditions")
           )
           ),#column
           column(8,
                  includeMarkdown("instructions/terms.md"))
         ))