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
## # This tab is used to input the count or normalized data files

tabPanel("Input Data", 
         fluidRow(column(4,wellPanel(
         #  downloadLink("instructionspdf",label="Download Instructions (pdf)"),
           radioButtons('data_file_type','Use example file or upload your own data',
                        c('Upload Data'="upload",
                    #      'START RData file'="previousrdata",
                          'Example Data'="examplecounts"
                        ),selected = "examplecounts"),
        #   conditionalPanel(condition="input.data_file_type=='previousrdata'",
        #                    fileInput('rdatafile','Upload START Generated RData File'),
        #                    conditionalPanel("output.fileUploaded",h4(strong("Check data contents then click:")))
        #   ),
           conditionalPanel(condition="input.data_file_type=='upload'",
                            
                            tags$hr(),
                            h5(helpText("Select the read.table parameters below")),
                            checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                            checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                            br(),
                            radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=' '), selected = ','),
                            br(),
                            fileInput('datafile', 'Choose File Containing Data (.CSV)',
                                      accept=c('text/csv', 
                                               'text/comma-separated-values,text/plain', 
                                               '.csv'))

         ),#column
        
         column(8,
                bsCollapse(id="input_collapse_panel",open="data_panel",multiple = FALSE,
                           bsCollapsePanel(title="Data Contents: Check Before `Submit`",value="data_panel",
                                           dataTableOutput('countdataDT')                       
                           ),
                           bsCollapsePanel(title="Analysis Results: Ready to View Other Tabs",value="analysis_panel",
                                           downloadButton('downloadResults_CSV','Save Results as CSV File'),
                                           downloadButton('downloadResults_RData',
                                                          'Save Results as START RData File for Future Upload',
                                                          class="mybuttonclass"),
                                           dataTableOutput('analysisoutput'),
                                           tags$head(tags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"))
                           )
                )#bscollapse
         )#column
         )#fluidrow
         )
         )
)#tabpanel
