tabPanel("Demo for generating reports",
         fluidRow(
           column(4,wellPanel(
           
           radioButtons('data_file_type','Select which item',
                        c('Rock selected'="Rock",
                          'Pressure selected'="Pressure",
                          'Car selected'="Cars"
                        ),selected = "Rock"),
           conditionalPanel("output.DownloadData",
                            actionButton("upload_data","Download Data",
                                         style="color: #fff; background-color: #CD0000; border-color: #9E0000")
           ),
           conditionalPanel("output.DownloadPlot",
                            actionButton("upload_data","Download Plot",
                                         style="color: #fff; background-color: #CD0000; border-color: #9E0000")
           ),
           conditionalPanel("output.DownloadPDF",
                            actionButton("upload_data","Download Report",
                                         style="color: #fff; background-color: #CD0000; border-color: #9E0000")
           )
           )
         ),
         
         column(8,
   
                            tabPanel(
                              title="Volcano Plot",
                                     #h5(textOutput("corPR")),
                                     #uiOutput("volcanoplot_2groups_ggvisUI"),
                                     #ggvisOutput("volcanoplot_2groups_ggvis")  
                                     plotlyOutput("volcanoplot",height=600)
                            )
   
         )#column
         )#fluidrow
)

