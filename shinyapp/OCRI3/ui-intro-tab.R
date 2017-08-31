tabPanel("Getting Started",
         fluidRow(
           column(4,wellPanel(
             h4("Getting Started with SOCRISP"),
             a("OCRI-I -- EdTAR", href="#introductions"),br(),
             a("OCRI/EdTAR Features", href = "#features"), br(),
             a("OCRI-II Features", href = "#OCRI-II"), br(),
             a("More on OCRI-II", href = "#OCRI-II-more"), br()
           )
           ),#column
           column(8,
                  includeMarkdown("instructions/myIntro.md"),
                  #)
           footer=p(hr(),p("ShinyApp created by ", strong("{Yicheng Li + Xiaoxin Chen}")," of ",align="center",width=4),
                    p(("Department of Mathematics and Statistics, University of North Carolina at Charlotte"),align="center",width=4),
                    p(("Cancer Research Program, North Carolina Central University"),align="center",width=4),
                    p(("Copyright (C) 2017, code licensed under GPLv3"),align="center",width=4),
                #    p(a("Liu, Y et al (2015) `Quantitative risk stratification of oral leukoplakia with exfoliative cytology` PloSOne.     doi: 10.1371/journal.pone.0126760.",href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0126760"),align="center",width=4),
                    p(a("Liu, Y et al (2017) `Quantitative prediction of oral cancer risk in patients with oral leukoplakia` Oncotarget.  doi: 10.18632/oncotarget.17550.",href="http://www.impactjournals.com/oncotarget/index.php?journal=oncotarget&page=article&op=view&path%5B%5D=17550&path%5B%5D=56152"),align="center",width=4)
                    
           ))
         ))

