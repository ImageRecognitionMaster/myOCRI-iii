#file1 <- input$file
#params <- list(choice = input$file, rda = readRDS("data/data.Rda"),df = data())

library(knitr)
library(rmarkdown)

rda1 <-readRDS("C:/Users/Yicheng/Desktop/chenresearch/exfoliative cell data 20161230/MotionDetection/rforest/data/data.Rda")
rda2 <-readRDS("C:/Users/Yicheng/Desktop/chenresearch/exfoliative cell data 20161230/MotionDetection/rforest/suprema.Rda")
dt <- read.csv("C:/Users/Yicheng/Desktop/fuyue data/CSV/OSCC/00872870.csv")

#params <- list( rda = rda1,df = dt)



render("C:/Users/Yicheng/Desktop/chenresearch/exfoliative cell data 20161230/MotionDetection/rforest/testFuncs/report-test1.Rmd", 
       output_file = "some.pdf",
                  params = list( rda = rda1, rdaz = rda2, df = dt))
