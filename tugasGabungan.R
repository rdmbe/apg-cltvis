library(tcltk)
library(tkrplot)
plothist <- function(samp.size,samp.num) {
  mean <- 0
  std <- 1
  i.it <- samp.num/samp.size
  data1 <- rnorm(n=samp.num,mean,std)
  data2 <- list()
  for(i in 1:i.it) {
    timing <- list()
    timing[["start"]]<- Sys.time()
    data2.samp <- sample(x = data1,size = samp.size,replace = FALSE)
    d2s.mean <- mean(data2.samp)
    data2[i]<-d2s.mean
    timing[["stop"]]<- Sys.time()
  }
  hist(data1 , main=paste("Histogram, mean = ", mean,", std = ", std, ", n = ",samp.size ,
                          sep="" ) ,col="blue" , xlab="", nclass=30)
  time <- timing[["stop"]]-timing[["start"]]
  time
}

CLTdemo <-function() {
  pos <<-2
  meaninput <<- 0
  stdinput <<- 1
  winPlot <- tktoplevel()
  tkwm.title(winPlot ,"Central Limit Theorem" )
  setFrame <-tkframe(winPlot ,borderwidth=2)
  sliderFramePlot1 <-tkframe(winPlot ,borderwidth=2,relief="groove")
  labelText <- tclVar(paste("Sample sizes"))
  SliderValue <- tclVar("2")
  SliderValueLabel <- tklabel(sliderFramePlot1 ,text=labelText )
  tkconfigure(SliderValueLabel,textvariable=SliderValue)
  slider <- tkscale(sliderFramePlot1 , from=2, to=1000, showvalue=T,
                    variable=SliderValue, resolution=1, orient="horizontal",length=400)
  plotFrame <- tkframe(winPlot ,relief="groove",borderwidth=2)
  img <- tkrplot(plotFrame,fun=plothist ,1.5,1.5)
  tkgrid(img)
  draw <- function(...){
    pos <<- as.numeric(tclvalue(SliderValue))
    img <- tkrreplot(img,fun=plothist , 1.5,1.5)
  }
  meanFrame <- tkframe(setFrame )
  meanVal <- tclVar(" 0 ")
  meanField <- ttkentry(meanFrame , width = "5",textvariable = meanVal )
  stdFrame <- tkframe(setFrame )
  stdVal <- tclVar(" 1 ")
  stdField <- ttkentry(stdFrame , width = "5",textvariable = stdVal )
  onOK <- function() {
    meaninput <<- as.numeric (tclvalue(meanVal ))
    stdinput <<- as.numeric (tclvalue(stdVal ))
    img <- tkrreplot(img,fun=plothist ,1.5,1.5)
  }
  OKbutton <- tkbutton(setFrame , text = "Set", foreground = "darkgreen", width = "12",
                       command = onOK, default = "active", borderwidth = 3)
  tkgrid(img)
  tkgrid(tklabel(meanFrame , text="Mean:"), meanField , sticky="w")
  tkgrid(tklabel(stdFrame , text="Standard Deviation:"), stdField , sticky="w")
  tkgrid(meanFrame , stdFrame , OKbutton, sticky="w")
  tkconfigure (slider, command = draw )
  slidertxt <- tklabel(sliderFramePlot1 ,text="Sample size:" )
  tkgrid(slidertxt )
  tkgrid.configure(slidertxt ,sticky="w")
  tkgrid(slider)
  tkgrid(plotFrame)
  tkgrid(setFrame)
  tkgrid(sliderFramePlot1)
}
CLTdemo ()