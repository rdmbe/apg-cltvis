library(tcltk2)
library (tkrplot)
plothist <- function (){
  mean <- meaninput
  std <- stdinput
  data2 <- list()
  for(i in 1:pos2){
  data1 <- rnorm (n=pos,mean,std)
  d1.mean <- mean(data1)
  data2[i] <- d1.mean
  }
  data2<-as.numeric(data2)
    hist (data2, main=paste("Histogram, mean=",mean,", std= ", std , ",n =", pos, sep =" "), col="blue", xlab="", nclass=30 )
  
}

CLTdemo <-function (){
  pos <<- 10
  pos2 <<- 1
  meaninput <<- 0
  stdinput <<- 1
  winPlot <- tktoplevel()
  tkwm.title(winPlot, "Central Limit Theorem")
  setFrame <- tkframe(winPlot, borderwidth=2)
  
  sliderFramePlot1 <- tkframe(winPlot, borderwidth=2)
  labelText <- tclVar(paste("sample sizes"))
  SliderValue <- tclVar("10")
  SliderValueLabel <- tklabel(sliderFramePlot1, text=labelText)
  tkconfigure (SliderValueLabel, textvariable=SliderValue)
  slider <- tkscale(sliderFramePlot1, from=10, to=100, showvalue=T,variable=SliderValue,resolution=1, orient="horizontal",length=400)
  
  sliderFramePlot2 <- tkframe(winPlot, borderwidth=2)
  labelText2 <- tclVar(paste("number of samples"))
  SliderValue2 <- tclVar("1")
  SliderValueLabel2 <- tklabel(sliderFramePlot2, text=labelText2)
  tkconfigure (SliderValueLabel2, textvariable=SliderValue2)
  slider2 <- tkscale(sliderFramePlot2, from=1, to=1000, showvalue=T,variable=SliderValue2,resolution=1, orient="horizontal",length=400)
  
  plotFrame <- tkframe(winPlot ,relief="groove",borderwidth=2)
  plotFrame2 <- tkframe(winPlot ,relief="groove",borderwidth=2)
  img <- tkrplot(plotFrame,fun=plothist ,1.5,1.5)
  tkgrid(img)
  
  
  draw <- function(...){
    pos <<- as.numeric(tclvalue(SliderValue))
    img <- tkrreplot(img,fun=plothist , 1.5,1.5)
  }
  
  draw2 <- function(...){
    pos2 <<- as.numeric(tclvalue(SliderValue2))
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
  tkconfigure (slider2, command = draw2 )
  slidertxt2 <- tklabel(sliderFramePlot1 ,text="Number of samples:" )
  tkgrid(slidertxt2 )
  tkgrid.configure(slidertxt2 ,sticky="w")
  tkgrid(slider2)
  tkgrid(plotFrame)
  tkgrid(setFrame)
  tkgrid(sliderFramePlot1)
  tkgrid(plotFrame2)
  tkgrid(sliderFramePlot2)
   
}
CLTdemo ()
