library(tcltk2)
library(tkrplot)
library(ggplot2)

#normal
normal.dist <- function(){
  means <- rep(NA,nos)
  for(i in 1:nos){
    means[i] = mean(rnorm(n = ss,mean = par1,sd = par2))
  }
  meanss<-as.data.frame(means)
  dummy <- ggplot(data=meanss, aes(x=means)) + geom_histogram(aes(y=..density..,fill=..density..),
                                                              col="green",binwidth = 0.2) +
    geom_density() +
    ggtitle(paste("Population distribution: ",dis,"\n(miu=",par1,", sigma=",par2,")",
                  "\nSample size:",ss," Num of Samples:",nos)) +
    stat_function(fun=dnorm,color="red",args=list(mean=mean(means),sd=sd(means)))
  print(dummy)
}

#gamma
gamma.dist <- function() {
  xbar = rep(NA,nos)
  for(i in 1:nos){
      xbar[i] = mean(rgamma(ss, shape = par1, rate = par2))
  }
  xbars<-as.data.frame(xbar)
  dummy <- ggplot(data=xbars, aes(x=xbar)) + geom_histogram(aes(y=..density..,fill=..density..),
                                                            col="green",binwidth = 0.2) +
    geom_density() +
    ggtitle(paste("Population distribution: ",dis,"\n(shape=",par1,", rate=",par2,")",
                  "\nSample size:",ss," Num of Samples:",nos)) +
    stat_function(fun=dnorm,color="red",args=list(mean=mean(xbar),sd=sd(xbar)))
  print(dummy)
}

#Beta
beta.dist <- function() {
  data2 <- rep(0,nos)
  for(i in 1:nos) {
    data1 <- rbeta(n=ss,par1,par2)
    data2[i] <- mean(data1)
  }
  data2s <- as.data.frame(data2)
  dummy <- ggplot(data=data2s, aes(x=data2)) + geom_histogram(aes(y=..density..,fill=..density..),
                                                              col="green",binwidth = 0.2) +
    geom_density() +
    ggtitle(paste("Population distribution: ",dis,"\n(alpha=",par1,", beta=",par2,")",
                  "\nSample size:",ss," Num of Samples:",nos)) +
    stat_function(fun=dnorm,color="red",args=list(mean=mean(data2),sd=sd(data2)))
  print(dummy)
}

#uniform
uniform.dist <- function() {
  xbar = rep(0,nos)
  for(i in 1:nos){
      xbar[i] = mean(runif(n = ss,min = par1,max = par2))
  }
  xbars<-as.data.frame(xbar)
  if(par1>par2){
    dummy <- ggtitle(paste("Lower bound=",par1,"is higher than Upper bound=",par2,
                           ". Change your upper bound higher!"))
  } else {
    dummy <- ggplot(data=xbars, aes(x=xbar)) + geom_histogram(aes(y=..density..,fill=..density..),
                                                              col="green", binwidth = 0.2) +
      geom_density() + ggtitle(paste("Population distribution: ",dis,"\n(min=",par1,", max=",par2,")",
                                     "\nSample size:",ss," Num of Samples:",nos)) +
      stat_function(fun=dnorm,color="red",args=list(mean=mean(xbar),sd=sd(xbar)))

  }
  print(dummy)
}

CLTvis <-function() {
  tclRequire("BWidget")
  ss <<- 30
  nos <<- 200
  dis <<- "Normal"
  par1 <<- 0
  par2 <<- 1

  winPlot <- tktoplevel()
  tktitle(winPlot) <- "CLT Visualization (c)Group 5"
  # Define a frame inside 'winPlot'
  winPlot$env$frm <- tk2frame(winPlot, borderwidth = 2, relief = "sunken",
                           padding = 0)

  img <- tkrplot::tkrplot(winPlot,fun=normal.dist,1.4,1.4)
  tkpack(img, side = "right",expand=FALSE, fill = "both")

  tkpack(winPlot$env$frm, expand = TRUE, fill = "both")

  winPlot$env$frm$frmDist <- tk2frame(winPlot$env$frm,padding=0)
  winPlot$env$frm$frmDistSelect <- tk2frame(winPlot$env$frm,padding=0)
  winPlot$env$frm$frmSamp <- tk2frame(winPlot$env$frm,padding=0)

  labelTextNormM <- tclVar(paste("mean"))
  SliderValueNormM <- tclVar("0")
  SliderValueLabelNormM <- tklabel(winPlot$env$frm$frmDist, text=labelTextNormM)
  tkconfigure(SliderValueLabelNormM, textvariable=SliderValueNormM)
  sliderNormM <- tkscale(winPlot$env$frm$frmDist, from=-50, to=50, showvalue=T,variable=SliderValueNormM,
                         resolution=1, orient="horizontal",length=400)
  sliderNormM.label <- tk2label(winPlot$env$frm$frmDist, text = "Mean:")

  labelTextNormSD <- tclVar(paste("standard deviation"))
  SliderValueNormSD <- tclVar("1")
  SliderValueLabelNormSD <- tklabel(winPlot$env$frm$frmDist, text=labelTextNormSD)
  tkconfigure(SliderValueLabelNormSD, textvariable=SliderValueNormSD)
  sliderNormSD <- tkscale(winPlot$env$frm$frmDist, from=1, to=30, showvalue=T,variable=SliderValueNormSD,
                          resolution=1, orient="horizontal",length=400)
  sliderNormSD.label <- tk2label(winPlot$env$frm$frmDist, text = "Standard Deviation:")

  distribVal <- c("Normal", "Gamma", "Beta", "Uniform")
  distribSel <- tclVar("Normal")
  distribField <- ttkcombobox(winPlot$env$frm$frmDistSelect, values=distribVal, textvariable=distribSel,
                              state="readonly")

  onOK <- function(...) {
    dis <<- tclvalue(distribSel)
    tkdestroy(winPlot$env$frm$frmDist)
    tkgrid.remove(winPlot$env$frm$frmSamp)
    if (dis == "Normal") {
      winPlot$env$frm$frmDist <- tk2frame(winPlot$env$frm,padding=0)
      tkgrid(winPlot$env$frm$frmDist,sticky="we",columnspan=1,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)

      par1 <<- 0
      par2 <<- 1
      labelTextNormM <- tclVar(paste("mean"))
      SliderValueNormM <- tclVar("0")
      SliderValueLabelNormM <- tklabel(winPlot$env$frm$frmDist, text=labelTextNormM)
      tkconfigure(SliderValueLabelNormM, textvariable=SliderValueNormM)
      sliderNormM <- tkscale(winPlot$env$frm$frmDist, from=-50, to=50, showvalue=T,variable=SliderValueNormM,
                             resolution=1, orient="horizontal",length=400)
      sliderNormM.label <- tk2label(winPlot$env$frm$frmDist, text = "Mean:")

      labelTextNormSD <- tclVar(paste("standard deviation"))
      SliderValueNormSD <- tclVar("1")
      SliderValueLabelNormSD <- tklabel(winPlot$env$frm$frmDist, text=labelTextNormSD)
      tkconfigure(SliderValueLabelNormSD, textvariable=SliderValueNormSD)
      sliderNormSD <- tkscale(winPlot$env$frm$frmDist, from=1, to=30, showvalue=T,variable=SliderValueNormSD,
                              resolution=1, orient="horizontal",length=400)
      sliderNormSD.label <- tk2label(winPlot$env$frm$frmDist, text = "Standard Deviation:")

      tkgrid(sliderNormM.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderNormM, columnspan = 5, padx = 10, pady = c(5,10))
      tkgrid(sliderNormSD.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderNormSD, columnspan = 5, padx = 10, pady = c(5,50))

      img <- tkrreplot(img,fun=normal.dist,1.4,1.4)
    } else if(dis == "Gamma") {
      winPlot$env$frm$frmDist <- tk2frame(winPlot$env$frm,padding=0)
      tkgrid(winPlot$env$frm$frmDist,sticky="we",columnspan=1,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)

      par1 <<- 1
      par2 <<- 1
      labelTextGamS <- tclVar(paste("shape"))
      SliderValueGamS <- tclVar("1")
      SliderValueLabelGamS <- tklabel(winPlot$env$frm$frmDist, text=labelTextGamS)
      tkconfigure(SliderValueLabelGamS, textvariable=SliderValueGamS)
      sliderGamS <- tkscale(winPlot$env$frm$frmDist, from=1, to=50, showvalue=T,variable=SliderValueGamS,
                            resolution=1, orient="horizontal",length=400)
      sliderGamS.label <- tk2label(winPlot$env$frm$frmDist, text = "Shape (Alpha):")

      labelTextGamR <- tclVar(paste("rate"))
      SliderValueGamR <- tclVar("1")
      SliderValueLabelGamR <- tklabel(winPlot$env$frm$frmDist, text=labelTextGamR)
      tkconfigure(SliderValueLabelGamR, textvariable=SliderValueGamR)
      sliderGamR <- tkscale(winPlot$env$frm$frmDist, from=1, to=50, showvalue=T,variable=SliderValueGamR,
                            resolution=1, orient="horizontal",length=400)
      sliderGamR.label <- tk2label(winPlot$env$frm$frmDist, text = "Rate (Beta):")
      tkgrid(sliderGamS.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderGamS,columnspan = 5, padx = 10, pady = c(5,10))
      tkgrid(sliderGamR.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderGamR,columnspan = 5, padx = 10, pady = c(5,50))

      img <- tkrreplot(img,fun=gamma.dist,1.4,1.4)
    } else if(dis == "Beta") {
      winPlot$env$frm$frmDist <- tk2frame(winPlot$env$frm,padding=0)
      tkgrid(winPlot$env$frm$frmDist,sticky="we",columnspan=1,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)

      par1 <<- 1
      par2 <<- 1
      labelTextBeA <- tclVar(paste("alpha"))
      SliderValueBeA <- tclVar("1")
      SliderValueLabelBeA <- tklabel(winPlot$env$frm$frmDist, text=labelTextBeA)
      tkconfigure(SliderValueLabelBeA, textvariable=SliderValueBeA)
      sliderBeA <- tkscale(winPlot$env$frm$frmDist, from=1, to=50, showvalue=T,variable=SliderValueBeA,
                           resolution=1, orient="horizontal",length=400)
      sliderBeA.label <- tk2label(winPlot$env$frm$frmDist, text = "Alpha:")

      labelTextBeB <- tclVar(paste("beta"))
      SliderValueBeB <- tclVar("1")
      SliderValueLabelBeB <- tklabel(winPlot$env$frm$frmDist, text=labelTextBeB)
      tkconfigure(SliderValueLabelBeB, textvariable=SliderValueBeB)
      sliderBeB <- tkscale(winPlot$env$frm$frmDist, from=1, to=50, showvalue=T,variable=SliderValueBeB,
                           resolution=1, orient="horizontal",length=400)
      sliderBeB.label <- tk2label(winPlot$env$frm$frmDist, text = "Beta:")
      tkgrid(sliderBeA.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderBeA, columnspan = 5, padx = 10, pady = c(5,10))
      tkgrid(sliderBeB.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderBeB,columnspan = 5, padx = 10, pady = c(5,50))

      img <- tkrreplot(img,fun=beta.dist,1.4,1.4)
    } else if(dis == "Uniform") {
      winPlot$env$frm$frmDist <- tk2frame(winPlot$env$frm,padding=0)
      tkgrid(winPlot$env$frm$frmDist,sticky="we",columnspan=1,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)

      par1 <<- 0
      par2 <<- 1
      labelTextUnifMin <- tclVar(paste("min"))
      SliderValueUnifMin <- tclVar("0")
      SliderValueLabelUnifMin <- tklabel(winPlot$env$frm$frmDist, text=labelTextUnifMin)
      tkconfigure(SliderValueLabelUnifMin, textvariable=SliderValueUnifMin)
      sliderUnifMin <- tkscale(winPlot$env$frm$frmDist, from=0, to=20, showvalue=T,
                               variable=SliderValueUnifMin, resolution=1, orient="horizontal",length=400)
      sliderUnifMin.label <- tk2label(winPlot$env$frm$frmDist, text = "Minimum:")

      labelTextUnifMax <- tclVar(paste("max"))
      SliderValueUnifMax <- tclVar("1")
      SliderValueLabelUnifMax <- tklabel(winPlot$env$frm$frmDist, text=labelTextUnifMax)
      tkconfigure(SliderValueLabelUnifMax, textvariable=SliderValueUnifMax)
      sliderUnifMax <- tkscale(winPlot$env$frm$frmDist, from=1, to=20, showvalue=T,
                               variable=SliderValueUnifMax, resolution=1, orient="horizontal",length=400)
      sliderUnifMax.label <- tk2label(winPlot$env$frm$frmDist, text = "Maximum:")
      tkgrid(sliderUnifMin.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderUnifMin,columnspan = 5, padx = 10, pady = c(5,10))
      tkgrid(sliderUnifMax.label, columnspan = 5, padx = 10, pady = c(5,2))
      tkgrid(sliderUnifMax,columnspan = 5, padx = 10, pady = c(5,50))

      img <- tkrreplot(img,fun=uniform.dist,1.4,1.4)
    } else {
      tkmessageBox(title="Missing...", message = "Select a distribution!")
    }
    tkgrid(winPlot$env$frm$frmSamp,sticky="we",columnspan=1,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)
    # tkpack(img, side = "right",expand=FALSE, fill = "both")
    # tkpack.configure(winPlot$env$frm, img)
  }

  OKbutton <- tkbutton(winPlot$env$frm$frmDistSelect , text = "Set", foreground = "darkgreen",
                       width = "12", command = onOK, default = "active", borderwidth = 3)
  lbl <- tk2label(winPlot$env$frm$frmDistSelect, text = "Population distribution: ")
  tkgrid(winPlot$env$frm$frmDistSelect,sticky="we",columnspan=3,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)
  tkgrid(lbl, distribField, OKbutton, padx = 10, pady = c(5,10))

  tkgrid(winPlot$env$frm$frmDist,sticky="we",columnspan=1,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)

  tkgrid(sliderNormM.label, columnspan = 5, padx = 10, pady = c(5,2))
  tkgrid(sliderNormM,columnspan = 5, padx = 10, pady = c(5,10))

  tkgrid(sliderNormSD.label, columnspan = 5, padx = 10, pady = c(5,2))
  tkgrid(sliderNormSD,columnspan = 5, padx = 10, pady = c(5,50))

  tkgrid(winPlot$env$frm$frmSamp,sticky="we",columnspan=1,padx=0,pady=0,rowspan=1,ipadx=0,ipady=0)

  labelText3 <- tclVar(paste("sample size"))
  SliderValue3 <- tclVar("30")
  SliderValueLabel3 <- tklabel(winPlot$env$frm$frmSamp, text=labelText3)
  tkconfigure(SliderValueLabel3, textvariable=SliderValue3)
  slider3 <- tkscale(winPlot$env$frm$frmSamp, from=10, to=100, showvalue=T,variable=SliderValue3,resolution=1, orient="horizontal",length=400)
  slider3.label <- tk2label(winPlot$env$frm$frmSamp, text = "Sample size:")
  tkgrid(slider3.label, columnspan = 5, padx = 10, pady = c(50,2))
  tkgrid(slider3,columnspan = 5, padx = 10, pady = c(5,10))

  labelText4 <- tclVar(paste("number of samples"))
  SliderValue4 <- tclVar("200")
  SliderValueLabel4 <- tklabel(winPlot$env$frm$frmSamp, text=labelText4)
  tkconfigure(SliderValueLabel4, textvariable=SliderValue4)
  slider4 <- tkscale(winPlot$env$frm$frmSamp, from=1, to=1000, showvalue=T,variable=SliderValue4,resolution=1, orient="horizontal",length=400)
  slider4.label <- tk2label(winPlot$env$frm$frmSamp, text = "Number of Samples:")
  tkgrid(slider4.label, columnspan = 5, padx = 10, pady = c(5,2))
  tkgrid(slider4,columnspan = 5, padx = 10, pady = c(5,10))

  draw <- function(...){
    ss <<- as.numeric(tclvalue(SliderValue3))
    nos <<- as.numeric(tclvalue(SliderValue4))
    par1 <<- as.numeric(tclvalue(SliderValueNormM))
    par2 <<- as.numeric(tclvalue(SliderValueNormSD))
    img <- tkrreplot(img,fun=normal.dist,1.4,1.4)
  }
  tkconfigure(sliderNormM, command = draw)
  tkconfigure(sliderNormSD, command = draw)
  tkconfigure(slider3, command = draw)
  tkconfigure(slider4, command = draw)
}
CLTvis()
