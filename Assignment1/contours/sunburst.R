library(aplpack)
bagplot(data_3d,create.plot=TRUE,
        show.outlier=TRUE,show.looppoints=TRUE,
        show.bagpoints=TRUE,dkmethod=2,
        show.whiskers=TRUE,show.loophull=FALSE,
        show.baghull=TRUE,verbose=FALSE, xlab = "glucose conc.", ylab = "Blood Pressure" , main= "Sunburst plot")
