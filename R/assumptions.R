assumptions <-
function(fit) {
    require(car)
    residplot <- function(fit, nbreaks=10) {
        z <- rstudent(fit)
        hist(z, breaks=nbreaks, freq=FALSE, 
            xlab="Studentized Residual", 
            main="Distribution of Errors")
        rug(jitter(z), col="brown")
        curve(dnorm(x, mean=mean(z), sd=sd(z)), 
            add=TRUE, col="blue", lwd=2)
        lines(density(z)$x, density(z)$y, 
            col="red", lwd=2, lty=2)
        legend("topright", 
            legend = c( "Normal Curve", "Kernel Density Curve"), 
            lty=1:2, col=c("blue","red"), cex=.7)
    }
    dev.new()
    residplot(fit)
    dev.new()
    qqPlot(fit, labels=FALSE,
        simulate=TRUE, main="Q-Q Plot (normality)")
    cat("EQUAL VARIANCE TESTS:\n============================\n")
    ev1 <- bartlett.test(formula(fit), model.frame(fit))
    ev2 <- fligner.test(formula(fit), model.frame(fit))
    require(HH)
    ev3 <- HH::hov(formula(fit), model.frame(fit))
    lapply(list(ev1, ev2, ev3), print)
    cat("OUTLIER TEST:\n============================\n")
    ot <- car::outlierTest(fit)
    print(ot)
    cat("\nINDEPENDENCE OF ERRORS:\n============================\n")
    ie <- durbinWatsonTest(fit)
    print(ie)
    crPlots(fit, ask = TRUE)
    cat("\nHOMODASCITY:\n============================\n")
    h1 <- ncvTest(fit)
    print(h1)
    (h2 <- spreadLevelPlot(fit))
    print(h2)
    require(gvlma)
    all <- summary(gvlma::gvlma(fit))
    print = all
        if (length(fit$terms[[3]]) > 1) {
        co <- data.frame(var = fit$terms[[3]], vif = vif(fit), problem = sqrt(vif(fit)) > 2)
        print(co)
    } else {
        co <- "only one predictor"
    }
    invisible(list(bartlett=ev1, flinger=ev2, hov = ev3, outlier=ot, 
    indep_errors = ie, linearity = h1, global = all, multicolinearity = co))
}
