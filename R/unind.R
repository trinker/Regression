unind <-
function(object = "clipboard", indents = 1,
         front.remove = 1, back.remove = 0, tb.remove =  FALSE, 
         top.remove = 1, bottom.remove = 1) {

    if (!is.null(indents)) front.remove <- indents*4
  
    y <- if (object == "clipboard") {
        rbind(unlist(readClipboard()))
    } else {
        strsplit(as.vector(object), "[\n]")[[1]]
    }
    y <- as.matrix(y)
    if(tb.remove){y  <- y[-c(1:top.remove, ((length(y)+1) - bottom.remove):length(y))]}
    j <- unlist(lapply(y,  function(x) substring(x, front.remove + 1, (nchar(x) - back.remove))))
    zz <- as.matrix(j)
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))

    writeClipboard(noquote(zz), format = 1)
    
    return(noquote(zz))
}
