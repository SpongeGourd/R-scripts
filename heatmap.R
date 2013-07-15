require(graphics); require(grDevices)

row.names(file_all) <- file_all$name

h2 <- file_all[1:11,2:18]

h3 <- apply(h2, c(1,2), as.numeric)

x  <- as.matrix(h3)

#cc <- rainbow(ncol(x), start=0, end=.1)

color.map <- function(ID) { if (ID=="U") "steelblue" else "orange" }

cc <- unlist(lapply(file_all[12,2:18], color.map))

library("gplots")

hv <- heatmap(x, col = redgreen(75), scale="column",
              ColSideColors = cc, margins=c(14,1))
