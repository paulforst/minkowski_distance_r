#Paul Forst
#Bellarmine MSA Summer 2017
#Minkowski Distance Function
#Input: 5 digit zip code and number of days forecast

#Known Issue(s):
#In some situations, the x-axis does not extend the full range of values and points end up appearing past the end of the axis.

#library(tidyverse)

foodpref <- function (poi, dataspec, normp=2, weights=NULL, nlmin="zero", nlmax="maxobs") {

        #Capture dataframe name for output in error messages
        df_name <- deparse(substitute(dataspec))
        
        #Error Checking on the input parameters
        if(is.null(poi)) {
                stop("Missing Person of Interest in function call")
        }
        
        if(is.null(dataspec)) {
                stop("Dataframe ", df_name, " is empty or null")
        } else if (ncol(dataspec) < 2) {
                stop("Dataframe ", df_name, " must have a minimum of two columns.")
        } else if (any(is.na(dataspec))) {
                stop("Dataframe ", df_name, " contains N/A or NULL values.")
        } #else if (is.numeric(dataspec[,-1])) {
          #      stop(df_name, " is not a valid dataframe")
        #} Cannot figure out how to properly test for non-numeric values in the dataframe values
        
        if(!poi %in% dataspec[,1]) {
                stop(poi, " does not exist in first column of the dataframe.")
        }
        
        if(!is.null(weights) & !is.numeric(weights)) {
                stop("The weights parameter does not contain all numeric values")
        }
        
        if(!is.null(weights) & length(weights) != ncol(dataspec) - 1) {
                stop("The weights vector and dataframe measurements are not equal lengths(weights = ", length(weights), ", dataframe measures = ", ncol(dataspec) - 1, "). ")
        }
        
        if(!is.numeric(normp)) {
                stop("The power parameter (normp) must be a numeric value")
        }
        
        if(!nlmin %in% c("minobs", "zero")) {
                stop("The leftmost point on the number line parameter (nlmin) must be either 'minobs' for the smallest calculated distance or 'zero'. ")
        }
        
        if(!nlmax %in% c("maxobs", "theomax")) {
                stop("The power parameter (nlmax) must be either 'maxobs' for the largest calculated distance or 'theomax' for the largest theoretical possible distance")
        }
        
        #Pull names out of dataframe
        names <- dataspec[,1]
        
        #POI rankings
        poi_ranks <- dataspec[dataspec[1] == poi, -1]
        
        #Remove name column from dataframe so that measurements can be used for dist formula
        dataspec <- dataspec[-1]
        
        #Check to see if there is a weighted vector. If so, multiple measurements in dataspec by weights and determine the maximum theoretical possible distance.
        if(!is.null(weights)){
                dataspec <- mapply('*', dataspec, weights)
                theomax <- (sum((weights*4)^normp))^(1/normp)
        } else {theomax <- (ncol(dataspec)*(4^normp))^(1/normp)}
        
        #Use dist function to calculate Minkowski distances between points
        distances <- as.matrix(dist(dataspec, method = "minkowski", p = normp))
        
        #Reduce distances matrix to a vector of the only the values related to the person of interest
        distances <- as.numeric(distances[match(poi, names), -match(poi, names)])
        names <- names[-match(poi, names)]
        output <- data.frame(names, distances)
        output <- output[order(distances),]
        
        #Generate plot values and visual
        if(nlmin == "minobs") {min_x <- floor(min(distances))
        } else {min_x <- 0}
        if(nlmax == "maxobs") {max_x <- ceiling(max(distances))
        } else {max_x <- ceiling(theomax)
                if (theomax / max(distances) > 10) {
                warning("Recommend using 'maxobs' instead of 'theomax' for better graphical display.")
                }
        }
        
        #Define plot parameters
        xlim <- c(min_x,max_x)
        ylim <- c(0,100)
        px <- output$distances
        py <- rep(0,length(px))
        lx_min <- min(c(xlim[1], min(px)))
        lx_max <- max(c(xlim[2], max(px)))
        lx.buf <- (lx_max - lx_min) / length(px)
        lx <- seq(lx_min+lx.buf,lx_max-lx.buf,len=length(px));
        ly <- 60;

        #Generate plot outline
        par(xaxs='i',yaxs='i',mar=c(5,1,1,1));
        plot(NA,xlim=xlim,ylim=ylim,axes=F,ann=F);
        axis(1);
        
        #Plot elements
        segments(px,py,lx,ly);
        points(px,py,pch=16,xpd=NA);
        text(lx,ly + 5,output$names,pos=3,srt=90);
}
