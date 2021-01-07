# home made function
# use at your own risk

# String manipulation

rmcoma <- function(txt){
  t1 <- gsub(",", "-", txt)
  t2 <- gsub(";", "-", t1)
  t3 <- gsub("\\n", " ", t2)
  t4 <- rmempty(t3)
  return(t4)
}

rmempty <- function(txt){
  #before
  t1 <- gsub("^ ", "", txt)
  while (any(t1!=txt, na.rm = TRUE)){
    txt <- t1
    t1 <- gsub("^ ", "", txt)
  }
  #after
  t1 <- gsub(" $", "", txt)
  while (any(t1!=txt, na.rm = TRUE)){
    txt <- t1
    t1 <- gsub(" $", "", txt)
  }
  return(txt)
}

ah2sp <- function(x, increment=360, rnd=10, proj4string=CRS(as.character(NA)), name="1"){
  require(alphahull)
  require(maptools)
  if (class(x) != "ahull"){
    stop("x needs to be an ahull class object")
  }
  # Extract the edges from the ahull object as a dataframe
  xdf <- as.data.frame(x$arcs)
  # Remove all cases where the coordinates are all the same      
  xdf <- subset(xdf,xdf$r > 0)
  res <- NULL
  if (nrow(xdf) > 0){
    # Convert each arc to a line segment
    linesj <- list()
    prevx<-NULL
    prevy<-NULL
    j<-1
    for(i in 1:nrow(xdf)){
      rowi <- xdf[i,]
      v <- c(rowi$v.x, rowi$v.y)
      theta <- rowi$theta
      r <- rowi$r
      cc <- c(rowi$c1, rowi$c2)
      # Arcs need to be redefined as strings of points. Work out the number of points to allocate in this arc segment.
      ipoints <- 2 + round(increment * (rowi$theta / 2),0)
      # Calculate coordinates from arc() description for ipoints along the arc.
      angles <- anglesArc(v, theta)
      seqang <- seq(angles[1], angles[2], length = ipoints)
      x <- round(cc[1] + r * cos(seqang),rnd)
      y <- round(cc[2] + r * sin(seqang),rnd)
      # Check for line segments that should be joined up and combine their coordinates
      if (is.null(prevx)){
        prevx<-x
        prevy<-y
      } else if (x[1] == round(prevx[length(prevx)],rnd) && y[1] == round(prevy[length(prevy)],rnd)){
        if (i == nrow(xdf)){
          #We have got to the end of the dataset
          prevx<-append(prevx,x[2:ipoints])
          prevy<-append(prevy,y[2:ipoints])
          prevx[length(prevx)]<-prevx[1]
          prevy[length(prevy)]<-prevy[1]
          coordsj<-cbind(prevx,prevy)
          colnames(coordsj)<-NULL
          # Build as Line and then Lines class
          linej <- Line(coordsj)
          linesj[[j]] <- Lines(linej, ID = as.character(j))
        } else {
          prevx<-append(prevx,x[2:ipoints])
          prevy<-append(prevy,y[2:ipoints])
        }
      } else {
        # We have got to the end of a set of lines, and there are several such sets, so convert the whole of this one to a line segment and reset.
        prevx[length(prevx)]<-prevx[1]
        prevy[length(prevy)]<-prevy[1]
        coordsj<-cbind(prevx,prevy)
        colnames(coordsj)<-NULL
        # Build as Line and then Lines class
        linej <- Line(coordsj)
        linesj[[j]] <- Lines(linej, ID = as.character(j))
        j<-j+1
        prevx<-NULL
        prevy<-NULL
      }
    }
    # Promote to SpatialLines
    lspl <- SpatialLines(linesj)
    # Convert lines to polygons
    # Pull out Lines slot and check which lines have start and end points that are the same
    lns <- slot(lspl, "lines")
    polys <- sapply(lns, function(x) {
      crds <- slot(slot(x, "Lines")[[1]], "coords")
      identical(crds[1, ], crds[nrow(crds), ])
    })
    # Select those that do and convert to SpatialPolygons
    polyssl <- lspl[polys]
    list_of_Lines <- slot(polyssl, "lines")
    res <- Polygons(lapply(list_of_Lines, function(x) { Polygon(slot(slot(x, "Lines")[[1]], "coords")) }), ID = name)
    #sppolys <- SpatialPolygons(list(Polygons(lapply(list_of_Lines, function(x) { Polygon(slot(slot(x, "Lines")[[1]], "coords")) }), ID = "1")), proj4string=proj4string)
    # Create a set of ids in a dataframe, then promote to SpatialPolygonsDataFrame
    #hid <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "ID"))
    #areas <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "area"))
    #df <- data.frame(hid,areas)
    #names(df) <- c("HID","Area")
    #rownames(df) <- df$HID
    #res <- SpatialPolygonsDataFrame(sppolys, data=df)
    #res <- res[which(res@data$Area > 0),]
  }  
  return(res)
}

lunique <- function(x)length(unique(x))

max97 <- function(dat, na.rm =TRUE){
  return(as.numeric(quantile(as.numeric(dat), probs = 0.975, na.rm = na.rm)))
}
min03 <- function(dat, na.rm =TRUE){
  return(as.numeric(quantile(as.numeric(dat), probs = 0.025, na.rm = na.rm)))
}

