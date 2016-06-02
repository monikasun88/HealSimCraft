
ncol = 4
nrow = 4

# Function to generate random value, 2 or 4 and put it in an empty spot
genRandNum <- function(currMat, ncol, nrow, nrun = 1) {
  for (a in 1:nrun) {
    retNum <- sample(c(2,2,2,2,2,2,2,2,2,4), 1)
    changedSpot <- sample(which(currMat == 0), 1)
    currMat[changedSpot] <- currMat[changedSpot] + retNum
  }
  return(currMat)
}

# test <- matrix(c(2,4,2,2,2,2,2,0,2,4,0,0,2,4,4,4), nrow = 4, ncol = 4, byrow = TRUE)

# Function to run when user chooses down
downResult <- function(currMat, ncol, nrow) {
  newMat <- currMat
  newMat <- apply(newMat, 2, function(x) x <- c(rep(0, sum(x == 0)), x[x != 0]) )
  for (colNum in 1:ncol) {
    for (rowNum in nrow:2) {
      if (newMat[rowNum,colNum] == newMat[rowNum-1, colNum] & newMat[rowNum,colNum] > 0) {
        newMat[rowNum,colNum] <- newMat[rowNum,colNum] * 2
        newMat[rowNum-1,colNum] <- 0
      } else {newMat[rowNum,colNum] <- newMat[rowNum,colNum]}
    }
  }
  newMat <- apply(newMat, 2, function(x) x <- c(rep(0, sum(x == 0)), x[x != 0]) )
  if (all(newMat == currMat)) {return(currMat)
  } else {
    currMat <- genRandNum(newMat, nrow, ncol, 1)
    return(currMat)
  }
}

# Function to run when user chooses up
upResult <- function(currMat, ncol, nrow) {
  newMat <- currMat
  newMat <- apply(newMat, 2, function(x) x <- c(x[x != 0], rep(0, sum(x == 0))) )
  for (colNum in 1:ncol) {
    for (rowNum in 1:(nrow-1)) {
      if (newMat[rowNum,colNum] == newMat[rowNum+1, colNum] & newMat[rowNum,colNum] > 0) {
        newMat[rowNum,colNum] <- newMat[rowNum,colNum] * 2
        newMat[rowNum+1,colNum] <- 0
      } else {newMat[rowNum,colNum] <- newMat[rowNum,colNum]}
    }
  }
  
  newMat <- apply(newMat, 2, function(x) x <- c(x[x != 0], rep(0, sum(x == 0))) )
  if (all(newMat == currMat)) {return(currMat)
  } else {
    currMat <- genRandNum(newMat, nrow, ncol, 1)
    return(currMat)
  }
}


# Function to run when user chooses right
rightResult <- function(currMat, ncol, nrow) {
  newMat <- currMat
  newMat <- t(apply(newMat, 1, function(x) x <- c(rep(0, sum(x == 0)), x[x != 0]) ))
  for (rowNum in 1:nrow) {
    for (colNum in ncol:2) {
      if (newMat[rowNum,colNum] == newMat[rowNum, colNum-1] & newMat[rowNum,colNum] > 0) {
        newMat[rowNum,colNum] <- newMat[rowNum,colNum] * 2
        newMat[rowNum,colNum-1] <- 0
      } else {newMat[rowNum,colNum] <- newMat[rowNum,colNum]}
    }
  }
  
  newMat <- t(apply(newMat, 1, function(x) x <- c(rep(0, sum(x == 0)), x[x != 0])))
  if (all(newMat == currMat)) {return(currMat)
  } else {
    currMat <- genRandNum(newMat, nrow, ncol, 1)
    return(currMat)
    
  }
}

# Function to run when user chooses left
leftResult <- function(currMat, ncol, nrow) {
  newMat <- currMat
  newMat <- t(apply(newMat, 1, function(x) x <- c(x[x != 0], rep(0, sum(x == 0)))))
  for (rowNum in 1:nrow) {
    for (colNum in 1:(ncol-1)) {        
      if (newMat[rowNum,colNum] == newMat[rowNum, colNum+1] & newMat[rowNum,colNum] > 0) {
        newMat[rowNum,colNum] <- newMat[rowNum,colNum] * 2
        newMat[rowNum,colNum+1] <- 0
      } else {newMat[rowNum,colNum] <- newMat[rowNum,colNum]}
    }
  }
  newMat <- t(apply(newMat, 1, function(x) x <- c(x[x != 0], rep(0, sum(x == 0)))))
  if (all(newMat == currMat)) {return(currMat)
  } else {
    currMat <- genRandNum(newMat, nrow, ncol, 1)
    return(currMat)
  }
}

# Colors of the plot
getCols <- function(currMat) {
  colorLib <- list(c("#FFFFFF", 0),
                   c("#EEE4DA", 2),
                   c("#EAE0C8", 4),
                   c("#F59563", 8),
                   c("#3399ff", 16),
                   c("#ffa333", 32),
                   c("#cef030", 64),
                   c("#E8D8CE", 128),
                   c("#990303", 256),
                   c("#6BA5DE", 512),
                   c("#DCAD60", 1024),
                   c("#B60022", 2048),
                   c("#66CCFF", 4096))
  
  colRet <- sapply(sapply(currMat, function(x) which(as.numeric(sapply(colorLib, function(x) x[2])) == x)), 
                   function(x) colorLib[[x]][1])
  
  textColRet <- as.vector(ifelse(currMat <= 4, "black", "white"))
  
  return(list(colRet, textColRet))
}


# Initiate inital matrix of empty squares with two random values
initMat <- matrix(rep(0, ncol * nrow), ncol = ncol, nrow = nrow)
initMat <- genRandNum(initMat, nrow, ncol, 2)
currMat <- initMat

shinyServer(function(input, output) {
  
  # Create a reactiveValues object, to let us use settable reactive values
  values <- reactiveValues()
  # To start out, lastAction == NULL, meaning nothing clicked yet
  values$lastAction <- NULL
  # An observe block for each button, to record that the action happened
  observe({
    if (input$up != 0) {
      values$lastAction <- 'up'}
  })
  observe({
    if (input$down != 0) {
      values$lastAction <- 'down'}
  })
  observe({
    if (input$left != 0) {
      values$lastAction <- 'left'}
  })
  observe({
    if (input$right != 0) {
      values$lastAction <- 'right'}
  })  
  observe({
    if (input$reset != 0) {
      values$lastAction <- 'reset'}
  })
  
  
  nextMat <- reactive({
    input$reset
    input$left
    input$right
    input$up
    input$down
    if (identical(values$lastAction, 'reset') | is.null(values$lastAction)) {
      initMat <- matrix(rep(0, ncol * nrow), ncol = ncol, nrow = nrow)
      initMat <- genRandNum(initMat, nrow, ncol, 2)
      currMat <<- initMat
    }
    if (identical(values$lastAction, 'up')) {
      currMat <<- upResult(currMat, ncol, nrow) 
    }
    if (identical(values$lastAction, 'down')) {
      currMat <<- downResult(currMat, ncol, nrow) 
    }
    if (identical(values$lastAction, 'left')) {
      currMat <<- leftResult(currMat, ncol, nrow) 
    }
    if (identical(values$lastAction, 'right')) {
      currMat <<- rightResult(currMat, ncol, nrow) 
    }
    currMat
  })

  output$plot_2048 <-  renderPlot({
    returnedCols <- reactive({getCols(nextMat())
      })  
    
    symbols(as.vector(sapply(1:nrow, function(x) rep(x, ncol))), rep(nrow:1,nrow), squares = rep(1, nrow*ncol),
              bg = returnedCols()[[1]], xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      currMatFinal <- reactive({ifelse(nextMat() == 0, "", nextMat())})
      text(as.vector(sapply(1:nrow, function(x) rep(x, ncol))), rep(nrow:1,nrow), currMatFinal(), font = 2, cex =2, 
           col = returnedCols()[[2]])
  })
  
})