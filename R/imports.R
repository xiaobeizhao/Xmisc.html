
## ************************************************************************
## 
## 
## 
## (c) Xiaobei Zhao
## 
## Mon Jan 23 21:20:41 CST 2023 -0600 (Week 04)
## 
## 
## Reference: 
## 
## 
## ************************************************************************


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


##' Concatenate vector into a string
##' 
##'
##' @title Concatenate vector into a string
##' @param x vector
##' @param sep character, a delimiter
##' @param capsule logical, weather to capsule with `c()'
##' @param quote logical, weather to surround elements by double quotes.
##' @return vector
##' @author Xiaobei Zhao
##' @examples
##' cat0(vconcat(head(letters)))
##' ## c("a", "b", "c", "d", "e", "f")
##' 
##' cat(vconcat(head(letters),sep='-'),'\n')
##' ## a-b-c-d-e-f
##' 
##' cat0(vconcat(capture.output(str(iris)),sep='\n'))
##' cat0(vconcat(structure(c("A","T","G"),names=1:3)))
##' cat0(vconcat(structure(c("A","T","G"),names=1:2)))
##' 
vconcat <- function(x,sep=NULL,capsule=NULL,quote=NULL){
  if (!length(sep)){
    sep=", "
    if (!length(quote)){
      if (is.character(x)) quote <- TRUE else quote <- FALSE
    }
    if (!length(capsule)){
      if (length(x)>1) capsule <- TRUE else capsule <- FALSE
    }
    if (!length(x)){
      if (!capsule){
        return("")
      } else {
        return("c()")
      }
    }
  } else {
    if (!length(quote)){
      quote <- FALSE
    }
    if (!length(capsule)){
      capsule <- FALSE
    }
  }

  if (is.list(x)){
    x <- structure(as.character(x),names=names(x))
  }

  if (! any(class(x) %in% c("logical","integer","numeric","character")) ){
    stop('Only logical, integer, real and character vectors are supported, but some coercion will be done.')
  }
  
  .flag <- is.character(x)
  if (quote & !.flag){
    warning('Surround non-character elements by double quotes. Try quote=FALSE.')
  }
  .x <- as.character(x)
  .names <- names(x)
  
  if (quote){
    .x <- paste0('"',.x,'"')
  }
  if (length(.names)){
    .names[is.na(.names)] <- ""
    .x <- paste0('"',.names,'"',"=",.x)
    .x <- lstrip(.x,"=")
  }
  ret <- paste(.x,sep='',collapse=sep)
  if (capsule){
    ret <- sprintf('c(%s)',ret)
  }
  return(ret)
}




## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


stop.not.equal.length <- function(
  x,
  y
  ){
  .x <- deparse(substitute(x))
  .y <- deparse(substitute(y))
  .xl <- length(x)
  .yl <- length(y)
  if (.xl!=.yl){
    ## .msg <- lprintf('`%(.x)s` [%(.xl)s] and `%(.y)s` [%(.yl)s] must be of equal length.')
    .msg <- sprintf('`%s` [%s] and `%s` [%s] must be of equal length.',.x,.xl,.y,.yl)
    stop(.msg)
  }
}



stop.not.all.in.vector <- function(
  x,
  y
  ){
  
  .x <- deparse(substitute(x))
  .y <- deparse(substitute(y))
  .xv <- vconcat(x)
  .yv <- vconcat(y)
  if ( ! all(x %in% y) ){
    ## .msg <- lprintf('`%(.x)s` [%(.xv)s] must be all in vector `%(.y)s` [%(.yv)s].')
    .msg <- sprintf('`%s` [%s] must be all in vector `%s` [%s].',.x,.xv,.y,.yv)
    stop(.msg)
  }
}



## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

vreplace <- function(
  v,
  x,
  y
  ){
  x <- unique(x)
  y <- unique(y)
  stop.not.equal.length(x,y)
  stop.not.all.in.vector(x,v)
  stop.not.all.in.vector(y,v)
  stop.not.all.in.vector(x,y)
  stop.not.all.in.vector(y,x)
  i <- vapply(x,function(e){which(v==e)},numeric(1),USE.NAMES=FALSE)
  j <- vapply(y,function(e){which(v==e)},numeric(1),USE.NAMES=FALSE)
  ret <- replace(v,i,v[j])
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

signchar <- function(
  x
  ){
  .sign <- c("+"=1,"."=0,"-"=-1)
  tmp <- sign(x)
  ret <- vapply(tmp,function(e){ret <- names(which(.sign==e)); if(!length(ret)){ret <- "."}; return(ret)},character(1))
  ret[ret=="."] <- ""
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


##' .. content for \description{} (no empty lines) ..
##'
##' imported from Xmisc.base
##' @title 
##' @param x 
##' @param y 
##' @param by 
##' @param by.x 
##' @param by.y 
##' @param ... 
##' @return 
##' @author
##' @examples
##' 
##' 
##' authors <- data.frame(
##'   surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
##'   nationality = c("US", "Australia", "US", "UK", "Australia"),
##'   deceased = c("yes", rep("no", 4)))
##' books <- data.frame(
##'   name = I(c("Tukey", "Venables", "Tierney",
##'     "Ripley", "Ripley", "McNeil", "R Core")),
##'   title = c("Exploratory Data Analysis",
##'     "Modern Applied Statistics ...",
##'     "LISP-STAT",
##'     "Spatial Statistics", "Stochastic Simulation",
##'     "Interactive Data Analysis",
##'     "An Introduction to R"),
##'   other.author = c(NA, "Ripley", NA, NA, NA, NA,
##'     "Venables & Smith"))
##' 
##' ## (m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
##' ## (m2 <- merge(books, authors, by.x = "name", by.y = "surname"))
##' 
##' (authors.ext <- df.add_col(authors, books, by.x = "surname", by.y = "name", sort=FALSE, all.x=TRUE))
##' (books.ext <- df.add_col(books, authors, by.x = "name", by.y = "surname", sort=FALSE, all.x=TRUE))
##' 
##' 
##' 
df.add_col <- function(
  x,
  y,
  by=NULL,
  by.x=by,
  by.y=by,
  ...
  ){

  ## -- ##
  .flag <- (is.data.table(x) | is.data.table(y))

  ## -- ##
  if (!is.data.table(x)){
    setDT(x)
    on.exit(setDF(x))
  }
  
  if (!is.data.table(y)){
    setDT(y)
    on.exit(setDF(y))
  }
  
  ## -- ##
  ## ## printme(list(by=by,by.x=by.x,by.y=by.y),".add_col")
  ## ## tmp <- merge(x,y,by=by,by.x=by.x,by.y=by.y,...)
  if (length(by.x) & length(by.y)){
    tmp <- merge(x,y,by.x=by.x,by.y=by.y,...)
  } else {
    tmp <- merge(x,y,by=by,...)
  }
  
  ## -- ##
  .colnames__x <- colnames(x)
  .colnames__tmp <- colnames(tmp)
  ret <- tmp[,unique(c(.colnames__x[.colnames__x %in% .colnames__tmp],.colnames__tmp)),with=FALSE]
  
  ## printme(head(x),".add_col")
  ## printme(head(y),".add_col")
  ## printme(head(ret),".add_col")
  if (!.flag){
    setDF(ret)
  }
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param row.na.rm 
##' @param col.na.rm 
##' @return 
##' @author
##' @examples
##' x <- rbind(cbind(head(iris),NA),NA)
##' df.rm_na(x)
##' df.rm_na(x,row.na.rm=TRUE)
##' df.rm_na(x,col.na.rm=TRUE)
##' df.rm_na(x,row.na.rm=TRUE,col.na.rm=TRUE)
##' 
df.rm_na <- function(
  x,
  row.na.rm=FALSE,
  col.na.rm=FALSE
  ){
  ret <- copy(x)
  if (is.data.table(ret)){
    setDF(ret)
    on.exit(setDT(ret))
  }
  if (row.na.rm){
    ret <- ret[rowSums(is.na(ret))<ncol(ret),,drop=FALSE]
  }
  
  if (col.na.rm){
    ret <- ret[,colSums(is.na(ret))<nrow(ret),drop=FALSE]
  }
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

is.na2 <- function(x){
  if (!length(dim(x))){
    ret <- x %in% c(NA,"NA","_na_","n/a","")
  } else {
    ret <- apply(x,2,is.na2)
  }
  ret
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


##' Strip a string with given chars at both ends
##'
##' 
##' @title Strip a string with given chars at both ends
##' @param x character, a string.
##' @param char character to trim.
##' @return character
##' @author Xiaobei Zhao
strip <- function(x,char=" "){
  if (!length(char)){
    return(x)
  }
  if (char==""){
    return(x)
  }
  gsub(sprintf("(^[%s]+)|([%s]+$)",char,char), "", x)
}



## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param col.names 
##' @return 
##' @author
##' @examples
##' na.data.table()
##' na.data.table(LETTERS[1:6])
##' 
na.data.table <- function(
  col.names=NULL,
  col.classes=NULL
  ){
  ret <- data.table(
    matrix(
      NA,
      ncol=length(col.names),
      nrow=1,
      dimnames=list(c(""), col.names)
      )
    )
  if (length(col.classes)){
    colclasses(ret) <- col.classes
  }
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

get_TIMENOW <- function(
  ){
  ret <- format(Sys.time(),usetz=TRUE)
  ret
}



## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


##' Write a file
##' 
##' 
##' @title Write a file
##' @param outFpath character, the file path
##' @param x character, a string
##' @param mode 
##' @param append logical, whether to append
##' @param logger character, see `get_loggers()`
##' @param sep see `writeLines`
##' @param ... additional arguemnts to `writeLines`
##' @return NULL
##' @author Xiaobei Zhao
write.file <- function(outFpath,x,mode=NULL,append=FALSE,logger=NULL,sep="\n", ...){
  if (!length(mode)){
    mode <- c("w","a")[append+1]
  }
  input <- file(outFpath,open=mode)
  writeLines(x, input, sep=sep, ...)
  close(input)
  invisible()
}
