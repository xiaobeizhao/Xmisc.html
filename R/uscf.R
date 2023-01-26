
## ************************************************************************
## https://xiaobeizhao.github.io/Xmisc.html/
## 
## 
## (c) Xiaobei Zhao
## 
## Sun Jan 22 04:36:29 CST 2023 -0600 (Week 03)
## 
## @Eaxmples
## Rscript -e 'require(Xmisc.html); Xmisc.html::uscf.main()'
## ## Rscript -e 'require(Xmisc.html); Xmisc.html::uscf.main(pcThreads=12)'
## Rscript -e 'require(Xmisc.html); Xmisc.html::uscf.get_tnmt.latest(30903445)' #NA
## Rscript -e 'require(Xmisc.html); Xmisc.html::uscf.get_rating.latest(30903445,tType="Correspondence")' #NA
## Rscript -e 'require(Xmisc.html); Xmisc.html::uscf.get_top100.table(section="Age 7 and Under")' 
## Rscript -e 'require(Xmisc.html); Xmisc.html::uscf.get_top100.table.new(section="Age 7 and Under")' 
## ## Rscript -e 'require(Xmisc.html); Xmisc.html::uscf.get_top100.table.update(section="Age 7 and Under")' 
##
## ## DONOT RUN
## ## ## require(Xmisc.html);section_v <- uscf.get_top100.section.list();uscf.get_top100.table.new(section=section_v);
## 
## @Reference: 
## https://xiaobeizhao.github.io/uscf/
## 
## ************************************************************************


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------
uscf.get_DEFAULT__tType <- function(
  ){
  c("Regular","Quick","Blitz","Correspondence")
}




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param section 
##' @return 
##' @author
##' @examples
##' for (.section in uscf.get_top100.section.list()){cat(.section," | ",uscf.section_to_tType(.section),"\n")}
##' 
uscf.section_to_tType <- function(
  section
  ){
  ret <- "Regular"
  for (e in uscf.get_DEFAULT__tType()){
    ## .p <- lprintf('\\b%(e)s\\b')
    .p <- sprintf('\\b%s\\b',e)
    if (grepl(.p,section,ignore.case=TRUE)){
      ret <- e
    }
  }
  return(ret)
}


##' .. content for \description{} (no empty lines) ..
##'
##' 
##' .. content for \details{} ..
##' @title 
##' @param id 
##' @return 
##' @author
##' @examples
##' uscf.get_MbrDtlTnmtHst(30903445)
##' 
uscf.get_MbrDtlTnmtHst <- function(
  id
  ){
  ## ## .inUrl <- lprintf('https://www.uschess.org/msa/MbrDtlTnmtHst.php?%(id)s')
  .inUrl <- sprintf('https://www.uschess.org/msa/MbrDtlTnmtHst.php?%s',id)
  ret <- html_to_table(
    inUrl=.inUrl,
    pattern='<!-- Detail: [0-9]+  -->.*?</tr>'
    )
  ## ## printme(id,"uscf.get_MbrDtlTnmtHst")
  if (ncol(ret)==5){
    setnames(ret,c("tDate","tName","Regular","Quick","Blitz"))
  }
  if (!nrow(ret)){
    ret <- na.data.table(c("tDate","tName","Regular","Quick","Blitz"))
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
##' @param id 
##' @return 
##' @author 
##' @examples
##' uscf.get_MbrDtlMain(30903445)
##' 
uscf.get_MbrDtlMain <- function(
  id
  ){
  ## .inUrl <- lprintf('https://www.uschess.org/msa/MbrDtlMain.php?%(id)s')
  .inUrl <- sprintf('https://www.uschess.org/msa/MbrDtlMain.php?%s',id)
  tmp <- html_to_table(
    inUrl=.inUrl,
    pattern='<table border="0" cellspacing="9">.*</table>'
    )
  ret <- tmp[!grepl("Tournament directors should use the appropriate",X1)]
  ret <- df.rm_na(ret,col.na.rm=TRUE)  
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param id 
##' @param tType 
##' @return 
##' @author 
##' @examples
##' uscf.get_tnmt.latest(30903445)
##' 
uscf.get_tnmt.latest <- function(
  id,
  tType=uscf.get_DEFAULT__tType()
  ){
  tType <- match.arg(tType)
  if (! tType %in% c("Correspondence")){
    tmp <- uscf.get_MbrDtlTnmtHst(id)
    ret <- tmp[!is.na(tmp[[tType]])]
    ret <- ret[!grepl("ONL",ret[[tType]])]
    if(!nrow(ret)){
      ret <- na.data.table(colnames(tmp))
    }
    ret <- ret[1]
    ret[,Rating:=gsub("^([0-9]+)[^=]* => ([0-9]+)$", "\\2", ret[[tType]])]
    ret[,Rating:=as.numeric(Rating)]
    ret <- ret[,c("Rating",tType,"tDate","tName"),with=FALSE]
  } else {
    .tnmt.main <- uscf.get_MbrDtlMain(id=id)
    ret <- .tnmt.main[grepl("^Correspondence Rating",X1)]$X2
    ret <- as.numeric(ret)
    ret <- data.table("Rating"=ret)
  }
  return(ret)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param id 
##' @param tType 
##' @return 
##' @author
##' @examples
##' uscf.get_rating.latest(30903445)
##' uscf.get_rating.latest(30903445,tType="Correspondence")
##' 
uscf.get_rating.latest <- function(
  id,
  tType=uscf.get_DEFAULT__tType()
  ){
  tmp <- uscf.get_tnmt.latest(
    id=id,
    tType=tType
    )
  ret <- tmp$Rating
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

uscf.get_top100.url.main <- function(
  ){
  "https://www.uschess.org/component/option,com_top_players/"
}


get_fpath.latest <- function(
  x
  ){
  if (!length(x)){
    x <- NA_character_
  }
  x <- x[!is.na(x)]
  if (length(x)){
    tmp <- file.info(x,full.names=TRUE)
    ## ## printme(tmp)
    ret <- rownames(tmp)[which.max(tmp$mtime)]
  } else {
    ret <- NA_character_
  }
  return(ret)
}

get_DEFAULT__date.type <- function(
  ){
  c("today","latest.csv","prev","update","yesterday")
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param section 
##' @param date.type 
##' @return 
##' @author
##' @examples
##' .uscf.get_top100.outFpath(section="Age 7 and Under")
##' .uscf.get_top100.outFpath(section="Overall")
##' 
.uscf.get_top100.outFpath <- function(
  section=NULL,
  date.type=get_DEFAULT__date.type()
  ){
  date.type <- match.arg(date.type)
  .section <- section
  .section <- gsub('\\^','',.section)
  .section <- gsub('[ ]+','',.section)
  if (.section %in% c("UnderAge21")){
    .section <- "Age21andUnder"
  }
  if (.section %in% c("GirlsUnder21")){
    .section <- "GirlsAge21andUnder"
  }

  .date <- switch(
    date.type,
    "today"=Sys.Date(),
    "latest.csv"="latest.csv",
    "update"="update",
    "yesterday"=Sys.Date()-1,
    "prev"="*"
    )
  ## ## ret <- lprintf('Top100_%(.section)s_%(.date)s.txt')
  if (!grepl("\\.csv",.date)){
    ret <- sprintf('Top100_%s_%s.txt',.section,.date)
  } else {
    ret <- sprintf('Top100_%s_%s',.section,.date)
  }
  return(ret)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param ... 
##' @return 
##' @author 
##' @examples
##' .uscf.get_top100.outFpath.prev(section="Age 7 and Under")
##' .uscf.get_top100.outFpath.prev(section="Overall")
##' 
.uscf.get_top100.outFpath.prev <- function(
  section=NULL
  ){  
  .section <- section
  x.today <- .uscf.get_top100.outFpath(section=.section,date.type="today")
  x.update <- .uscf.get_top100.outFpath(section=.section,date.type="update")
  x.prev <- .uscf.get_top100.outFpath(section=.section,date.type="prev")
  x.prev <- Sys.glob(x.prev)
  ## ## printme(x.prev,".uscf.get_top100.outFpath.prev")
  x.prev <- x.prev[!x.prev %in% c(x.today,x.update)]
  ret <- get_fpath.latest(x.prev)
  return(ret)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param section 
##' @param date.type 
##' @return 
##' @author 
##' @examples
##' uscf.get_top100.outFpath(section="Age 7 and Under")
##' uscf.get_top100.outFpath(section="Age 7 and Under",date.type="prev")
##' uscf.get_top100.outFpath(section="Age 7 and Under",date.type="update")
##' 
uscf.get_top100.outFpath <- function(
  section=NULL,
  date.type=get_DEFAULT__date.type()
  ){
  date.type <- match.arg(date.type)
  
  if (date.type %in% c("prev")){
    ret <- .uscf.get_top100.outFpath.prev(section=section)
  } else {
    ret <- .uscf.get_top100.outFpath(section=section,date.type=date.type)
  }  
  return(ret)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param inUrl 
##' @return 
##' @author 
##' @examples
##' uscf.get_top100.main()
##' 
uscf.get_top100.main <- function(
  inUrl=uscf.get_top100.url.main()
  ){
  pattern="<li>.*?</li>";href.keep=TRUE;href.base="https://www.uschess.org";
  tmp <- html_to_table(
    inUrl=inUrl,
    pattern=pattern,
    href.keep=href.keep,
    href.base=href.base
    )
  ret <- tmp
  ret <- ret[grepl("https://www.uschess.org//component/option,com_top_players",X1)]
  ret <- ret[,"X1",with=FALSE]
  ret[,url:=unlist(stringr::str_extract_all(ret$X1,'"https://[^"]*"'))]
  ret[,section:=mapply(function(.url,.x){ret <- gsub(.url,"",.x,fixed=TRUE)},url,X1)]
  ret[,url:=strip(url,'"')]
  ret[,section:=trimws(section)]
  ret <- df.rm_na(ret,col.na.rm=TRUE)  
  return(ret)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param inUrl 
##' @param section 
##' @return 
##' @author
##' @examples
##' uscf.get_top100.url(section="Age 7 and Under")
##' 
uscf.get_top100.url <- function(
  section=NULL
  ){
  .section <- section
  tmp <- uscf.get_top100.main()
  ret <- tmp[grepl(.section,section)][1]$url
  return(ret)
}

uscf.get_top100.section.list <- function(
  ){
  tmp <- uscf.get_top100.main()
  ret <- tmp$section
  ret <- ret[!grepl("Blitz",ret)]
  ret <- ret[!grepl("Quick",ret)]
  ret <- ret[!grepl("Online",ret)]
  ret <- ret[!grepl("regardless",ret)]
  return(ret)  
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param section 
##' @return 
##' @author
##' @examples
##'
##' uscf.get_top100.table(section="Age 7 and Under")
##' uscf.get_top100.table(section="Overall")
##' uscf.get_top100.table(section="Correspondence Players")
##' 
uscf.get_top100.table <- function(
  section=NULL
  ){
  .section <- section
  inUrl <- uscf.get_top100.url(
    section=.section
    )
  pattern="<table>.*?</table>"
  tmp <- html_to_table(inUrl=inUrl,pattern=pattern)
  ## printme(tmp,"uscf.get_top100.table")

  tmp <- df.rm_na(tmp,row.na.rm=TRUE,col.na.rm=TRUE)
  ret <- copy(tmp)
  if (grepl("\\bAge\\b",section,ignore.case=TRUE) | ncol(ret)==6){
    .col.names <- c("No.","Name_ID","Age","State","Nation","Rating")
  } else {
    .col.names <- c("No.","Name_ID","State","Nation","Rating")
  }
  setnames(ret,.col.names)
  
  Name_ID.p <- "^(.+) \\(([0-9]+)\\)$"
  ret[,Name:=gsub(Name_ID.p,"\\1",Name_ID)]
  ret[,ID:=gsub(Name_ID.p,"\\2",Name_ID)]
  .col.names2 <- unlist(strsplit(.col.names,"_"))
  ret <- ret[,.col.names2,with=FALSE]
  return(ret)
}



## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------


.uscf.get_top100.rating.change <- function(
  x,
  WHICH.COL__prev,
  WHICH.COL__next  
  ){
  
  ## -- ##
  ret <- copy(x)
  .col.names0 <- colnames(x)
  ## ## print(head(x));print(list(WHICH.COL__prev=WHICH.COL__prev,WHICH.COL__next=WHICH.COL__next));

  ## -- ##
  stop.not.all.in.vector(WHICH.COL__prev,.col.names0)
  stop.not.all.in.vector(WHICH.COL__next,.col.names0)
  setnames(ret,WHICH.COL__prev,"x.prev")
  setnames(ret,WHICH.COL__next,"x.next")
  
  ## -- ##
  ret[,x.prev:=as.numeric(x.prev)]
  ret[,x.next:=as.numeric(x.next)]
  ret[,x.change:=NA_character_]
  ret[,x.change:=signchar(x.next-x.prev)]
  ret[,x.change:=paste0("(",x.change,abs(x.next-x.prev),")")]
  ret[x.change %in% c("(0)"),x.change:=""]

  ## -- ##
  .col.names1 <- colnames(ret)
  .col.which.prev <- which(.col.names1=="x.prev")
  .col.which.next <- which(.col.names1=="x.next")
  .col.names <- .col.names1
  .col.names <- .col.names[seq_len(.col.which.next-1)]
  .col.names <- .col.names[!.col.names %in% c("x.prev","x.next","x.change")]
  .col.names <- unique(c(.col.names,"x.prev","x.next","x.change",.col.names1))
  ret <- ret[,.col.names,with=FALSE]
  
  ## -- ##
  setnames(ret,"x.prev",WHICH.COL__prev)
  setnames(ret,"x.next",WHICH.COL__next)
  if (hasName(ret,"(+/-)")){
    ret[,`(+/-)`:=NULL]
  }
  setnames(ret,"x.change","(+/-)")

  return(ret)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param pcThreads 
##' @param outFpath 
##' @return 
##' @author
##' 
.uscf.get_top100.table.new <- function(
  section=NULL,
  pcThreads=1,
  outFpath=NULL,
  outFpath.csv=NULL
  ){

  ## -- ##
  .section <- section
  .tType <- uscf.section_to_tType(.section)
  if (!length(outFpath)){
    outFpath <- uscf.get_top100.outFpath(section=.section,date.type="today")
  }
  if (!length(outFpath.csv)){
    outFpath.csv <- uscf.get_top100.outFpath(section=.section,date.type="latest.csv")
  }
  
  ## -- ##
  inData <- uscf.get_top100.table(section=.section)## [1:6]
  id_v <- inData$ID
  
  ## -- ##
  options(width=200)
  
  ## -- ##
  .func_per_id.OLD <- function(#only working with single core at command line 
    id,
    tType
    ){
    tryCatch(
      {
        ret <- uscf.get_tnmt.latest(id=id,tType=tType)
        return(ret)
      },
      error=function(e){
        return(e$message)
      })
  }

  .func_per_id <- function(
    id,
    tType
    ){
    uscf.get_tnmt.latest(id=id,tType=tType)
  }

  
  
  ## -- ##
  .msg <- 'ERROR: Possible error with multiple cores, try single core instead: pcThreads=1'
  ## "CKR_DEVICE_ERROR", "Error in readLines", ..
  cat("\n")
  tmp <- parallel::mclapply(
    seq_len(length(id_v)),
    function(i){
      if (pcThreads==1){
        svMisc::progress(i);
      }
      ret <- .func_per_id(id=id_v[i],tType=.tType)
      Sys.sleep(0.01);
      if (i==length(id_v)){
        cat("\n")
      };
      return(ret)
    },
    mc.cores=pcThreads
    )
  tmp <- do.call(rbind,tmp)
  tmp <- as.data.table(tmp)
  if (any(grepl("A PKCS #11 module returned CKR_DEVICE_ERROR",tmp[[1]])) | any(grepl("cannot open the connection to",tmp[[1]]))){
    print(head(tmp))
    stop(.msg)
  }
  ## print(head(tmp))
  setnames(tmp,"Rating","Rating.new")
  
  ## -- ##
  ret <- copy(inData)
  ret <- cbind(ret,tmp[,c("Rating.new"),with=FALSE])
  
  ret <- .uscf.get_top100.rating.change(
    x=ret,
    WHICH.COL__prev="Rating",
    WHICH.COL__next="Rating.new"
    )
  ret <- cbind(ret,tmp[,colnames(tmp)[!colnames(tmp) %in% c("Rating.new")],with=FALSE])
  
  ## -- ##
  ret <- ret[order(-as.numeric(Rating.new))]
  print(head(ret))

  ## -- ##
  attr(ret,"outFpath") <- outFpath
  write.table(file=outFpath,ret,row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)

  ## -- ##
  write.file(outFpath.csv,sprintf("## Last Updated: %s",get_TIMENOW()))
  suppressWarnings(write.table(file=outFpath.csv,ret,row.names=FALSE,col.names=TRUE,sep=",",quote=TRUE,append=TRUE))

  ## -- ##
  invisible(ret)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param section 
##' @param ... 
##' @return 
##' @author 
##' @examples
##' uscf.get_top100.table.new(section="Age 7 and Under",pcThreads=12);
##' uscf.get_top100.table.new(section="Overall",pcThreads=12);
##' uscf.get_top100.table.new(section="Women",pcThreads=12);
##' uscf.get_top100.table.new(section="Correspondence Players",pcThreads=12);
##' 
uscf.get_top100.table.new <- function(
  section=NULL,
  ...
  ){
  for (.isection in section){
    cat(.isection,"\n")
    .uscf.get_top100.table.new(
      section=.isection,
      ...
      )
  }
  invisible()
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param section 
##' @param ... 
##' @return 
##' @author
##' @examples
##' 
##' .uscf.get_top100.table.update(section="Age 7 and Under");
##' 
.uscf.get_top100.table.update <- function(
  section=NULL
  ){
  ## -- ##
  outFpath.prev <- uscf.get_top100.outFpath(section=section,date.type="prev")
  outFpath.today <- uscf.get_top100.outFpath(section=section,date.type="today")
  outFpath.update <- uscf.get_top100.outFpath(section=section,date.type="update")
  ## ## catme(outFpath.prev)
  ## ## catme(outFpath.today)
  ## ## catme(outFpath.update)
  
  
  ## -- ##
  if (is.na(outFpath.today)){
    stop("Invalid outFpath.today: ",outFpath.today)
  }
  tmp.today <- read.table(outFpath.today,header=TRUE,sep="\t",quote='',stringsAsFactors=FALSE,check.names=FALSE)
  setDT(tmp.today)
  
  ## -- ##
  options(width=200)
  
  ## -- ##
  if (!is.na(outFpath.prev)){
    tmp.prev <- read.table(outFpath.prev,header=TRUE,sep="\t",quote='',stringsAsFactors=FALSE,check.names=FALSE)
    setDT(tmp.prev)
    tmp.prev <- tmp.prev[,c("ID","Rating.new"),with=FALSE]
  } else {
    tmp.prev <- tmp.today[,c("ID","Rating.new"),with=FALSE]
  }
  setnames(tmp.prev,"Rating.new","Rating.prev")
  
  ## -- ##
  ret <- copy(tmp.today)
  ret <- df.add_col(ret,tmp.prev[,c("ID","Rating.prev"),with=FALSE],by="ID",sort=FALSE,all.x=TRUE)


  ret <- .uscf.get_top100.rating.change(
    x=ret,
    WHICH.COL__prev="Rating.prev",
    WHICH.COL__next="Rating.new"
    )
  
  ## -- ##
  ret <- ret[order(-as.numeric(Rating.new))]
  print(head(ret))
  
  ## - ##
  attr(ret,"outFpath.update") <- outFpath.update
  write.table(file=outFpath.update,ret,row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
  invisible(ret)
}


uscf.get_top100.table.update <- function(
  section=NULL,
  ...
  ){
  for (.isection in section){
    cat(.isection,"\n")
    .uscf.get_top100.table.update(
      section=.isection,
      ...
      )
  }
  invisible()
}

## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

uscf.main <- function(
  pcThreads=1
  ){
  ## setwd("~/ESSDIR/Project/Self/data/uscf/docs");
  
  ## section_v <- uscf.get_top100.section.list()
  section_v <- "Age 7 and Under"
  
  uscf.get_top100.table.new(section=section_v,pcThreads=pcThreads)
  ## uscf.get_top100.table.update(section=section_v)
  
}
