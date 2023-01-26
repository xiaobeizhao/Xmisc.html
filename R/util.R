
## ************************************************************************
## 
## 
## 
## (c) Xiaobei Zhao
## 
## Mon Jan 23 21:15:40 CST 2023 -0600 (Week 04)
## 
## 
## Reference: 
## 
## 
## ************************************************************************



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param inUrl 
##' @return 
##' @author
##' @examples
##'
##' inUrl="https://www.uschess.org/msa/MbrDtlTnmtHst.php?30903445";pattern='<!-- Detail: [0-9]+  -->.*?</tr>';
##' inUrl="http://www.uschess.org/component/option,com_top_players/Itemid,371?op=list&month=2301&f=usa&l=R:Regular%20Top%20Age%207%20and%20Under.&h=Top%20Age%207%20and%20Under"; pattern="<table>.*?</table>";
##' html_to_table(inUrl=inUrl,pattern=pattern);
##'
##' inUrl="https://www.uschess.org/component/option,com_top_players/";pattern="<li>.*?</li>";href.keep=TRUE;href.base="https://www.uschess.org";
##' html_to_table(inUrl=inUrl,pattern=pattern,href.keep=href.keep,href.base=href.base);
##' 
html_to_table <- function(
  inUrl,
  pattern,
  href.keep=FALSE,
  href.base=NULL #href.base="https://www.uschess.org"
  )
{
  ## -- ##
  
  ## -- ##
  sink(file=R.utils::nullfile());
  ## doc.raw <- RCurl::getURL(inUrl)
  ## doc <- htmltidy::tidy_html(doc.raw) ## 
  doc <- suppressWarnings(suppressMessages({htmltidy::tidy_html(url(inUrl),verbose=FALSE)})) ## cannot open the connection to 'https://...
  sink()
  
  ## -- ##
  ## cat(doc)
  doc <- gsub("[\r\n]"," ",doc)
  ## doc.sub <- stringr::str_extract_all(doc,'<table width="960" border="1" valign="top" cellspacing="0" cellpadding="4">.*</table>')
  doc.sub <- stringr::str_extract_all(doc,pattern)[[1]]
  doc.sub <- gsub("<li>","<tr><td>",doc.sub)
  doc.sub <- gsub("</li>","</td></tr>",doc.sub)
  if (href.keep){
    if (length(href.base))
    doc.sub <- gsub('<a href=[ ]*"([^"]*)"[ ]*>',paste0('"',href.base,"/\\1",'"'),doc.sub)
  }
  doc.sub <- paste0(doc.sub,collapse="")

  
  if (grepl("<table>",doc.sub)){
    doc.table <- doc.sub
  } else {
    doc.table <- paste0("<table> ",doc.sub,"</table>")
  }
  `%>%` <- rvest::`%>%`
  tmp <- as.data.table(rvest::read_html(doc.table) %>% rvest::html_table(fill=TRUE))
  ret <- tmp
  ret <- apply(ret,1:2,function(e){if (is.na2(e)){e <- NA}; return(e)})
  ret <- as.data.table(ret)
  return(ret)
}


## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

