#' Predict gender using name word and character n-grams
#'
#' This function pulls account information from the Twitter API. It is adapted from code authored by Dr. Sean Fitzhugh and Dr. Emma Spiro
#' @param users A vector of Twitter user ID values or screen names 
#' @param is.ID TRUE if your input is ID values. FALSE if your input is screen names
#' @param bulk TRUE if inputting more than one screen name or ID
#' @param credential Your API authentication token
#' @param rawdata Whether you want to store the raw .json output
#' @param datadir Where you would like hte raw .json output to be stored
#' @keywords twitter 
#' @return res A data frame containing detailed profile metadata
#' @export
#' @examples 
#' getUserInfo(df$id_str, credential=user.signature, rawdata=FALSE, datadir="", is_id=TRUE)


getUserInfo<-function(users, is.ID=TRUE, bulk=TRUE, credential=NULL, rawdata=FALSE, datadir=""){
  require(httr)
  if (bulk==TRUE){
    ui.base.url <- "https://api.twitter.com/1.1/users/lookup.json?"
  } else {
    ui.base.url <- "https://api.twitter.com/1.1/users/show.json?"
  }
  
  if (is.ID==TRUE){  # MODDED THIS LINE 11/3/16
    ui.base.url <- paste(ui.base.url, "user_id=", sep="")
  } else {
    ui.base.url <- paste(ui.base.url, "screen_name=", sep="")
  }
  # THIS DOES NOT MONITOR RATE LIMITES PLEASE DO THAT FROM BASH SCRIPT
  tmp <- GET(url = paste(ui.base.url,paste(users, collapse = ","),sep=""), config=credential)
  dat <- httr::content(tmp, as ="text") 
  
  # save raw JSON
  if (rawdata){
    tim <- Sys.time()
    tim <- gsub(" ","_",tim)
    write(dat, file=paste(datadir,"/userdata/",tim,".JSON",sep=""))
  }
  
  # Parse json
  dat <- RJSONIO::fromJSON(dat)
  
  if (bulk){
    if (tmp$status==200){ # everything ok
      res <- do.call(rbind, lapply(dat, parseUserObj, timeline=FALSE))
      note <- rep("", nrow(res))
      res <- cbind(res, note)
      u <- unlist(strsplit(users, ","))
      if (is.ID){
        u <- u[which(u%in%tolower(as.character(res$id_str))==FALSE)]
        m <- matrix(NA, nr=length(u), ncol=ncol(res))
        colnames(m) <- colnames(res)
        m[,"id_str"]=u
        m[,"note"] <- rep("user not found")
      } else {
        u <- u[which(u%in%tolower(as.character(res$screen_name))==FALSE)]
        m <- matrix(NA, nr=length(u), ncol=ncol(res))
        colnames(m) <- colnames(res)
        m[,"screen_name"]=u
        m[,"note"] <- rep("user not found")
      }
      res <- rbind(res, m)
    } else {
      res <- "Users not found."
    }
    # need to adjust for users that were not found
    
  } else {
    if (tmp$status==200){
      res <- parseUserObj(dat)
    } else {
      res <- "Users not found."
    }
  }
  # clean some of the text elements so as not to mess up the db
  res$description <- cleanText(res$description)
  res$laststatus_text <- cleanText(res$laststatus_text)
  rownames(res) <- NULL
  return(res)
}
