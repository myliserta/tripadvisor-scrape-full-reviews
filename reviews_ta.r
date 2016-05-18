#load the libraries
libs <- c("RSelenium", "dplyr","plyr","rvest","magrittr","methods")
lapply(libs,require,character.only=TRUE)

#start selenium server (download the standalone server inside the working directory)
startServer()

#gives time for the server to open up
Sys.sleep(2)

#setup the browser type and open up the browser. download phantomjs, you can also use firefox
mybrowser <- remoteDriver$new(browserName="phantomjs")
mybrowser$open()

#load the links of the hotels (you need to change hotels_to_load with your file). be sure the links are under the "link" column.
df <- read.csv('hotels_to_load.csv')
for (m in 1:length(df$link)){ 
  links <- sapply(df$link[m],as.character) 
 
#remove duplicates if any
  links <- unique(links)

#manipulate the URL to extract information
  splitlinks <- strsplit(links, "-Reviews-")
  linksframe <- data.frame(do.call(rbind,splitlinks)) 
  url_0 <- links  
  lefturl <- linksframe$X1
  lefturl <- paste(lefturl,'-Reviews-',sep='')
  righturl<- linksframe$X2
  righturl_no_html <- gsub('.html','',righturl)

#browse the url
  mybrowser$navigate(url_0)
  

#get the number of pages (to loop through) containing the reviews  
reviews_no <- url_0 %>% 
  read_html() %>% 
  html_nodes("#PAGE")
  
howmanypages <- reviews_no %>%
  html_nodes(".language") %>%
  html_nodes("ul>li>label span") %>%
  html_text(trim=TRUE) %>%
  as.character()
  
 howmanypages <- round_any(as.numeric(gsub("\\(|\\,|\\)","",howmanypages[[1]])),10,f=floor)
  
  
#initialize variables
tripadvisor <- NULL 
list <- NULL
lp_th_rw <- c("")

#loop through and get the reviews
  for (i in seq(10,howmanypages,10)){
    x<-paste("or",i,"-",sep="")
    lp_th_rw <- c(lp_th_rw,x)}
  
  
  for(b in (lp_th_rw)){
    list[b] <- paste(lefturl,b,righturl,'#REVIEWS',sep="")}
  
  url<-unlist(list)
  for(u in 1:length(url)){
    mybrowser$navigate(url[[u]])
    
    count <- c(1, 6)
    
    for (j in count){
      clickfulltext <- mybrowser$findElements(using = 'css selector', value='span.noQuotes')

      if( length(clickfulltext) <= j )
      {clickfulltext[[j]]$clickElement()}
      else {break}
      
      url2 <- mybrowser$getCurrentUrl()
      url2 <- gsub("[[1]] ", "", url2, fixed=TRUE)
      
      reviews <- url2 %>% 
        read_html() %>% 
        html_nodes("#REVIEWS .review")
      
      
      id <- reviews %>%
        html_nodes(".entry p") %>%
        html_attr("id")
      id <- gsub("review_","",id)
      
      quote <- reviews %>%
        html_nodes(".quote") %>%
        html_text(trim=TRUE)
      quote <- iconv(quote,to="ASCII//TRANSLIT")
      quote <- gsub("\"","",quote)
      
      rating <- reviews %>%
        html_nodes(".rating_s_fill") %>%
        html_attr("alt") %>%
        gsub(" of 5 stars", "", .) %>%
        as.integer()

      date <- reviews %>%
        html_nodes(".rating .ratingDate") %>%
        html_text(trim=TRUE)%>%
        as.character()

      date <- gsub("\\,|\n|Reviewed |NEW", "", date)
 
      
      member <- lapply(reviews, function(pn) {
        pn %>% html_node(".username") %>% html_text(trim=TRUE) %>%
          ifelse(identical(., character(0)), NA, .)
      })
      member <- unlist(member)
        
      review <- reviews %>%
        html_nodes(".entry") %>%
        html_text(trim=TRUE) %>%
        as.character()
      
      rowthing <- data.frame(id, member, quote, date, rating, review, stringsAsFactors = FALSE)
      rowthing <- as.data.frame(rowthing)
      tripadvisor<-rbind(rowthing, tripadvisor)
      tripadvisor <- unique(tripadvisor)
      mybrowser$goBack()}
  }
  tripadvisor <- cbind(hotel_name = righturl_no_html,tripadvisor)
  
  
  save(tripadvisor, file = paste(righturl_no_html, ".rda",sep=""))
  write.csv(tripadvisor, file = paste(righturl_no_html, ".csv",sep=""))
  rm(list= ls()[!(ls() %in% c('mybrowser','df'))])

}

rm(list=ls())
