library(twitteR)
library(rotl)
library(rgbif)
library(maps)
source('.secret.R')
setup_twitter_oauth(consumer.key, consumer.secret)
my.timeline <- userTimeline("phylobot", n=1)
max.ID <- as.data.frame(my.timeline[[1]])$id

ProcessTweet <- function(x) {
	tweet.df <- as.data.frame(x)
	print(Sys.time())
	print(tweet.df)
	if(grepl('#icanhaztree', tweet.df$text, ignore.case=TRUE)) {
		ProcessICanHazTree(tweet.df)
	} else if(grepl('#icanhazncbi', tweet.df$text, ignore.case=TRUE)) {
		ProcessICanHazNCBI(tweet.df)
	} else if(grepl('#icanhazeol', tweet.df$text, ignore.case=TRUE)) {
		ProcessICanHazEOL(tweet.df)
	} else if(grepl('#icanhazmap', tweet.df$text, ignore.case=TRUE)) {
                ProcessICanHazMap(tweet.df)
        } else if(grepl('#icanhazjob', tweet.df$text, ignore.case=TRUE)) {
		return.tweet <- paste('@', tweet.df$screenName, ' see jobs at https://sites.google.com/site/wikibiologypostings/home/2015-2016-wiki-biology-jobs, @evoldir, etc.', sep="")
               updateStatus(return.tweet, inReplyTo=tweet.df$replyToSID, bypassCharLimit=TRUE)
	} else {
		#some sort of failover: perhaps talk to hubot	
	}
	Sys.sleep(5)
	return(as.numeric(tweet.df$id))
}

#https://tree.opentreeoflife.org/opentree/argus/ottol@5342306/Anolis
ProcessICanHazTree <- function(x) {
	if(grepl('doi:', x$text, ignore.case=TRUE)) {
#handle DOI
	} else {		
		substring <- gsub(".*#icanhaztree ", "", x$text, ignore.case=TRUE)
		tnrs.result <- NULL
		try(tnrs.result <- tnrs_match_names(names=substring))
		if(!is.null(tnrs.result)) {
			if(dim(tnrs.result)[1]>0) {
				ott.id <- tnrs.result[1,]$ott_id
				return.url <- paste('https://tree.opentreeoflife.org/opentree/argus/ottol@',ott.id, '/', tnrs.result$unique_name, sep="")
				return.tweet <- paste('@', x$screenName, " ", return.url, " from @opentreeoflife via @phylotastic", sep="")
				updateStatus(return.tweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)
			}
		} else {
				
		}
	}
}

#http://www.ncbi.nlm.nih.gov/nuccore/?term=anolis+carolinensis
ProcessICanHazNCBI <- function(x) {
	substring <- gsub(" ",'+', gsub(".*#icanhazncbi ", "", x$text, ignore.case=TRUE))
	return.url <- paste('http://www.ncbi.nlm.nih.gov/nuccore/?term=', substring, sep="")
	return.tweet <- paste('@', x$screenName, " ", return.url, " via @phylotastic", sep="")
	updateStatus(return.tweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)
}

#http://eol.org/search?q=anolis+carolinensis&show_all=true
ProcessICanHazEOL <- function(x) {
	substring <- gsub(" ",'+', gsub(".*#icanhazeol ", "", x$text, ignore.case=TRUE))
	return.url <- paste('http://eol.org/search?q=', substring, '&search=Go', sep="")
	return.tweet <- paste('@', x$screenName, " ", return.url, " from @eol via @phylotastic", sep="")
	updateStatus(return.tweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)
}

ProcessICanHazMap <- function(x, limit=2000) {
        substring <- gsub(".*#icanhazmap ", "", x$text, ignore.case=TRUE)
        map.file <- NULL
     	key <- NULL 
       	try(key <- ((name_suggest(substring))$key)[1])
        if(!is.null(key)) {
			dat <- occ_search(taxonKey=key, return='data', limit=limit)
			dat <- dat[which(!is.na(dat$decimalLongitude)),]
			dat <- dat[which(!is.na(dat$decimalLatitude)),]
			if(dim(dat)[1]>0) {
				jpeg("phylobotmap.jpg")
				map()
				title(main=paste(substring,"(first", limit,"points only)"))
				points(dat$decimalLongitude, dat$decimalLatitude, pch=20, col=rgb(1,0,0,0.5))
				dev.off()
				map.file <- "phylobotmap.jpg"
             }
        }


        if(!is.null(map.file)) {
                return.tweet <- paste('@', x$screenName, " map from @GBIF via @phylotastic", sep="")
                updateStatus(return.tweet, inReplyTo=x$replyToSID, mediaPath=map.file, bypassCharLimit=TRUE)
        } else {
                return.tweet <- paste('@', x$screenName, " mapping failed for ", substring, sep="")
                updateStatus(return.tweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)
        }
}

while(1<2) {
        local.mentions <- mentions(sinceID = max.ID)
        for (i in sequence(length(local.mentions))) {
                max.ID=as.character(max(as.numeric(max.ID),ProcessTweet(local.mentions[[i]])))
        }
	my.timeline <- userTimeline("phylobot", n=1)
        max.ID <- as.character(max(as.numeric(max.ID), as.numeric(as.data.frame(my.timeline[[1]])$id)))
        Sys.sleep(61)
}
