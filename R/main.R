library(twitteR)
library(rotl)
library(rgbif)
library(maps)
library(rdryad)
library(treebase)
source('.secret.R') #includes authorization
#setup_twitter_oauth(consumer.key, consumer.secret)
my.timeline <- userTimeline("phylobot", n=1)
max.ID <- as.data.frame(my.timeline[[1]])$id

ProcessTweet <- function(x) {
	tweet.df <- as.data.frame(x)
	print(Sys.time())
	print(tweet.df)
	if(grepl('#icanhaztree', tweet.df$text, ignore.case=TRUE) | grepl('#tree', tweet.df$text, ignore.case=TRUE)) {
		ProcessICanHazTree(tweet.df)
	} else if(grepl('#icanhazncbi', tweet.df$text, ignore.case=TRUE) | grepl('#ncbi', tweet.df$text, ignore.case=TRUE)) {
		ProcessICanHazNCBI(tweet.df)
	} else if(grepl('#icanhazeol', tweet.df$text, ignore.case=TRUE) | grepl('#eol', tweet.df$text, ignore.case=TRUE)) {
		ProcessICanHazEOL(tweet.df)
	} else if(grepl('#icanhazmap', tweet.df$text, ignore.case=TRUE) | grepl('#map', tweet.df$text, ignore.case=TRUE)) {
                ProcessICanHazMap(tweet.df)
  } else if(grepl('#icanhazjob', tweet.df$text, ignore.case=TRUE) | grepl('#job', tweet.df$text, ignore.case=TRUE)) {
		return.tweet <- paste('@', tweet.df$screenName, ' see jobs at https://docs.google.com/spreadsheets/d/1XcEuPa7YPSHaw11OB9sPKZfQ8ZCYrdiMKzQZC8QSN7s/edit#gid=1240849863, @evoldir, etc.', sep="")
               updateStatus(return.tweet, inReplyTo=tweet.df$replyToSID, bypassCharLimit=TRUE)
	} else {
		#some sort of failover: perhaps talk to hubot
	}
	Sys.sleep(5)
	return(as.numeric(tweet.df$id))
}

#https://tree.opentreeoflife.org/opentree/argus/ottol@5342306/Anolis
ProcessICanHazTree <- function(x) {
	if(grepl('doi', x$text, ignore.case=TRUE)) {
		#first open tree, then dryad, then treebase
		#studies_find_studies(property="ot:studyPublication", value="http://dx.doi.org/10.1600/036364411X605092")
		ot.result <- NULL
		raw.doi <- gsub("doi:*\\s*", 'http://dx.doi.org/', x$text)
		raw.doi <- gsub(".*#icanhaztree", "", raw.doi)
		raw.doi <- gsub(".*#tree", "", raw.doi)
		raw.doi <- gsub("\\s+","", raw.doi)
		ot.returntweet <- NULL
		dryad.returntweet <- NULL
		treebase.returntweet <- NULL
		try(ot.result <- rotl::studies_find_studies(property="ot:studyPublication", value=raw.doi))
		if(!is.null(ot.result)) {
			study.info <- NULL
			try(study.info <- get_publication(get_study_meta(ot.result$study_ids))[1])
			return.url <- paste0('https://tree.opentreeoflife.org/curator/study/view/', ot.result$study_ids[1], '/?tab=metadata')
			return.tweet <- paste('@', x$screenName, " trees at ", return.url, " from @opentreeoflife via @phylotastic", ifelse(is.null(study.info),"", paste0(" from ", study.info)), sep="")
			ot.returntweet <- strtrim(return.tweet, 280)
			updateStatus(ot.returntweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)

		}
		dryad.result <- NULL
		try(dryad.result <- rdryad::d_solr_search(q=paste0("dc.relation.isreferencedby:", raw.doi) ,
   fl="dc.identifier"))
	 	if(!is.null(dryad.result)) {
			if(nrow(dryad.result>0)) {
				dryad.id <- strsplit(as.data.frame(dryad.result, stringsAsFactors=FALSE)[1,1], ',')[[1]][1]
				return.url <- paste0('http://datadryad.org/resource/', dryad.id)
				dryad.returntweet <- paste0('@', x$screenName, " data (perhaps not trees) at ", return.url, " from @datadryad via @phylotastic")
				updateStatus(dryad.returntweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)
			}
		}
		treebase.result <- NULL
		#try(treebase.result <- search_treebase(raw.doi, by="doi"))
		# Note: this will fail due to https://github.com/ropensci/treeBASE/issues/15

	} else {
		substring <- gsub(".*#icanhaztree ", "", x$text, ignore.case=TRUE)
		substring <- gsub(".*#tree ", "", substring, ignore.case=TRUE)
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
	substring <- gsub(" ",'+', gsub(".*#ncbi ", "", substring, ignore.case=TRUE))
	return.url <- paste('http://www.ncbi.nlm.nih.gov/nuccore/?term=', substring, sep="")
	return.tweet <- paste('@', x$screenName, " ", return.url, " via @phylotastic", sep="")
	updateStatus(return.tweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)
}

#http://eol.org/search?q=anolis+carolinensis&show_all=true
ProcessICanHazEOL <- function(x) {
	substring <- gsub(" ",'+', gsub(".*#icanhazeol ", "", x$text, ignore.case=TRUE))
	substring <- gsub(" ",'+', gsub(".*#eol ", "", substring, ignore.case=TRUE))
  return.url <- paste('http://eol.org/search?q=', substring, '&search=Go', sep="")
	return.tweet <- paste('@', x$screenName, " ", return.url, " from @eol via @phylotastic", sep="")
	updateStatus(return.tweet, inReplyTo=x$replyToSID, bypassCharLimit=TRUE)
}

ProcessICanHazMap <- function(x, limit=2000) {
	substring <- gsub(".*#icanhazmap ", "", x$text, ignore.case=TRUE)
	substring <- gsub(".*#map ", "", substring, ignore.case=TRUE)
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
