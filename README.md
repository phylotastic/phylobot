# phylobot
Interactive twitter bot to field phylogenetic or related queries

Usage: tweet to @phylobot with a query

## Trees

This serves as an interface to Open Tree of Life using Phylotastic and the rotl package.

`@phylobot #tree Formica` or `@phylobot #icanhaztree Formica` will get a link to a tree of *Formica* ants

`@phylobot #tree doi:10.1111/evo.13117` will look for data or trees from that study. Note the doi format: add `doi:` and the the DOI string (no http, etc. just the string of numbers and letters).

## Maps

`@phylobot #map Formica` will give you a map of the first 2000 data points from GBIF.

## NCBI

`@phylobot #ncbi Formica` will give you a link to that taxon at NCBI.

## EOL

`@phylobot #eol Formica` will give you a link to that taxon at EOL.

## Jobs

`@phylobot #Icanhazjob` will give you a link to the Ecology and Evolution jobs wiki.

# Long term plans

This is just a prototype now and somewhat brittle. Longer term plans are to make queries more flexible, serve up chronograms from datelife, and provide info to other services. A major goal is to have it record what papers are being asked for by DOI to gauge interest and start recording which papers are desired but which have not deposited their data.

# Installation

To run, `nohup R CMD main.R > /dev/null`. You'll need a file with keys, called `.secret.R`, in the same directory as `main.R`. Its contents:

```
twitteR::setup_twitter_oauth("STRING","STRING","STRING","STRING")
```

where the STRINGs are those used to authorize the bot to tweet (something like `3ryaj5uhrjaFc1989EK`).
