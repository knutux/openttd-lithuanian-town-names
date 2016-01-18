writeUtf8 <- function(lines, file, bom=F) {
    BOM <- charToRaw('\xEF\xBB\xBF')
    con <- file(file, "wb")
    if(bom) writeBin(BOM, con, endian="little")
    sapply(lines, function(line) {
        writeBin(charToRaw(line), con, endian = 'little')
        writeBin(charToRaw('\n'), con, endian = 'little')
    })
#    writeBin(charToRaw(paste(lines, sep='\n', collapse='')), con, endian="little")
    close(con)
}

extractTownNames <- function (fileName)
{
    library (httr)

    nextPage <- ""
    baseUrl <- 'https://lt.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Kategorija:Nepilni_(Lietuvos_gyvenviet%C4%97s)&format=json&cmtype=page&cmlimit=100'
    pageList <- list()
    cnt <- 0

    while (cnt < 1000)
        {
        cnt <- cnt + 1

        if (nextPage != "")
            {
            url <- paste (baseUrl, "&cmcontinue=", nextPage, sep='')
            }
        else
            url <- baseUrl

        response <- GET (url)
        if (is.null(response))
            stop (paste('Could not fetch from', url))
    
        json <- content(response)
        json <- jsonlite::fromJSON(jsonlite::toJSON(json))

        if (json$batchcomplete != "")
            break

        nextPage <- json$continue$cmcontinue
        pageNames <- json$query$categorymembers$title
        print (paste('fetched', length(pageNames), 'page names'))

        m <- sub(' \\(.+\\)', '', pageNames, perl=TRUE)
        pageList <- c(pageList, m)
        if (length(pageNames)< 100)
            {
            print(json)
            break
            }

        Sys.sleep(1)
    }

    townNames <- unlist(unique (pageList))

    print (paste('Writing', length(townNames), 'names to a file', fileName))

    writeUtf8(paste0('        text("', townNames, '", 1),'), fileName)

    print ('Success!')
}