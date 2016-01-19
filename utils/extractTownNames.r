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

extractNamesFromCategory <- function (category)
{
    cachedFileName = paste0(category,'.list');
    if (file.exists(cachedFileName))
        return (load(cachedFileName))
        
    baseUrl <- paste0('https://lt.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Kategorija:',category,'&format=json&cmtype=page&cmlimit=100')
    pageList <- list()
    cnt <- 0
    nextPage <- ""

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

        pageList <- c(pageList, pageNames)
        if (length(pageNames)< 100)
        {
            #print(json)
            break
        }
        
        Sys.sleep(1)
    }

    save (pageList,file=cachedFileName)
    pageList
}

extractTownNames <- function (fileName)
{
    library (httr)

    incomplete <- extractNamesFromCategory ('Nepilni_(Lietuvos_gyvenviet%C4%97s)')
    towns <- extractNamesFromCategory ('Lietuvos_miesteliai')
    pageList <- c(incomplete, towns)
    print (paste('Extracted', length(incomplete), 'village and ', length(towns), 'town names. Total -', length(incomplete)+length(towns)))
    pageList <- sub(' \\(.+\\)', '', pageList, perl=TRUE)
    pageList <- sub(' [I]+$', '', pageList, perl=TRUE)
    pageList <- sub('u GS$', 'ai', pageList, perl=TRUE)
    pageList <- sub('es GS$', 'e', pageList, perl=TRUE)
    pageList <- sub('io GS$', 'is', pageList, perl=TRUE)
    pageList <- sub('o GS$', 'as', pageList, perl=TRUE)
    
    townNames <- unlist(unique (pageList))

    print (paste('Writing', length(townNames), 'names to a file', fileName))

    writeUtf8(paste0('        text("', townNames, '", 1),'), fileName)

    print ('Success!')
}