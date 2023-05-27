if (is.R()) {
    .myseqalong <- match.fun("seq_along")
    .mymatchfun <- match.fun



    # Subtracts time2 from time1.
    # If both are of class POSIXt, gives the result in hours.
    difftime.default <- function(time1, time2) {
        if (length(time1) != length(time2)) {
            stop("Length of time1 and time2 should match.")
        }
        if (inherits(time1, "POSIXt") && inherits(time2, "POSIXt")) {
            as.double(difftime(time1, time2), units="hours")
        } else if (inherits(time1, "Date") && inherits(time2, "Date")) {
            as.numeric(time1 - time2) * 24
        } else if (is.numeric(time1) && is.numeric(time2)) {
            time1 - time2
        } else {
            stop("Cannot subtract times that are not either numeric or of class POSIXt or Date.")
        }
    }

    difftime.hours <- function(time1, time2) {
        if (length(time1) != length(time2)) {
            stop("Length of time1 and time2 should match.")
        }
        if (!(inherits(time1, "POSIXt") && inherits(time2, "POSIXt"))) {
            stop("Both time1 and time2 should be POSIXt objects")
        }
        as.double(difftime(time1, time2), units="hours")
    }


    difftime.days <- function(time1, time2) {
        if (length(time1) != length(time2)) {
            stop("Length of time1 and time2 should match.")
        }
        if (!((inherits(time1, "POSIXt") || (inherits(time1, "Date"))) && (inherits(time2, "POSIXt")) || inherits(time2, "Date"))) {
            stop("Both time1 and time2 should be Date or POSIXt objects")
        }
        if (inherits(time1, "POSIXt")) {
            time1 <- as.Date(strftime(time1, "%Y-%m-%d"))  # Drop the time
        }
        if (inherits(time2, "POSIXt")) {
            time2 <- as.Date(strftime(time2, "%Y-%m-%d"))  # Drop the time
        }
        as.integer(as.double(difftime(time1, time2), units="days"))
    }

    age.in.years <- function(on.this.date, birth.date) {
        this.yr <- as.numeric(format(on.this.date, "%Y"))
        birth.yr <- as.numeric(format(birth.date, "%Y"))
        elap.yr <- this.yr - birth.yr

        bday.this.year <- as.Date(paste(this.yr, format(birth.date, "%m-%d"), sep="-"))

        as.integer(ifelse(on.this.date < bday.this.year, elap.yr - 1, elap.yr))
    }


    write.nonmem.csv <- function(x, file, digits=6, colnames.toupper=TRUE) {
        if (!is.null(digits)) {
            for (nm in names(x)) {
                if (is.numeric(x[[nm]])) {
                    x[[nm]] <- round(x[[nm]], digits)
                }
            }
        }
        for (nm in names(x)) {
            if (inherits(x[[nm]], "POSIXt")) {
                x[[nm]] <- strftime(x[[nm]], format="%Y-%m-%dT%H:%M:%S")
            }
        }
        if (substring(names(x)[1], 1, 1) != "#") {
            names(x)[1] <- paste("#", names(x)[1], sep="")
        }
        if (colnames.toupper) {
            names(x) <- toupper(names(x))
        }
        write.csv(x, file=file, quote=FALSE, row.names=FALSE, na=".")
    }

    read.nonmem.csv <- function(file, colnames.tolower=TRUE) {
        x <- read.csv(file=file, na.strings=c(".", "", "NA"))
        # Remove the leading '#' (comment character in NONMEM).
        # Note: R converts the '#' to 'X.'
        if (grepl("^X\\.", names(x)[1])) {
            names(x)[1] <- substring(names(x)[1], 3)
        }
        if (colnames.tolower) {
            names(x) <- tolower(names(x))
        }
        x
    }

    read.nonmem.table <- function(file, colnames.tolower=TRUE) {
        skip <- 1          # Always skip one line
        header <- TRUE     # Always read header
        x <- read.table(file, skip=skip, header=header, check.names=FALSE, na.strings=c(".", ""))
        if (colnames.tolower) {
            names(x) <- tolower(names(x))
        }
        if (!is.null(x$id)) {
            x$id <- asID(x$id)
        }
        x
    }

} else {
    # Assume S-PLUS
    .myseqalong <- function(along.with) {seq(along.with)}
    .mymatchfun <- function(FUN) {if (is.character(FUN)) getFunction(FUN) else FUN}



    # Subtracts time2 from time1.
    # If both are of class timeDate, gives the result in hours.
    difftime.default <- function(time1, time2) {
        if (length(time1) != length(time2)) {
            stop("Length of time1 and time2 should match.")
        }
        if (inherits(time1, "timeDate") && inherits(time2, "timeDate")) {
            as.double(time1 - time2) * 24
        } else if (is.numeric(time1) && is.numeric(time2)) {
            time1 - time2
        } else {
            stop("Cannot subtract times that are not either numeric or of class timeDate.")
        }
    }

    difftime.hours <- function(time1, time2) {
        if (length(time1) != length(time2)) {
            stop("Length of time1 and time2 should match.")
        }
        if (!(inherits(time1, "timeDate") && inherits(time2, "timeDate"))) {
            stop("Both time1 and time2 should be timeDate objects")
        }
        as.double(time1 - time2) * 24
    }

    # XXX: I have no idea how to do this with timeDate object in s-plus
    #difftime.days <- function(time1, time2) {
    #}

    # There is no grepl in S-SPLUS, so write one
    grepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
        res <- rep(FALSE, length(x))
        match <- grep(pattern, x, value=FALSE, perl=perl, fixed=fixed, ignore.case=ignore.case, subpattern = 0)
        res[match] <- TRUE
        res
    }

    # There is no is.unsorted in S-SPLUS, so write one
    is.unsorted <- function(x) {
        (!all((1:length(x) == order(x))))
    }

    write.nonmem.csv <- function(x, file, digits=8, colnames.toupper=TRUE) {
        if (!is.null(digits)) {
            for (nm in names(x)) {
                if (is.numeric(x[[nm]])) {
                    x[[nm]] <- round(x[[nm]], digits)
                }
            }
        }
        for (nm in names(x)) {
            if (inherits(x[[nm]], "timeDate")) {
                # XXX: Needs testing
                x[[nm]]@format <- "%Y-%m-%dT%H:%M:%S"
                x[[nm]] <- as.character(x[[nm]])
            }
        }
        if (substring(names(x)[1], 1, 1) != "#") {
            names(x)[1] <- paste("#", names(x)[1], sep="")
        }
        if (colnames.toupper) {
            names(x) <- toupper(names(x))
        }
        exportData(x, file=file, type="ASCII", delimiter=",", quote=FALSE, rowNames=FALSE, colNames=TRUE, na.string=".")
    }

    read.nonmem.csv <- function(file, colnames.tolower=TRUE) {
        x <- importData(file, type="ASCII", colNameRow=1, startRow=2, na.string=".", delimiter=",")
        # Remove the leading '#' (comment character in NONMEM).
        # Note: R converts the '#' to 'X.'
        if (substring(names(x)[1], 1, 1) == ".") {
            names(x)[1] <- substring(names(x)[1], 2)
        }
        if (colnames.tolower) {
            names(x) <- tolower(names(x))
        }
        x
    }

    # XXX: Needs testing
    read.nonmem.table <- function(file, skip=1, header=TRUE) {
        x <- importData(file, type="ASCII", colNameRow=(1 + skip), startRow=(2 + skip), na.string=".", delimiter=" ")
        names(x) <- tolower(names(x))
        x$id <- asID(x$id)
        x
    }

}


# ordering to restore the original order of the data after unlist(tapply(...))
#.restoreOrder <- function(id) {
#    order(unlist(tapply(.myseqalong(id), id, I, simplify=FALSE)))
#}



mapping <- function(from, to, ch.as.fact=TRUE) {
    if (missing(to)) {
        return (mapping(from=names(from), to=from, ch.as.fact=ch.as.fact))
    }
    if (length(from) != length(to)) {
        stop("Lengths of from and to should be the same")
    }
    from.dup <- duplicated(from)
    to.dup <- duplicated(to)

    from.unique.from <- from[!from.dup]
    from.unique.to <- from[!to.dup]
    to.unique.from <- to[!from.dup]
    to.unique.to <- to[!to.dup]

    if (ch.as.fact) {
        if (is.character(from)) {
            from.unique.to <- factor(from.unique.to, levels=from.unique.from)
        }
        if (is.character(to)) {
            to.unique.from <- factor(to.unique.from, levels=to.unique.to)
        }
    }

    fn <- function(x) {
        x <- factor(x, levels=from.unique.from)
        to.unique.from[x]
    }

    attr(fn, "inverse") <- function(z) {
        z <- factor(z, levels=to.unique.to)
        from.unique.to[z]
    }
    fn
}

#mapping <- function(from, to) {
#    if (missing(to)) {
#        return (mapping(names(from), from))
#    }
#    if (length(from) != length(to)) {
#        stop("Lengths of from and to should be the same")
#    }
#    .internal <- function(from, to) {
#        cond <- lapply(from, function(x) as.call(list(as.name("=="), as.name("x"), x)))
#        ifstmt <- as.logical(NA)
#        for (i in (length(from):1)) {
#            ifstmt <- as.call(list(as.name("if"), cond[[i]], to[i], ifstmt))
#        }
#        Vectorize(eval(as.call(list(as.name("function"), pairlist(x=substitute()), ifstmt))))
#    }
#    fn <- .internal(from, to)
#    attr(fn, "inverse") <- .internal(to, from)
#    fn
#}

asID <- function(id, ..., sep="|") {
    args <- list(...)
    if (length(args) > 0) {
        if (!all(sapply(args, length) == length(id))) {
            stop("Elements of a compound ID must have the same length")
        }
        if (any(is.na(id)) || !all(sapply(args, nmissing) == 0)) {
            warning("One or more arguments contains missing values. 'NA' will be treated as a distict value when forming the compound ID.")
        }
        id2 <- do.call(function(...) paste(..., sep=sep), args)
        id <- paste(id, id2, sep=sep)
    } else {
        if (any(is.na(id))) {
            warning("Argument contains missing values.")
        }
    }
    id <- factor(id, levels=unique(id))
    if (is.unsorted(id)) {
        # Don't issue a warning here.  The caller's intension may be to sort later.
        #warning("Not a valid block-format ID")
    }
    id
}

is.sorted <- function(...) {
    args <- list(...)
    if ((length(args) == 1) && is.atomic(args[[1]])) {
        # Handle the most common cases (numeric and factor) more efficiently
        !is.unsorted(args[[1]])
    } else {
        !is.unsorted(order(...))
    }
}

.checkID <- function(id) {
    if (!is.factor(id)) {
        stop("Not a valid ID: A block-format ID must be a factor.")
    } else if (any(is.na(id))) {
        stop("Not a valid ID: A block-format ID must not contain missing values.")
    } else if (is.unsorted(unique(id))) {
        stop("Not a valid ID: A block-format ID must be sorted.  Try using asID(...) to create a valid ID.")
    } else if (is.unsorted(id)) {
        first.prob1 <- which(diff(order(id)) > 1)[1]
        first.prob2 <- which(diff(as.integer(id)) < 0)[1] + 1
        id2 <- gsub("\r", "|", id)
        temp <- c(        "    position    id",
                  sprintf("        %4s    %-s", first.prob1, id[first.prob1]),
                          "         ...    ...",
                  sprintf("        %4s    %-s", first.prob2-1, id[first.prob2-1]),
                  sprintf("   ---> %4s    %-s", first.prob2, id[first.prob2]))

        temp2 <- paste(temp, collapse="\n")
        stop("Not a valid ID: A block-format ID must consist of contiguous blocks.\n",
             "There can only be one block per ID.\n",
             "The first recurring ID was identified here:\n\n\n",
             temp2,
             "\n\n")
    }
}

blk.singleValue <- function(x, id, ind=NULL, select=c("first", "last"), fill=NA) {
    .checkID(id)
    if (length(x) == 1) {
        x <- rep(x, length(id))
    }
    if (length(x) != length(id)) {
        stop("Lengths of x and id must match.")
    }
    if (is.null(ind)) {
        ind <-rep(TRUE, length(id))
    }
    if (!is.logical(ind)) {
        stop("Expecting a logical ind.")
    }
    if (length(ind) != length(id)) {
        stop("Lengths of ind and id must match.")
    }

    select <- match.arg(select)

    if (is.character(select) && select == "first") {
        apply.fun <- function(z, ...) z[1]
    } else if (is.character(select) && select == "last") {
        apply.fun <- function(z, ...) z[length(z)]
    } else {
        stop(sprintf("Unrecognized value for 'select' argument: %s", select))
    }


    yl <- tapply(.myseqalong(id)[ind], id[ind], apply.fun, simplify=FALSE)

    use.fill <- sapply(yl, function(z) (is.null(z) || length(z) == 0 ))

    yl <- lapply(yl, function(z) {if (is.null(z) || length(z) == 0 ) NA else z})

    if (any(sapply(yl, length) != 1)) {
        # Unreachable code
        warning("Something is wrong.  Only one value per ID expected.  The first value will be used.")
        yl <- lapply(yl, function(z) z[1])
    }

    y <- x[unlist(yl)]
    y[use.fill] <- fill
    y
}


blk.repeatValue <- function(x, id, id2=id, ind=NULL, select=c("first", "last"), fill=NA) {
    .checkID(id2)
    if (missing(id)) {
        if (length(x) == nlevels(id2)) {
            id <- factor(levels(id2), levels=levels(id2))
        } else {
            stop("Lengths of x and id must match. When id is missing, its default value is asID(levels(id2)).")
        }
    }

    x1 <- blk.singleValue(x, id, ind, select, fill=fill)

    x2 <- x1[factor(id2, levels=levels(id))]
    x2[is.na(factor(id2, levels=levels(id)))] <- fill
    x2
}

blk.applyNto1 <- function(x, id, apply.fun, ind=NULL, fill=NA, ...) {
    .checkID(id)
    if (length(x) != length(id)) {
        stop("Lengths of x and id must match.")
    }
    if (is.null(ind)) {
        ind <-rep(TRUE, length(id))
    }
    if (!is.logical(ind)) {
        stop("Expecting a logical ind.")
    }
    if (length(ind) != length(id)) {
        stop("Lengths of ind and id must match.")
    }

    if (is.character(apply.fun) && apply.fun == "first") {
        apply.fun <- function(z, ...) z[1]
    } else if (is.character(apply.fun) && apply.fun == "last") {
        apply.fun <- function(z, ...) z[length(z)]
    } else {
        apply.fun <- .mymatchfun(apply.fun)
    }

    yl <- tapply(x[ind], id[ind],
        function(z, apply.fun, ...) {
            res <- apply.fun(z, ...)
            if (length(res) != 1) {
                warning(sprintf("Expected 1 value, got %s.", length(res)))
                res <- rep(res, length.out=1)
            }
            res
        },
        simplify=FALSE, apply.fun=apply.fun, ...)

    yl <- lapply(yl, function(z) {if (is.null(z) || length(z) == 0 ) fill else z})

    if (any(sapply(yl, length) != 1)) {
        # Unreachable code
        warning("Something is wrong.  Only one value per ID expected.  The first value will be used.")
        yl <- lapply(yl, function(z) z[1])
    }

    y <- do.call(c, yl)
    y
}

blk.applyNtoN <- function(x, id, apply.fun, ind=NULL, fill=NA, ...) {
    .checkID(id)
    if (length(x) != length(id)) {
        stop("Lengths of x and id must match.")
    }
    if (is.null(ind)) {
        ind <-rep(TRUE, length(id))
    }
    if (!is.logical(ind)) {
        stop("Expecting a logical ind.")
    }
    if (length(ind) != length(id)) {
        stop("Lengths of ind and id must match.")
    }

    if (is.character(apply.fun) && apply.fun == "first") {
        apply.fun <- function(z, ...) z[1]
    } else if (is.character(apply.fun) && apply.fun == "last") {
        apply.fun <- function(z, ...) z[length(z)]
    } else {
        apply.fun <- .mymatchfun(apply.fun)
    }

    yl <- tapply(x[ind], id[ind],
        function(z, apply.fun, ...) {
            res <- apply.fun(z, ...)
            if (length(z) %% length(res) != 0) {
                warning(sprintf("Expected length of vector to be a factor of %s, but it was %s.", length(z), length(res)))
            }
            rep(res, length.out=length(z))
        },
        simplify=FALSE, apply.fun=apply.fun, ...)

    y <- do.call(c, yl)
    z <- rep(y[1], length(x))
    z[ind] <- y
    z[!ind] <- fill
    z
}

blk.firstOnly <- function(id, ind=NULL) {
    .checkID(id)
    if (is.null(ind)) {
        ind <-rep(TRUE, length(id))
    }
    if (!is.logical(ind)) {
        warning("Expecting a logical ind")
        ind <- as.logical(ind)
    }
    if (length(ind) != length(id)) {
        stop("Lengths of ind and id must match")
    }
    (ind * unlist(tapply(ind, id, cumsum, simplify=FALSE))) == 1
}
blk.onlyFirst <- blk.firstOnly

blk.lastOnly <- function(id, ind=NULL) {
    .checkID(id)
    if (is.null(ind)) {
        ind <-rep(TRUE, length(id))
    }
    if (!is.logical(ind)) {
        warning("Expecting a logical ind")
        ind <- as.logical(ind)
    }
    if (length(ind) != length(id)) {
        stop("Lengths of ind and id must match")
    }
    unlist(tapply(ind, id, function(x) {
        if (!any(x)) x
        else {
            y <- cumsum(x)
            (x * y) == max(y)
        }
    }, simplify=FALSE))
}
blk.onlyLast <- blk.lastOnly


blk.firstOnwards <- function(id, ind, include.first=TRUE, fill=FALSE) {
    .checkID(id)
    if (!is.logical(ind)) {
        warning("Expecting a logical ind")
        ind <- as.logical(ind)
    }
    if (length(ind) != length(id)) {
        stop("Lengths of ind and id must match")
    }
    unlist(tapply(ind, id, function(x, include.first, fill) {
        if (!any(x)) rep(fill, length(x))
        else {
            y <- cumsum(x)
            if (include.first) {
                y > 0
            } else {
                (y > 0) & (x * y != 1)
            }
        }
    }, include.first=include.first, fill=fill, simplify=FALSE))
}


blk.lastOnwards <- function(id, ind, include.last=TRUE, fill=FALSE) {
    .checkID(id)
    if (!is.logical(ind)) {
        warning("Expecting a logical ind")
        ind <- as.logical(ind)
    }
    if (length(ind) != length(id)) {
        stop("Lengths of ind and id must match")
    }
    unlist(tapply(ind, id, function(x, include.last, fill) {
        if (!any(x)) rep(fill, length(x))
        else {
            y <- cumsum(x)
            if (include.last) {
                y == max(y)
            } else {
                (y == max(y)) & (!x)
            }
        }
    }, include.last=include.last, fill=fill, simplify=FALSE))
}

blk.untilFirst <- function(id, ind, include.first=TRUE, fill=FALSE) {
    !blk.firstOnwards(id, ind, !include.first, !fill)
}


blk.untilLast <- function(id, ind, include.last=TRUE, fill=FALSE) {
    !blk.lastOnwards(id, ind, !include.last, !fill)
}

blk.locf <- function(x, id, na.action=c("fill", "carry.back"), fill=NA) {
    .checkID(id)
    if (length(x) != length(id)) {
        stop("Lengths of x and id must match")
    }
    
    na.action <- match.arg(na.action)

    ii <- ifelse(is.na(x), NA, .myseqalong(x))
    ii <- unlist(tapply(ii, id, function(y, na.action) {
        if (all(is.na(y))) return (y)
        z <- cumsum(!is.na(y))
        if (na.action == "carry.back") {
            z[z==0] <- 1
        }
        ifelse(z==0, NA, y[!is.na(y)][ifelse(z>0, z, 1)])
    }, na.action=na.action, simplify=FALSE))
    #ifelse(is.na(ii), fill, x[ii])  # Note: doesn't work with class POSIXt
    y <- x
    y[!is.na(ii)] <- x[ii[!is.na(ii)]]
    y[is.na(ii)] <- fill
    y
}

blk.nocb <- function(x, id, na.action=c("fill", "carry.forward"), fill=NA) {
    .checkID(id)
    if (length(x) != length(id)) {
        stop("Lengths of x and id must match")
    }
    
    na.action <- match.arg(na.action)
    if (na.action == "carry.forward") na.action <- "carry.back"

    rid <- factor(rev(id), levels=rev(levels(id)))
    rev(blk.locf(rev(x), rid, na.action, fill))
}

blk.sequentialID <- function(id) {
    cumsum(blk.firstOnly(id=id))
}

blk.count <- function(id, id2=id, ind=NULL) {
    res <- blk.applyNto1(x=rep(1, length(id)), id=id, apply.fun=sum, ind=ind, fill=0)
    if (!is.null(id2)) {
        res <- blk.repeatValue(res, id=asID(levels(id)), id2=id2, fill=0)
    }
    res
}

#blk.isSorted <- function(x, id, ind=NULL) {
#    .checkID(id)
#    if (is.null(ind)) {
#        ind <-rep(TRUE, length(id))
#    }
#    if (!is.logical(ind)) {
#        warning("Expecting a logical ind")
#        ind <- as.logical(ind)
#    }
#    if (length(ind) != length(id)) {
#        stop("Lengths of ind and id must match")
#    }
#    if (missing(x)) {
#        x <- id
#    }
#    all(order(id[ind], x[ind]) == (1:sum(ind)))
#}


percentChange <- function(x, y) {
    100*(x - y) / y
}

#blk.changeFromBaseline <- function(x, baseline=x, id, id.baseline=NULL, diff.op="percentChange", ...) {
#    if (!is.null(id.baseline)) {
#        baseline <- blk.map(baseline, id.baseline, id, ...)
#    } else {
#        baseline <- blk.repeatValue(baseline, id=id, ...)
#    }
#    .mymatchfun(diff.op)(x, baseline)
#}

blk.changeFromBaseline <- function(x, id, baseline=blk.singleValue(x, id, ind=bl.ind, select=bl.select), bl.ind=NULL, bl.select="first", diff.op=percentChange, ...) {
    baseline <- blk.repeatValue(baseline, id2=id)
    .mymatchfun(diff.op)(x, baseline, ...)
}

blk.relativeTime <- function(time, id, baseline=blk.singleValue(time, id, ind=bl.ind, select=bl.select), bl.ind=NULL, bl.select="first", diff.op=difftime.default, ...) {
    blk.changeFromBaseline(time, id, baseline, diff.op=diff.op, ...)
}


blk.tad <- function(time, id, dose.ind, na.action=c("neg.time", "fill"), fill=NA, diff.op=difftime.default) {
    .checkID(id)
    if (!is.logical(dose.ind)) {
        warning("Expecting a logical ind")
        ind <- as.logical(ind)
    }
    if (length(dose.ind) != length(id)) {
        stop("Lengths of dose.ind and id must match")
    }
    if (length(time) != length(id)) {
        stop("Lengths of time and id must match")
    }
    na.action <- match.arg(na.action)
    diff.op <- .mymatchfun(diff.op)

    temp <- time
    temp[!dose.ind] <- NA
    dose.time <- blk.locf(temp, id, "carry.back")
    tdiff <- diff.op(time, dose.time)

    if (na.action == "fill") {
        tdiff[tdiff < 0] <- fill
    }
    return(tdiff)
}

blk.tad2 <- function(time, id, dose.t, id2, na.action=c("neg.time", "fill"), fill=NA, diff.op=difftime.default) {
    .checkID(id)
    .checkID(id2)

    temp <- rbind(
        data.frame(id=id,  time=time,   code=0),
        data.frame(id=id2, time=dose.t, code=1))

    temp <- temp[order(temp$id, temp$time, temp$code),]
    blk.tad(temp$time, id=temp$id, dose.ind=temp$code==1, na.action=na.action, fill=fill, diff.op=diff.op)[temp$code==0]
}

blk.shift <- function(x, id, shift.by=1, ind=NULL, fill=NA) {
    .checkID(id)
    if (length(x) != length(id)) {
        stop("Lengths of x and id must match.")
    }
    if (is.null(ind)) {
        ind <-rep(TRUE, length(id))
    }

    if (shift.by == 0) return (x)

    ii <- rep(NA, length(x))
    ii[ind] <- unlist(tapply(.myseqalong(x)[ind], id[ind], function(z, shift.by) {
        if (length(z) <= abs(shift.by)) {
            rep(NA, length(z))
        } else if (shift.by > 0) {
            c(rep(NA, length=shift.by), z[1:(length(z) - shift.by)])
        } else {
            shift.by <- -shift.by
            c(z[(shift.by + 1):length(z)], rep(NA, length=shift.by))
        }
    }, shift.by=shift.by, simplify=FALSE))
    y <- x[NA]
    y[!is.na(ii)] <- x[ii[!is.na(ii)]]
    y[is.na(ii)] <- fill
    #y <- ifelse(is.na(ii), fill, x[ii])
    y

    #y <- rep(fill, length(x))
    #y[ind] <- unlist(tapply(x[ind], id[ind], function(z, shift.by, fill) {
    #    if (length(z) <= shift.by) {
    #        rep(fill, length(z))
    #    } else if (shift.by > 0) {
    #        c(rep(fill, length=shift.by), z[1:(length(z) - shift.by)])
    #    } else {
    #        shift.by <- -shift.by
    #        c(z[(shift.by + 1):length(z)], rep(fill, length=shift.by))
    #    }
    #}, shift.by=shift.by, fill=fill, simplify=FALSE))
    #y
}

# Re-implement the standard diff function to allow for a diff.op function
.mydiff <- function(x, lag=1, diff.op="-", ...) {
    if (length(lag) > 1L || lag < 1L)  {
        stop("'lag' must be an integer >= 1")
    }

    diff.op <- .mymatchfun(diff.op)
 
    if (lag >= length(x)) {
        return (diff.op(x[1], x[1])[0])  # Zero length
    }

    r <- x
    i1 <- -seq_len(lag)
    r <- diff.op(r[i1], r[-length(r):-(length(r) - lag + 1L)], ...)
    r
}


blk.diff <- function(x, id, ind=NULL, lag=1, fill=NA, diff.op="-", ...) {
    .checkID(id)

    if (is.null(ind)) {
        ind <-rep(TRUE, length(id))
    }

    y <- rep(fill, length(x))
    y[ind] <- unlist(tapply(x[ind], id[ind], function(y, lag, fill, ...) {
        if (length(y) <= lag) {
            rep(fill, length(y))
        } else {
            c(rep(fill, lag), .mydiff(y, lag=lag, diff.op=diff.op, ...))
        }
    }, lag=lag, fill=fill, simplify=FALSE, ...))
    y
}

# Alias, with different default for diff.op
blk.intereventTime <- function(time, id, ind=NULL, lag=1, fill=NA, diff.op=difftime.default, ...) {
    blk.diff(x=time, id=id, ind=ind, lag=lag, fill=fill, diff.op=diff.op, ...)
}


blk.AUC.by.trapezoid <- function(x, y, id, id2=id, ind=NULL, fill=NA, diff.op="-", ...) {
    temp <- data.frame(id=id, x=x, y=y)
    if (!is.null(ind)) {
        temp <- temp[ind,]
    }
    temp$z <- with(temp, blk.diff(x, id=id, diff.op=diff.op, ...) * (y + blk.shift(y, id=id))/2)
    temp <- temp[!blk.firstOnly(id=temp$id),]
    res <- blk.applyNto1(temp$z, id=temp$id, apply.fun="sum", fill=fill)
    if (!is.null(id2)) {
        res <- blk.repeatValue(res, id=asID(levels(temp$id)), id2=id2)
    }
    res
}


blk.time.above.threshold <- function(x, y, threshold, id, id2=id, ind=NULL, fill=NA, diff.op.x="-", diff.op.y="-", ...) {
    temp <- data.frame(id=id, x=x, y=y)
    if (!is.null(ind)) {
        temp <- temp[ind,]
    }
    temp$dx <- with(temp, blk.diff(x, id=id, diff.op=diff.op.x, ...))
    temp$dy <- with(temp, blk.diff(y, id=id, diff.op=diff.op.y, ...))
    temp$z <- with(temp, ifelse(dy==0, 1*(y > threshold), (y - threshold)/dy))
    temp$z <- with(temp, ifelse(dy >= 0, z, 1-z))
    temp$z <- with(temp, dx * pmin(pmax(z, 0), 1))
    temp <- temp[!blk.firstOnly(id=temp$id),]
    res <- blk.applyNto1(temp$z, id=temp$id, apply.fun="sum", fill=fill)
    if (!is.null(id2)) {
        res <- blk.repeatValue(res, id=asID(levels(temp$id)), id2=id2)
    }
    res
}


.consecLengths <- function(ind) {
    y <- table(cumsum(!ind)) - 1
    if (ind[1]) {
        y[1] <- y[1] + 1
    }
    y <- y[y>0]
    y
}

# May be useful as well for non-block data?
.findConsecutive <- function(ind, min.consec=2) {
    y <- .consecLengths(ind)
    z <- rep(0, length(ind))
    z[ind] <- rep(ifelse(y >= min.consec, 1, 0), times=y)
    y <- y[y >= min.consec]
    z[z==1] <- rep(.myseqalong(y), times=y)
    z
}

blk.findConsecutive <- function(id, ind, min.consec=2) {
    .checkID(id)

    y <- unlist(tapply(ind, id, .consecLengths, simplify=FALSE))
    z <- rep(0, length(ind))
    z[ind] <- rep(ifelse(y >= min.consec, 1, 0), times=y)
    y <- y[y >= min.consec]
    z[z==1] <- rep(.myseqalong(y), times=y)
    z
}

blk.isSteadyState <- function(time, id, dose.ind, dose.interval, tol, min.time.ss, dose=NULL, diff.op=difftime.default) {
    .checkID(id)

    if (is.null(dose)) {
        delta.dose <- 0
    } else {
        if (!is.numeric(dose)) {
            stop("'dose' must be numeric times.")
        }
        delta.dose <- blk.diff(dose, id=id, ind=dose.ind, fill=0)
    }
    itime <- blk.intereventTime(time, id=id, ind=dose.ind, fill=Inf, diff.op=diff.op)

    cumtime <- cumsum(ifelse(dose.ind, dose.interval, 0))
    flag <- !dose.ind | ((delta.dose == 0) & (abs(itime - dose.interval) <= tol))
    cumtime2 <- blk.tad(cumtime, id, !flag, fill=0)
    ss <- dose.ind & (cumtime2 >= min.time.ss)
    ss

    #min.consec <- ceiling(min.time.ss/dose.interval)
    #cons <- blk.findConsecutive(id, ind=((delta.dose == 0) & (abs(itime - dose.interval) <= tol)), min.consec=min.consec)
    #ss <- blk.shift(cons > 0, id=id, shift.by=min.consec-1, ind=cons > 0, fill=FALSE)
    #ss
}

blk.ii <- function(time, id, dose.ind, diff.op=difftime.default) {

    ii <- blk.intereventTime(time, id=id, ind=dose.ind, diff.op=diff.op)
    ii <- blk.shift(ii, id=id, shift.by= -1, ind=dose.ind, fill=0)
    ii[!dose.ind] <- NA
    ii
}

blk.addl <- function(ii, id, dose.ind, dose, tol.ii=1e-5, tol.dose=1e-5, min.consec=2, include.last=FALSE, diff.op=difftime.default) {

    PCSmisc:::.checkID(id)

    if (!is.numeric(dose)) {
        stop("'dose' must be numeric times.")
    }
    if (!(length(ii)==length(id) && length(dose.ind)==length(id) && length(dose)==length(id))) {
        stop("'ii', 'id', 'dose.ind' and 'dose' must all have the same length.")
    }

    y <- blk.diff(ii, id=id, ind=dose.ind)
    y <- blk.shift(y, id=id, shift.by= -1, ind=dose.ind, fill=0)

    z <- blk.diff(dose, id=id, ind=dose.ind)
    z <- blk.shift(z, id=id, shift.by= -1, ind=dose.ind, fill=0)

    flag <- dose.ind & (abs(y) < tol.ii) & (abs(z) < tol.dose)

    # Do this twice, because the last ii can be anything
    for (i in 1:2) {
        flag2 <- blk.shift(flag, id=id, fill=FALSE)
        flag[dose.ind & !flag & flag2] <- flag2[dose.ind & !flag & flag2]
    }

    if (!include.last) {
        flag <- flag & blk.shift(dose.ind, id=id, shift.by= -1, fill=FALSE)
    }

    cons <- blk.findConsecutive(id, ind=(dose.ind & flag), min.consec=min.consec)

    addl <- ifelse(dose.ind, 0, NA)
    addl[cons>0] <- ifelse(blk.firstOnly(asID(cons[cons>0])), blk.count(id=asID(cons[cons>0])) - 1, -1)
    addl
}

# Description: Flags doses preceding a steady state event when no non-dose event has occurred between the two.
blk.noninformativeDose <- function(id, dose.ind, ss.ind=NULL) {
    .checkID(id)
    x <- rep.int(FALSE, length(id))
    if (!is.null(ss.ind)) {
        cons <- blk.findConsecutive(id, ind=dose.ind)
        x[cons > 0] <- blk.untilLast(id=asID(cons[cons > 0]), ind=ss.ind[cons > 0], include.last=FALSE)
    }
    x | blk.lastOnwards(id=id, ind=!dose.ind, include.last=FALSE)
}

blk.concomitant <- function(time, id, begin.exposure, end.exposure, id2=id) {
    .checkID(id)
    .checkID(id2)

    temp <- rbind(
        data.frame(id=id,  time=time,           code=2, x=0),
        data.frame(id=id2, time=begin.exposure, code=0, x=1),
        data.frame(id=id2, time=end.exposure,   code=1, x=-1))

    temp <- temp[order(temp$id, temp$time, temp$code),]
    y    <- unlist(tapply(temp$x, temp$id, cumsum, simplify=FALSE))
    y[temp$code==2]
}

blk.countPastEvents <- function(time, id, event.t=time, id2=id) {
    .checkID(id)
    .checkID(id2)

    temp <- rbind(
        data.frame(id=id,  time=time,    code=1),
        data.frame(id=id2, time=event.t, code=0))

    temp <- temp[order(temp$id, temp$time, temp$code),]
    unlist(tapply(temp$code==0, temp$id, cumsum, simplify=FALSE))[temp$code==1]
}

blk.locf2 <- function(time, id, observ.x, observ.t, id2=id, na.action=c("fill", "carry.back"), fill=NA) {
    .checkID(id)
    .checkID(id2)

    if (any(is.na(time))) {
        stop("time contains missing values")
    }
    if (any(is.na(observ.t))) {
        stop("observ.t contains missing values")
    }
    temp <- rbind(
        data.frame(id=id,  time=time,     code=1, x=NA),
        data.frame(id=id2, time=observ.t, code=0, x=observ.x))

    temp <- temp[order(temp$id, temp$time, temp$code),]
    blk.locf(temp$x, temp$id, na.action, fill)[temp$code==1]
}

blk.nearestMatch <- function(time, id, observ.x, observ.t, id2=id, direction=c("forward", "backward", "both"), tol=Inf, fill=NA, na.rm=FALSE, diff.op=difftime.default) {
    .checkID(id)
    .checkID(id2)

    if (any(is.na(time))) {
        stop("time contains missing values")
    }
    if (any(is.na(observ.t))) {
        stop("observ.t contains missing values")
    }
    if (length(time) != length(id)) {
        stop("Lengths of time and id must match")
    }
    if (length(observ.t) != length(id2)) {
        stop("Lengths of observ.t and id2 must match")
    }
    if (length(observ.x) != length(id2)) {
        stop("Lengths of observ.x and id2 must match")
    }
    if (!is.sorted(id, time)) {
        warning("time is not sorted within blocks of id.")
    }
    if (!is.sorted(id2, observ.t)) {
        warning("observ.t is not sorted within blocks of id2.")
    }

    diff.op <- .mymatchfun(diff.op)

    direction <- match.arg(direction)

    if (!is.numeric(tol)) {
        stop("tol must be numeric.")
    }
    if (length(tol) > 2 || length(tol) < 1) {
        stop("tol should have length 1 or 2.")
    }
    if (length(tol) == 1) {
        tol <- rep(tol, 2)
    }
    tol.f <- tol[1]
    tol.b <- tol[2]

    if (na.rm) {
        keep     <- !is.na(observ.x)
        observ.x <- observ.x[keep]
        observ.t <- observ.t[keep]
        id2      <- id2[keep]
    }

    temp <- rbind(
        data.frame(id=id,  time=time,     code=1, ix.f=NA,               time.f=time[NA], ix.b=NA,               time.b=time[NA]),
        data.frame(id=id2, time=observ.t, code=0, ix.f=.myseqalong(id2), time.f=observ.t, ix.b=.myseqalong(id2), time.b=observ.t))

    temp <- temp[order(temp$id, temp$time, temp$code),]

    if (direction=="forward" || direction=="both") {
        temp$ix.f   <- blk.locf(temp$ix.f,   temp$id, fill=NA)
        temp$time.f <- blk.locf(temp$time.f, temp$id, fill=NA)
    }
    if (direction=="backward" || direction=="both") {
        temp$ix.b   <- blk.nocb(temp$ix.b,   temp$id, fill=NA)
        temp$time.b <- blk.nocb(temp$time.b, temp$id, fill=NA)
    }

    if (direction=="forward") {
        dtfor <- ifelse(is.na(temp$time.f), NA, diff.op(temp$time, temp$time.f))
        ix <- ifelse(!is.na(dtfor) & dtfor <= tol.f, temp$ix.f, NA)
    } else if (direction=="backward") {
        dtbac <- ifelse(is.na(temp$time.b), NA, diff.op(temp$time.b, temp$time))
        ix <- ifelse(!is.na(dtbac) & dtbac <= tol.b, temp$ix.b, NA)
    } else {
        dtfor <- ifelse(is.na(temp$time.f), NA, diff.op(temp$time  , temp$time.f))
        dtbac <- ifelse(is.na(temp$time.b), NA, diff.op(temp$time.b, temp$time  ))
        ix <- ifelse(!is.na(dtfor) & (is.na(dtbac) | dtfor <= dtbac),
              ifelse(dtfor <= tol.f, temp$ix.f, NA),
              ifelse(!is.na(dtbac) & dtbac <= tol.b, temp$ix.b, NA))
    }
    ix1 <- ix[temp$code==1]
    res <- observ.x[ix1]
    res[is.na(ix1)] <- fill

    if (!is.sorted(id, time)) {
        res[order(order(id, time))]   # This will restore the order
    } else {
        res
    }
}

stats.default <- function(x, useNA="ifany") {
    if (is.factor(x) || is.character(x)) {
        y <- table(x, useNA=useNA)
        nn <- names(y)
        nn[is.na(nn)] <- "Missing"
        names(y) <- nn
        lapply(y, function(z) list(FREQ=z, PCT=100*z/sum(y)))
    } else if (is.numeric(x)) {
        list(
            N=sum(!is.na(x)),
            NMISS=sum(is.na(x)),
            MEAN=signif.pad(mean(x, na.rm=TRUE), 3),
            SD=signif.pad(sd(x, na.rm=TRUE), 3),
            MIN=signif.pad(min(x, na.rm=TRUE), 3),
            MEDIAN=signif.pad(median(x, na.rm=TRUE), 3),
            MAX=signif.pad(max(x, na.rm=TRUE), 3),
            CV=signif.pad(100*sd(x, na.rm=TRUE)/abs(mean(x, na.rm=TRUE)), 3),
            GMEAN=if (any(na.omit(x) <= 0)) NA else signif.pad(exp(mean(log(x), na.rm=TRUE)), 3),
            GCV=if (any(na.omit(x) <= 0)) NA else signif.pad(100*sd(x, na.rm=TRUE)/exp(mean(log(x), na.rm=TRUE)), 3))
    } else {
        stop(paste("Unrecognized variable type:", class(x)))
    }
}

blk.stats <- function(x, id, ind=NULL, stratify.by=NULL, stats.fn=stats.default, ...) {
    if (!is.null(stratify.by) && !is.list(stratify.by)) {
        error("stratify.by must be a list or null.")
    }
    if (is.null(stratify.by) || length(stratify.by) == 0) {
        stratum <- rep(1, length(x))
    } else {
        stratum <- do.call(asID, stratify.by)
    }
    single.x     <- blk.singleValue(x, id, ind=ind)
    single.strat <- blk.singleValue(stratum, id)

    stats.fn <- .mymatchfun(stats.fn)
    res <- tapply(single.x, single.strat, stats.fn, simplify=FALSE, ...)
    if (length(res) == 1) {
        res[[1]]
    } else {
        res
    }
}

blk.medianImputation <- function(x, id, stratify.by=NULL) {
    if (!is.null(stratify.by) && !is.list(stratify.by)) {
        error("stratify.by must be a list or null.")
    }
    if (is.null(stratify.by) || length(stratify.by) == 0) {
        stratum <- rep(1, length(x))
    } else {
        stratum <- do.call(asID, stratify.by)  # OK if not sorted
    }
    single.x     <- blk.singleValue(x, id)
    single.strat <- blk.singleValue(stratum, id)
    med <- tapply(single.x, single.strat, median, na.rm=TRUE, simplify=TRUE)
    ifelse(is.na(x), med[stratum], x)
}

###############################################################################

# Calculate estimated creatinine clearance using the Cockroft-Gault formula
crcl.cg <- function(..., scr.mg.dL, weight.kg, age.yr, is.female, scr.umol.L, scr.conversion.factor=88.4) {
    if (missing(scr.mg.dL) || is.null(scr.mg.dL)) {
        if (missing(scr.umol.L) || is.null(scr.umol.L)) {
            stop("One of scr.mg.dL and scr.umol.L must be present")
        } else {
            # Either way, this gives similar (but not identical) results.
            scr.mg.dL <- (scr.umol.L / scr.conversion.factor)
            #(1.23 * (140 - age.yr) * weight.kg * (1.04/1.23)^is.female / (scr.umol.L))
        }
    } else {
        if (!(missing(scr.umol.L) || is.null(scr.umol.L))) {
            stop("Only one of scr.mg.dL and scr.umol.L must be present")
            #scr.mg.dL[is.na(scr.mg.dL)] <- (scr.umol.L[is.na(scr.mg.dL)] / scr.conversion.factor)
        }
    }
    (140 - age.yr) * weight.kg * (0.85)^is.female / (72 * scr.mg.dL)
}


# Calculate esimated GFR using Modification of Diet in Renal Disease (MDRD) formula
# Units of GFR are mL/min
egfr.mdrd <- function(..., scr.mg.dL, age.yr, is.female, is.black, bun.mg.dL=NULL, albumin.g.dL=NULL, scr.umol.L, scr.conversion.factor=88.4) {
    if (missing(scr.mg.dL) || is.null(scr.mg.dL)) {
        if (missing(scr.umol.L) || is.null(scr.umol.L)) {
            stop("One of scr.mg.dL and scr.umol.L must be present")
        } else {
            scr.mg.dL <- (scr.umol.L / scr.conversion.factor)
        }
    } else {
        if (!(missing(scr.umol.L) || is.null(scr.umol.L))) {
            stop("Only one of scr.mg.dL and scr.umol.L must be present")
            #scr.mg.dL[is.na(scr.mg.dL)] <- (scr.umol.L[is.na(scr.mg.dL)] / scr.conversion.factor)
        }
    }
    if (!is.null(bun.mg.dL) && !is.null(albumin.g.dL)) {
        170 * scr.mg.dL^(-0.999) * age.yr^(-0.176) * (1.18)^is.black * (0.762)^is.female * bun.mg.dL^(-0.17) * albumin.g.dL^(0.318)
    } else {
        186 * scr.mg.dL^(-1.154) * age.yr^(-0.203) * (1.212)^is.black * (0.742)^is.female
    }
}

# Convert number to character with
# a constant number of significant digist
# padding with zeros if necessary.
signif.pad <- function(x, digits=3, round.integers=FALSE) {
    if (round.integers) {
        cx <- as.character(signif(x, digits))  # Character representation of x
    } else {
        cx <- ifelse(x >= 10^digits, as.character(round(x)), as.character(signif(x, digits)))  # Character representation of x
    }

    d <- gsub("[^0-9]", "", cx)            # The 'digits' of x
    d <- sub("^0*", "", d)                 # Remove any leading zeros
    nd <- nchar(d)                         # How many actual digits
    npad <- pmax(0, digits - nd)           # How many digits are missing
    pad <- sapply(npad, function(n) paste(rep("0", times=n), collapse=""))

    has.dec <- grepl("\\.", cx)                      #  Does cx already contain a decimal point?
    add.dec <- ifelse(!has.dec & npad > 0, ".", "")  #  If not, and if padding is required, we need to add a decimal point first

    paste(cx, add.dec, pad, sep="")
}

# Make all "" into NA
#allFactorsEmptyStringToMissing <- function(data) {
#    if (!is.data.frame(data)) {
#        warning("Applying to a non-data.frame")
#    }
#    for (i in (1:length(data))) {
#        if (is.factor(data[[i]])) {
#            data[[i]][data[[i]] == ""] <- NA
#            data[[i]] <- droplevels(data[[i]])
#        }
#    }
#    data
#}

# How many missing values?
nmissing <- function(x) {
    sum(is.na(x))
}

# How many unique values?
nunique <- function(x, na.rm=FALSE) {
    if (na.rm) {
        length(unique(x[!is.na(x)]))
    } else {
        length(unique(x))
    }
}


blk.isConstant <- function(x, id, ind=NULL) {
    all(blk.applyNto1(x, id, nunique, ind=ind, fill=0) <= 1)
}

blk.isVarying <- function(x, id, ind=NULL) {
    any(blk.applyNto1(x, id, nunique, ind=ind, fill=0) > 1)
}

mixedData <- function(x) {
    chr <- as.character(x)
    num <- suppressWarnings(as.numeric(chr))
    obj <- chr
    attr(obj, "numeric") <- num
    class(obj) <- "mixedData"
    obj
}

print.mixedData <- function(obj) {
    print(as.character(obj), quote=FALSE)
}

as.character.mixedData <- function(obj) {
    as.character(unclass(obj))
}

as.double.mixedData <- function(obj, fill=NA) {
    num <- attr(obj, "numeric")
    num[is.na(num) & !is.na(obj)] <- fill
    num
}
as.data.frame.mixedData <- function(obj, ...) 
{
    nm <- deparse(substitute(obj), width.cutoff = 500L)
    as.data.frame.vector(obj, ..., nm = nm)
}

unique.mixedData <- function(obj) {
    num <- attr(obj, "numeric")
    chr <- as.character(unclass(obj))
    unique(ifelse(is.na(num), chr, "<Number>"))
}

generate.assertions <- function(dat, max.level.show=12, digits.compare=6) {
    datname <- deparse(substitute(dat))
    cat(sprintf("### %s", datname), "\n")
    assert <- sprintf("stopifnot(nrow(%s) == %d)", datname, nrow(dat))
    assert <- sprintf("stopifnot(ncol(%s) == %d)", datname, ncol(dat))
    cat(assert, "\n")
    cat("\n")
    for (col in names(dat)) {
        if (!is.null(attr(dat[,col], "label"))) {
            assert <- sprintf("stopifnot(attr(%s$%s, \"label\") == \"%s\")", datname, col, attr(dat[,col], "label"))
            cat(assert, "\n")
        }
        assert <- sprintf("stopifnot(class(%s$%s) == \"%s\")", datname, col, class(dat[,col]))
        cat(assert, "\n")
        assert <- sprintf("stopifnot(nmissing(%s$%s) == %d)", datname, col, nmissing(dat[,col]))
        cat(assert, "\n")
        if (is.factor(dat[,col])) {
            assert <- sprintf("stopifnot(nlevels(%s$%s) == %d)", datname, col, nlevels(dat[,col]))
            cat(assert, "\n")
            if (nlevels(dat[,col]) > 0 && nlevels(dat[,col]) <= max.level.show) {
                lev <- paste(paste("\"", levels(dat[,col]), "\"", sep=""), sep="", collapse=", ")
                assert <- sprintf("stopifnot(levels(%s$%s) == c(%s))", datname, col, lev)
                cat(assert, "\n")
            }
        } else if (is.numeric(dat[,col]) && !all(is.na(dat[,col]))) {
            r <- round(range(dat[,col], na.rm=TRUE), digits=digits.compare)
            assert <- sprintf("stopifnot(round(range(%s$%s, na.rm=TRUE), %d) == c(%f, %f))", datname, col, digits.compare, r[1], r[2])
            cat(assert, "\n")
        }

        cat("\n")
    }
    invisible(NULL)
}

