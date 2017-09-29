require(PCSmisc)

set.seed(123)

id <- factor(rep(1:7, times=c(1, 6, 1, 0, 3, 3, 2)), levels=1:7)
x <- (1:length(id))
x[id=="2"][c(2, 4, 5)] <- NA
x[id=="3"][1]          <- NA
x[id=="5"][1]          <- NA
x[id=="6"]             <- NA


###################
# blk.count
###################
stopifnot(blk.count(id=id, id2=NULL) == c(1, 6, 1, 0, 3, 3, 2))
stopifnot(blk.count(id=id, id2=NULL, ind=is.na(x)) == c(0, 3, 1, 0, 1, 3, 0))



###################
# blk.locf
###################

df <- data.frame(id=id, x=x, ans1=blk.locf(x, id), ans2=blk.locf(x, id, "carry.back"), ans3=blk.locf(x, id, fill=999))
# Have a look at df
#print(df)

# Check invariants
stopifnot(df$x[!is.na(df$x)] == df$ans1[!is.na(df$x)])             # Doesn't change non-NA values
stopifnot(df$ans1[!is.na(df$ans1)] == df$ans2[!is.na(df$ans1)])    # Imputed values are the same except for carry.back case
stopifnot(df$ans1[!is.na(df$ans1)] == df$ans3[!is.na(df$ans1)])    # Imputed values are the same except for fill case
stopifnot(df$ans3[is.na(df$ans1)] == 999)                          # The fill value is used in place of NA
stopifnot(is.na(df$ans1[df$ans2 == 999]))                          # The fill value is used in place of NA
stopifnot(is.na(df$x[is.na(df$ans1)]))                             # Any remaining NA values were NA to begin with

# Check results
stopifnot(df$ans1[id=="2"][2] == df$x[id=="2"][1])
stopifnot(df$ans1[id=="2"][4] == df$x[id=="2"][3])
stopifnot(df$ans1[id=="2"][5] == df$x[id=="2"][3])
stopifnot(is.na(df$ans1[id=="3"][1]))
stopifnot(is.na(df$ans2[id=="3"][1]))
stopifnot(is.na(df$ans1[id=="5"][1]))
stopifnot(df$ans2[id=="5"][1] == df$x[id=="5"][2])
stopifnot(is.na(df$ans1[id=="6"]))
stopifnot(is.na(df$ans2[id=="6"]))



# Check that it also works with POSIXt objects, and with a single block
id.date <- factor(c(1, 1, 1))
date <- strptime(c("2011-05-01", NA, "2011-05-3"), "%Y-%m-%d")
stopifnot(blk.locf(date, id.date)[2] == date[1])



###################################
# blk.singleValue and blk.applyNto1
###################################

df <- data.frame(id=levels(id),
    ans1 = blk.singleValue(x, id=id, fill=999),
    ans2 = blk.singleValue(x, id=id, ind=!is.na(x)),
    ans3 = blk.applyNto1(x, id=id, ind=!is.na(x), apply.fun=sum))

stopifnot(nrow(df) == length(levels(id)))

stopifnot(df$ans1[1] == x[id=="1"][1])
stopifnot(df$ans1[2] == x[id=="2"][1])
stopifnot(is.na(df$ans1[3]) && is.na(x[id=="3"][1]))
stopifnot(df$ans1[4] == 999)  # No value for id=4 so expect fill
stopifnot(is.na(df$ans1[5]) && is.na(x[id=="5"][1]))
stopifnot(is.na(df$ans1[6]) && is.na(x[id=="5"][6]))
stopifnot(df$ans1[7] == x[id=="7"][1])

stopifnot(is.na(df$ans2[4]))  # No value for id=4 so expect NA
stopifnot(df$ans2[5] == x[id=="5"][2])  # First non-NA value

stopifnot(df$ans3[1] == sum(x[id=="1"], na.rm=TRUE))
stopifnot(df$ans3[2] == sum(x[id=="2"], na.rm=TRUE))
stopifnot(is.na(df$ans3[3]))
stopifnot(is.na(df$ans3[4]))  # No value for id=4 so expect NA
stopifnot(df$ans3[5] == sum(x[id=="5"], na.rm=TRUE))
stopifnot(is.na(df$ans3[6]))
stopifnot(df$ans3[7] == sum(x[id=="7"], na.rm=TRUE))


###################################
# blk.repeatValue and blk.applyNtoN
###################################

df <- data.frame(id=id, x=x,
    ans1 = blk.repeatValue(x, id=id, fill=999),
    ans2 = blk.repeatValue(x, id=id, ind=!is.na(x)),
    ans3 = blk.applyNtoN(x, id=id, apply.fun=sum, na.rm=TRUE))

stopifnot(nrow(df) == length(id))
 
stopifnot(df$ans1[id=="1"] == x[id=="1"][1])
stopifnot(df$ans1[id=="2"] == x[id=="2"][1])
stopifnot(is.na(df$ans1[id=="3"]) && is.na(x[id=="3"][1]))
stopifnot(is.na(df$ans1[id=="5"]) && is.na(x[id=="5"][1]))
stopifnot(is.na(df$ans1[id=="6"]) && is.na(x[id=="5"][6]))
stopifnot(df$ans1[id=="7"] == x[id=="7"][1])

stopifnot(df$ans2[id=="5"] == x[id=="5"][2])  # First non-NA value

stopifnot(df$ans3[id=="1"] == sum(x[id=="1"], na.rm=TRUE))
stopifnot(df$ans3[id=="2"] == sum(x[id=="2"], na.rm=TRUE))
stopifnot(df$ans3[id=="3"] == 0)
stopifnot(df$ans3[id=="5"] == sum(x[id=="5"], na.rm=TRUE))
stopifnot(df$ans3[id=="6"] == 0)
stopifnot(df$ans3[id=="7"] == sum(x[id=="7"], na.rm=TRUE))

##########################
# blk.repeatValue with id2
##########################
id2 <- factor(rep(c(1:5, 7:8), each=2))
df <- data.frame(id=id2,
    ans1 = blk.repeatValue(x, id=id, id2=id2, fill=999),
    ans2 = blk.repeatValue(x, id=id, id2=id2, ind=!is.na(x)),
    ans3 = blk.repeatValue(blk.applyNto1(x, id=id, ind=!is.na(x), apply.fun=sum), id=asID(levels(id)), id2=id2))

stopifnot(nrow(df) == length(id2))
 
stopifnot(df$ans1[id2=="1"] == x[id=="1"][1])
stopifnot(df$ans1[id2=="2"] == x[id=="2"][1])
stopifnot(is.na(df$ans1[id2=="3"]) && is.na(x[id=="3"][1]))
stopifnot(df$ans1[id2=="4"] == 999)  # No value for id=4 so expect fill
stopifnot(is.na(df$ans1[id2=="5"]) && is.na(x[id=="5"][1]))
stopifnot(df$ans1[id2=="7"] == x[id=="7"][1])
stopifnot(df$ans1[id2=="8"] == 999)  # No value for id=8 so expect fill

stopifnot(is.na(df$ans2[id2=="4"]))  # No value for id=4 so expect NA
stopifnot(df$ans2[id2=="5"] == x[id=="5"][2])  # First non-NA value
stopifnot(is.na(df$ans2[id2=="8"]))  # No value for id=8 so expect NA

stopifnot(df$ans3[id2=="1"] == sum(x[id=="1"], na.rm=TRUE))
stopifnot(df$ans3[id2=="2"] == sum(x[id=="2"], na.rm=TRUE))
stopifnot(is.na(df$ans3[id2=="3"]))
stopifnot(is.na(df$ans3[id2=="4"]))  # No value for id=4 so expect NA
stopifnot(df$ans3[id2=="5"] == sum(x[id=="5"], na.rm=TRUE))
stopifnot(df$ans3[id2=="7"] == sum(x[id=="7"], na.rm=TRUE))
stopifnot(is.na(df$ans3[id2=="8"]))  # No value for id=8 so expect NA

