require(PCSmisc)

mytapply <- function(x, id, fun) {
    do.call(c, tapply(x, id, fun))
}

# Generate test data
set.seed(123)
id  <- rep(c("C", "A", "B"), times=sample(2:8, 3, replace=TRUE))
id2 <- mytapply(id, id, function(x) sort(sample(1:3, length(x), replace=TRUE)))
x <- sample(1:10, length(id), replace=TRUE)
df <- data.frame(id=asID(id), id2=asID(id2), x=x)

with(df, blk.sequentialID(id=id))


# Generate test data
set.seed(123)
test.df1 <- data.frame(id=rep(1:5, each=7),
                 time=rep(c(0, 2^(0:5)), len=35),
                 evid=0,
                 amt=NA,
                 conc=ifelse(rbinom(35, 1, 0.1), NA, exp(rnorm(35))))

test.df2 <- data.frame(id=rep(1:5, each=2),
                   time=rep(c(0.1, 12), len=10),
                   evid=1,
                   amt=c(200, 400),
                   conc=NA)

test.dat <- rbind(test.df1, test.df2)
test.dat <- test.dat[order(test.dat$id, test.dat$time, test.dat$evid),]
test.dat$id <- asID(test.dat$id)

cbind(test.dat, ind=blk.onlyFirst(id=test.dat$id))
cbind(test.dat, ind=blk.onlyFirst(test.dat$id, !is.na(test.dat$amt)))
cbind(test.dat, ind=blk.onlyFirst(test.dat$id, is.na(test.dat$conc) & is.na(test.dat$amt)))

cbind(test.dat, ind=blk.onlyLast(id=test.dat$id))
cbind(test.dat, ind=blk.onlyLast(test.dat$id, !is.na(test.dat$amt)))
cbind(test.dat, ind=blk.onlyLast(test.dat$id, is.na(test.dat$conc) & is.na(test.dat$amt)))

cbind(test.dat, ind=blk.firstOnwards(test.dat$id, !is.na(test.dat$amt)))
cbind(test.dat, ind=blk.firstOnwards(test.dat$id, is.na(test.dat$conc) & is.na(test.dat$amt)))

cbind(test.dat, ind=blk.lastOnwards(test.dat$id, !is.na(test.dat$amt)))
cbind(test.dat, ind=blk.lastOnwards(test.dat$id, is.na(test.dat$conc) & is.na(test.dat$amt)))

cbind(test.dat, ind=blk.untilFirst(test.dat$id, !is.na(test.dat$amt)))
cbind(test.dat, ind=blk.untilFirst(test.dat$id, is.na(test.dat$conc) & is.na(test.dat$amt)))

cbind(test.dat, ind=blk.untilLast(test.dat$id, !is.na(test.dat$amt)))
cbind(test.dat, ind=blk.untilLast(test.dat$id, is.na(test.dat$conc) & is.na(test.dat$amt)))

cbind(test.dat, val=blk.locf(test.dat$conc, test.dat$id))
cbind(test.dat, val=blk.locf(test.dat$amt, test.dat$id, fill=0))
cbind(test.dat, val=blk.locf(test.dat$amt, test.dat$id, na.action="carry.back"))

cbind(test.dat, val=blk.repeatValue(test.dat$conc, test.dat$id, ind=test.dat$time==2))
cbind(test.dat, val=blk.repeatValue(test.dat$conc, test.dat$id, ind=!is.na(test.dat$conc) & test.dat$time>2))
cbind(test.dat, val=blk.repeatValue(test.dat$conc, test.dat$id, ind=!is.na(test.dat$conc) & test.dat$conc<0.3))

cbind(test.dat, tad=blk.tad(test.dat$time, test.dat$id, !is.na(test.dat$amt)))
cbind(test.dat, tad=blk.tad(test.dat$time, test.dat$id, !is.na(test.dat$amt), na.action="neg.time"))

cbind(test.dat, newID=blk.sequentialID(test.dat$id))

cbind(test.dat, sht=blk.shift(test.dat$amt, id=test.dat$id, shift.by=2))
cbind(test.dat, sht=blk.shift(test.dat$conc, id=test.dat$id, shift.by=-1))

cbind(test.dat, consec=blk.findConsecutive(test.dat$id, is.na(test.dat$amt)))

cbind(test.dat, itime=blk.intereventTime(test.dat$time, id=test.dat$id))
cbind(test.dat, itime=blk.intereventTime(test.dat$time, id=test.dat$id, !is.na(test.dat$amt)))





# Phenobarb
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

# Make sure that it behaves ok even if number of rows is inferior to the specified lag
cbind(dat, DIFF=blk.diff(time, id=asID(Subject), lag=13))

detach(dat)




###################################

set.seed(123)
id <- factor(rep(c("A", "B", "D", "E"), times=sample(2:8, 4, replace=TRUE)), levels=c("A", "B", "C", "D", "E"))
df <- data.frame(
    id  = id,
    id2 = unlist(tapply(id, id, function(x) sort(sample(1:3, length(x), replace=TRUE)), simplify=FALSE)),
    num = round(100*runif(length(id)), 1),
    fac = factor(unlist(tapply(id, id, function(x) rep(sample(1:2, 1), length(x)), simplify=FALSE)), labels=c("F", "M")),
    dt1  = strptime("2011-05-01", "%Y-%m-%d", tz="CET") + as.difftime(unlist(tapply(id, id, function(x) sort(runif(length(x), -1000, 1000)))), units="hours"))


df$dt2 <- as.POSIXlt(df$dt1)
df$dt3 <- as.Date(df$dt1)

unlist(tapply(df$fac, df$id, function(x) x[1], simplify=FALSE))      # Does not work for factors
do.call(c, tapply(df$fac, df$id, function(x) x[1], simplify=FALSE))  # Does not work for factors

unlist(tapply(df$dt1, df$id, function(x) x[1], simplify=FALSE))       # Does not work for POSIXct
do.call(c, tapply(df$dt1, df$id, function(x) x[1], simplify=FALSE))   # works for POSIXct (but drops TZ)

#unlist(tapply(df$dt2, df$id, function(x) x[1], simplify=FALSE))       # Does not work for POSIXlt
#try(do.call(c, tapply(df$dt2, df$id, function(x) x[1], simplify=FALSE)))   # Does not work for POSIXlt

unlist(tapply(df$dt3, df$id, function(x) x[1], simplify=FALSE))       # Does not work for Date
do.call(c, tapply(df$dt3, df$id, function(x) x[1], simplify=FALSE))   # Works for Date

tapply(seq_along(df$id), df$id, function(x) x[1], simplify=FALSE)
unlist(tapply(seq_along(df$id), df$id, function(x) x[1], simplify=FALSE))

df$num[unlist(tapply(seq_along(df$id), df$id, function(x) x[1], simplify=FALSE))]  # Works
df$fac[unlist(tapply(seq_along(df$id), df$id, function(x) x[1], simplify=FALSE))]  # Works
df$dt1[unlist(tapply(seq_along(df$id), df$id, function(x) x[1], simplify=FALSE))]  # Works
df$dt2[unlist(tapply(seq_along(df$id), df$id, function(x) x[1], simplify=FALSE))]  # Works
df$dt3[unlist(tapply(seq_along(df$id), df$id, function(x) x[1], simplify=FALSE))]  # Works

blk.singleValue(df$num, df$id, fill=0)                          # Works
blk.singleValue(df$fac, df$id, fill="F")                        # Works
blk.singleValue(df$dt1, df$id)                                  # Works
blk.singleValue(df$dt2, df$id)                                  # Works
blk.singleValue(df$dt3, df$id, fill=as.Date("2011-03-27"))      # Works

blk.repeatValue(df$num, df$id, fill=0)                          # Works
blk.repeatValue(df$fac, df$id, fill="F")                        # Works
blk.repeatValue(df$dt1, df$id)                                  # Works
blk.repeatValue(df$dt2, df$id)                                  # Works
blk.repeatValue(df$dt3, df$id, fill=as.Date("2011-03-27"))      # Works

blk.repeatValue(blk.singleValue(df$num, df$id), id2=df$id, fill=0)                          # Works
blk.repeatValue(blk.singleValue(df$fac, df$id), id2=df$id, fill="F")                        # Works
blk.repeatValue(blk.singleValue(df$dt1, df$id), id2=df$id)                                  # Works
blk.repeatValue(blk.singleValue(df$dt2, df$id), id2=df$id)                                  # Works
blk.repeatValue(blk.singleValue(df$dt3, df$id), id2=df$id, fill=as.Date("2011-03-27"))      # Works

data.frame(id=levels(df$id), 
    num.1 = blk.singleValue(df$num, df$id, fill=0),                          # Works
    fac.1 = blk.singleValue(df$fac, df$id, fill="F"),                        # Works
    dt1.1 = blk.singleValue(df$dt1, df$id),                                  # Works
    dt2.1 = blk.singleValue(df$dt2, df$id),                                  # Works
    dt3.1 = blk.singleValue(df$dt3, df$id, fill=as.Date("2011-03-27")))      # Works

data.frame(id=levels(df$id), 
    num.1 = blk.singleValue(df$num, df$id, ind=(df$id2==2), fill=0),                          # Works
    fac.1 = blk.singleValue(df$fac, df$id, ind=(df$id2==2), fill="F"),                        # Works
    dt1.1 = blk.singleValue(df$dt1, df$id, ind=(df$id2==2)),                                  # Works
    dt2.1 = blk.singleValue(df$dt2, df$id, ind=(df$id2==2)),                                  # Works
    dt3.1 = blk.singleValue(df$dt3, df$id, ind=(df$id2==2), fill=as.Date("2011-03-27")))      # Works

data.frame(id=levels(df$id), 
    num.1 = blk.singleValue(df$num, df$id, select="last", ind=(df$id2==2), fill=0),                          # Works
    fac.1 = blk.singleValue(df$fac, df$id, select="last", ind=(df$id2==2), fill="F"),                        # Works
    dt1.1 = blk.singleValue(df$dt1, df$id, select="last", ind=(df$id2==2)),                                  # Works
    dt2.1 = blk.singleValue(df$dt2, df$id, select="last", ind=(df$id2==2)),                                  # Works
    dt3.1 = blk.singleValue(df$dt3, df$id, select="last", ind=(df$id2==2), fill=as.Date("2011-03-27")))      # Works

cbind(df, 
    num.1 = blk.repeatValue(df$num, df$id, fill=0),                          # Works
    fac.1 = blk.repeatValue(df$fac, df$id, fill="F"),                        # Works
    dt1.1 = blk.repeatValue(df$dt1, df$id),                                  # Works
    dt2.1 = blk.repeatValue(df$dt2, df$id),                                  # Works
    dt3.1 = blk.repeatValue(df$dt3, df$id, fill=as.Date("2011-03-27")))      # Works


id2 <- factor(c("A", "G", "B", "C"), levels=c("A", "G", "B", "C"))

blk.repeatValue(df$num, df$id, id2, fill=0)                          # Works
blk.repeatValue(df$fac, df$id, id2, fill="F")                        # Works
blk.repeatValue(df$dt1, df$id, id2)                                  # Works
blk.repeatValue(df$dt2, df$id, id2)                                  # Works
blk.repeatValue(df$dt3, df$id, id2, fill=as.Date("2011-03-27"))      # Works


data.frame(id2=id2, 
    num.1 = blk.repeatValue(df$num, df$id, id2, fill=0),                          # Works
    fac.1 = blk.repeatValue(df$fac, df$id, id2, fill="F"),                        # Works
    dt1.1 = blk.repeatValue(df$dt1, df$id, id2),                                  # Works
    dt2.1 = blk.repeatValue(df$dt2, df$id, id2),                                  # Works
    dt3.1 = blk.repeatValue(df$dt3, df$id, id2, fill=as.Date("2011-03-27")))      # Works



data.frame(id2=id2, 
    num.1 = blk.repeatValue(df$num, df$id, id2, ind=(df$id2==2), fill=0),                          # Works
    fac.1 = blk.repeatValue(df$fac, df$id, id2, ind=(df$id2==2), fill="F"),                        # Works
    dt1.1 = blk.repeatValue(df$dt1, df$id, id2, ind=(df$id2==2)),                                  # Works
    dt2.1 = blk.repeatValue(df$dt2, df$id, id2, ind=(df$id2==2)),                                  # Works
    dt3.1 = blk.repeatValue(df$dt3, df$id, id2, ind=(df$id2==2), fill=as.Date("2011-03-27")))      # Works




blk.applyNto1(df$num, df$id, min, fill=0)
blk.applyNtoN(df$num, df$id, min, fill=0)

data.frame(id=levels(df$id), 
    c.1 = blk.applyNto1(df$id, df$id, length, ind=(df$id2==2), fill=0),       # Works
    c.2 = blk.count(df$id, NULL, ind=(df$id2==2)),                            # Works
    unq = blk.applyNto1(df$fac, df$id, nunique, fill=0),                      # Works
    avg = blk.applyNto1(df$num, df$id, mean, fill=0),                         # Works
    min = blk.applyNto1(df$num, df$id, min, fill=0),                          # Works
    max = blk.applyNto1(df$num, df$id, max, fill=0))                          # Works

cbind(df, 
    c.1 = blk.applyNtoN(df$id, df$id, length, ind=(df$id2==2), fill=0),  # Works, but compare to blk.count()
    c.2 = blk.count(df$id, ind=(df$id2==2)),                             # Works
    unq = blk.applyNtoN(df$fac, df$id, nunique),                         # Works
    avg = blk.applyNtoN(df$num, df$id, mean),                            # Works
    min = blk.applyNtoN(df$num, df$id, min),                             # Works
    max = blk.applyNtoN(df$num, df$id, max))                             # Works



# From help(POSIXt):
#
#     Using ‘c’ on ‘"POSIXlt"’ objects converts them to the current time
#     zone, and on ‘"POSIXct"’ objects drops any ‘"tzone"’ attributes
#     (even if they are all marked with the same time zone).
#

# Watch out!  Same time, different time zone!
blk.singleValue(df$dt1, df$id, select="last")
blk.applyNto1(df$dt1, df$id, apply.fun=max)
blk.singleValue(df$dt1, df$id, select="last") - blk.applyNto1(df$dt1, df$id, apply.fun=max)

# Watch out!  Same time, different time zone!
blk.repeatValue(df$dt1, df$id, select="last")
blk.applyNtoN(df$dt1, df$id, apply.fun=max)
blk.repeatValue(df$dt1, df$id, select="last") - blk.applyNtoN(df$dt1, df$id, apply.fun=max)


temp <- cbind(df, 
    shft = blk.shift(df$num, df$id))

blk.applyNto1(temp$shft, temp$id, apply.fun=sum)
blk.applyNto1(temp$shft, temp$id, apply.fun=sum, na.rm=TRUE)   # Extra arguments to apply.fun




x <- mixedData(c("1", "3", "BQL", "QNS", "5", "2"))
x
as.character(x)
as.numeric(x, fill=-1)
as.double(x, fill=-1)
unique(x)


crcl.cg(scr.mg.dL=0.9, weight.kg=112.9, age.yr=53, is.female=FALSE)
egfr.mdrd(scr.mg.dL=0.9, age.yr=53, is.female=FALSE, is.black=FALSE)

crcl.cg(scr.umol.L=0.9*88.4, weight.kg=112.9, age.yr=53, is.female=FALSE)
egfr.mdrd(scr.umol.L=0.9*88.4, age.yr=53, is.female=FALSE, is.black=FALSE)

