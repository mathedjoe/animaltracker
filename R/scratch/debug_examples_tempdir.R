tdir <- tempdir()

write.csv(data.frame(x=1:300), file.path(tdir, "baban.csv"))

list.files(tdir)

tdf <- read.csv(file.path(tdir, "baban.csv"))
