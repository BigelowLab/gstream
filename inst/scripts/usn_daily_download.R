library(yaml)
cfg = yaml::read_yaml("~/.gstream")

dst = tempfile(fileext = ".sub")
ok = download.file(cfg$usn$dailyuri, dst)
x = read_wall_data_usn(tail(allfiles,n=1))

today = format(x$date[1], "%Y-%m-%d.sub") 
year = substring(today, 1,4)
opath = file.path(cfg$usn$rawpath, year)
if (!dir.exists(dirname(opath))) ok = dir.create(dirname(opath), recursive = TRUE)
mv = file.copy(from = dst, to = file.path(opath, today),
               overwrite = TRUE)
unlink(dst)


cat(ok, today, "\n",
    file = file.path(cfg$path, "usn", "download_log.txt"),
    append = TRUE)
