
file.remove("time_log.csv")

tic()
elapsed = toc(quiet = TRUE)
df = data.frame(datetime = format(Sys.time(), usetz = FALSE),
                local_user = par$local_user,
                chunck_name = "init",
                duration_sec = round(elapsed$toc - elapsed$tic),
                duration_min = round((elapsed$toc - elapsed$tic)/60,2),
                duration_hour = round((elapsed$toc - elapsed$tic)/60/60,4),
                dataset = par$data_type)
write_csv(df, path = "time_log.csv", append = TRUE,col_names = TRUE)


