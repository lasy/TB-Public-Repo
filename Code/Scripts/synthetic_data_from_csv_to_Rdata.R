


users = read.csv("../../../TB-Data/generated_csv/users.csv")
save(users, file = "../../../TB-Data/input_data/synthetic/users_filtered.Rdata")


cycles = read.csv("../../../TB-Data/generated_csv/cycles.csv")
save(cycles, file = "../../../TB-Data/input_data/synthetic/cycles_filtered.Rdata")

tracking = read.csv("../../../TB-Data/generated_csv/tracking.csv")
save(tracking, file = "../../../TB-Data/input_data/synthetic/tracking_filtered/tracking_filtered.Rdata")
