### Setup
library(data.table)

###
runners <- c(
    "A", # Runner 1
    "B", # Runner 2
    "C", # Runner 3
    "D", # Runner 4
    "E", # Runner 5
    "F", # Runner 6
    "G", # Runner 7
    "H", # Runner 8
    "I", # Runner 9
    "Austin", # Runner 10
    "K", # Runner 11
    "L"  # Runner 12
)
runner.dt <- data.table(runner_name = runners, runner_num = 1:12)
runner.dt[, pace_min := 8]
runner.dt[, pace_sec := 30]
DT::datatable(runner.dt, editable = T)

leg.dt <- fread("data/leg_data.csv")
leg.dt <- merge(leg.dt, runner.dt, by = "runner_num")
leg.dt[, total := (pace_min + pace_sec / 60) * distance]

makeTimeText <- function(time) {
    hour = (time - time %% 60) / 60
    min = (time - hour * 60) - time %% 1
    sec = round(time %% 1 * 60)
    text = paste0(ifelse(hour > 0, paste0(hour, ":"), ""), ifelse(min < 10, "0", ""), min, ":", ifelse(sec < 10, "0", ""), sec)
    return(text)
}
leg.dt[, total_txt := makeTimeText(total)]
leg.dt

person.totals <- leg.dt[, .(total = sum(total)), by = .(runner_name)]
person.totals[, total_text := makeTimeText(total)]
person.totals

van.totals <- leg.dt[, .(total = sum(total)), by = .(van, stage)]
van.totals[, total_txt := makeTimeText(total)]
van.totals

overall.total <- sum(van.totals$total)
makeTimeText(overall.total) 
