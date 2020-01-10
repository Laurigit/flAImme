#ADM_AI_CONF
#rm(STG_AI_CONF)
#rm(SRC_AI_CONF)
required_data(c("SRC_AI_CONF"), force_update = TRUE)
required_data(c("STG_AI_CONF", "STG_CYCLER"), force_update = TRUE)
temp <- STG_AI_CONF
temp[, KEY := 1]

settings <- temp[, .N, by = Setting][, KEY := 1]
cyclers <- STG_CYCLER[, .(CYCLER_ID, KEY = 1)]
cross_join <- merge(settings, cyclers, by = "KEY", all = TRUE, allow.cartesian = TRUE)

#join settings
join_first <- STG_AI_CONF[cross_join, on = c("CYCLER_ID", "Setting")]

setting2 <- temp[CYCLER_ID == -1, .(Setting, Value)]
#join rest settings
join_rst <- setting2[join_first, on = .(Setting)]
fixed_table <- join_rst[, .(Setting, CYCLER_ID, Value = ifelse(is.na(i.Value), Value, i.Value))]
ADM_AI_CONF <- fixed_table
