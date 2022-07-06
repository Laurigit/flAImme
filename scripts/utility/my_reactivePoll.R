#https://stackoverflow.com/questions/52635904/shiny-reactivepoll-re-use-for-multiple-database-table-names/52638013
my_reactivePoll <- function(session, table_name, query, timeout = 1000, con)

{

  my.dataframe <- reactivePoll(timeout, session,
                               checkFunc = function() {
                               #  query <- paste0("SELECT count(", colname, ") from ",table_name)
                                 lastFeedback <-suppressWarnings(  as.data.table(dbFetch(dbSendQuery(con, query),
                                                      n = -1)))

                                 # checking logic here
                                 lastFeedback
                               },

                               # This function returns the content of the logfile
                               valueFunc = function() {
                                 data <-  dbSelectAll(table_name, con)
                                 # value logic here


                                 data
                               })

  my.dataframe

}
