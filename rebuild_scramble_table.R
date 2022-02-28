library(glue)
library(stringr)

string1 = "1822631341052061172282914101911251223133814615271651718181193420402112223123282433253262127322842993029311632243335341535303673737381739364039"

rebuild_scramble_table <-function(string, num){
  number_left = as.character(seq(1:num))
  match_list = c()
  for (i in num:1) {
    # cat("i = ", i, "\n")
    # cat("string_remain = ", string, "\n")
    pattern = paste0(glue("{i}"), "(?=([0-9]{1,2}))")
    len = nchar(string)
    matches <- str_match_all(substr(string, len - 3, len), pattern)
    # cat("possible matches: \n")
    # print(matches)
    matched_array = matches[[1]]
    match = matched_array[matched_array[, 2] %in% number_left]
    # cat("matched number = ", match[2], "\n")
    number_left = number_left[!number_left %in% match[2]]
    # cat ("number left = \n")
    # print (number_left)
    match_list = append(match_list, match[2], after = 0)
    match_len = sum(nchar(match))
    # cat("characters to trim from the end: ", match_len, '\n')
    string = substr(string, 1, len - match_len)
    # cat("--end--\n")
    # cat(" \n")

  }
   match_list
}

match_list <- rebuild_scramble_table(string1, 40)
print(match_list)
