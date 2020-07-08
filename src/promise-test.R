library(magrittr)
library(tidyverse)
library(promises)
library(zeallot)
library(future)

long_operation = function(x) {
  Sys.sleep(5)
  list(
       sum = sum(x),
       mean = mean(x),
       max = max(x),
       min = min(x)
  )
}

run_async = function(input) {
 
  output = 
    future(input) %...>%
    long_operation()
  print("We are waiting for result!")
  print("...")

  output %...T>%
  print()

}
