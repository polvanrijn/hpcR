library(testthat)
library(hpcR)

host = Sys.getenv('HOST')
overwrite_default = list(
  telegram = list(
    token = Sys.getenv('TELEGRAM_TOKEN'),
    chat_id = Sys.getenv('TELEGRAM_CHAT_ID')
  )
)

test_that("Connection", {
  # First make sure the ssh is working, e.g.:
  # ssh -L 5902:localhost:22 -N hpc
  settings = connect(host, overwrite_default)
  disconnect(settings)
})

settings = connect(host, overwrite_default)

## Define some function that will be recycled ------
execute_on_server = function(data){
  # Libraries
  library(ggplot2)

  # Chart
  p <- ggplot(data) +
    # Top
    geom_point( aes(x = var1, y = var2))
  xlab("value of x")

  message('PDF created')
  ggsave("test.pdf")
  print('Returning the value')
  return('Success!')
}

# Load variables into workspace
data = data.frame(
  var1 = rnorm(1000),
  var2 = rnorm(1000, mean=2)
)

######################################################

test_that("Setup telegram bot", {
  run_fn(settings, execute_on_server, list(data), download_on_finish = list('test.pdf'))
})

test_that("Internal errors are passed to telegram", {
  error_fn = function(){
    stop('Stop this bullshit')
  }
  run_fn(settings, error_fn, list())

  warning_fn = function(){
    warning('Stop this bullshit')
  }
  # Try a warning
  run_fn(settings, warning_fn, list())
  run_fn(settings, function(){warning('Inline is possible to')}, list())
})

test_that("External errors/warnings are passed to telegram", {
  # 1) Invoke an external error (since undefined)
  error_fn = function(){
    ggsave()
  }
  run_fn(settings, error_fn, list())

  # 2) Invoke an external warning
  warning_fn = function(){
    cor( c( 1 , 1 ), c( 2 , 3 ) )
  }
  run_fn(settings, warning_fn, list())

  # 3)Also works with inline
  run_fn(settings, function(){cor( c( 1 , 1 ), c( 2 , 3 ) )}, list())
})


test_that("Temp can be removed", {
  empty_remote_tmp(settings)
})

test_that("Install a package on server", {
  install_package_hpc(settings, 'dplyr')
})


test_that("Send function to server", {
  out = run_fn(settings, execute_on_server, list(data), download_on_finish = list('test.pdf'))
  paste("The function outputted:", out)
})

disconnect(settings)
