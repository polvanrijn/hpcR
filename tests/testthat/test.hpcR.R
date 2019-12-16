library(testthat)
library(hpcR)

host = Sys.getenv('SSH_HOST')
overwrite_default = list(
  telegram = list(
    token = Sys.getenv('TELEGRAM_TOKEN'),
    chat_id = Sys.getenv('TELEGRAM_CHAT_ID')
  ),
  tunnel = list(
    executable = Sys.getenv('SSH_EXECUTABLE'),
    args = strsplit(Sys.getenv('SSH_TUNNEL_ARGS'), ",")[[1]],
    timeout = 1
  )
)

test_that("Connection", {
  # First make sure the ssh is working, e.g.:
  # ssh -L 5902:localhost:22 -N hpc
  connect(host, overwrite_default)
  disconnect()
})

connect(host, overwrite_default)

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
  hpc_run(execute_on_server, list(data), download_on_finish = list('test.pdf'))
})

test_that("Internal errors are passed to telegram", {
  error_fn = function(){
    stop('Stop this bullshit')
  }
  hpc_run(error_fn, list())

  warning_fn = function(){
    warning('Stop this bullshit')
  }
  # Try a warning
  hpc_run(warning_fn, list())
  hpc_run(function(){warning('Inline is possible to')}, list())
})

test_that("External errors/warnings are passed to telegram", {
  # 1) Invoke an external error (since undefined)
  error_fn = function(){
    ggsave()
  }
  hpc_run(error_fn, list())

  # 2) Invoke an external warning
  warning_fn = function(){
    cor( c( 1 , 1 ), c( 2 , 3 ) )
  }
  hpc_run(warning_fn, list())

  # 3)Also works with inline
  hpc_run(function(){cor( c( 1 , 1 ), c( 2 , 3 ) )}, list())
})

test_that("Send file to telegram", {
  hpc_run(function(){telegram_upload_file('test.pdf')}, list())
})


test_that("Temp can be removed", {
  hpc_clear_tmp_folder()
})

test_that("Install a package on server", {
  hpc_install_CRAN_package('dplyr')
})


test_that("Send function to server", {
  out = hpc_run(execute_on_server, list(data), download_on_finish = list('test.pdf'))
  paste("The function outputted:", out)
  disconnect()
})

test_that("SLURM is working", {
  host = Sys.getenv('SSH_HOST')
  overwrite_default = list(
    telegram = list(
      token = Sys.getenv('TELEGRAM_TOKEN'),
      chat_id = Sys.getenv('TELEGRAM_CHAT_ID')
    ),
    tunnel = list(
      executable = Sys.getenv('SSH_EXECUTABLE'),
      args = strsplit(Sys.getenv('SSH_TUNNEL_ARGS'), ",")[[1]],
      timeout = 1
    )
  )
  connect(host, overwrite_default)

  df = data.frame(a = 1:20, b = 21:40)

  multiply = function(df){
    products = c()
    for (i in 1:nrow(df)){
      products = c(products, df[i, 1]*df[i, 2])
    }
    return(products)
  }

  result_normal = hpc_run(multiply, list(df))
  disconnect()

  # Do it the slurm way
  overwrite_default$slurm = list(
    enabled = T,
    mode = 'single',
    nodes = 8,
    cpus_per_node = 32,
    options = list(
      partition = 'octopus'
    ),
    r_path = 'module use /hpc/shared/EasyBuild/modules/all; module load R; R'
  )

  connect(host, overwrite_default)
  sjob = hpc_run(multiply, list(df = df))
  Sys.sleep(10)
  result_slurm = receive_output(sjob)

  if(!identical(result_slurm, result_normal)){
    stop('Slurm and normal way give different results')
  }
  disconnect()

  overwrite_default$slurm$mode = 'parallel'
  connect(host, overwrite_default)
  multiply_row_wise = function(a, b){
    return(a*b)
  }

  sjob_parallel = hpc_run(multiply_row_wise, list(df = df))
  Sys.sleep(10)
  result_slurm_parallel = unlist(receive_output(sjob_parallel))
  if(!identical(result_slurm_parallel, result_normal)){
    stop('Slurm and normal way give different results')
  }
  disconnect()
})


