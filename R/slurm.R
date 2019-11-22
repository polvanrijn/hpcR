# TODO Rename to slurm_receive_output
receive_output = function(settings, sjob, prettyname = NULL){
  jobname = to_job_name(sjob)

  if (is.null(prettyname)){
    prettyname = jobname
  }

  send_on_start = settings$telegram$send_on_start
  send_on_finish = settings$telegram$send_on_finish
  slurm_enabled = settings$slurm$enabled
  output_fn = function(jobname){
    result_path = paste0("_rslurm_", jobname,"/results_0.RDS")
    has_result = file.exists(result_path)

    if (has_result){
      return(readRDS(result_path))
    } else{
      return(FALSE)
    }
  }
  settings$slurm$enabled = F
  settings$telegram$send_on_start = F
  settings$telegram$send_on_finish = F
  tryCatch({
    out = hpc_run(settings, output_fn, list(jobname = jobname))
    message(paste("Downloaded", prettyname))
  }, error = function(er){
    warning(er)
  })

  settings$slurm$enabled = slurm_enabled
  settings$telegram$send_on_start = send_on_start
  settings$telegram$send_on_finish = send_on_finish
  return(out)
}

to_job_name = function(sjob){
  if (typeof(sjob) == "list") {
    jobname = sjob$jobname
  } else {
    jobname = sjob
  }
  return(jobname)
}

slurm_send = function(){
  # mirror of hpc_run
}

slurm_result = function(settings, sjob){
  if (slurm_finished(settings, sjob)){
    hpc_copy_from(settings, paste0(get_slurm_dir(settings, sjob), '/results_0.RDS'))
    return(readRDS(paste0(settings$tmp_paths$local, '/results_0.RDS')))
  } else {
    warning('Result not finished!')
  }
}

slurm_finished = function(settings, sjob){
  path = paste0(get_slurm_dir(settings, sjob), '/results_0.RDS')
  return(hpc_path_exists(settings, path))
}

slurm_launch_script = function(settings, sjob){
  hpc_copy_from(settings, paste0(get_slurm_dir(settings, sjob), '/slurm_run.R'))
  file.edit(paste0(settings$tmp_paths$local, '/slurm_run.R'))
}

slurm_code = function(settings, sjob){
  hpc_copy_from(settings, paste0(get_slurm_dir(settings, sjob), '/f.RDS'))
  readRDS(paste0(settings$tmp_paths$local, '/f.RDS'))
  f_name_code = paste0(settings$tmp_paths$local, '/slurm_code.R')
  f = file(f_name_code)
  writeLines(deparse(readRDS(paste0(settings$tmp_paths$local, '/f.RDS'))), f)
  close(f)
  file.edit(f_name_code)
}

slurm_status = function(settings, sjob = NULL, follow = T){
  # If NULL reply with sqeue
  # else show output function
  if (is.null(sjob)) {
    if (follow) {
      cmd = 'squeue'
    } else{
      cmd = 'squeue -i 1'
    }
  } else{
    cmd = ''
    if (follow) {
      cmd = 'tail -f '
    }
    cmd = paste0(cmd, ' ',get_slurm_dir(settings, sjob), '/slurm_0.out')
  }
  hpc_execute(settings, cmd, live_output = T)
}

slurm_kill = function(settings, jobid){
  hpc_execute(settings, paste('scancel', jobid), live_output = T)
}

slurm_knit = function(path){
  # TODO: first check if it contains setwd --> reject such files
  # TODO: Fetch files from *.read('...,  *.read(", readRDS(... and transmit to server
  # TODO: make sure knitr and rmarkdown are installed on server
  # TODO: return html or upload to FTP
}

build_jobname = function(fn_name, task_ID, suffix = ''){
  return(paste0(paste("slurmjob", fn_name, task_ID, sep = "_"), suffix))
}

get_slurm_dir = function(settings, sjob){
  jobname = to_job_name(sjob)
  return(paste0(settings$tmp_paths$hpc, '/_rslurm_',jobname))
}
