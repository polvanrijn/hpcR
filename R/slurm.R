receive_output = function(settings, sjob){
  slurm_enabled = settings$slurm$enabled
  output_fn = function(sjob){
    result_path = paste0("_rslurm_", sjob$jobname,"/results_0.RDS")
    has_result = file.exists(result_path)

    if (has_result){
      return(readRDS(result_path))
    } else{
      return(FALSE)
    }
  }
  settings$slurm$enabled = F
  out = run_hpc(settings, output_fn, list(sjob = sjob))
  settings$slurm$enabled = slurm_enabled
  return(out)
}

build_jobname = function(fn_name, task_ID, suffix = ''){
  return(paste0(paste("slurmjob", fn_name, task_ID, sep = "_"), suffix))
}
