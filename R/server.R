#####################################
# HPC high level functions ----------

run_fn = function(settings, fn, args = NULL, download_on_finish = list()){
  settings = renew_session(settings)
  if (!is.function(fn)){
    stop("fn must be a function")
  }
  if (is.null(args)){
    num_parameters = 0
  } else if (is.list(args)){
    num_parameters = length(args)
  } else{
    stop('args must be a list!')
  }

  if (!is.list(download_on_finish)){
    stop('download_on_finish must be a list!')
  }

  fn_name = extract_fn_name(fn) # name of the function
  fn_str = deparse(fn) # function as string
  real_parameters = compute_real_params(fn_str[1])

  # Check if number of parameters match
  real_num_parameters = length(real_parameters)
  if (real_num_parameters != num_parameters){
    stop("The number of parameters passed to this function must match the function statement")
  }

  # In the case of named parameters, check if names match
  if (!is.null(names(args))){
    named_parameters = TRUE
    if (!all(real_parameters %in% names(args))){
      stop("Named parameters must have exactly the same name as specified in the function")
    }

    # change order of parameters so they match the function
    args = args[real_parameters]
  } else{
    named_parameters = FALSE
  }


  # add the name of the function
  fn_str[1] = paste(fn_name, "=", fn_str[1])

  # ID for the current task
  task_ID = paste(as.numeric(Sys.time())*10^6)

  if (length(args) > 0){
    for (i in 1:length(args)){
      param_name = real_parameters[i]

      # Save RDS locally
      RDS_path_local = build_path(settings$tmp_paths$local, paste0(param_name, ".RDS"))
      RDS_path_hpc = build_path(settings$tmp_paths$hpc, paste0(param_name, "_", task_ID, ".RDS"))

      if (named_parameters){
        saveRDS(args[[param_name]], RDS_path_local)
      } else{
        saveRDS(args[[i]], RDS_path_local)
      }
      upload_and_delete(settings, RDS_path_local, RDS_path_hpc)
    }
  }

  # Save function to file
  fn_hpc_path = build_path(settings$tmp_paths$hpc, paste0(fn_name, "_", task_ID, ".R"))
  fn_local_path = build_path(settings$tmp_paths$local, "/temp.R")

  # Build the script
  build_script_and_save(settings, fn_local_path, fn_str, real_parameters, fn_name, task_ID)

  # Upload and delete file locally
  upload_and_delete(settings, fn_local_path, fn_hpc_path)

  # execute on the server
  cmd = paste('Rscript', fn_hpc_path)
  #if (setting$debug){
  exec_live(settings, cmd)
  #} else {
  #  exec(settings, cmd)
  #}


  download(settings, paste0(settings$tmp_paths$hpc, "/output.RDS"), settings$tmp_paths$local)

  # download the results from the server
  if(length(download_on_finish) > 0){
    filenames = c()
    for(fname in download_on_finish){
      filenames = c(filenames, fname)
      fname = paste0(settings$tmp_paths$hpc, "/", fname)
      download(settings, fname, settings$tmp_paths$download)
    }
    message(paste(paste(filenames, collapse = ', '), "have been downloaded to:", settings$tmp_paths$download))
  }

  return(readRDS(paste0(settings$tmp_paths$local,"/output.RDS")))
}


#####################################
# HPC low level functions -----------

exec = function(settings, command){
  settings = renew_session(settings)
  out = ssh::ssh_exec_internal(settings$session, command = command)
  return(rawToChar(out$stdout))
}

exec_live = function(settings, command){
  settings = renew_session(settings)
  return(ssh::ssh_exec_wait(settings$session, command = command))
}

create_remote_tmp = function(settings){
  settings = renew_session(settings)
  session = settings$session
  tryCatch(
    {
      ssh::ssh_exec_internal(session, paste('cd', settings$tmp_paths$hpc))
    }
    ,
    error = function(e){
      tryCatch(
        {
          ssh::ssh_exec_internal(session, paste('mkdir', settings$tmp_paths$hpc))
        },
        error = function(e){
          stop(paste("Folder", settings$tmp_paths$hpc, "could not be created"))
        }
      )
    }
  )
}



upload_and_delete = function(settings, from, to){
  # Upload
  upload(settings, from, to)

  # Delete
  file.remove(from)
}

upload = function(settings, from, to){
  if (!file.exists(from)){
    stop(paste("File", from, "does not exist"))
  }
  filename = tail(strsplit(from, '\\/')[[1]], 1)

  # Upload to server
  settings = renew_session(settings)
  session = settings$session
  out = ssh::scp_upload(session, from)

  # Move it to the tmp folder on the server
  ssh::ssh_exec_internal(session, paste('mv', filename, to))
}

download = function(settings, files, to = settings$tmp_paths$local){
  if (!dir.exists(to)){
    stop(paste("Directory", to, "does not exist"))
  }

  # Download to server
  settings = renew_session(settings)
  ssh::scp_download(settings$session, files, to, verbose = settings$debug)
}

install_package_hpc = function(settings, package_name){
  raw = exec(settings, paste0("R -e '!require(\"", package_name, "\")'"))
  out = extract_output(raw)
  if (out[1]){
    message(paste(package_name, "is not installed yet, so let's install it"))
    ssh::ssh_exec_wait(settings$session, paste0("R -e 'install.packages(\"", package_name, "\", repos=\"http://cran.us.r-project.org\")'"))
    message(paste(package_name, "is now installed on the server"))
  } else {
    message(paste(package_name, "is already installed on the server"))
  }
}

empty_remote_tmp = function(settings){
  settings = renew_session(settings)
  ssh::ssh_exec_internal(settings$session, paste('rm -R', settings$tmp_paths$hpc))
  create_remote_tmp(settings)
}

