#####################################
# HPC high level functions ----------
hpc_run = function(fn, args = NULL, download_on_finish = list(), settings = NULL){
  settings = get_settings(settings = settings)
  if (!is.function(fn)) {
    stop("fn must be a function")
  }
  if (is.null(args)) {
    num_parameters = 0
  } else if (is.list(args)) {
    num_parameters = length(args)
  } else{
    stop('args must be a list!')
  }

  slurm_parallel = FALSE
  if (settings$slurm$enabled) {
    if (is.null(names(args))) {
      stop("The rslurm package requires named parameter. Please use a named list as input")
    }

    if (settings$slurm$mode == 'parallel') {
      slurm_parallel = TRUE
      if (!(length(args) == 1 && is.data.frame(args[[1]]))) {
        stop('If you want to use slurm in parallel mode, you need to pass a list containing a single data frame. Each row is used as separate parameters to the function.')
      }
    } else if (settings$slurm$mode == 'single') {
      if (length(args) < 1) {
        stop("The rslurm package requires at least one named parameter")
      }
    }
  }

  if (!is.list(download_on_finish)) {
    stop('download_on_finish must be a list!')
  }

  fn_name = read_function_name(fn) # name of the function
  fn_str = deparse(fn) # function as string
  # hpc_install_missing_packages(settings, fn_str) # make sure all mentioned libraries are installed on the cluster

  real_parameters = read_parameter_names_from_function(fn_str[1])

  # Check if number of parameters match
  real_num_parameters = length(real_parameters)
  if (real_num_parameters != num_parameters) {
    if (!slurm_parallel) {
      stop("The number of parameters passed to this function must match the function statement")
    }
  }

  # In the case of named parameters, check if names match
  if (!is.null(names(args))) {
    named_parameters = TRUE
    if (!all(real_parameters %in% names(args))) {
      if (!slurm_parallel) {
        stop("Named parameters must have exactly the same name as specified in the function")
      }
    }
    if (!slurm_parallel) {
      # change order of parameters so they match the function
      args = args[real_parameters]
    }
  } else {
    named_parameters = FALSE
  }


  # add the name of the function
  fn_str[1] = paste(fn_name, "=", fn_str[1])

  # ID for the current task
  task_ID = paste(as.numeric(Sys.time())*10^6)

  if (length(args) > 0 && !slurm_parallel) {
    for (i in 1:length(args)) {
      param_name = real_parameters[i]
      if (named_parameters) {
        identifier = param_name
      } else {
        identifier = i
      }
      # Save parameter RDS locally and move to HPC
      RDS_path_local = build_path(settings$tmp_paths$local, paste0(param_name, ".RDS"))
      RDS_path_hpc = build_path(settings$tmp_paths$hpc, paste0(param_name, "_", task_ID, ".RDS"))
      saveRDS(args[[identifier]], RDS_path_local)
      hpc_move_to(RDS_path_local, RDS_path_hpc, settings = settings)
    }
  } else if (slurm_parallel) {
    # Save parameter RDS locally and move to HPC
    RDS_path_local = build_path(settings$tmp_paths$local, paste0(names(args), ".RDS"))
    RDS_path_hpc = build_path(settings$tmp_paths$hpc, paste0(names(args), "_", task_ID, ".RDS"))
    saveRDS(args[[names(args)]], RDS_path_local)
    hpc_move_to(RDS_path_local, RDS_path_hpc, settings = settings)
  }

  # Save function to file
  fn_hpc_path = build_path(settings$tmp_paths$hpc, paste0(fn_name, "_", task_ID, ".R"))
  fn_local_path = build_path(settings$tmp_paths$local, "/temp.R")

  # Build the script
  build_script_and_save(fn_local_path, fn_str, real_parameters, fn_name, task_ID, args, settings = settings)

  # Upload and delete file locally
  hpc_move_to(fn_local_path, fn_hpc_path, settings = settings)

  # execute on the server
  cmd = paste('Rscript', fn_hpc_path)
  #if (setting$debug){
  hpc_execute(cmd, live_output = T, settings = settings)
  #} else {
  #  hpc_execute(cmd, settings = settings)
  #}

  if (settings$slurm$enabled) {
    # Avoid being thrown out
    RDS_file = build_jobname(fn_name, task_ID, suffix = '.RDS')
  } else{
    RDS_file = 'output.RDS'
  }
  hpc_copy_from(paste0(settings$tmp_paths$hpc, "/", RDS_file), settings$tmp_paths$local, settings = settings)

  # download the results from the server
  if (length(download_on_finish) > 0) {
    filenames = c()
    for (fname in download_on_finish) {
      filenames = c(filenames, fname)
      fname = paste0(settings$tmp_paths$hpc, "/", fname)
      hpc_copy_from(fname, settings$tmp_paths$download, settings = settings)
    }
    message(paste(paste(filenames, collapse = ', '), "have been downloaded to:", settings$tmp_paths$download))
  }

  RDS_local_path = paste0(settings$tmp_paths$local, "/", RDS_file)
  attempts = 0
  while (TRUE) {
    if (file.exists(RDS_local_path)) {
      return(readRDS(RDS_local_path))
    } else {
      attempts = attempts + 1
    }

    if (attempts > 5) {
      warning(paste("Could not download", RDS_file))
      return(NULL)
    }
  }
}



#####################################
# HPC low level functions -----------
hpc_execute = function(command, live_output = F, settings = NULL){
  settings = get_settings(settings = settings)
  if (live_output) {
    return(ssh::ssh_exec_wait(settings$session, command = command))
  } else{
    out = ssh::ssh_exec_internal(settings$session, command = command)
    return(rawToChar(out$stdout))
  }
}

# HPC package management -----------
hpc_install_CRAN_package = function(package_name, silent = F, settings = NULL){
  settings = get_settings(settings = settings)
  raw = hpc_execute(paste0("R -e '!require(\"", package_name, "\")'"), settings = settings)
  out = process_console_output(raw)
  if (out[1]) {
    if (!silent) {
      message(paste(package_name, "is not installed yet, so let's install it"))
    }
    ssh::ssh_exec_wait(settings$session, paste0("R -e 'install.packages(\"", package_name, "\", repos=\"http://cran.us.r-project.org\")'"))
    if (!silent) {
      message(paste(package_name, "is now installed on the server"))
    }
  } else {
    if (!silent) {
      message(paste(package_name, "is already installed on the server"))
    }
  }
}

hpc_install_Github_package = function(package_repo, settings = NULL){
  settings = get_settings(settings = settings)
  raw = hpc_execute("R -e '!require(\"devtools\")'", settings = settings)
  out = process_console_output(raw)
  if (!out[1]) {
    hpc_install_CRAN_package("devtools", settings = settings)
  }
  ssh::ssh_exec_wait(settings$session, paste0("R -e 'devtools::install_github(\"", package_repo, "\")'"))
}

hpc_install_missing_packages = function(settings, fn_str) {
  package_names = get_required_packages_from_code(fn_str)

  for (pack_name in package_names) {
    hpc_install_CRAN_package(pack_name, T, settings = settings)
  }
}


# HPC upload and download -----
hpc_move_to = function(from, to, settings = NULL){
  settings = get_settings(settings = settings)
  # Upload
  hpc_copy_to(from, to, settings = settings)

  # Delete
  file.remove(from)
}

hpc_copy_to = function(from, to, settings = NULL){
  settings = get_settings(settings = settings)
  zip_name = paste0(tempdir(), '/temp.zip')
  from_split = strsplit(from, '\\/')[[1]]
  filename = tail(from_split, 1)
  unzip_it = FALSE
  if (length(from) > 1) {
    # Multiple files
    for (f in from) {
      if (!file.exists(f)) {
        stop(paste(f, 'does not exist'))
      }
    }

    # Zip it
    zip(zip_name, from, flags = "-j")
    from = zip_name
    unzip_it = TRUE
  } else {
    dot_count = stringr::str_count(filename, "\\.")
    if (dot_count > 1) {
      stop(paste(filename, "may not contain more than one dot"))
    } else if (dot_count == 1) {
      # is a file
      if (!file.exists(from)) {
        stop('File does not exist')
      }
    } else if (dot_count == 0) {
      # is a folder
      if (!dir.exists(from)) {
        stop('Folder does not exist')
      }
      zip(zip_name, paste0(from, list.files(from)), flags = "-j -q")
      from = zip_name
      unzip_it = TRUE
    }
  }
  # Upload to server
  settings = renew_session(settings)
  out = ssh::scp_upload(settings$session, from, to, verbose = settings$debug)

  # Sys.sleep(1)

  if (unzip_it) {
    file.remove(zip_name)
    sep = '/'
    if (tail(stringr::str_extract_all(filename, '.')[[1]], 1) == '/') {
      sep = ''
    }
    remote_zip_location = paste0(to, sep, "temp.zip")
    # For now extract into 'to', other possibility: remote_unzip_location = paste0(to, sep, filename)
    remote_unzip_location = to
    hpc_execute(paste('mkdir -p', remote_unzip_location), T, settings = settings)
    hpc_execute(paste('unzip -o', remote_zip_location, '-d', remote_unzip_location),  T, settings = settings)
    hpc_execute(paste('rm', remote_zip_location), T, settings = settings)
  }

  # filename = tail(strsplit(from, '\\/')[[1]], 1)
  # Move it to the tmp folder on the server
  #ssh::ssh_exec_internal(settings$session, paste('mv', filename, to))
}

hpc_copy_variable_to = function(variable, name, remote_location = '.', settings = NULL) {
  settings = get_settings(settings = settings)
  if (stringr::str_detect(name, ".RDS") != 1) {
    stop('Must be ending with .RDS')
  }

  if (!hpc_path_exists(remote_location, settings = settings)) {
    stop('Path does not exist on server')
  }

  local_path = paste0(tempdir(), "/", name)
  saveRDS(variable, local_path)
  hpc_move_to(local_path, remote_location, settings = settings)
}

hpc_copy_from = function(files, to = settings$tmp_paths$local, settings = NULL){
  settings = get_settings(settings = settings)
  if (!dir.exists(to)) {
    stop(paste("Directory", to, "does not exist"))
  }

  # Download to server
  ssh::scp_download(settings$session, files, to, verbose = settings$debug)
}

hpc_path_exists = function(path, settings = NULL){
  settings = get_settings(settings = settings)
  cmd = paste0('test -e ', path, ' && echo TRUE || echo FALSE')
  out = hpc_execute(cmd, settings = settings)
  return(evaluate(out))
}

hpc_make_dir = function(remote_dir_path, settings = NULL){
  settings = get_settings(settings = settings)
  if (hpc_path_exists(remote_dir_path, settings = settings)) {
    message('Dir already exist')
  } else {
    hpc_execute(paste('mkdir -p', remote_dir_path), T, settings = settings)
  }
}

hpc_file_list = function(remote_dir_path, settings = NULL){
  settings = get_settings(settings = settings)
  ls_results = hpc_ls(remote_dir_path)
  return(ls_results[!grepl('/', ls_results)])
}

hpc_ls = function(remote_dir_path, settings = NULL){
  settings = get_settings(settings = settings)
  if (hpc_path_exists(remote_dir_path)) {
    return(strsplit(hpc_execute(paste('ls -p', remote_dir_path), settings = settings), "\n")[[1]])
  } else {
    message('Dir does not exist')
  }
}

# HPC tmp folder -----
hpc_create_tmp_folder = function(settings){
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
          ssh::ssh_exec_internal(session, paste('mkdir -p', settings$tmp_paths$hpc))
        },
        error = function(e){
          stop(paste("Folder", settings$tmp_paths$hpc, "could not be created"))
        }
      )
    }
  )
}

hpc_clear_tmp_folder = function(settings = NULL){
  settings = get_settings(settings = settings)
  ssh::ssh_exec_internal(settings$session, paste('rm -R', settings$tmp_paths$hpc))
  hpc_create_tmp_folder(settings)
}


