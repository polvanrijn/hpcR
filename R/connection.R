#####################################
# Connection functions --------------

connect = function(host, overwrite_default = list(), settings_name = "settings"){
  if (settings_object_exist(settings_name)) {
    message("Old connection already exist")
    tryCatch({
      empty_tunnel = is.null(settings$tunnel$process)
      if (!empty_tunnel) {
        old_settings = get(settings_name, envir = .GlobalEnv)
        disconnect(old_settings)
      } else {
        remove_settings_object(settings_name, settings)
      }
    }, error = function(ex){
      remove_settings_object(settings_name, settings)
    })

  }
  # Setup settings
  settings = set_settings(host, overwrite_default)

  # (Re)Create folder
  hpc_create_tmp_folder(settings)

  # Asign settings to global variable
  assign(settings_name, settings, envir = .GlobalEnv)
}


disconnect = function(settings_name = "settings", settings = NULL){
  settings = get_settings(settings = settings)

  ssh::ssh_disconnect(settings$session)

  if (settings$tunnel$enabled){
    if (subprocess::process_state(settings$tunnel$process) == "running"){
      if (subprocess::process_terminate(settings$tunnel$process)){
        message('Successfully disconnected from tunnel')
      } else{
        stop('Could not disconnect from tunnel!')
      }
    } else {
      message('Already disconnected from tunnel')
    }
  }
  remove_settings_object(settings_name, settings)
}

settings_object_exist = function(settings_name = "settings"){
  objs = ls(pos = ".GlobalEnv")
  return(length(objs[grep(settings_name, objs)]) == 1)
}

remove_settings_object = function(settings_name = "settings", settings = NULL){
  objs = ls(pos = ".GlobalEnv")
  rm(list = objs[grep(settings_name, objs)], pos = ".GlobalEnv")
}

get_settings = function(renew_session_connection = T, settings = NULL){
  if (is.null(settings)) {
    if (settings_object_exist()) {
      settings = get("settings", envir = .GlobalEnv)
    } else {
      stop('No connection active')
    }
  }

  if (renew_session_connection) {
    settings = renew_session(settings)
  }

  return(settings)
}

renew_session = function(settings){
  # Make sure the tunnel is on
  if (settings$tunnel$enabled){
    setup_tunnel = ''
    if(is.null(settings$tunnel$process)){
      setup_tunnel = 'new tunnel'
    } else if (subprocess::process_state(settings$tunnel$process) != "running"){
      setup_tunnel = 'existing tunnel'
    }

    if (setup_tunnel != ''){
      # Setup tunnel
      settings$tunnel$process = subprocess::spawn_process(settings$tunnel$executable, settings$tunnel$args)

      if (subprocess::process_state(settings$tunnel$process) != "running"){
        stop(paste(c('Cannot connect to ', setup_tunnel, '. Is the command correct? ', paste(c(settings$tunnel$executable, settings$tunnel$args), collapse = ' '))))
      } else {
        message(paste('Connected to', setup_tunnel))
      }
    }
  }

  # Always reconnect, except when connection still working
  reconnect = TRUE

  # Check if we are connected
  tryCatch(
    {
      session_info = ssh::ssh_session_info(settings$session)
      if (session_info$connected){
        reconnect = FALSE
      }
    },
    error = function(e){}
  )

  if (settings$tunnel$enabled && setup_tunnel == 'existing tunnel'){
    reconnect = TRUE
  }

  if (reconnect){
    # If we need to create session
    stop_loop = F
    num_tries = 50
    for (try in 1:num_tries){
      tryCatch(
        {
          settings$session = ssh::ssh_connect(settings$host)
          session_info = ssh::ssh_session_info(settings$session)
          if (session_info$connected){
            stop_loop = T
          }
        },
        error = function(e){
          Sys.sleep(settings$tunnel$timeout)
        }
      )
      if (stop_loop){
        break
      }
    }

    if (try == num_tries){
      stop(paste('Could not connect, tried', num_tries, 'times with a delay of each', settings$tunnel$timeout, 'seconds'))
    } else{
      message("Connected to new session")
    }
  }

  if (!dir.exists(settings$tmp_paths$local)){
    message("Local tmp directory was removed, so we created a new one")
    settings$tmp_paths$local = tempdir()
  }

  return(settings)
}

set_settings = function(host, overwrite_default = list(), settings = list(
  debug = F,
  tmp_paths = list(
    hpc = '~/hpcR',
    local = tempdir(),
    download_local = '~/Downloads'
  ),
  telegram = list(
    enabled = F,
    token = NULL,
    chat_id = NULL,
    output_types = c('print', 'message'),
    redirect_print = T,
    send_on_start = T,
    send_on_finish = T,
    silence_unimportant_msg = T
  ),
  tunnel = list(
    enabled = F,
    executable = NULL,
    args = NULL,
    process = NULL,
    timeout = 5
  ),
  slurm = list(
    enabled = F,
    mode = 'parallel',
    options = list(),
    nodes = 2,
    cpus_per_node = 2,
    r_path = 'R'
  )
)){
  settings$host = host

  if (!is.list(overwrite_default)){
    stop('overwrite_default must be a list')
  }

  if (length(overwrite_default) != 0){
    level_1_names = names(overwrite_default)
    if (!all(level_1_names %in% names(settings))){
      stop('Must contain exactly the same labels as settings file')
    }
    for (i in 1:length(overwrite_default)){
      l1_label = level_1_names[i]
      sub_item = overwrite_default[[l1_label]]
      if (is.list(sub_item)){
        if (length(sub_item) == 0){
          stop('You may not add an empty list')
        }

        level_2_names = names(sub_item)
        if (!all(level_2_names %in% names(settings[[l1_label]]))){
          stop('Must contain exactly the same labels as settings file')
        }

        for (l2_label in level_2_names){
          settings[[l1_label]][[l2_label]] = sub_item[[l2_label]]
        }
      } else{
        if (is.list(settings[[l1_label]])){
          stop("You may not replace a list with only one value")
        }
        settings[[l1_label]] = sub_item
      }
    }
  }

  # SSH TUNNEL
  if (!(is.null(settings$tunnel$executable) && is.null(settings$tunnel$args))){
    if (!(is.character(settings$tunnel$executable) && is.character(settings$tunnel$args))){
      stop("'executable' must be a valid path to ssh executable and 'args' must be a character array with each argument")
    }
    if (length(settings$tunnel$args) < 1){
      stop("'args' must be a character array with each argument")
    }

    if (!file.exists(settings$tunnel$executable)){
      stop(paste(settings$tunnel$executable), " does not exist!")
    }

    settings$tunnel$enabled = T
  }

  # Start the session
  settings = renew_session(settings)

  # Do some checks
  # TELEGRAM
  if (!is.null(settings$telegram$token) && !is.null(settings$telegram$chat_id)){
    #for (package_name in c('telegram.bot')){
    #  hpc_install_CRAN_package(package_name, T, settings = settings)
    #}
    settings$telegram$enabled = TRUE
  } else{
    if (is.null(settings$telegram$token)){
      warning('You need to enter a valid telegram token')
    } else if (is.null(settings$telegram$chat_id)){
      warning('You need to enter a valid chat ID')
    }
  }

  # SLURM
  if (settings$slurm$enabled){
    # hpc_install_CRAN_package('rslurm', T, settings = settings)

    if (!settings$slurm$mode %in% c('parallel', 'single')){
      stop('Only supported modes are "parallel" executing parameters from dataframe in parallel or "single" just running a function on slurm cluster.')
    }

    # if (!settings$slurm$outtype %in% c('table', 'raw')){
    #   stop('Only supported out types "table" or "raw".')
    # }

    if (!is.list(settings$slurm$options)){
      stop('Options must be a list!')
    }

    numeric_vars = c('nodes', 'cpus_per_node')
    for (v in numeric_vars){
      if (!is.numeric(settings$slurm[[v]])){
        stop(paste(v, "must be numeric!"))
      }
    }
  }
  return(settings)
}
