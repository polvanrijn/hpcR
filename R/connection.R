#####################################
# Connection functions --------------

connect = function(host, overwrite_default = list()){
  # Default params
  settings = list(
    host = host,
    debug = F,
    tmp_paths = list(
      hpc = '/tmp/hpcR',
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
    slurm = list(
      enabled = F
    )
  )

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

  # Create session
  settings$session = ssh::ssh_connect(settings$host)

  # Check if telegram is installed
  if (!is.null(settings$telegram$token) && !is.null(settings$telegram$chat_id)){
    for (package_name in c('telegram.bot')){
      install_package_hpc(settings, package_name)
    }
    settings$telegram$enabled = TRUE
  } else{
    if (is.null(settings$telegram$token)){
      warning('You need to enter a valid telegram token')
    } else if (is.null(settings$telegram$chat_id)){
      warning('You need to enter a valid chat ID')
    }
  }

  # (Re)Create folder
  create_remote_tmp(settings)

  # Return settings
  return(settings)
}


disconnect = function(settings){
  settings = renew_session(settings)
  session = settings$session
  ssh::ssh_disconnect(session)
}

renew_session = function(settings){
  session = settings$session
  reconnect = TRUE

  tryCatch(
    {
      session_info = ssh::ssh_session_info(session)
      if (session_info$connected){
        reconnect = FALSE
      }
    },
    error = function(e){}
  )

  if (reconnect){
    session = ssh::ssh_connect(settings$host)
  }

  settings$session = session

  if (!dir.exists(settings$tmp_paths$local)){
    message("Local tmp directory was removed, so we created a new one")
    settings$tmp_paths$local = tempdir()
  }
  return(settings)
}








