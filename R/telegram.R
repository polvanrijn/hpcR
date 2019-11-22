# Rename all functions to telegram_*, e.g. telegram_add_functions
add_telegram_functions = function(settings, prefix){

  telegram_prep = function(){
    library(telegram.bot)
    bot = Bot(token = token)

    telegram_send_message = function(message, disable_notification = T){
      bot$sendMessage(
        chat_id,
        text = message,
        disable_notification,
        parse_mode = "Markdown"
      )
    }

    telegram_upload_file = function(filename){
      if (any(grepl('\\/', filename))){
        path = filename
      } else{
        path = paste0(getwd(), '/', filename)
      }

      if (file.exists(path)){
        if (file.size(path) < 10000000){
          command = paste0('curl -v -F "chat_id=', chat_id, '" -F document=@', path, ' https://api.telegram.org/bot', token, '/sendDocument')
          system(command)
        } else {
          telegram_initial_message(paste(filename, "is larger than 10 MB and therefore cannot be uploaded"))
        }
      } else {
        telegram_initial_message(paste(filename, "does not exist and therefore cannot be uploaded"))
      }
    }

    telegram_initial_message = function(message){
      message = paste0("*", message, "*")
      telegram_send_message(message, F)
    }

    telegram_notify = function(type, message, custom_message = NULL){
      if (type %in% c('message', 'warning', 'stop', 'print')){
        eval_txt = paste0(type, '("', message, '")')

        if (is.null(custom_message)){
          if (type == 'warning'){
            message = paste0('Warning: `', message, '`')
          }
          else if(type == 'message'){
            message = paste0('_', message, '_')
          }
        } else{
          message = custom_message
        }

        # Send message
        telegram_send_message(message)

        # Execute the original
        eval(parse(text=eval_txt))
      }
    }
  }

  # Return the function
  telegram_str = deparse(telegram_prep)
  telegram_str = telegram_str[3:(length(telegram_str)-1)]
  telegram_str = reindent_code(telegram_str)

  prefix = c(
    prefix,
    "",
    "# Now write the variables into the script",
    paste0("token ='", settings$telegram$token, "'"),
    paste0("chat_id ='", settings$telegram$chat_id, "'"),
    "",
    "# Now call the functions required for telegram",
    telegram_str
  )
  return(prefix)
}

add_telegram_try_catch = function(settings, fn_call){
  fn_call = c(
    "tryCatch({",
    fn_call,
    "},",
    "error = function(e){",
    "t = paste(capture.output(traceback(1, max.lines = 1)), collapse = '\n')",
    "telegram_notify('stop', paste0(e, t), paste0('*SCRIPT ABORTED: ', e, '* `', t, '`'))",
    "})"
  )

  if (settings$telegram$send_on_start && !settings$slurm$enabled){
    fn_call = c(
      "telegram_initial_message('Script started')",
      fn_call
    )
  }

  if (settings$telegram$send_on_finish && !settings$slurm$enabled){
    fn_call = c(
      fn_call,
      "telegram_initial_message('Script finished')"
    )
  }

  return(fn_call)
}

redirect_print_function_to_telegram = function(settings, fn_str, task_ID){
  if (settings$telegram$redirect_print){
    for (type in settings$telegram$output_types){
      fn_str = stringr::str_replace_all(fn_str, paste0("\\s+", type, "\\(|^", type, "\\("), paste0("telegram_notify('", type, "', "))
    }
  }

  if (settings$slurm$enabled){
    head_fn = fn_str[1:2]
    middle_fn = fn_str[3:(length(fn_str) - 1)]
    if (settings$slurm$enabled){
      middle_fn = add_telegram_try_catch(settings, middle_fn)
    }
    bottom_fn = fn_str[length(fn_str)]

    lines = c(head_fn)

    if (settings$telegram$enabled){
      lines = c(lines, add_telegram_functions(settings, ''))
    }
    if (settings$telegram$send_on_start){
      lines = c(lines, paste0("telegram_initial_message('SLURM job (", task_ID, ") started')"))
    }

    lines = c(lines, middle_fn)

    if (settings$telegram$send_on_finish){
      msg = paste0("telegram_initial_message('SLURM job (", task_ID, ") ended')")
      idx_returns = grep("return\\(.+?\\)", lines)
      if (length(idx_returns) > 0){
        for (idx in idx_returns){
          return_stm = lines[idx]
          padding = ''
          if (substr(return_stm, 1,1) == " "){
            padding = stringr::str_extract(return_stm, "\\s+")
          }
          msg = paste0(padding, msg)
          lines = insert_before(lines, msg, idx)
        }
      } else{
        lines = c(lines, msg)
      }
    }

    lines = c(lines, bottom_fn)
    fn_str = lines
  }

  return(fn_str)
}

