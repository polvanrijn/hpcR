add_telegram_functions = function(settings, prefix){
  # TODO Add general function that adds possiblility to send figures

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

    telegram_initial_message = function(message){
      message = paste0("*", message, "*")
      telegram_send_message(message, F)
    }

    telegram_notify = function(type, message, custom_message = NULL){
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

  if (settings$telegram$send_on_start){
    fn_call = c(
      "telegram_initial_message('Script started')",
      fn_call
    )
  }

  if (settings$telegram$send_on_finish){
    fn_call = c(
      fn_call,
      "telegram_initial_message('Script finished')"
    )
  }

  return(fn_call)
}

redirect_print_function_to_telegram = function(settings, fn_str){
  if (settings$telegram$redirect_print){
    for (type in settings$telegram$output_types){
      fn_str = stringr::str_replace_all(fn_str, paste0("\\s+", type, "\\(|^", type, "\\("), paste0("telegram_notify('", type, "', "))
    }
  }
  return(fn_str)
}

