
extract_output = function(raw_response){
  outputs = str_extract(raw_response, "\\[\\d+\\] .+?\n")
  outputs = str_rm_all(str_rm_all(outputs, '\n'), '\\[\\d+\\] ')
  eval_output = c()
  for (out in outputs){
    eval_output = c(eval_output, evaluate(out))
  }
  return(eval_output)
}


compute_real_params = function(l){
  start_idx = stringr::str_locate(l, "\\(")[1]
  end_idx = stringr::str_locate(l, "\\)")[1]

  if (start_idx + 1 == end_idx){
    return(c())
  } else {
    params = substr(l, start_idx + 1, end_idx - 1)
    params = str_rm_all(params, " ")
    comma_idx = stringr::str_locate_all(params, ",")[[1]]
    if (nrow(comma_idx) == 0){
      return(c(params))
    } else{
      param_list = c()
      start = 0
      for (end in c(as.numeric(comma_idx[,"start"]), nchar(params) + 1)){
        param_list = c(param_list, substr(params, start + 1, end - 1))
        start = end
      }
      return(param_list)
    }
  }
}

extract_fn_name = function(fn){
  fn_name = deparse(substitute(fn))
  if (length(fn_name) == 1){
    return(fn_name)
  } else{
    return(str_rm_all(fn_name[1], " = function .+?$"))
  }
}

build_path = function(dir, filename){
  path = dir
  if (substr(dir, nchar(dir), nchar(dir)) != "/"){
    path = paste0(path, "/")
  }
  return(paste0(path, filename))
}

evaluate = function(string){
  return(eval(parse(text=string)))
}

reindent_code = function(lines, indentation_level = 0){
  unnecessary_indent = min(stringr::str_locate(lines, "\\s+")[,"end"]) + 1
  lines = substring(lines, unnecessary_indent)

  if (indentation_level != 0){
    lines = paste(paste(rep(" ", 4*indentation_level), collapse = ""), lines)
  }

  return(lines)
}

build_script_and_save = function(settings, fn_local_path, fn_str, real_parameters, fn_name, task_ID){
  fn_call = paste0("saveRDS(", fn_name, "(", paste0(real_parameters, collapse = ", "), "), 'output.RDS')")

  if (settings$telegram$enabled){
    fn_str = redirect_print_function_to_telegram(settings, fn_str)
    fn_call = add_telegram_try_catch(settings, fn_call)
  }

  script = c(
    "# First set the current working directory",
    get_prefix(settings),
    "",
    "# Create the function",
    fn_str,
    "",
    "# Import the variables",
    get_param_imports(settings, real_parameters, task_ID),
    "",
    "# Call the function and return the output back",
    fn_call
  )

  fileConn = file(fn_local_path)
  writeLines(script, fileConn)
  close(fileConn)
}

get_prefix = function(settings){
  prefix = c(paste0('setwd("', settings$tmp_paths$hpc, '")'))

  if (settings$telegram$enabled){
    prefix = add_telegram_functions(settings, prefix)
  }

  return(prefix)
}

get_param_imports = function(settings, real_parameters, task_ID){
  if (length(real_parameters) != 0){
    return(paste0(real_parameters, " = readRDS('", settings$tmp_paths$hpc, "/", real_parameters, "_", task_ID, ".RDS')"))
  } else {
    return(c(''))
  }
}

get_fn_call = function(){

}

## String operations
str_extract = function(string, pattern){
  return(stringr::str_extract(string, pattern))
}

str_rm_all = function(string, pattern){
  return(stringr::str_replace_all(string, pattern, ''))
}


