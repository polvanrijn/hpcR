process_console_output = function(raw_response){
  outputs = str_extract(raw_response, "\\[\\d+\\] .+?\n")
  outputs = str_rm_all(str_rm_all(outputs, '\n'), '\\[\\d+\\] ')
  eval_output = c()
  for (out in outputs){
    eval_output = c(eval_output, evaluate(out))
  }
  return(eval_output)
}

read_parameter_names_from_function = function(l){
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

read_function_name = function(fn){
  fn_name = substitute(fn)
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

# TODO remove, it is never used
evaluate = function(string){
  return(eval(parse(text=string)))
}

reindent_code = function(lines, indentation_level = 0){
  unnecessary_indent = min(stringr::str_locate(lines, "\\s+")[,"end"]) + 1
  if (is.na(unnecessary_indent)){
    unnecessary_indent = 0
  }
  lines = substring(lines, unnecessary_indent)

  if (indentation_level != 0){
    lines = paste(paste(rep(" ", 4*indentation_level), collapse = ""), lines)
  }

  return(lines)
}

build_script_and_save = function(fn_local_path, fn_str, real_parameters, fn_name, task_ID, args, settings = NULL) {
  # TODO use a templating engine, e.g.
  # template_r <- readLines(system.file("templates/slurm_run_R.txt",
  #                                     package = "rslurm"))
  # script_r <- whisker::whisker.render(template_r, list(pkgs = pkgs,
  #                                                      add_obj = !is.null(add_objects), nchunk = nchunk, cpus_per_node = cpus_per_node,
  #                                                      libPaths = libPaths))
  # writeLines(script_r, file.path(tmpdir, "slurm_run.R"))
  settings = get_settings(settings = settings)
  if (settings$slurm$enabled) {
    rscript_path = paste0(settings$slurm$r_path, "script")
    pkgs = get_required_packages_from_code(fn_str)
    jobname = build_jobname(fn_name, task_ID)
    slurm_call = c("packrat::init('~/tmp/')", "library(rslurm)")
    if (settings$slurm$mode == 'parallel') {
      slurm_call = c(
        slurm_call,
        paste("pars", names(args), sep = " = "),
        paste0("sjob = slurm_apply(", fn_name, ", pars, jobname = '", jobname, "', nodes = ", settings$slurm$nodes, ", cpus_per_node = ", settings$slurm$cpus_per_node, ", pkgs = ", deparse(pkgs), ", slurm_options = ", deparse(settings$slurm$options), ", rscript_path = ", deparse(rscript_path), ")")
      )
    } else if (settings$slurm$mode == 'single'){
      # You get something like:
      # pars = list(a = a, b = b)
      slurm_call = c(
        slurm_call,
        paste0("pars = list(", paste(real_parameters, real_parameters, sep = " = ", collapse = ", "), ")"),
        paste0("sjob = slurm_call(", fn_name, ", pars, jobname = '", jobname, "', pkgs = ", deparse(pkgs), ", slurm_options = ", deparse(settings$slurm$options), ", rscript_path = ", deparse(rscript_path), ")")
      )
    }

    fn_call = c(
      slurm_call,
      paste0("saveRDS(sjob, '", build_jobname(fn_name, task_ID, suffix = ".RDS"), "')")
    )
  } else{
    fn_call = paste0("saveRDS(", fn_name, "(", paste0(real_parameters, collapse = ", "), "), 'output.RDS')")
  }


  if (settings$telegram$enabled) {
    fn_str = redirect_print_function_to_telegram(settings, fn_str, task_ID)
    if (!settings$slurm$enabled) {
      fn_call = add_telegram_try_catch(settings, fn_call)
    }
  }

  script = c(
    "# First set the current working directory",
    get_prefix(settings),
    "",
    "# Create the function",
    fn_str,
    "",
    "# Import the variables",
    get_param_imports(real_parameters, task_ID, args, settings = settings),
    "",
    "# Call the function and return the output back",
    fn_call
  )

  fileConn = file(fn_local_path)
  writeLines(script, fileConn)
  close(fileConn)
}

get_prefix = function(settings = NULL){
  settings = get_settings(settings = settings)
  prefix = c(paste0('setwd("', settings$tmp_paths$hpc, '")'))

  if (settings$telegram$enabled && !settings$slurm$enabled) {
    prefix = add_telegram_functions(settings, prefix)
  }

  return(prefix)
}

get_param_imports = function(real_parameters, task_ID, args, settings = NULL){
  settings = get_settings(settings = settings)
  if (settings$slurm$enabled && settings$slurm$mode == 'parallel') {
    var_name = names(args)
    return(paste0(var_name, " = readRDS('", settings$tmp_paths$hpc, "/", var_name, "_", task_ID, ".RDS')"))
  } else {
    if (length(real_parameters) != 0) {
      return(paste0(real_parameters, " = readRDS('", settings$tmp_paths$hpc, "/", real_parameters, "_", task_ID, ".RDS')"))
    } else {
      return(c(''))
    }
  }
}

get_required_packages_from_code = function(fn_str) {
  library_lines = str_extract(fn_str, "$|\\s+library\\(.+?\\)")
  library_lines = str_rm_all(str_rm_all(str_rm_all(library_lines[library_lines != ""], "\\s+"), "library\\("), "\\)")

  # an example of a multi_library_definition is library(dplyr, ggplot2)
  multi_library_definitions = grepl(",", library_lines)
  multi_lib_idxs = which(multi_library_definitions)

  if (length(multi_lib_idxs) > 0){
    single_lib_lines = library_lines[!multi_library_definitions]
    for (idx in multi_lib_idxs){
      single_lib_lines = c(single_lib_lines, str_split(library_lines[idx], ",")[[1]])
    }
    library_lines = single_lib_lines
  }

  if (length(library_lines) == 0){
    library_lines = c()
  }

  return(library_lines)
}

insert_before = function(arr, new_val, idx){
  if (!is.character(arr)){
    stop('Needs to be a character array')
  }

  before = arr[1:(idx - 1)]
  after = arr[idx:length(arr)]
  return(c(before, new_val, after))
}

## String operations
str_extract = function(string, pattern){
  return(stringr::str_extract(string, pattern))
}

str_rm_all = function(string, pattern){
  return(stringr::str_replace_all(string, pattern, ''))
}


