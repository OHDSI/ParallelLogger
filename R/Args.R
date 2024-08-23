# @file Args.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of ParallelLogger
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create an argument function
#'
#' @details
#' This function can be used to create a function that has (almost) the same interface as the
#' specified function, and the output of this function will be a list of argument values.
#'
#' @param functionName   The name of the function for which we want to create an args function.
#' @param excludeArgs    Exclude these arguments from appearing in the args function.
#' @param includeArgs    Include these arguments in the args function.
#' @param addArgs        Add these arguments to the args functions. Defined as a list with format name
#'                       = default.
#' @param rCode          A character vector representing the R code where the new function should be
#'                       appended to.
#' @param newName        The name of the new function. If not specified, the new name will be
#'                       automatically derived from the old name.
#'
#' @return
#' A character vector with the R code including the new function.
#'
#' @examples
#' createArgFunction("read.csv", addArgs = list(exposureId = "exposureId"))
#' @export
createArgFunction <- function(functionName,
                              excludeArgs = c(),
                              includeArgs = NULL,
                              addArgs = list(),
                              rCode = c(),
                              newName) {
  args <- formals(functionName)
  if (!is.null(includeArgs)) {
    args <- args[names(args) %in% includeArgs]
  }
  args <- args[!(names(args) %in% excludeArgs)]
  args <- append(args, addArgs)
  toChar <- function(x) {
    if (is.null(x)) {
      "NULL"
    } else if (is(x, "call")) {
      paste(capture.output(x), collapse = "")
    } else if (is(x, "character")) {
      paste("\"", x, "\"", sep = "")
    } else {
      as.character(x)
    }
  }
  args <- sapply(args, toChar)
  argInfo <- data.frame(name = names(args))
  argInfo$default <- NULL
  for (i in 1:length(args)) {
    argInfo$default[argInfo$name == names(args)[[i]]] <- args[[i]]
  }
  html <- capture.output(tools::Rd2HTML(.getHelpFile(help(functionName))))
  argsStartPos <- grep("<h3>Arguments</h3>", html) + 1
  tableEndPos <- grep("</table>", html)
  argInfo$help <- ""
  if (length(argsStartPos) == 1 && length(tableEndPos) > 0) {
    argsEndPos <- min(tableEndPos[tableEndPos > argsStartPos])
    parameterHelp <- xml2::read_html(paste(html[argsStartPos:argsEndPos], collapse = "\n"))
    parameterHelp <- xml2::xml_find_all(parameterHelp, "//table//tr//td")
    parameterHelp <- xml2::xml_text(parameterHelp)
    parameterHelp <- iconv(parameterHelp, from = "UTF-8", to = "ASCII")
    
    for (i in 1:(length(parameterHelp) / 2)) {
      argInfo$help[argInfo$name == parameterHelp[i * 2 - 1]] <- gsub("\n", " ", parameterHelp[i * 2])
    }
  }
  if (length(rCode) != 0) {
    rCode <- c(rCode, "")
  }
  rCode <- c(rCode, paste("#' Create a parameter object for the function", functionName))
  rCode <- c(rCode, "#'")
  rCode <- c(rCode, "#' @details")
  rCode <- c(rCode, "#' Create an object defining the parameter values.")
  rCode <- c(rCode, "#'")
  for (i in 1:nrow(argInfo)) {
    rCode <- c(rCode, paste("#' @param", argInfo$name[i], argInfo$help[i]))
  }
  rCode <- c(rCode, "#'")
  rCode <- c(rCode, "#' @export")
  if (missing(newName)) {
    createFunArgsName <- paste("create",
                               toupper(substr(functionName, 1, 1)),
                               substr(functionName, 2, nchar(functionName)),
                               "Args",
                               sep = ""
    )
  } else {
    createFunArgsName <- newName
  }
  header <- paste(createFunArgsName, "<- function(")
  for (i in 1:nrow(argInfo)) {
    if (i == 1) {
      start <- header
    } else {
      start <- paste(rep(" ", nchar(header)), collapse = "")
    }
    if (argInfo$default[i] == "") {
      end <- ""
    } else {
      end <- paste(" = ", argInfo$default[i], sep = "")
    }
    if (i == nrow(argInfo)) {
      end <- paste(end, ") {", sep = "")
    } else {
      end <- paste(end, ",", sep = "")
    }
    
    rCode <- c(rCode, paste(start, argInfo$name[i], end, sep = ""))
  }
  rCode <- c(rCode, "  analysis <- list()")
  rCode <- c(rCode, paste0("  for (name in names(formals(", createFunArgsName, "))) {"))
  rCode <- c(rCode, "    analysis[[name]] <- get(name)")
  rCode <- c(rCode, "  }")
  rCode <- c(rCode, "  class(analysis) <- \"args\"")
  rCode <- c(rCode, "  return(analysis)")
  rCode <- c(rCode, "}")
  return(rCode)
}

# copied from utils:::.getHelpFile because triple : not allowed
.getHelpFile <- function(file) {
  path <- dirname(file)
  dirpath <- dirname(path)
  if (!file.exists(dirpath)) {
    stop(gettextf("invalid %s argument", sQuote("file")), domain = NA)
  }
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  if (!file.exists(paste0(RdDB, ".rdx"))) {
    stop(gettextf(
      "package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed",
      sQuote(pkgname)
    ), domain = NA)
  }
  fetchRdDB(RdDB, basename(file))
}

# Copied from tools:::fetchRdDB
fetchRdDB <- function(filebase, key = NULL) {
  fun <- function(db) {
    vals <- db$vals
    vars <- db$vars
    datafile <- db$datafile
    compressed <- db$compressed
    envhook <- db$envhook
    fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]], datafile, compressed, envhook)
    if (length(key)) {
      if (!key %in% vars) {
        stop(gettextf("No help on %s found in RdDB %s", sQuote(key), sQuote(filebase)), domain = NA)
      }
      fetch(key)
    } else {
      res <- lapply(vars, fetch)
      names(res) <- vars
      res
    }
  }
  res <- lazyLoadDBexec(filebase, fun)
  if (length(key)) {
    res
  } else {
    invisible(res)
  }
}

#' Select variables from a list of objects of the same type
#'
#' @param x        A list of objects of the same type.
#' @param select   A character vector of names of variables to select.
#'
#' @examples
#'
#' x <- list(
#'   a = list(name = "John", age = 25, gender = "M"),
#'   b = list(name = "Mary", age = 24, gender = "F")
#' )
#' selectFromList(x, c("name", "age"))
#'
#' # $a
#' # $a$name
#' # [1] "John"
#' #
#' # $a$age
#' # [1] 25
#' #
#' #
#' # $b
#' # $b$name
#' # [1] "Mary"
#' #
#' # $b$age
#' # [1] 24
#' @export
selectFromList <- function(x, select) {
  return(sapply(x, function(x) {
    x[names(x)[names(x) %in% select]]
  }, simplify = FALSE))
}

#' Exclude variables from a list of objects of the same type
#'
#' @param x         A list of objects of the same type.
#' @param exclude   A character vector of names of variables to exclude.
#'
#' @export
excludeFromList <- function(x, exclude) {
  return(sapply(x, function(x) {
    x[names(x)[!(names(x) %in% exclude)]]
  }, simplify = FALSE))
}

#' In a list of object of the same type, find those that match the input
#'
#' @details
#' Typically, toMatch will contain a subset of the variables that are in the objects in the list. Any
#' object matching all variables in \code{toMatch} will be included in the result.
#'
#' @param x         A list of objects of the same type.
#' @param toMatch   The object to match.
#'
#' @return
#' A list of objects that match the \code{toMatch} object.
#'
#' @examples
#' x <- list(
#'   a = list(name = "John", age = 25, gender = "M"),
#'   b = list(name = "Mary", age = 24, gender = "F")
#' )
#'
#' matchInList(x, list(name = "Mary"))
#'
#' # $a
#' # $a$name
#' # [1] "John"
#' #
#' # $a$age
#' # [1] 25
#' #
#' #
#' # $b
#' # $b$name
#' # [1] "Mary"
#' #
#' # $b$age
#' # [1] 24
#' @export
matchInList <- function(x, toMatch) {
  selected <- selectFromList(x, names(toMatch))
  result <- list()
  for (i in 1:length(x)) {
    if (isTRUE(all.equal(selected[[i]], toMatch))) {
      result[[length(result) + 1]] <- x[[i]]
    }
  }
  return(result)
}

convertAttrToMember <- function(object) {
  if (is.function(object)) {
    return(list(serialized_code = as.character(serialize(object, NULL))))
  } else if (is.list(object)) {
    if (length(object) > 0) {
      for (i in 1:length(object)) {
        if (!is.null(object[[i]])) {
          object[[i]] <- convertAttrToMember(object[[i]])
        }
      }
    }
    a <- names(attributes(object))
    a <- a[!a %in% c("names", "class")]
    class <- class(object)
    if (length(class) > 1 || class != "list") {
      class(object) <- "list"
      object$attr_class <- class
    }
    if (length(a) > 0) {
      object[paste("attr", a, sep = "_")] <- attributes(object)[a]
    }
  }
  return(object)
}

convertMemberToAttr <- function(object) {
  if (is.list(object)) {
    if (length(object) > 0) {
      if (length(object) == 1 && !is.null(names(object)) && names(object) == "serialized_code") {
        return(unserialize(as.raw(sapply(object$serialized_code, strtoi, base = 16L))))
      }
      for (i in 1:length(object)) {
        if (!is.null(object[[i]])) {
          object[[i]] <- convertMemberToAttr(object[[i]])
        }
      }
      attrNames <- names(object)[grep("^attr_", names(object))]
      cleanNames <- gsub("^attr_", "", attrNames)
      if (any(cleanNames == "class")) {
        class(object) <- object$attr_class
        object$attr_class <- NULL
        attrNames <- attrNames[attrNames != "attr_class"]
        cleanNames <- cleanNames[cleanNames != "class"]
      }
      if (any(cleanNames == "row.names") &&  length(object$attr_row.names) ==  0) {
        object$attr_row.names <- as.character(c())
      }
      attributes(object)[cleanNames] <- object[attrNames]
      object[attrNames] <- NULL
    }
  }
  return(object)
}

#' Convert a settings object to a JSON string
#'
#' @details
#' Convert a settings object to a JSON string, using pretty formatting and preserving object classes
#' and attributes.
#'
#' @param object   R object to be converted.
#'
#' @return
#' A JSON string representing the R object.
#'
#' @export
convertSettingsToJson <- function(object) {
  object <- convertAttrToMember(object)
  json <- jsonlite::toJSON(object, pretty = TRUE, force = TRUE, null = "null", auto_unbox = TRUE)
  return(json)
}


#' Save a settings object as JSON file
#'
#' @details
#' Save a setting object as a JSON file, using pretty formatting and preserving object classes and
#' attributes.
#'
#' @param object     R object to be saved.
#' @param fileName   File name where the object should be saved.
#'
#' @export
saveSettingsToJson <- function(object, fileName) {
  fileName <- normalizePath(fileName, mustWork = FALSE)
  json <- convertSettingsToJson(object)
  write(json, fileName)
}

#' Converts a JSON string to a settings object
#'
#' @details
#' Converts a JSON string generated using the \code{\link{convertSettingsToJson}} function to a
#' settings object, restoring object classes and attributes.
#'
#' @param json   A JSON string.
#'
#' @return
#' An R object as specified by the JSON.
#'
#' @export
convertJsonToSettings <- function(json) {
  object <- jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  object <- convertMemberToAttr(object)
  # object <- restoreDataFrames(object)
  return(object)
}

#' Load a settings object from a JSON file
#'
#' @details
#' Load a settings object from a JSON file, restoring object classes and attributes.
#'
#' @param fileName   Name of the JSON file to load.
#'
#' @return
#' An R object as specified by the JSON.
#'
#' @export
loadSettingsFromJson <- function(fileName) {
  fileName <- normalizePath(fileName)
  if (!file.exists(fileName))
    stop(sprintf("File '%s' not found", fileName))
  json <- readChar(fileName, file.info(fileName)$size)
  object <- convertJsonToSettings(json)
  return(object)
}

# restoreDataFrames <- function(object) {
#   if (is.list(object)) {
#     if (length(object) > 0) {
#       if (is(object[[1]], "data.frame")) {
#         object <- do.call("rbind", object)
#       } else {
#         for (i in 1:length(object)) {
#           if (!is.null(object[[i]])) {
#             object[[i]] <- restoreDataFrames(object[[i]])
#           }
#         }
#       }
#     }
#   }
#   return(object)
# }
