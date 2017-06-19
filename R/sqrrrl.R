#' Generate SELECT statement snippets
#'
#' Generate an SQL snippet for SELECT or SELECT DISTINCT
#'
#' @examples
#' SELECT(letters[1:3])
#' SELECT(letters[1:3], 't2' = letters[4:6])
#' SELECT(a = 'apple', b = 'banana', c = 'cherry')
#' SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry')
#' SELECT('t1' = c(a = 'apple', b = 'banana'), c = 'cherry', 't2' = c(d = 'dragon_fruit'))
#'
#' @param .distinct If true, uses SELECT DISTINCT as keyword
#' @param ... Columns to select. Single arguments are assumed to be column
#'   names. Single named arguments are renamed columns. Named arguments with a
#'   vector of length greater than 1 or a vector with named entries are assumed
#'   to be table names; entry names are column aliases.
#' @export
SELECT <- function(..., .distinct = FALSE) {
  cols <- list(...)
  has_names <- !is.null(names(cols))
  if (!has_names) {
    cols <- list(unlist(cols))
  } else {
    col_names <- names(cols)
    # Names can be table name or variable name
    # If named entry length > 1 or named entry has names, then table name
    # Else variable alias
    entry_length <- sapply(cols, length)
    has_entry_names <- sapply(cols, function(x) !is.null(names(x)))
    for (i in seq_along(cols)) {
      if (entry_length[i] == 1 & !has_entry_names[i]) {
        cols[[i]] <- setNames(cols[[i]], names(cols[i]))
        attributes(cols)$names[i] <- ''
      }
    }
  }
  SELECT_(cols, .distinct)
}

parse_table_cols <- function(table) {
  table$cols <- escape_col(table$cols)
  if (!is.null(table$name) && table$name != '') {
    if (!is.null(names(table$cols))) {
      col_names <- names(table$cols)
      table$cols <- setNames(paste(table$name, table$cols, sep = '.'), col_names)
    } else {
      table$cols <- paste(table$name, table$cols, sep = '.')
    }
  }
  if (is.null(names(table$cols))) {
    commas(table$cols)
  } else {
    sapply(seq_along(table$cols), function(x) {
      if (names(table$cols)[x] == '') table$cols[x]
      else paste(table$cols[x], 'as', names(table$cols)[x])
    })
  }
}

#' @describeIn SELECT Standard eval version of SELECT
#'
#' @examples
#' SELECT_(list('t1' = c('a', z = 'b'), 't2' = 'c'))
#'
#' @param table_cols Named list of tables and columns
#' @export
SELECT_ <- function(table_cols, .distinct = FALSE) {
  select <- ifelse(.distinct, 'SELECT DISTINCT', 'SELECT')
  if (!length(table_cols)) return(paste(select, "*"))
  table_cols <- lapply(seq_along(table_cols), function(i) {
    list('name' = names(table_cols[i]), cols = table_cols[[i]])
  })
  cols <- lapply(table_cols, parse_table_cols)
  cols <- unlist(cols)
  paste(select, commas(cols))
}

#' @describeIn SELECT Alias for `SELECT(..., .distinct = TRUE)`
#' @export
SELECT_DISTINCT <- function(...) SELECT(..., .distinct = TRUE)

#' @export
FROM <- function(...) {
  tables <- c(...)
  tables <- paste(tables, names(tables), sep = ' ', collapse = ', ')
  paste("FROM", tables)
}

#' @export
WHERE  <- function(..., cond=TRUE) ifelse(cond, paste('WHERE', AND(...)), '')

#' @export
AND   <- function(...) paste(c(...), collapse = ' AND ')

#' @export
OR    <- function(...) paste(c(...), collapse = ' OR ')

#' @export
GROUP_BY <- function(...) paste("GROUP BY", commas(...))

#' Joins of all flavors
#'
#' Produces a JOIN snippet
#'
#' @param type Join type string (can be lowercase): LEFT, RIGHT, INNER, CROSS,
#'   NATURAL {LEFT, RIGHT} [OUTER].
#' @param left_ref Reference for left table (name or alias)
#' @param right_tbls Character vector of right-hand side table names, names will
#'   be used as table aliases
#' @param on Vector or list of conditions. If a vector, all conditions are
#'   applied to all tables. If a list, conditions are matched by order to the
#'   RHS table names. Names of entries in any vectors are used as LHS column names.
#' @param cond Optional additional conditions.
#' @param prefer_using Should USING clause be used instead of ON where possible?
#' @rdname join
#' @export
JOIN <- function(type = '', left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  # Clean up RHS table names and add aliases if named
  if (is.list(right_tbls)) right_tbls <- unlist(lapply(right_tbls, `[`, 1))
  right_tbl_names <- paste(right_tbls, names(right_tbls))
  right_tbl_names <- gsub(' $', '', right_tbl_names)
  right_tbl_refs <- prefer_names(right_tbls)

  match_on_tbls <- function(left, right, on) {
    left_ids <- escape_col(prefer_names(on))
    right_ids <- escape_col(unname(on))
    paste(paste(left, left_ids, sep = '.'), paste(right, right_ids, sep = '.'), sep = '=')
  }

  if (length(on) != 1 && is.list(on)) {
    if (length(on) != 1 && length(on) != length(right_tbl_refs)) {
      stop("List of ON conditions must be same length as vector of RHS tables")
    }
    join_conditions <- sapply(seq_along(right_tbl_refs), function(i) {
        match_on_tbls(left_ref, right_tbl_refs[i], on[[i]])
    })
    join_conditions <- "ON" %+% parens(AND(join_conditions))
  } else {
    if (is.list(on)) on <- on[[1]]
    if (prefer_using && is.null(names(on))) {
      join_conditions <- "USING" %+% parens(escape_col(on))
    } else {
      join_conditions <- sapply(right_tbl_refs, match_on_tbls, left = left_ref, on = on)
      if (length(join_conditions) > 1) join_conditions <- "ON" %+% parens(AND(join_conditions))
      else join_conditions <- "ON" %+% join_conditions
    }
  }

  AND(
    paste(
      toupper(type),
      'JOIN',
      ifelse(length(right_tbl_names) > 1, parens(commas(right_tbl_names)), right_tbl_names),
      join_conditions
    ),
    cond
  )
}

#' @rdname join
LEFT_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN('left', left_ref, right_tbls, on, cond, prefer_using)
}

#' @rdname join
RIGHT_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN('right', left_ref, right_tbls, on, cond, prefer_using)
}

#' @rdname join
INNER_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN('inner', left_ref, right_tbls, on, cond, prefer_using)
}

#' @rdname join
OUTER_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN('outer', left_ref, right_tbls, on, cond, prefer_using)
}


#' INSERT INTO tbl VALUES
#'
#' Inserts into a table the values in a vector or data.frame. Subsets the values
#' by column name.
#'
#' @examples
#' INSERT_INTO_VALUES('table', 1:3)
#' INSERT_INTO_VALUES('table', c('a' = 1, 'b' = 2, 'c' = 3))
#' INSERT_INTO_VALUES('iris', iris[c(1, 51, 101), ], c("Sepal.Length", "Petal.Length", "Species"))
#'
#' @param tbl Table name to insert into
#' @param vals Values for insertion, can be a vector or a data.frame
#' @param cols Columns to include
#' @export
INSERT_INTO_VALUES <- function(tbl, vals, cols = NULL) {
  if (is.null(vals) || !length(vals)) return(NULL)
  val_col_mismatch <- FALSE
  if (!is.null(cols)) {
    if (is.data.frame(vals)) val_col_mismatch <- ncol(vals) < length(cols)
    else val_col_mismatch <- length(vals) < length(cols)
  }
  if (val_col_mismatch) stop("Number of value columns/entries was less than the number of columns specified.")

  if (is.null(cols)) {
    if (is.data.frame(vals)) cols <- colnames(vals)
    else if (!is.null(names(vals))) cols <- names(vals)
  } else {
    if (!is.null(names(vals)) && !is.data.frame(vals) && any(names(vals) == '')) names(vals) <- cols
    if (is.data.frame(vals)) vals <- vals[, cols]
    else if (!is.null(names(vals))) vals <- vals[cols]
  }

  if (!is.data.frame(vals)) vals <- vec2df(vals)

  if (is.data.frame(vals)) {
    for (valcol in colnames(vals)) {
      vals[, valcol] <- quotes(vals[, valcol])
    }
  }

  paste(
    "INSERT INTO",
    tbl,
    if(!is.null(cols)) parens(escape_col(cols)),
    'VALUES',
    commas(apply(vals, 1, function(x) parens(commas(x))))
  )
}
