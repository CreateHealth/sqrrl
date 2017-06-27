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
  cols <- compact(cols)
  has_names <- !is.null(names(cols))
  if (!length(cols)) {
    cols <- NULL
  } else if (!has_names) {
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
  table$cols <- escape_col(table$cols, .ignore_dot = TRUE)
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
SELECT_ <- function(table_cols = NULL, .distinct = FALSE) {
  select <- ifelse(.distinct, 'SELECT DISTINCT', 'SELECT')
  if (is.null(table_cols) || !length(table_cols)) return(paste(select, "*"))
  table_cols <- lapply(seq_along(table_cols), function(i) {
    list('name' = names(table_cols[i]), cols = table_cols[[i]])
  })
  cols <- lapply(table_cols, parse_table_cols)
  cols <- unlist(cols)
  select %+% commas(cols)
}

#' @describeIn SELECT Alias for `SELECT(..., .distinct = TRUE)`
#' @export
SELECT_DISTINCT <- function(...) SELECT(..., .distinct = TRUE)

#' General SQL Snippet Functions
#'
#' Create SQL snippets to build SQL queries.
#'
#' @name general
NULL

#' @describeIn general Create FROM SQL snippet with optional table aliases
#' @examples
#' FROM('table1', 'table2')
#' FROM(t1 = 'table1', t2 = 'table2')
#' @export
FROM <- function(...) {
  tables <- c(...)
  tables <- paste(tables, names(tables), sep = ' ', collapse = ', ')
  "FROM" %+% tables
}

#' @describeIn general Generate WHERE SQL snippet if `cond` evaluates to TRUE, with
#'   arguments to WHERE concatenated by [`AND`]
#' @examples
#' WHERE('col1 IS NOT NULL')
#' WHERE(cond = TRUE, 'col1 = 2', 'col2 >= 10')
#' WHERE(cond = FALSE, 'col1 = 2', 'col2 >= 10')
#' @export
WHERE  <- function(..., cond=TRUE) ifelse(cond, 'WHERE' %+% AND(...), '')

#' @describeIn general Concatenate arguments with `AND`
#' @examples
#' AND(eq(id = 3, class = 'text_value'), geq(date = '2017-06-14'))
#' @export
AND   <- function(...) paste(c(...), collapse = ' AND ')

#' @describeIn general Concatenate arguments with `OR`
#' @examples
#' OR(eq(id = 9, id = 12), leq(id = 5))
#' @export
OR    <- function(...) paste(c(...), collapse = ' OR ')

#' @describeIn general Create `GROUP BY` SQL snippet, arguments are column names
#'   separated by commas
#' @examples
#' GROUP_BY('col1', 'col2', 'col3')
#' @export
GROUP_BY <- function(...) "GROUP BY" %+% commas(...)

#' @describeIn general Create `LIMIT` SQL snippet
#' @examples
#' LIMIT(10)
#' @export
LIMIT <- function(n = 1) if (is.numeric(n) && n > 0) "LIMIT" %+% as.integer(n)

#' @describeIn general Add `DESC` or `ASC` after column name
#' @export
DESC <- function(x) x %+% 'DESC'
ASC <- function(x) x %+% 'ASC'

#' @describeIn general Create `ORDER BY` SQL snippet, arguments are column names
#'   separated by commas
#' @examples
#' ORDER_BY('col1', 'col2', 'col3')
#' ORDER_BY(DESC('col1'), 'col2', ASC('col3'))
#' @export
ORDER_BY <- function(...) "ORDER BY" %+% commas(...)

#' Joins of all flavors
#'
#' Produces a JOIN snippet
#'
#' @examples
#' JOIN('left_tbl', 'right_tbl', 'id')
#' LEFT_JOIN('l', c('r' = 'right_tbl'), 'id')
#' LEFT_JOIN('l', c('r' = 'right_tbl'), 'id', prefer_using = FALSE)
#' RIGHT_JOIN('left_tbl', 'right_tbl', c('left.col1' = 'right.col1', 'id2'))
#' INNER_JOIN('left_tbl', c('right_1', 'right_2'), 'id_col')
#' OUTER_JOIN('l', c(r1 = 'right_1', r2 = 'right_2'), list('col1', 'col2'))
#' JOIN(type = "natural right", 'l', c(r1 = 'right_1', r2 = 'right_2'), list(c(left.col1 = 'col1', c(left.col2 = 'col2'))))
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
#' @name join
#' @export
JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE, type = '') {
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
#' @export
LEFT_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN(type = 'left', left_ref, right_tbls, on, cond, prefer_using)
}

#' @rdname join
#' @export
RIGHT_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN(type = 'right', left_ref, right_tbls, on, cond, prefer_using)
}

#' @rdname join
#' @export
INNER_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN(type = 'inner', left_ref, right_tbls, on, cond, prefer_using)
}

#' @rdname join
#' @export
OUTER_JOIN <- function(left_ref, right_tbls, on, cond = NULL, prefer_using = TRUE) {
  JOIN(type = 'outer', left_ref, right_tbls, on, cond, prefer_using)
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

  "INSERT INTO" %+%
    (tbl %+%
       if(!is.null(cols)) parens(escape_col(cols, .ignore_dot = FALSE))) %+%
    'VALUES' %+%
    commas(apply(vals, 1, function(x) parens(commas(x))))
}


SET <- function(...) paste("SET", commas(eq(...)))
SET_ <- function(set) {
  if (is.null(names(set))) {
    stop("set must be a named vector of column-value pairs.")
  }
  set <- vec2df(set)
  set <- sapply(names(set), function(x) eq_(x, set[[x]]), USE.NAMES = FALSE)
  "SET" %+% commas(set)
}

#' UPDATE
#'
#' Create `UPDATE` SQL statement
#'
#' @examples
#' UPDATE('iris', c(some_column = 1, some_other_col = "high"), eq(another_col = 2), geq(a_third_col = 10))
#' UPDATE('t1', c(col1 = 'col1 + 1'))
#' UPDATE('t1', c(col1 = 'col1 + 1', col2 = 'col1'))
#' UPDATE('t', c(id = 'id + 1'), .order = DESC('id'))
#'
#' @param tables Table name(s) for update (can be named)
#' @param set Named vector of column-value pairs, where the vector name is the
#'   column name.
#' @param ... Conditions passed on to `WHERE` clause (optional)
#' @param .ignore Add `IGNORE` keyword to `UPDATE` clause
#' @param .order Optional vector of columns passed to [`ORDER_BY`]
#' @param .limit Optional number of rows for `LIMIT` condition
UPDATE <- function(tables, set, ..., .ignore = FALSE, .order = NULL, .limit = NULL) {
  if (is.null(names(set))) {
    stop("`set` must be a named vector of column-value pairs.")
  }
  where <- c(...)
  update <- ifelse(!.ignore, "UPDATE", "UPDATE IGNORE") %+%
    paste(tables, names(tables), sep = ' ', collapse = ', ') %+%
    SET_(set) %+%
    WHERE(cond = length(where), where) %+%
    (
    if (!is.null(.order)) ORDER_BY(.order) %+%
    if (!is.null(.limit)) LIMIT(.limit)
    )
  gsub(" +$", "", update)
}