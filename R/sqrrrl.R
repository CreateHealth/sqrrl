#' @export
SELECT <- function(...) {
  cols <- c(...)
  if (!length(cols)) return("SELECT *")
  if (is.null(names(cols))) {
    paste('SELECT', commas(cols))
  } else {
    cols <- sapply(seq_along(cols), function(x) {
      if (names(cols)[x] == '') cols[x]
      else paste(cols[x], 'as', names(cols)[x])
    })
    paste("SELECT", commas(cols))
  }
}

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
    left_ids <- prefer_names(on)
    right_ids <- unname(on)
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
      join_conditions <- "USING" %+% parens(on)
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

  if (!is.data.frame(vals)) vals <- matrix(vals, nrow = 1)
  for (valcol in colnames(vals)) {
    vals[, valcol] <- quotes(vals[, valcol])
  }

  paste(
    "INSERT INTO",
    tbl,
    if(!is.null(cols)) parens(cols),
    'VALUES',
    commas(apply(vals, 1, function(x) parens(commas(x))))
  )
}
