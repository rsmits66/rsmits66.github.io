#!/usr/bin/env Rscript
# ============================================================
#  update-tools.R
#  Refreshes the "Interactive Data Science Tools" page (tools.html)
#  so its catalog matches the files in the interactive_tools/ folder.
#
#  HOW TO RUN IT IN RStudio (no terminal needed):
#    1. Keep this file saved INSIDE your website folder -- the same
#       folder that contains tools.html and the interactive_tools
#       folder.
#    2. Open it in RStudio and click the  Source  button (top-right of
#       the editor window).  You'll see a report appear in the Console.
#
#  By default it only PREVIEWS what would change and does not touch any
#  files.  When you're happy with the preview and want to apply it,
#  change UPDATE_FILE below from FALSE to TRUE and click Source again.
#  (A backup of the old page is saved to tools.html.bak first.)
# ============================================================


## ========================= SETTINGS =========================
## The only things you might ever need to change are right here.

UPDATE_FILE <- TRUE    # FALSE = just preview.  TRUE = actually update tools.html.

REPO_DIR <- ""         # Leave blank to auto-find your website folder.
                       # Only fill this in if the auto-detect fails. Use
                       # FORWARD slashes, e.g.:
                       #   REPO_DIR <- "C:/Users/rsmits/rsmits66.github.io"

## ============================================================


# ---- settings can also be set from the command line (optional) ----
.args <- commandArgs(trailingOnly = TRUE)
if (any(.args %in% c("--write", "--update")))  UPDATE_FILE <- TRUE
if (any(.args %in% c("--check", "--preview"))) UPDATE_FILE <- FALSE

ROOT <- "interactive_tools"   # display name used in the report
SKIP_FILE <- "(^\\.)|(\\(\\d+\\)\\.html$)"   # ignore dotfiles and "name (1).html" copies

CAT_ORDER <- c("linear-algebra", "optimization", "probability-and-statistics",
               "analysis", "topology", "geometry")
SUB_ORDER <- c("linear-models", "probability-basics", "statistical-learning", "time-series",
               "stochastic-processes", "basic-statistics", "geostatistics",
               "harmonic-analysis", "differential-geometry", "algebraic-geometry")


# ---------- find the website folder ----------
# Pure base R -- no packages. RStudio's "Source" button runs source(),
# which records the script's path; that's what we read out of the call stack.
find_repo <- function() {
  # 1) explicit override wins
  if (nzchar(REPO_DIR)) return(normalizePath(REPO_DIR, winslash = "/", mustWork = FALSE))
  # 2) clicked "Source" (or used source()): the file path is in the call stack
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(dirname(normalizePath(of, winslash = "/")))
  }
  # 3) run with Rscript: the --file= argument
  m <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]), winslash = "/")))
  # 4) last resort: current working directory
  getwd()
}

REPO <- find_repo()
TOOLS_DIR <- file.path(REPO, "interactive_tools")
PAGE_PATH <- file.path(REPO, "tools.html")

if (!dir.exists(TOOLS_DIR) || !file.exists(PAGE_PATH)) {
  stop(paste0(
    "Couldn't find your website files.\n",
    "  Looked in: ", REPO, "\n",
    "  Expected to see both 'tools.html' and an 'interactive_tools' folder there.\n\n",
    "Fix: make sure this script is saved inside your website folder, then Source it again.\n",
    "Or set REPO_DIR near the top of this script to that folder's path (use / not \\)."
  ), call. = FALSE)
}


# ---------- helpers ----------
html_files <- function(d) {
  fs <- list.files(d, pattern = "\\.html$", ignore.case = TRUE)
  fs <- fs[!grepl(SKIP_FILE, fs, perl = TRUE, ignore.case = TRUE)]
  fs[order(tolower(fs), method = "radix")]
}
sub_dirs <- function(d) {
  ds <- list.dirs(d, full.names = FALSE, recursive = FALSE)
  ds[nzchar(ds)]
}
order_by <- function(items, pref) {
  key <- vapply(items, function(x) { i <- match(x, pref); if (is.na(i)) length(pref) + 1L else i }, integer(1))
  items[order(key, items)]
}
deslug <- function(s) {
  w <- strsplit(s, "-", fixed = TRUE)[[1]]
  w <- vapply(w, function(x) if (x == "and") "and" else paste0(toupper(substring(x, 1, 1)), substring(x, 2)), character(1))
  paste(w, collapse = " ")
}
slug <- function(s) {
  s <- tolower(s)
  s <- gsub("&", "and", s, fixed = TRUE)
  s <- gsub("[^a-z0-9]+", "-", s, perl = TRUE)
  gsub("^-|-$", "", s, perl = TRUE)
}
js_str <- function(s) paste0('"', s, '"')

paths_of <- function(tools) {
  ps <- character(0)
  for (cat in tools) {
    cs <- slug(cat$name)
    if (!is.null(cat$items)) for (f in cat$items) ps <- c(ps, paste0(ROOT, "/", cs, "/", f))
    if (!is.null(cat$subs)) for (sub in cat$subs) for (f in sub$items)
      ps <- c(ps, paste0(ROOT, "/", cs, "/", slug(sub$name), "/", f))
  }
  ps
}

# locate the `var TOOLS = [ ... ]` array; returns 1-based [start,end] of '[' .. ']'
find_bounds <- function(text) {
  anchor <- regexpr("var TOOLS", text, fixed = TRUE)[1]
  if (anchor < 0) stop('Could not find "var TOOLS" in tools.html', call. = FALSE)
  rest <- substring(text, anchor)
  eq_rel <- regexpr("=", rest, fixed = TRUE)[1]
  br_rel <- regexpr("[", substring(rest, eq_rel), fixed = TRUE)[1]
  start <- anchor + (eq_rel - 1L) + (br_rel - 1L)
  cv <- strsplit(substring(text, start), "", fixed = TRUE)[[1]]
  depth <- 0L; in_str <- ""; esc <- FALSE; i <- 1L
  while (i <= length(cv)) {
    c <- cv[i]
    if (nzchar(in_str)) {
      if (esc) esc <- FALSE
      else if (c == "\\") esc <- TRUE
      else if (c == in_str) in_str <- ""
    } else {
      if (c == '"' || c == "'") in_str <- c
      else if (c == "[") depth <- depth + 1L
      else if (c == "]") { depth <- depth - 1L; if (depth == 0L) break }
    }
    i <- i + 1L
  }
  list(start = start, end = start + i - 1L)
}

# read the existing catalog by transliterating the JS literal into R and eval-ing it
parse_existing <- function(text) {
  b <- find_bounds(text)
  raw <- substr(text, b$start, b$end)
  raw <- gsub("subs\\s*:\\s*\\[",  "subs=list(",  raw, perl = TRUE)
  raw <- gsub("items\\s*:\\s*\\[", "items=c(",    raw, perl = TRUE)
  raw <- gsub("name\\s*:",         "name=",       raw, perl = TRUE)
  raw <- gsub("{", "list(", raw, fixed = TRUE)
  raw <- gsub("}", ")",     raw, fixed = TRUE)
  raw <- gsub("[", "list(", raw, fixed = TRUE)
  raw <- gsub("]", ")",     raw, fixed = TRUE)
  repeat { z <- gsub(",(\\s*\\))", "\\1", raw, perl = TRUE); if (identical(z, raw)) break; raw <- z }
  eval(parse(text = raw))
}

serialize <- function(tools) {
  I <- "    "
  out <- "[\n"
  for (ci in seq_along(tools)) {
    cat <- tools[[ci]]
    has_items <- !is.null(cat$items) && length(cat$items) > 0
    has_subs  <- !is.null(cat$subs)  && length(cat$subs)  > 0
    out <- paste0(out, I, I, "{\n")
    out <- paste0(out, I, I, I, "name: ", js_str(cat$name), ",\n")
    if (has_items) {
      out <- paste0(out, I, I, I, "items: [\n")
      for (f in cat$items) out <- paste0(out, I, I, I, I, js_str(f), ",\n")
      out <- paste0(out, I, I, I, "]", if (has_subs) "," else "", "\n")
    }
    if (has_subs) {
      out <- paste0(out, I, I, I, "subs: [\n")
      for (sub in cat$subs) {
        out <- paste0(out, I, I, I, I, "{ name: ", js_str(sub$name), ", items: [\n")
        for (f in sub$items) out <- paste0(out, I, I, I, I, I, js_str(f), ",\n")
        out <- paste0(out, I, I, I, I, "] },\n")
      }
      out <- paste0(out, I, I, I, "]\n")
    }
    out <- paste0(out, I, I, "}", if (ci < length(tools)) "," else "", "\n")
  }
  paste0(out, I, "]")
}


# ---------- tool-title extraction ----------
# A tool's display title comes from its own page: the <h1> if present,
# otherwise the <title> with any "<em> -- subtitle</em>" trimmed off,
# otherwise NULL so the page falls back to the file-name title.
# Everything here works on raw bytes so UTF-8 (en-dashes, accents, etc.)
# is preserved; output is written straight into the UTF-8 page.

FT_START <- "/* FILE_TITLES:START */"
FT_END   <- "/* FILE_TITLES:END */"
EMDASH   <- rawToChar(as.raw(c(0xE2, 0x80, 0x94)))     # the "long dash" subtitle separator
JUNK_TITLES <- c("", "document", "untitled", "untitled document", "page", "index", "home", "title")

read_bytes <- function(path) {
  sz <- file.info(path)$size
  if (is.na(sz) || sz <= 0) return("")
  readChar(path, sz, useBytes = TRUE)
}

.utf8 <- function(cp) {
  if (cp < 0x80)         as.raw(cp)
  else if (cp < 0x800)   as.raw(c(0xC0 + cp %/% 0x40, 0x80 + cp %% 0x40))
  else if (cp < 0x10000) as.raw(c(0xE0 + cp %/% 0x1000, 0x80 + (cp %/% 0x40) %% 0x40, 0x80 + cp %% 0x40))
  else                   as.raw(c(0xF0 + cp %/% 0x40000, 0x80 + (cp %/% 0x1000) %% 0x40, 0x80 + (cp %/% 0x40) %% 0x40, 0x80 + cp %% 0x40))
}

decode_entities <- function(s) {
  ms <- regmatches(s, gregexpr("&#[xX]?[0-9A-Fa-f]+;", s, perl = TRUE, useBytes = TRUE))[[1]]
  for (h in unique(ms)) {
    body <- sub(";$", "", sub("^&#", "", h))
    cp <- if (grepl("^[xX]", body)) strtoi(sub("^[xX]", "", body), 16L) else strtoi(body, 10L)
    if (!is.na(cp) && cp > 0) s <- gsub(h, rawToChar(.utf8(cp)), s, fixed = TRUE)
  }
  named <- list(quot=34, apos=39, lt=60, gt=62, nbsp=32, mdash=8212, ndash=8211,
                hellip=8230, lsquo=8216, rsquo=8217, ldquo=8220, rdquo=8221,
                times=215, deg=176, le=8804, ge=8805, ne=8800, minus=8722, amp=38)
  for (nm in names(named)) s <- gsub(paste0("&", nm, ";"), rawToChar(.utf8(named[[nm]])), s, fixed = TRUE)
  s
}

clean_text <- function(s) {
  if (is.null(s) || !nzchar(s)) return("")
  s <- gsub("<br\\s*/?>", " ", s, perl = TRUE, ignore.case = TRUE, useBytes = TRUE)
  s <- gsub("</(p|div|h[1-6]|li|tr|section|article)\\s*>", " ", s, perl = TRUE, ignore.case = TRUE, useBytes = TRUE)
  s <- gsub("<[^>]+>", "", s, perl = TRUE, useBytes = TRUE)
  s <- decode_entities(s)
  s <- gsub("\\s+", " ", s, perl = TRUE, useBytes = TRUE)
  trimws(s)
}

first_inner <- function(html, tag) {
  has <- grepl(paste0("(?is)<", tag, "\\b[^>]*>.*?</", tag, ">"), html, perl = TRUE, useBytes = TRUE)
  if (!has) return(NULL)
  sub(paste0("(?is).*?<", tag, "\\b[^>]*>(.*?)</", tag, ">.*"), "\\1", html, perl = TRUE, useBytes = TRUE)
}

extract_title <- function(path) {
  html <- read_bytes(path)
  if (!nzchar(html)) return(NULL)
  h1 <- clean_text(first_inner(html, "h1"))
  if (nzchar(h1) && !(tolower(h1) %in% JUNK_TITLES)) return(h1)
  ttl <- clean_text(first_inner(html, "title"))
  ttl <- trimws(sub(paste0("\\s*", EMDASH, ".*$"), "", ttl, perl = TRUE, useBytes = TRUE))
  if (nzchar(ttl) && !(tolower(ttl) %in% JUNK_TITLES)) return(ttl)
  NULL
}

# JS double-quoted literal for a title (escape \ and "), kept as UTF-8 bytes
js_title <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub('"', '\\"', s, fixed = TRUE)
  s <- gsub("[\r\n\t]", " ", s, perl = TRUE, useBytes = TRUE)
  paste0('"', s, '"')
}

# build the body that goes between the FILE_TITLES markers
build_ft_body <- function(title_map) {
  keys <- sort(names(title_map), method = "radix")
  if (!length(keys)) return("\n      ")
  lines <- vapply(keys, function(k) paste0("      ", js_str(k), ": ", js_title(title_map[[k]])), character(1))
  paste0("\n", paste(lines, collapse = ",\n"), "\n      ")
}

ft_marker_pos <- function(text) {
  i <- regexpr(FT_START, text, fixed = TRUE)[1]
  j <- regexpr(FT_END,   text, fixed = TRUE)[1]
  if (i < 0 || j < 0) stop("Could not find the FILE_TITLES markers in tools.html.", call. = FALSE)
  list(after_start = i + nchar(FT_START), before_end = j)
}
current_ft_body <- function(text) {
  p <- tryCatch(ft_marker_pos(text), error = function(e) return(NULL))
  if (is.null(p)) return(NULL)
  substr(text, p$after_start, p$before_end - 1L)
}
splice_ft <- function(text, body) {
  p <- ft_marker_pos(text)
  paste0(substr(text, 1, p$after_start - 1L), body, substr(text, p$before_end, nchar(text)))
}


# ---------- 1. scan the folder ----------
new_tools <- list()
for (cat in order_by(sub_dirs(TOOLS_DIR), CAT_ORDER)) {
  cdir <- file.path(TOOLS_DIR, cat)
  obj <- list(name = deslug(cat))
  items <- html_files(cdir)
  if (length(items)) obj$items <- items
  subs <- order_by(sub_dirs(cdir), SUB_ORDER)
  if (length(subs)) {
    obj$subs <- lapply(subs, function(s) list(name = deslug(s), items = html_files(file.path(cdir, s))))
  }
  new_tools[[length(new_tools) + 1L]] <- obj
}
new_paths <- paths_of(new_tools)

# ---------- 2. read what the page currently lists ----------
src <- readChar(PAGE_PATH, file.info(PAGE_PATH)$size, useBytes = TRUE)
old_paths <- tryCatch(paths_of(parse_existing(src)), error = function(e) {
  cat("(Could not read the current catalog for comparison -- will rebuild it from scratch.)\n")
  character(0)
})

# ---------- 2b. read each tool's own title ----------
title_map <- list()
for (p in new_paths) {
  t <- tryCatch(extract_title(file.path(REPO, p)), error = function(e) NULL)
  if (!is.null(t) && nzchar(t)) title_map[[basename(p)]] <- t   # the file name is the title key
}
new_ft_body <- build_ft_body(title_map)
old_ft_body <- current_ft_body(src)
titles_changed <- is.null(old_ft_body) || !identical(trimws(new_ft_body), trimws(old_ft_body))

# ---------- 3. report what changed ----------
added   <- sort(setdiff(new_paths, old_paths))
removed <- sort(setdiff(old_paths, new_paths))
changed <- length(added) > 0 || length(removed) > 0 || titles_changed

cat(sprintf("\nWebsite folder: %s\n", REPO))
cat(sprintf("Found %d tool%s in %s/.\n\n", length(new_paths), if (length(new_paths) == 1) "" else "s", ROOT))
cat(sprintf("New tools (%d):\n", length(added)))
for (p in added) cat("   + ", p, "\n", sep = "")
cat(sprintf("Removed tools (%d):\n", length(removed)))
for (p in removed) cat("   - ", p, "\n", sep = "")
cat(sprintf("Titles read from tool pages: %d\n", length(title_map)))
if (titles_changed && length(added) == 0 && length(removed) == 0)
  cat("   (one or more tool titles changed since the page was last built)\n")

# ---------- 4. apply, or just preview ----------
if (!changed) {
  cat("\nNothing changed -- the page already lists every tool found in the folder.\n")
  cat("If you just added a tool but don't see it in the list above, double-check that the file is:\n")
  cat("   * inside interactive_tools/, in one of the category subfolders\n")
  cat("     (for example, interactive_tools/linear-algebra/),\n")
  cat("   * an .html file (other file types are ignored), and\n")
  cat("   * not named like '... (1).html' (those duplicate copies are skipped on purpose).\n")
} else if (!UPDATE_FILE) {
  cat("\nPREVIEW ONLY -- tools.html was NOT changed.\n")
  cat("To apply these changes: set  UPDATE_FILE <- TRUE  near the top of this script, then click Source again.\n")
} else {
  b <- find_bounds(src)
  updated <- paste0(substr(src, 1, b$start - 1L), serialize(new_tools), substr(src, b$end + 1L, nchar(src)))
  updated <- splice_ft(updated, new_ft_body)
  writeLines(src,     paste0(PAGE_PATH, ".bak"), sep = "", useBytes = TRUE)
  writeLines(updated, PAGE_PATH,                 sep = "", useBytes = TRUE)
  cat("\nDone -- tools.html updated.\n")
  cat("A backup of the old version was saved to tools.html.bak.\n")
  cat("Commit and push, and your live site will show the new list.\n")
}
