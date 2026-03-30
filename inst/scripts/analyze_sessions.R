#!/usr/bin/env r
# Analyze Claude Code session history

library(jsonlite)

claude_dir <- "~/.claude"
projects_dir <- file.path(claude_dir, "projects")

# --- 1. Parse history.jsonl (user prompts with project info) ---
cat("=== Parsing history.jsonl ===\n")
history_lines <- readLines(file.path(claude_dir, "history.jsonl"))
cat("Total history entries:", length(history_lines), "\n")

history <- lapply(history_lines, function(l) {
  tryCatch(fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
})
history <- Filter(Negate(is.null), history)

# Extract project, display text, timestamp
hist_df <- data.frame(
  project = vapply(history, function(h) basename(h$project %||% "unknown"), ""),
  display = vapply(history, function(h) h$display %||% "", ""),
  timestamp = vapply(history, function(h) h$timestamp %||% 0, 0.0),
  session = vapply(history, function(h) h$sessionId %||% "", ""),
  stringsAsFactors = FALSE
)
hist_df$date <- as.Date(as.POSIXct(hist_df$timestamp / 1000, origin = "1970-01-01"))

# --- 2. Project frequency ---
cat("\n=== PROJECT FREQUENCY (top 20) ===\n")
proj_counts <- sort(table(hist_df$project), decreasing = TRUE)
print(head(proj_counts, 20))

# --- 3. Sessions per project ---
cat("\n=== SESSIONS PER PROJECT ===\n")
session_files <- list.files(projects_dir, pattern = "\\.jsonl$", recursive = TRUE, full.names = TRUE)
proj_sessions <- table(dirname(session_files))
names(proj_sessions) <- basename(names(proj_sessions))
print(sort(proj_sessions, decreasing = TRUE))

# --- 4. Parse session JSONL files for user messages ---
cat("\n=== Parsing session files (", length(session_files), " files) ===\n")

extract_user_messages <- function(filepath) {
  lines <- tryCatch(readLines(filepath, warn = FALSE), error = function(e) character(0))
  if (length(lines) == 0) return(data.frame(project = character(0), text = character(0), stringsAsFactors = FALSE))

  proj <- basename(dirname(filepath))
  msgs <- character(0)

  for (l in lines) {
    j <- tryCatch(fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(j) || !identical(j$type, "user")) next

    content <- j$message$content
    if (is.null(content)) next

    for (c in content) {
      txt <- NULL
      if (is.list(c) && identical(c$type, "text")) txt <- c$text
      else if (is.character(c)) txt <- c
      if (!is.null(txt) && nchar(txt) > 0 && nchar(txt) < 5000) {
        msgs <- c(msgs, txt)
      }
    }
  }

  if (length(msgs) == 0) return(data.frame(project = character(0), text = character(0), stringsAsFactors = FALSE))
  data.frame(project = proj, text = msgs, stringsAsFactors = FALSE)
}

all_msgs <- do.call(rbind, lapply(session_files, extract_user_messages))
cat("Total user messages extracted:", nrow(all_msgs), "\n")

# --- 5. Slash command usage ---
cat("\n=== SLASH COMMAND USAGE ===\n")
slash_msgs <- all_msgs$text[grepl("^/", all_msgs$text)]
slash_cmds <- sub("^(/\\S+).*", "\\1", slash_msgs)
print(sort(table(slash_cmds), decreasing = TRUE))

# --- 6. Common action patterns ---
cat("\n=== COMMON ACTION PATTERNS ===\n")

# Classify messages by intent
patterns <- list(
  "bug_fix" = "\\b(fix|bug|broken|error|fail|crash|wrong|issue)\\b",
  "feature_add" = "\\b(add|create|implement|build|new|feature|make)\\b",
  "refactor" = "\\b(refactor|clean|simplify|reorganize|rename|move|split)\\b",
  "test" = "\\b(test|tinytest|expect_|check)\\b",
  "docs" = "\\b(doc|readme|claude\\.md|comment|describe|explain)\\b",
  "git_ops" = "\\b(commit|push|pr|pull request|merge|branch|squash)\\b",
  "package_dev" = "\\b(install|build|namespace|export|cran|description|roxygen|tinyrox)\\b",
  "explore" = "\\b(look|find|search|show|list|what|where|how|why)\\b",
  "config" = "\\b(config|setting|setup|env|docker|container|api)\\b",
  "ci_cd" = "\\b(ci|github action|workflow|deploy|r-ci)\\b",
  "version_bump" = "\\b(bump|version)\\b",
  "torch_ml" = "\\b(torch|tensor|model|train|gpu|cuda|nn|neural)\\b",
  "audio_tts" = "\\b(audio|tts|speech|voice|chatterbox|qwen|whisper|stt)\\b",
  "skills_plugins" = "\\b(skill|plugin|hook|mcp)\\b",
  "memory" = "\\b(remember|memory|forget|always|never)\\b"
)

classify <- function(texts) {
  counts <- vapply(patterns, function(pat) {
    sum(grepl(pat, texts, ignore.case = TRUE))
  }, 0L)
  sort(counts, decreasing = TRUE)
}

cat("\nAll projects:\n")
print(classify(all_msgs$text))

# --- 7. Per-project breakdown ---
cat("\n=== TOP PROJECTS DETAIL ===\n")
top_projects <- names(head(sort(table(all_msgs$project), decreasing = TRUE), 10))
for (p in top_projects) {
  cat("\n---", p, "---\n")
  pmsg <- all_msgs$text[all_msgs$project == p]
  cat("Messages:", length(pmsg), "\n")
  print(head(classify(pmsg), 8))
}

# --- 8. Activity over time ---
cat("\n=== ACTIVITY OVER TIME ===\n")
# Filter /init and /login noise from display
real_prompts <- hist_df[!grepl("^/(init|login|compact|clear)", hist_df$display), ]
cat("Real prompts (excluding init/login/compact):", nrow(real_prompts), "\n")
if (nrow(real_prompts) > 0) {
  daily <- table(real_prompts$date)
  cat("\nDaily activity (last 14 days):\n")
  recent <- daily[as.Date(names(daily)) >= Sys.Date() - 14]
  if (length(recent) > 0) print(recent)
}

# --- 9. Long/complex prompts (likely workflows) ---
cat("\n=== LONG PROMPTS (potential workflow candidates) ===\n")
long_msgs <- all_msgs[nchar(all_msgs$text) > 200 & !grepl("^\\[Request interrupted", all_msgs$text), ]
long_msgs <- long_msgs[order(-nchar(long_msgs$text)), ]
cat("Prompts over 200 chars:", nrow(long_msgs), "\n")
for (i in seq_len(min(30, nrow(long_msgs)))) {
  cat("\n[", long_msgs$project[i], "] (", nchar(long_msgs$text[i]), " chars):\n")
  cat(substr(long_msgs$text[i], 1, 300), "\n")
}

# --- 10. Repeated exact/near-exact prompts ---
cat("\n=== REPEATED PROMPTS ===\n")
# Normalize: lowercase, trim whitespace
normalized <- trimws(tolower(all_msgs$text))
# Filter out very short and system messages
normalized <- normalized[nchar(normalized) > 10 & !grepl("^\\[request", normalized)]
repeated <- sort(table(normalized), decreasing = TRUE)
repeated <- repeated[repeated >= 3]
cat("Prompts repeated 3+ times:\n")
if (length(repeated) > 0) {
  for (i in seq_len(min(20, length(repeated)))) {
    cat(sprintf("  [%dx] %s\n", repeated[i], substr(names(repeated)[i], 1, 120)))
  }
}

# --- 11. Unique session count and duration proxy ---
cat("\n=== SESSION STATS ===\n")
sessions_per_proj <- tapply(hist_df$session, hist_df$project, function(x) length(unique(x)))
cat("Total unique sessions:", sum(sessions_per_proj), "\n")
cat("Projects worked on:", length(sessions_per_proj), "\n")
cat("\nSessions per project:\n")
print(sort(sessions_per_proj, decreasing = TRUE))

cat("\n=== DONE ===\n")
