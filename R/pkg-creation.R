#' quick git stage -> commit -> push
#' @param commit_msg Git Commit Message
#' @export
quick_git <- function(commit_msg = NULL){
  changes <- gert::git_status()
  n <- nrow(changes) # if(n == 0) stop("No")
  usethis::ui_info("{n} unstaged files")
  if(n > 0){
    # files <- paste(changes$file, collapse = "', '")
    usethis::ui_info("{usethis::ui_path(changes$file)}")
    # git_add()/git_rm() #e.g., git_add(c("dev/", "man/"))
    gert::git_add(".")
    usethis::ui_done("Files staged")
  }

  if(is.null(commit_msg))
    stop("You need to include a commit message with your commit!")
  usethis::ui_info("Commit Message: {usethis::ui_field(commit_msg)}")
  gert::git_commit(commit_msg) #now commit
  usethis::ui_done("Git commit")

  gert::git_push() #and push!
  usethis::ui_done("Changes pushed!")

  usethis::ui_info("Here's a log of your most recent commits")
  return(gert::git_log())
}
