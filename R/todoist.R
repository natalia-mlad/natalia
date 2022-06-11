#' get_todoist_tasks
#' @return tasks in a tibble with a bunch of info
#' @export
get_todoist_tasks <- function() {
  objects <- rtodoist::get_all()
  tasks <- rtodoist::get_tasks()$items %>%
    tibble(items = .) %>%
    unnest_wider(items) %>%
    unnest_wider(due, names_sep = "_") %>%
    tidy_my_data(na.rm = F, quiet = T)
  projects <- rtodoist::get_projects()$projects %>%
    tibble(projects = .) %>%
    unnest_wider(projects) %>%
    tidy_my_data(na.rm = F, quiet = T)
  sections <- objects$sections %>%
    tibble(x = .) %>%
    unnest_wider(x) %>%
    tidy_my_data(na.rm = F, quiet = T)
  tasks <- sections %>%
    select(id, name) %>%
    rename(section_id = id, section_name = name) %>%
    full_join(tasks)
  tasks <- projects %>%
    select(id, name, parent_id) %>%
    rename(
      project_id = id,
      project_name = name,
      project_parent_id = parent_id
    ) %>%
    full_join(tasks) %>%
    filter(project_parent_id != filter(projects, name == "Examples")$id)

  return(tasks)
}
