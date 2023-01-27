#' get_todoist_tasks
#' @return tasks in a tibble with a bunch of information
#' @export
get_todoist_tasks <- function() {
  objects <- rtodoist::get_all()
  tasks <- rtodoist::get_tasks()$items %>%
    dplyr::tibble(items = .) %>%
    tidyr::unnest_wider(items) %>%
    tidyr::unnest_wider(due, names_sep = "_") %>%
    tidy_my_data(na.rm = F, quiet = T)
  projects <- rtodoist::get_projects()$projects %>%
    dplyr::tibble(projects = .) %>%
    tidyr::unnest_wider(projects) %>%
    tidy_my_data(na.rm = F, quiet = T)
  sections <- objects$sections %>%
    dplyr::tibble(x = .) %>%
    tidyr::unnest_wider(x) %>%
    tidy_my_data(na.rm = F, quiet = T)
  tasks <- sections %>%
    dplyr::select(id, name) %>%
    dplyr::rename(section_id = id, section_name = name) %>%
    dplyr::full_join(tasks)
  tasks <- projects %>%
    dplyr::select(id, name, parent_id) %>%
    dplyr::rename(
      project_id = id,
      project_name = name,
      project_parent_id = parent_id
    ) %>%
    dplyr::full_join(tasks) %>%
    dplyr::filter(project_parent_id != dplyr::filter(projects, name == "Examples")$id)

  return(tasks)
}
