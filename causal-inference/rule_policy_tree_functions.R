policy_tree_stats <- function(tree, list_length) {
    # initalise the new tree
    new_tree <- list()
    for (index in 1:list_length) {
        print("---")
        print(index)
        new_tree[[index]] <- list()
        # new_tree[[index]][['parent']] <- list()
    }
    new_tree[[1]][["parent"]] <- list()

    for (index in 1:list_length) {
        node <- tree$nodes[[index]]
        if (!node$is_leaf) {
            if (!is.null(node$left_child)) {
                child_id <- node$left_child
                parent_split <- node$split_variable
                parent_name <- tree[["columns"]][[parent_split]]
                print(parent_name)
                new_rule <- paste0("(X$", parent_name, "<=", node$split_value, ")")
                new_tree[[child_id]][["parent"]] <- c(new_tree[[index]][["parent"]], new_rule)
            }

            if (!is.null(node$right_child)) {
                child_id <- node$right_child
                parent_split <- node$split_variable
                parent_name <- tree[["columns"]][[parent_split]]
                print(parent_name)
                new_rule <- paste0("(X$", parent_name, ">", node$split_value, ")")
                new_tree[[child_id]][["parent"]] <- c(new_tree[[index]][["parent"]], new_rule)
            }
        }
    }

    for (index in 1:list_length) {
        joined_string <- paste(new_tree[[index]][["parent"]], collapse = "&")
        print(joined_string)
        new_tree[[index]][["joined"]] <- joined_string
    }



    for (index in 1:list_length) {
        if (new_tree[[index]][["joined"]] == "") {
            ATE <- average_treatment_effect_w_ss(c.forest, target.sample = "overlap")
            new_tree[[index]][["ate"]] <- cal.pvalue(ATE)
        } else {
            ATE <- average_treatment_effect_w_ss(c.forest,
                target.sample = "overlap",
                subset = eval(parse(text = new_tree[[index]][["joined"]]))
            )
            new_tree[[index]][["ate"]] <- cal.pvalue(ATE)
        }
    }
    return(new_tree)
}

############

###########
CATE_TABLE <- function(new_tree, list_length) {
    # saves output as a table.
    flat_df <- c()
    for (index in 1:list_length) {
        flat_df[[index]] <- as.data.frame(
            as.list(unlist(new_tree[[index]], recursive = FALSE, use.names = T))
        )
    }
    library(dplyr)
    result_df <- do.call(bind_rows, flat_df)

    result_df <- tibble::rownames_to_column(result_df, var = "node")


    return(result_df)
}


##############

update_tree <- function(new_tree, tree, list_length) {
    # write inside original tree nested list the relevant statistics that are used to generate the trees.
    for (index in 1:list_length) {
        estimate <- new_tree[[index]][["ate"]][["estimate"]]
        p <- new_tree[[index]][["ate"]][["p_val.estimate"]]
        sample_p <- new_tree[[index]][["ate"]][["sample_size"]] / new_tree[[1]][["ate"]][["sample_size"]]
        tree[["nodes"]][[index]][["stats"]] <- c(estimate = estimate, p = p, sample_p = sample_p)
    }

    return(tree)
}


##############

# Function to save data frame as CSV and HTML
save_as_csv_and_html <- function(tree, path, outcome, depth) {
    csv_file_path <- paste0(path, outcome, "_policy_d", depth, ".csv")
    html_file_path <- paste0(path, outcome, "_policy_d", depth, ".html")

    list_length <- length(tree[["nodes"]])

    tree_stats <- policy_tree_stats(tree, list_length)
    table_tree_stats <- CATE_TABLE(tree_stats, list_length)

    write.csv(table_tree_stats, file = csv_file_path, row.names = FALSE)

    tree <- update_tree(tree_stats, tree, list_length)
    save_html(plot_policy_tree(tree, leaf.labels = c("do not innovate", "innovate")), file = html_file_path)
    cat("CSV and HTML files have been saved.\n")
}
