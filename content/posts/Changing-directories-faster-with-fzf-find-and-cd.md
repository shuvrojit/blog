+++
title = "Changing directories faster with fzf, find and cd"
author = ["Shuvrojit Biswas"]
date = 2024-06-01
lastmod = 2024-06-01T04:31:57+06:00
tags = ["linux", "productivity", "directory-navigation", "command-line"]
categories = ["productivity"]
draft = false
weight = 1001
+++

Have you ever had this problem when you had to change directories but didn't want go there by using `cd <relative-path>`. We can somehow use pushd and popd. But it's not going to solve the problem. You still need to push the directory. What if you just write the directory name and press enter and voila you're there. No matter how many levels deep your directory is, it's still going to find it.

When working in the terminal, we often find ourselves needing to navigate to specific directories quickly. However, traditional methods like manually typing out directory paths or using the cd command with tab completion can be time-consuming and error-prone, especially in complex directory structures. Additionally, tools like find lack interactivity, making it difficult to search and select directories efficiently.

We can use fd and fzf to streamline directory navigation. fd is a blazing-fast alternative to find that efficiently searches directories, while fzf provides an interactive fuzzy search interface. By combining these two tools, we can quickly locate and navigate to directories with ease.

The key to our solution lies in creating a custom function that integrates fd and fzf for directory navigation.

**Steps to finding directory and changing there:**

**Search with fd:** We can use fd to search for directories based on the provided search term. fd is much faster than find and can efficiently traverse directory structures.

**Interactive Selection with fzf:** The search results from fd are piped into fzf, providing an interactive selection interface. We can quickly filter and select directories using fuzzy search.
**Change Directory with cd:** Once a directory is selected using fzf, the custom function changes the current directory to the selected directory.

To implement this solution, we can define a custom function in our shell configuration file (~/.bashrc, ~/.zshrc). The function utilizes fd and fzf to search for and select directories interactively, enhancing the efficiency of directory navigation in the terminal.

```sh
cd_to_dir() {
    local selected_dir
    selected_dir=$(fd -t d . "$1" | fzf +m --height 50% --preview 'tree -C {}')

    if [[ -n "$selected_dir" ]]; then
        # Change to the selected directory
        cd "$selected_dir" || return 1
    fi
}

alias cdd='cd_to_dir ~/Desktop/'
alias cds='cd_to_dir'
```

`cdd` command will only look for directories inside `~/Desktop`. `cds` will take a directory path to search within.

Efficient directory navigation is essential for productivity in the terminal environment. By leveraging tools like fd and fzf, we can significantly improve our workflow, save time and reduce the cognitive load associated with navigating complex directory structures. With a custom function that integrates these tools, we can enjoy quick and interactive directory navigation.

I hope this will turn your terminal experience into something better. Maybe you can extend this function to search and open files using different applications.
