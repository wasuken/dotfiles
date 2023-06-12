local status, null_ls = pcall(require, "neo-tree")
if (not status) then return end

vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])
