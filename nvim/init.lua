-- copy from https://github.com/craftzdog/dotfiles-public/commit/c0048102745c55069b096a30042554132b877c89
-- customized
require('craftzdog.base')
require('craftzdog.highlights')
require('craftzdog.maps')
require('craftzdog.plugins')

local has = vim.fn.has
local is_mac = has "macunix"
local is_win = has "win32"

if is_mac then
  require('craftzdog.macos')
end
if is_win then
  require('craftzdog.windows')
end
