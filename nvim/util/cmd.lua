local function random_buffer_jump()
  local buffers = vim.api.nvim_list_bufs()
  local random_buf = buffers[math.random(#buffers)]
  vim.api.nvim_set_current_buf(random_buf)
  local line_count = vim.api.nvim_buf_line_count(random_buf)
  local random_line = math.random(line_count)
  vim.api.nvim_win_set_cursor(0, { random_line, 0 })
end

local function start_random_buffer_jump()
  if _G.random_buffer_jump_timer == nil then
    _G.random_buffer_jump_timer = vim.loop.new_timer()
    _G.random_buffer_jump_timer:start(0, 5000, vim.schedule_wrap(function()
      random_buffer_jump()
    end))
    print("Random buffer jump started.")
  else
    print("Random buffer jump is already running.")
  end
end

local function stop_random_buffer_jump()
  if _G.random_buffer_jump_timer ~= nil then
    _G.random_buffer_jump_timer:stop()
    _G.random_buffer_jump_timer:close()
    _G.random_buffer_jump_timer = nil
    print("Random buffer jump stopped.")
  else
    print("Random buffer jump is not running.")
  end
end

-- コマンドを登録
vim.api.nvim_create_user_command('StartRandomBufferJump', function()
  start_random_buffer_jump()
end, {})

vim.api.nvim_create_user_command('StopRandomBufferJump', function()
  stop_random_buffer_jump()
end, {})

-- モジュールとしてエクスポート
return {
  random_buffer_jump = random_buffer_jump,
  start_random_buffer_jump = start_random_buffer_jump,
  stop_random_buffer_jump = stop_random_buffer_jump,
}

