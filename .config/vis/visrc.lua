require('vis')

vis.events.subscribe(vis.events.INIT, function()
  vis:command('set tabwidth 2')
  vis:command('set expandtab on')
  vis:command('set autoindent on')

  local fzf_action = vis:action_register("fzf", function()
    local file_cmd = io.popen("fzf")
    local output = file_cmd:read()
    local _, _, status = file_cmd:close()

    if status == 0 then
      vis:command(string.format('e %s', output))
    end

    vis:feedkeys("<vis-redraw>")

    return true
  end, "FZF works")
  vis:map(vis.modes.NORMAL, "\\f", fzf_action)
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
  vis:command('set number')
  vis:command('set cursorline')
end)
