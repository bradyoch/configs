require("vis")

vis.events.subscribe(vis.events.INIT, function()
  vis:command("set tabwidth 2")
  vis:command("set expandtab on")
  vis:command("set autoindent on")

  local fzf_action = vis:action_register("FZF Open", function()
    local file_cmd = io.popen("fd -tf | fzf")
    local output = file_cmd:read()
    local _, _, status = file_cmd:close()

    if status == 0 then
      vis:command(string.format("e %s", output))
    end

    vis:feedkeys("<vis-redraw>")
    return true
  end, "Find file in directory with FZF")
  vis:map(vis.modes.NORMAL, "<M-p>f", fzf_action)

  --Additional file type declarations
  table.insert(vis.ftdetect.filetypes.javascript.ext, "%.ts$")

  -- Commands
  vis:command_register("rg", function(argv, _, _, _, _)
    local file_cmd = io.popen(string.format("rg --smart-case --no-heading -- %s | fzf", argv[1]))
    local output = file_cmd:read()
    local _, _, status = file_cmd:close()

    if status == 0 then
      vis:command(string.format("e %s", string.gsub(output, ":.*$", "")))
    end
    vis:feedkeys("<vis-redraw>")
    return true
  end, "Search for a phrase with rg")

  -- Format buffer command
  vis:command_register("format", function(argv, _, win, _, range)
    local status, out, err
    local file = win.file
    if win.syntax == "javascript" then
      status, out, err = vis:pipe(file, range, "prettier --parser typescript --single-quote")
    elseif win.syntax == "python" then
      status, out, err = vis:pipe(file, range, "black -")
    else
      vis:info(string.format("No registered formatter for %s", win.syntax))
      return
    end

    if not status then
      vis:info(err)
    else
      file:delete(range)
      file:insert(range.start, out)
    end
  end)
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
  vis:command("set number")
  vis:command("set cursorline")

  if win.syntax=="python" then
    vis:command("set tabwidth 4")
  end
end)

-- Trim trailing whitespace
vis.events.subscribe(vis.events.FILE_SAVE_PRE, function(file, _)
  local lines = file.lines
  for i=1, #lines do
    local trimmed = lines[i]:match('^(.-)%s+$')
    if trimmed then lines[i] = trimmed end
  end

  local empty_lines = 0
  for i=#lines, 0, -1 do
    if lines[i] ~= "" then
      break
    end
    empty_lines = empty_lines + 1
  end
  if empty_lines > 0 then
    file:delete(file.size - empty_lines, empty_lines)
  end

  return true
end)
