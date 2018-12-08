textadept.editing.strip_trailing_spaces = true;

buffer:set_theme('dark', { font = 'Fira Code', fontsize = 9 })

events.connect(events.VIEW_NEW, function()
  buffer.h_scroll_bar = false;
  buffer.v_scroll_bar = false;
end)

local caps_mode_binding = {
  [72] = buffer.char_left,
  [74] = buffer.line_down,
  [75] = buffer.line_up,
  [76] = buffer.char_right,
  [79] = buffer.new_line
}

events.connect(events.KEYPRESS, function(code, sh, ct, al, me, caps)
  if caps and keys.MODE == nil then
    local f = caps_mode_binding[code]
    if f then
      f(_G.buffer)
    end
    return true
  end
end)

keys['cx'] = function()
  if buffer:get_sel_text() == '' then
    buffer:line_cut()
  else
    buffer:cut()
  end
end
keys['cc'] = function()
  if buffer:get_sel_text() == '' then
    buffer:line_copy()
  else
    buffer:copy()
  end
end

local function quick_open(file_name)
  file_name = file_name:gsub("~", os.getenv("HOME"))

  if (file_name:sub(1, 1) ~= "/") then
    file_name = buffer.filename:match('^.+/')..file_name
  end

  io.open_file(file_name)
end

keys['cu'] = function()
  ui.command_entry.enter_mode('quick_open')
end
keys.quick_open ={
  ['\n'] = function() return ui.command_entry.finish_mode(quick_open) end
}
