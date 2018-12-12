textadept.editing.strip_trailing_spaces = true;

buffer:set_theme('dark', { font = 'Fira Code', fontsize = 8 })

events.connect(events.VIEW_NEW, function()
  buffer.h_scroll_bar = false;
  buffer.v_scroll_bar = false;
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
