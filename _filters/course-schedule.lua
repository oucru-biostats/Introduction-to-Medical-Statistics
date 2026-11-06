--[[
course-schedule.lua
-------------------
Renders the day-by-day "Course Topics" section of the home page from
_data/schedule.yml (loaded into the page metadata via `metadata-files`).

Usage in a .qmd file:

    ---
    metadata-files: [_data/schedule.yml]
    filters: [_filters/course-schedule.lua]
    ---

    ::: {#course-schedule}
    :::

The empty div is replaced by one section per day: a collapsible <details>
block containing a tabset with one tab per session. The unlock buttons are
handled by unlock.html.

Plain Pandoc Lua (no Quarto-only API is required), so it can be tested with:

    pandoc index.qmd --metadata-file _data/schedule.yml \
      --lua-filter _filters/course-schedule.lua -t html
]]

local stringify = pandoc.utils.stringify

local TIME_LABELS = {
  morning   = "☀️ Morning",
  afternoon = "🌤️ Afternoon",
}

local BUTTON_CLASSES = { "btn", "btn-outline-primary", "btn-sm" }

-- Helpers --------------------------------------------------------------------

local function warn(msg)
  msg = "[course-schedule] " .. msg
  if quarto and quarto.log then
    quarto.log.warning(msg)
  else
    io.stderr:write("WARNING: " .. msg .. "\n")
  end
end

--- Metadata value -> trimmed string, or nil when absent/empty.
local function str(value)
  if value == nil then return nil end
  local s = stringify(value):match("^%s*(.-)%s*$")
  if s == "" then return nil end
  return s
end

--- Metadata value -> Inlines (keeps any markdown formatting in the YAML).
local function inlines(value)
  if pandoc.utils.type(value) == "Inlines" then return value end
  return pandoc.Inlines(pandoc.Str(stringify(value)))
end

local function text(s)
  return pandoc.read(s, "markdown-smart").blocks[1].content
end

local function slug(s)
  return (s:lower():gsub("[^%w]+", "-"):gsub("^%-+", ""):gsub("%-+$", ""))
end

local function html_escape(s)
  return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;")
           :gsub('"', "&quot;"):gsub("'", "&#39;"))
end

local project_dir = (quarto and quarto.project and quarto.project.directory) or "."

local function check_file(path)
  local f = io.open(project_dir .. "/" .. path, "r")
  if f then
    f:close()
  else
    warn("file not found: " .. path)
  end
end

--- An outline button linking to a course file.
local function button(label, path)
  check_file(path)
  return pandoc.Link(text(label), path, "",
    pandoc.Attr("", BUTTON_CLASSES, { target = "_blank" }))
end

--- Raw-HTML "unlock" widget; the links stay in a <template> until the code
--- is entered (see unlock.html).
local function unlock_widget(code, button_label, links)
  local anchors = {}
  for _, link in ipairs(links) do
    check_file(link.path)
    anchors[#anchors + 1] = string.format(
      '<a href="%s" class="btn btn-success btn-sm" target="_blank">%s</a>',
      html_escape(link.path), html_escape(link.label))
  end
  return pandoc.RawBlock("html", string.format(
    '<div class="unlock-session" data-code="%s">\n' ..
    '  <button type="button" class="btn btn-primary">%s</button>\n' ..
    '  <template>%s</template>\n' ..
    '</div>',
    html_escape(code), html_escape(button_label), table.concat(anchors, " ")))
end

local function labelled_item(label, rest)
  local first = pandoc.Plain({ pandoc.Strong(text(label)) })
  if rest and rest.t == "Plain" then
    first.content:insert(pandoc.Space())
    first.content:extend(rest.content)
    return { first }
  end
  return { first, rest }
end

-- Session --------------------------------------------------------------------

local function materials_list(s)
  local items = {}

  local slides = str(s.slides)
  if slides then
    items[#items + 1] = labelled_item("Lecture:",
      pandoc.Plain({ button("Slides", slides) }))
  end

  local ex = s.exercises
  if ex then
    local sub = {}
    if str(ex.webr) then
      sub[#sub + 1] = { pandoc.Plain({ button("Web-R version", str(ex.webr)) }) }
    end
    if str(ex.rstudio) then
      local line = pandoc.Plain({ button("RStudio version", str(ex.rstudio)) })
      if str(ex.script) then
        line.content:insert(pandoc.Space())
        line.content:insert(button("Script", str(ex.script)))
      end
      sub[#sub + 1] = { line }
    end
    if #sub > 0 then
      items[#items + 1] = labelled_item("Exercises:", pandoc.BulletList(sub))
    end
  end

  local ans = s.answers
  if ans then
    local code = str(s["answers-code"])
    if code then
      local links = {}
      if str(ans.webr) then links[#links + 1] = { label = "Web-R Answers", path = str(ans.webr) } end
      if str(ans.rstudio) then links[#links + 1] = { label = "RStudio Answers", path = str(ans.rstudio) } end
      items[#items + 1] = labelled_item("Answers:",
        unlock_widget(code, "Click to unlock answers", links))
    else
      local sub = {}
      if str(ans.webr) then
        sub[#sub + 1] = { pandoc.Plain({ button("Web-R version", str(ans.webr)) }) }
      end
      if str(ans.rstudio) then
        sub[#sub + 1] = { pandoc.Plain({ button("RStudio version", str(ans.rstudio)) }) }
      end
      if #sub > 0 then
        items[#items + 1] = labelled_item("Answers:", pandoc.BulletList(sub))
      end
    end
  end

  return pandoc.BulletList(items)
end

local function session_blocks(s, day_id, default_code)
  local time = str(s.time)
  local id = day_id .. "-" .. (time and slug(time) or slug(str(s.title) or "session"))

  local heading = pandoc.Inlines({})
  if time then
    heading:extend(text((TIME_LABELS[time:lower()] or time) .. ":"))
    heading:insert(pandoc.Space())
  end
  heading:extend(inlines(s.title))

  local columns = pandoc.Blocks({
    pandoc.Header(4, "Contents", pandoc.Attr(id .. "-contents")),
  })
  if s.contents then
    local topics = {}
    for _, topic in ipairs(s.contents) do
      topics[#topics + 1] = { pandoc.Plain(inlines(topic)) }
    end
    columns:insert(pandoc.OrderedList(topics))
  end
  if str(s.qr) then
    columns:insert(unlock_widget(default_code, "Click to unlock QR code",
      { { label = "QR code", path = str(s.qr) } }))
  end
  columns:insert(pandoc.Header(4, "Materials", pandoc.Attr(id .. "-materials")))
  columns:insert(materials_list(s))

  return {
    pandoc.Header(3, heading, pandoc.Attr(id)),
    pandoc.Div(columns, pandoc.Attr("", {}, { ["layout-ncol"] = "2" })),
  }
end

-- Day ------------------------------------------------------------------------

local function day_blocks(day, default_code)
  local n = str(day.day) or "?"
  local id = "day-" .. slug(n)

  local title = text("Day " .. n .. ":")
  title:insert(pandoc.Space())
  title:extend(inlines(day.title))

  local tabs = pandoc.Blocks({})
  for _, s in ipairs(day.sessions or {}) do
    tabs:extend(session_blocks(s, id, default_code))
  end

  return {
    pandoc.Header(2, title, pandoc.Attr(id)),
    pandoc.RawBlock("html",
      '<details class="day-materials">\n<summary>Click to view the materials</summary>'),
    pandoc.Div(tabs, pandoc.Attr("", { "panel-tabset" })),
    pandoc.RawBlock("html", "</details>"),
  }
end

-- Entry point ----------------------------------------------------------------

function Pandoc(doc)
  local schedule = doc.meta.schedule
  local default_code = str(doc.meta["unlock-code"]) or ""

  return doc:walk({
    Div = function(div)
      if div.identifier ~= "course-schedule" then return nil end
      if not schedule then
        warn("no `schedule` metadata found; is _data/schedule.yml listed in metadata-files?")
        return pandoc.Div(pandoc.Para(text("Course schedule unavailable.")),
          pandoc.Attr("", { "callout-warning" }))
      end
      local blocks = pandoc.Blocks({})
      for _, day in ipairs(schedule) do
        blocks:extend(day_blocks(day, default_code))
      end
      return blocks
    end,
  })
end
