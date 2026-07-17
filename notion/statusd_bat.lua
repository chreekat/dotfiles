--
-- statusd module for the %bat meter: battery charge, ETA, and charge state.
--

local defaults = { update_interval = 10*1000 }
local settings = table.join(statusd.get_config("bat"), defaults)

local bat_timer

local function update_bat()
    statusd.popen_bgread(os.getenv("HOME") .. "/.tmux/bat.sh", function(str)
        statusd.inform("bat", (string.gsub(str, "%s+$", "")))
        bat_timer:set(settings.update_interval, update_bat)
    end)
end

bat_timer = statusd.create_timer()
update_bat()
