--
-- statusd module for the %disp meter: current autorandr display state.
--

local defaults = { update_interval = 10*1000 }
local settings = table.join(statusd.get_config("disp"), defaults)

local disp_timer

local function update_disp()
    statusd.popen_bgread(os.getenv("HOME") .. "/.tmux/disp.sh", function(str)
        statusd.inform("disp", (string.gsub(str, "%s+$", "")))
        disp_timer:set(settings.update_interval, update_disp)
    end)
end

disp_timer = statusd.create_timer()
update_disp()
