--
-- statusd module for the %disp meter: current autorandr display state.
--

local defaults = { update_interval = 10*1000 }
local settings = table.join(statusd.get_config("disp"), defaults)

local disp_timer

local function update_disp()
    -- Fed one chunk per resume, then nil at end of output.
    statusd.popen_bgread(os.getenv("HOME") .. "/.tmux/disp.sh",
                         coroutine.wrap(function(chunk)
        local out = ""
        while chunk do
            out = out .. chunk
            chunk = coroutine.yield()
        end
        statusd.inform("disp", (string.gsub(out, "%s+$", "")))
        disp_timer:set(settings.update_interval, update_disp)
    end))
end

disp_timer = statusd.create_timer()
update_disp()
