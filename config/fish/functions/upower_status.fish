function upower_status --description "Check full power status"
    echo "=== Power Status ==="
    set display_info (upower -i /org/freedesktop/UPower/devices/DisplayDevice 2>/dev/null)

    for line in $display_info
        if string match -q "*state:*" $line; or string match -q "*percentage:*" $line; or string match -q "*time to*" $line; or string match -q "*warning-level:*" $line
            echo (string trim $line)
        end
    end

    echo -e "\n=== AC Adapter ==="
    set ac_info (upower -i /org/freedesktop/UPower/devices/line_power_AC0 2>/dev/null)
    set ac_status "no"

    for line in $ac_info
        if string match -q "*online:*" $line
            set ac_status (string trim $line | string split ":" | string trim)[2]
        end
    end

    if test "$ac_status" = "yes"
        echo "AC adapter connected 🔌"
    else
        echo "Running on battery 🔋"
    end
end
