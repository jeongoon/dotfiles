function upower_percent --description "Show battery percentage only"
    set battery_info (upower -i /org/freedesktop/UPower/devices/battery_BAT0 2>/dev/null)
    set percentage ""

    for line in $battery_info
        if string match -q "*percentage:*" $line
            set percentage (string match -r "[0-9]+" $line)
            break
        end
    end

    if test -n "$percentage"
        # Add color coding
        if test $percentage -gt 60
            set_color green
        else if test $percentage -gt 20
            set_color yellow
        else
            set_color red
        end

        echo "$percentage%"
        set_color normal
    else
        echo "N/A"
    end
end
