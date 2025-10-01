function upower_battery --description "Check battery status"
    # Get battery information
    set battery_info (upower -i /org/freedesktop/UPower/devices/battery_BAT0 2>/dev/null)

    if test -z "$battery_info"
        echo "Battery information not found."
        return 1
    end

    # Extract key information line by line
    for line in $battery_info
        if string match -q "*state:*" $line; or string match -q "*percentage:*" $line; or string match -q "*time to*" $line
            echo (string trim $line)
        end
    end
end
