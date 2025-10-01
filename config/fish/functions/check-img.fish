function check-img
    set num $argv[1]
    if test -f ~/Pictures/iCloud/IMG_$num.HEIC
        echo "✓ IMG_$num.HEIC exists"
        echo "Use '$num' in Emacs capture"
    else
        echo "❌ IMG_$num.HEIC not found"
        find ~/Pictures -name "*$num*.HEIC" 2>/dev/null
    end
end

# 사용: check-img 1234
