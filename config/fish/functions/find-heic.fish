# ~/.config/fish/functions/find-heic.fish
function find-heic --description "Find HEIC files in iCloud Photos"
    set -l input $argv[1]
    set -l search_dir (test -n "$argv[2]" && echo $argv[2] || echo ~/Pictures/iCloud)
    
    # 입력값 체크
    if test -z "$input"
        echo "Usage: find-heic <number or filename> [directory]"
        echo "Example: find-heic 1234"
        echo "         find-heic IMG_1234"
        return 1
    end
    
    # 숫자만 있으면 IMG_ 추가
    if string match -qr '^[0-9]+$' $input
        set input "IMG_$input"
    end
    
    # .HEIC 확장자 추가
    if not string match -qi "*.HEIC" $input
        set input "$input.HEIC"
    end
    
    # 검색
    set -l files (find $search_dir -type f -iname "$input" 2>/dev/null)
    
    if test (count $files) -eq 0
        echo "❌ Not found: $input in $search_dir"
        return 1
    else if test (count $files) -eq 1
        echo "✅ Found: $files[1]"
        # 클립보드에 복사 (Mac)
        if type -q pbcopy
            echo -n $files[1] | pbcopy
            echo "📋 Copied to clipboard"
        end
        echo $files[1]
    else
        echo "🔍 Multiple files found:"
        for i in (seq (count $files))
            echo "  [$i] $files[$i]"
        end
        read -P "Select number: " num
        if test -n "$num" -a "$num" -ge 1 -a "$num" -le (count $files)
            echo $files[$num]
            if type -q pbcopy
                echo -n $files[$num] | pbcopy
                echo "📋 Copied to clipboard"
            end
        end
    end
end
