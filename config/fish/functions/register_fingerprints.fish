#!/usr/bin/env fish

# ============================================
# 지문 등록 도우미 스크립트 (완전판)
# 건조한 손을 위한 듀얼 컨디션 등록 시스템
# ============================================

# Color codes for better readability
set -g COLOR_GREEN '\033[0;32m'
set -g COLOR_YELLOW '\033[1;33m'
set -g COLOR_BLUE '\033[0;34m'
set -g COLOR_RED '\033[0;31m'
set -g COLOR_CYAN '\033[0;36m'
set -g COLOR_MAGENTA '\033[0;35m'
set -g COLOR_NC '\033[0m'  # No Color

# ===========================================
# 메인 등록 함수 - 왼손/오른손 트릭 사용
# ===========================================
function register_fingerprints --description "Register fingerprints for dry and moist conditions using different hands"
    
    # Check if fprintd-enroll is available
    if not command -v fprintd-enroll &> /dev/null
        echo -e "$COLOR_RED""Error: fprintd-enroll is not installed.$COLOR_NC"
        echo "Please install it with: sudo apt install fprintd libpam-fprintd"
        return 1
    end
    
    echo -e "$COLOR_BLUE""=== 듀얼 컨디션 지문 등록 도우미 ===$COLOR_NC"
    echo ""
    echo -e "$COLOR_CYAN""┌─────────────────────────────────────────────────┐"
    echo -e "│         건조한 손을 위한 특별 등록 전략         │"
    echo -e "├─────────────────────────────────────────────────┤"
    echo -e "│  💧 촉촉한 상태 → 오른손으로 등록              │"
    echo -e "│  🏜️  건조한 상태 → 왼손으로 등록               │"
    echo -e "└─────────────────────────────────────────────────┘$COLOR_NC"
    echo ""
    echo -e "$COLOR_YELLOW""실제로는 오른손만 사용하지만,"
    echo -e "시스템은 다른 상태를 다른 손으로 인식합니다!$COLOR_NC"
    echo ""
    
    # Define fingers for each condition
    set -l dry_fingers "left-index-finger" "left-middle-finger"
    set -l moist_fingers "right-index-finger" "right-middle-finger"
    set -l dry_actual "오른손 검지 (건조)" "오른손 중지 (건조)"
    set -l moist_actual "오른손 검지 (촉촉)" "오른손 중지 (촉촉)"
    
    echo "등록을 시작하시겠습니까? (y/n)"
    read -l start
    if test "$start" != "y"
        echo "취소되었습니다."
        return
    end
    
    # STEP 1: 건조한 상태 등록
    echo ""
    echo -e "$COLOR_GREEN""=== STEP 1: 건조한 상태 등록 (왼손으로 저장) ===$COLOR_NC"
    echo -e "$COLOR_YELLOW""실제로는 오른손을 사용하지만, 시스템에는 왼손으로 저장됩니다.$COLOR_NC"
    echo ""
    
    for i in (seq (count $dry_fingers))
        set -l finger $dry_fingers[$i]
        set -l actual_finger $dry_actual[$i]
        
        echo -e "$COLOR_BLUE""[$i/2] $actual_finger → $finger 로 저장$COLOR_NC"
        echo "----------------------------------------"
        echo -e "$COLOR_YELLOW""준비사항:"
        echo "  • 손을 깨끗이 닦고 완전히 말리세요"
        echo "  • 로션이나 크림을 바르지 마세요"
        echo "  • 건조한 상태를 유지하세요$COLOR_NC"
        echo ""
        echo "준비되면 Enter를 누르세요... (건너뛰려면 's' 입력)"
        read -l response
        
        if test "$response" = "s"
            echo -e "$COLOR_YELLOW""건너뛰었습니다.$COLOR_NC"
            continue
        end
        
        # Check if already enrolled
        set -l enrolled (fprintd-list $USER 2>/dev/null | grep -c $finger)
        if test $enrolled -gt 0
            echo -e "$COLOR_YELLOW""이미 등록되어 있습니다. 덮어쓰시겠습니까? (y/n)$COLOR_NC"
            read -l overwrite
            if test "$overwrite" != "y"
                continue
            end
        end
        
        echo -e "$COLOR_CYAN""💡 중요: 실제 오른손을 센서에 대세요!$COLOR_NC"
        
        # Initialize status variable outside of conditional blocks
        set -l enroll_status 1
        
        # Force unbuffered output using stdbuf or script command
        if command -v stdbuf &> /dev/null
            # Use stdbuf to disable buffering
            echo -e "$COLOR_YELLOW""센서에 손가락을 여러 번 대라는 메시지가 나올 것입니다...$COLOR_NC"
            stdbuf -o0 -e0 sudo fprintd-enroll -f $finger $USER
            set enroll_status $status
        else if command -v script &> /dev/null
            # Alternative: use script command for real-time output
            echo -e "$COLOR_YELLOW""센서에 손가락을 여러 번 대라는 메시지가 나올 것입니다...$COLOR_NC"
            script -q -c "sudo fprintd-enroll -f $finger $USER" /dev/null
            set enroll_status $status
        else
            # Fallback to normal command with warning
            echo -e "$COLOR_YELLOW""주의: 진행 메시지가 늦게 나타날 수 있습니다.$COLOR_NC"
            echo -e "$COLOR_YELLOW""손가락을 대고 → 떼고 → 다시 대기를 5-10번 반복하세요.$COLOR_NC"
            sudo fprintd-enroll -f $finger $USER
            set enroll_status $status
        end
        
        if test $enroll_status -eq 0
            echo -e "$COLOR_GREEN""✓ $actual_finger 를 $finger 로 등록 성공!$COLOR_NC"
        else
            # Check for duplicate enrollment
            set -l last_output (sudo fprintd-enroll -f $finger $USER 2>&1 | tail -n 5)
            if string match -q "*enroll-duplicate*" $last_output
                echo -e "$COLOR_YELLOW""⚠️  중복 감지: 이미 다른 손가락으로 등록된 지문과 유사합니다.$COLOR_NC"
                echo -e "$COLOR_CYAN""센서가 두 상태를 같은 지문으로 인식했습니다. 좋은 신호입니다!$COLOR_NC"
            else
                echo -e "$COLOR_RED""✗ 등록 실패$COLOR_NC"
                echo "다시 시도하시겠습니까? (y/n)"
                read -l retry
                if test "$retry" = "y"
                    if command -v stdbuf &> /dev/null
                        stdbuf -o0 -e0 sudo fprintd-enroll -f $finger $USER
                    else
                        sudo fprintd-enroll -f $finger $USER
                    end
                end
            end
        end
        echo ""
    end
    
    # STEP 2: 촉촉한 상태 등록
    echo -e "$COLOR_GREEN""=== STEP 2: 촉촉한 상태 등록 (오른손으로 저장) ===$COLOR_NC"
    echo -e "$COLOR_YELLOW""이제 손을 촉촉하게 만든 후 등록합니다.$COLOR_NC"
    echo ""
    
    for i in (seq (count $moist_fingers))
        set -l finger $moist_fingers[$i]
        set -l actual_finger $moist_actual[$i]
        
        echo -e "$COLOR_BLUE""[$i/2] $actual_finger → $finger 로 저장$COLOR_NC"
        echo "----------------------------------------"
        echo -e "$COLOR_YELLOW""준비사항:"
        echo "  • 손가락에 하~ 불어서 습기를 주세요"
        echo "  • 또는 물티슈로 살짝 닦은 후 등록"
        echo "  • 또는 핸드크림을 소량 바른 직후$COLOR_NC"
        echo ""
        echo "준비되면 Enter를 누르세요... (건너뛰려면 's' 입력)"
        read -l response
        
        if test "$response" = "s"
            echo -e "$COLOR_YELLOW""건너뛰었습니다.$COLOR_NC"
            continue
        end
        
        # Check if already enrolled
        set -l enrolled (fprintd-list $USER 2>/dev/null | grep -c $finger)
        if test $enrolled -gt 0
            echo -e "$COLOR_YELLOW""이미 등록되어 있습니다. 덮어쓰시겠습니까? (y/n)$COLOR_NC"
            read -l overwrite
            if test "$overwrite" != "y"
                continue
            end
        end
        
        echo -e "$COLOR_CYAN""💡 실제 오른손을 센서에 대세요!$COLOR_NC"
        
        # Initialize status variable
        set -l enroll_status 1
        
        # Force unbuffered output
        if command -v stdbuf &> /dev/null
            echo -e "$COLOR_YELLOW""센서에 손가락을 여러 번 대라는 메시지가 나올 것입니다...$COLOR_NC"
            stdbuf -o0 -e0 sudo fprintd-enroll -f $finger $USER
            set enroll_status $status
        else if command -v script &> /dev/null
            echo -e "$COLOR_YELLOW""센서에 손가락을 여러 번 대라는 메시지가 나올 것입니다...$COLOR_NC"
            script -q -c "sudo fprintd-enroll -f $finger $USER" /dev/null
            set enroll_status $status
        else
            echo -e "$COLOR_YELLOW""주의: 진행 메시지가 늦게 나타날 수 있습니다.$COLOR_NC"
            echo -e "$COLOR_YELLOW""손가락을 대고 → 떼고 → 다시 대기를 5-10번 반복하세요.$COLOR_NC"
            sudo fprintd-enroll -f $finger $USER
            set enroll_status $status
        end
        
        if test $enroll_status -eq 0
            echo -e "$COLOR_GREEN""✓ $actual_finger 를 $finger 로 등록 성공!$COLOR_NC"
        else
            echo -e "$COLOR_RED""✗ 등록 실패$COLOR_NC"
            echo "다시 시도하시겠습니까? (y/n)"
            read -l retry
            if test "$retry" = "y"
                if command -v stdbuf &> /dev/null
                    stdbuf -o0 -e0 sudo fprintd-enroll -f $finger $USER
                else
                    sudo fprintd-enroll -f $finger $USER
                end
            end
        end
        
        if test $status -eq 0
            echo -e "$COLOR_GREEN""✓ $actual_finger 를 $finger 로 등록 성공!$COLOR_NC"
        else
            echo -e "$COLOR_RED""✗ 등록 실패$COLOR_NC"
            echo "다시 시도하시겠습니까? (y/n)"
            read -l retry
            if test "$retry" = "y"
                sudo fprintd-enroll -f $finger $USER
            end
        end
        echo ""
    end
    
    echo -e "$COLOR_GREEN""=== 등록 완료 ===$COLOR_NC"
    echo ""
    show_registration_summary
    echo ""
    echo -e "$COLOR_CYAN""이제 손 상태에 관계없이 오른손으로 인증할 수 있습니다!$COLOR_NC"
    echo -e "$COLOR_YELLOW""시스템이 자동으로 왼손(건조)과 오른손(촉촉) 중에서 매칭합니다.$COLOR_NC"
end

# ===========================================
# 등록 요약 표시
# ===========================================
function show_registration_summary --description "Show fingerprint registration summary"
    
    echo -e "$COLOR_BLUE""=== 등록 요약 ===$COLOR_NC"
    
    set -l registered (fprintd-list $USER 2>/dev/null | grep -oE '(left|right)-[a-z]+-finger')
    
    if test (count $registered) -eq 0
        echo "등록된 지문이 없습니다."
        return
    end
    
    echo -e "$COLOR_CYAN""┌────────────────────────────────────────────┐"
    echo -e "│          실제 손가락 매핑 정보             │"
    echo -e "├────────────────────────────────────────────┤$COLOR_NC"
    
    for finger in $registered
        switch $finger
            case "left-index-finger"
                echo -e "$COLOR_GREEN""│ 왼손 검지  = 오른손 검지 (건조한 상태)    │$COLOR_NC"
            case "left-middle-finger"
                echo -e "$COLOR_GREEN""│ 왼손 중지  = 오른손 중지 (건조한 상태)    │$COLOR_NC"
            case "right-index-finger"
                echo -e "$COLOR_YELLOW""│ 오른손 검지 = 오른손 검지 (촉촉한 상태)   │$COLOR_NC"
            case "right-middle-finger"
                echo -e "$COLOR_YELLOW""│ 오른손 중지 = 오른손 중지 (촉촉한 상태)   │$COLOR_NC"
            case '*'
                echo -e "│ $finger (수동 등록)                        │"
        end
    end
    
    echo -e "$COLOR_CYAN""└────────────────────────────────────────────┘$COLOR_NC"
    echo ""
    echo "총 "(count $registered)" 개의 지문이 등록되어 있습니다."
end

# ===========================================
# 빠른 등록 (특정 손가락/상태)
# ===========================================
function quick_register --description "Quick registration for a specific finger and condition"
    
    echo -e "$COLOR_BLUE""=== 빠른 지문 등록 ===$COLOR_NC"
    echo ""
    echo "등록할 손가락과 상태를 선택하세요:"
    echo "  1) 오른손 검지 - 건조 (left-index-finger로 저장)"
    echo "  2) 오른손 검지 - 촉촉 (right-index-finger로 저장)"
    echo "  3) 오른손 중지 - 건조 (left-middle-finger로 저장)"
    echo "  4) 오른손 중지 - 촉촉 (right-middle-finger로 저장)"
    echo ""
    echo "선택 (1-4): "
    read -l choice
    
    switch $choice
        case 1
            set -l finger "left-index-finger"
            echo -e "$COLOR_YELLOW""오른손 검지를 건조한 상태로 준비하세요.$COLOR_NC"
            echo -e "$COLOR_CYAN""시스템에는 왼손 검지로 저장됩니다.$COLOR_NC"
            echo "Enter를 누르면 시작..."
            read
            sudo fprintd-enroll -f $finger $USER
        case 2
            set -l finger "right-index-finger"
            echo -e "$COLOR_YELLOW""오른손 검지를 촉촉한 상태로 준비하세요.$COLOR_NC"
            echo "Enter를 누르면 시작..."
            read
            sudo fprintd-enroll -f $finger $USER
        case 3
            set -l finger "left-middle-finger"
            echo -e "$COLOR_YELLOW""오른손 중지를 건조한 상태로 준비하세요.$COLOR_NC"
            echo -e "$COLOR_CYAN""시스템에는 왼손 중지로 저장됩니다.$COLOR_NC"
            echo "Enter를 누르면 시작..."
            read
            sudo fprintd-enroll -f $finger $USER
        case 4
            set -l finger "right-middle-finger"
            echo -e "$COLOR_YELLOW""오른손 중지를 촉촉한 상태로 준비하세요.$COLOR_NC"
            echo "Enter를 누르면 시작..."
            read
            sudo fprintd-enroll -f $finger $USER
        case '*'
            echo "잘못된 선택입니다."
    end
end

# ===========================================
# 지문 테스트
# ===========================================
function test_fingerprint --description "Test fingerprint with condition awareness"
    
    echo -e "$COLOR_BLUE""=== 지문 인식 테스트 ===$COLOR_NC"
    echo -e "$COLOR_CYAN""오른손을 사용하세요. 시스템이 자동으로 상태를 판단합니다.$COLOR_NC"
    echo ""
    
    show_registration_summary
    echo ""
    
    echo -e "$COLOR_YELLOW""팁: 인식이 안 되면 손 상태를 바꿔보세요."
    echo "    건조 ↔ 촉촉 상태 전환$COLOR_NC"
    echo ""
    echo "테스트를 시작하려면 Enter... (취소는 Ctrl+C)"
    read
    
    while true
        echo "손가락을 센서에 대세요..."
        fprintd-verify
        
        if test $status -eq 0
            echo -e "$COLOR_GREEN""✓ 인식 성공!$COLOR_NC"
            echo -e "$COLOR_CYAN""(건조/촉촉 중 하나의 상태로 인식되었습니다)$COLOR_NC"
        else
            echo -e "$COLOR_RED""✗ 인식 실패$COLOR_NC"
            echo -e "$COLOR_YELLOW""다른 상태로 시도해보세요:"
            echo "  • 건조하다면 → 하~ 불어서 습기 주기"
            echo "  • 촉촉하다면 → 손가락 닦고 말리기$COLOR_NC"
        end
        
        echo ""
        echo "계속 테스트? (y/n)"
        read -l continue_test
        if test "$continue_test" != "y"
            break
        end
    end
end

# ===========================================
# 모든 지문 삭제
# ===========================================
function delete_all_fingerprints --description "Delete all registered fingerprints"
    
    echo -e "$COLOR_RED""경고: 모든 등록된 지문을 삭제합니다!$COLOR_NC"
    show_registration_summary
    echo ""
    echo "정말로 삭제하시겠습니까? (yes 입력)"
    read -l confirm
    
    if test "$confirm" = "yes"
        sudo fprintd-delete $USER
        if test $status -eq 0
            echo -e "$COLOR_GREEN""모든 지문이 삭제되었습니다.$COLOR_NC"
        else
            echo -e "$COLOR_RED""지문 삭제 실패$COLOR_NC"
        end
    else
        echo "취소되었습니다."
    end
end

# ===========================================
# 특정 지문 삭제
# ===========================================
function delete_fingerprint --description "Delete a specific fingerprint"
    
    echo -e "$COLOR_BLUE""현재 등록된 지문:$COLOR_NC"
    fprintd-list $USER
    echo ""
    
    echo "삭제할 손가락 이름을 입력하세요 (예: right-index-finger):"
    echo "모두 삭제하려면 'all' 입력"
    read -l finger_to_delete
    
    if test "$finger_to_delete" = "all"
        delete_all_fingerprints
    else if test -n "$finger_to_delete"
        echo -e "$COLOR_RED""$finger_to_delete 를 삭제합니다.$COLOR_NC"
        sudo fprintd-delete $USER $finger_to_delete
        if test $status -eq 0
            echo -e "$COLOR_GREEN""삭제 완료.$COLOR_NC"
        else
            echo -e "$COLOR_RED""삭제 실패. 올바른 손가락 이름인지 확인하세요.$COLOR_NC"
        end
    else
        echo "취소되었습니다."
    end
end

# ===========================================
# 사용 팁
# ===========================================
function fingerprint_tips --description "Show tips for the dual-condition registration strategy"
    
    echo -e "$COLOR_BLUE""=== 듀얼 컨디션 지문 등록 전략 ===$COLOR_NC"
    echo ""
    
    echo -e "$COLOR_CYAN""📋 핵심 전략:$COLOR_NC"
    echo "  • 왼손 = 건조한 상태의 오른손"
    echo "  • 오른손 = 촉촉한 상태의 오른손"
    echo "  • 실제로는 오른손만 사용!"
    echo ""
    
    echo -e "$COLOR_GREEN""✅ 장점:$COLOR_NC"
    echo "  1. 계절/날씨에 관계없이 인식"
    echo "  2. 하루 중 언제든 인식 가능"
    echo "  3. 손 상태 걱정 없음"
    echo "  4. 한 손만 사용하면서도 두 가지 상태 커버"
    echo ""
    
    echo -e "$COLOR_YELLOW""🔧 문제 해결:$COLOR_NC"
    echo "  인식 안 될 때:"
    echo "    • 건조 → 손가락에 하~ 불기"
    echo "    • 촉촉 → 손가락 닦기"
    echo "  그래도 안 되면:"
    echo "    • quick_register로 해당 상태 재등록"
    echo ""
    
    echo -e "$COLOR_RED""⚠️  주의사항:$COLOR_NC"
    echo "  • 다른 사람에게 이 전략을 알리지 마세요"
    echo "  • 보안상 실제 매핑을 비밀로 유지"
    echo ""
    
    echo -e "$COLOR_BLUE""💡 추가 팁:$COLOR_NC"
    echo "  • 아침: 보통 건조 → 왼손으로 인식"
    echo "  • 운동 후: 촉촉 → 오른손으로 인식"
    echo "  • 샤워 후: 촉촉 → 오른손으로 인식"
    echo "  • 에어컨 아래: 건조 → 왼손으로 인식"
end

# ===========================================
# 지문 목록 표시
# ===========================================
function list_fingerprints --description "List all registered fingerprints with mapping"
    show_registration_summary
end

# ===========================================
# 중복 등록 처리 도움말
# ===========================================
function handle_duplicate_enrollment --description "Handle duplicate fingerprint detection"
    
    echo -e "$COLOR_YELLOW""=== 중복 지문 감지 대응 ===$COLOR_NC"
    echo ""
    echo -e "$COLOR_CYAN""좋은 소식: 센서가 다른 상태에서도 당신의 지문을 인식합니다!"
    echo "나쁜 소식: 같은 지문을 두 번 등록할 수 없습니다.$COLOR_NC"
    echo ""
    echo "해결 방법:"
    echo "  1. 이미 등록된 손가락으로도 다양한 상태에서 인식 가능"
    echo "  2. 다른 손가락들을 추가로 등록 (약지, 새끼손가락 등)"
    echo "  3. 가족/신뢰하는 사람의 손가락을 백업으로 등록"
    echo ""
    echo -e "$COLOR_GREEN""추천: 오른손 검지만 잘 등록되어 있다면 충분할 수 있습니다.$COLOR_NC"
end

# ===========================================
# 지문 등록 진단
# ===========================================
function diagnose_fingerprint --description "Diagnose fingerprint registration issues"
    
    echo -e "$COLOR_BLUE""=== 지문 등록 진단 ===$COLOR_NC"
    echo ""
    
    # List currently registered fingers
    echo -e "$COLOR_GREEN""현재 등록된 지문:$COLOR_NC"
    set -l registered (fprintd-list $USER 2>/dev/null)
    if test -z "$registered"
        echo "  (없음)"
    else
        echo "$registered"
    end
    echo ""
    
    # Test each registered finger
    echo -e "$COLOR_YELLOW""간단한 테스트를 진행합니다...$COLOR_NC"
    echo "각 상태에서 오른손 검지를 대보세요:"
    echo ""
    
    for condition in "건조한" "보통" "촉촉한"
        echo "$condition 상태로 손가락을 준비하고 Enter..."
        read
        
        echo "테스트 중..."
        set -l verify_output (timeout 5 fprintd-verify 2>&1)
        
        if string match -q "*verify-match*" $verify_output
            echo -e "$COLOR_GREEN""  ✓ $condition 상태: 인식 성공$COLOR_NC"
        else
            echo -e "$COLOR_RED""  ✗ $condition 상태: 인식 실패$COLOR_NC"
        end
    end
    
    echo ""
    echo -e "$COLOR_BLUE""진단 완료!$COLOR_NC"
    echo "인식이 안 되는 상태가 있다면:"
    echo "  1. 다른 손가락을 추가 등록"
    echo "  2. 더 많은 압력/각도 변화로 재등록"
end

# ===========================================
# fprintd 서비스 재시작
# ===========================================
function reset_fingerprint_service --description "Reset fingerprint service and clear cache"
    
    echo -e "$COLOR_YELLOW""지문 서비스를 재시작합니다...$COLOR_NC"
    
    # Stop the service
    sudo systemctl stop fprintd
    
    # Clear any potential cache files
    sudo rm -f /var/lib/fprint/.cache* 2>/dev/null
    
    # Kill any hanging fprintd processes
    sudo pkill -f fprintd 2>/dev/null
    
    # Wait a moment
    sleep 2
    
    # Restart the service
    sudo systemctl start fprintd
    
    echo -e "$COLOR_GREEN""✓ 서비스가 재시작되었습니다.$COLOR_NC"
    echo "잠시 기다려주세요..."
    sleep 2
end

# ===========================================
# 인터랙티브 등록 함수 (실시간 피드백)
# ===========================================
function enroll_with_feedback --description "Enroll fingerprint with real-time feedback"
    set -l finger $argv[1]
    set -l user $argv[2]
    
    echo -e "$COLOR_CYAN""등록을 시작합니다. 실시간 안내를 따라주세요.$COLOR_NC"
    echo ""
    
    # Python script for real-time feedback
    python3 -c "
import subprocess
import sys
import re

finger = '$finger'
user = '$user'

print('🔵 등록 프로세스 시작...')
print('손가락을 센서에 대주세요.')
print('')

process = subprocess.Popen(
    ['sudo', 'fprintd-enroll', '-f', finger, user],
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    universal_newlines=True,
    bufsize=1
)

scan_count = 0
for line in iter(process.stdout.readline, ''):
    line = line.strip()
    if not line:
        continue
    
    if 'Swipe' in line or 'Place' in line:
        scan_count += 1
        print(f'👆 [{scan_count}번째 스캔] 손가락을 대주세요...')
    elif 'remove' in line or 'lift' in line:
        print('   ↪️  손가락을 떼주세요')
    elif 'center' in line:
        print('   ⚠️  더 중앙에 대주세요')
    elif 'retry' in line or 'again' in line:
        print('   🔄 다시 시도해주세요')
    elif 'Enroll result' in line:
        if 'enroll-completed' in line:
            print('✅ 등록 완료!')
        elif 'enroll-duplicate' in line:
            print('⚠️  중복된 지문 감지')
        elif 'enroll-failed' in line:
            print('❌ 등록 실패')
    else:
        print(f'   {line}')

process.wait()
sys.exit(process.returncode)
" 2>/dev/null
    
    return $status
end

# ===========================================
# 개선된 빠른 등록
# ===========================================
function quick_register_improved --description "Improved quick registration with feedback"
    
    echo -e "$COLOR_BLUE""=== 개선된 빠른 지문 등록 ===$COLOR_NC"
    echo ""
    echo "등록할 손가락과 상태를 선택하세요:"
    echo "  1) 오른손 검지 - 건조 (left-index-finger로 저장)"
    echo "  2) 오른손 검지 - 촉촉 (right-index-finger로 저장)"
    echo "  3) 오른손 중지 - 건조 (left-middle-finger로 저장)"
    echo "  4) 오른손 중지 - 촉촉 (right-middle-finger로 저장)"
    echo ""
    echo "선택 (1-4): "
    read -l choice
    
    set -l finger ""
    set -l condition ""
    
    switch $choice
        case 1
            set finger "left-index-finger"
            set condition "건조"
            echo -e "$COLOR_YELLOW""오른손 검지를 건조한 상태로 준비하세요.$COLOR_NC"
        case 2
            set finger "right-index-finger"
            set condition "촉촉"
            echo -e "$COLOR_YELLOW""오른손 검지를 촉촉한 상태로 준비하세요.$COLOR_NC"
        case 3
            set finger "left-middle-finger"
            set condition "건조"
            echo -e "$COLOR_YELLOW""오른손 중지를 건조한 상태로 준비하세요.$COLOR_NC"
        case 4
            set finger "right-middle-finger"
            set condition "촉촉"
            echo -e "$COLOR_YELLOW""오른손 중지를 촉촉한 상태로 준비하세요.$COLOR_NC"
        case '*'
            echo "잘못된 선택입니다."
            return
    end
    
    echo -e "$COLOR_CYAN""시스템에는 $finger 로 저장됩니다.$COLOR_NC"
    echo ""
    echo "준비되면 Enter를 누르세요..."
    read
    
    # Use the improved enrollment function
    enroll_with_feedback $finger $USER
    
    if test $status -eq 0
        echo -e "$COLOR_GREEN""✓ $condition 상태 등록 성공!$COLOR_NC"
    else
        echo -e "$COLOR_RED""✗ 등록 실패$COLOR_NC"
    end
end

# ===========================================
# 사용 가능한 명령어 목록
# ===========================================
function fingerprint_help --description "Show all available fingerprint commands"
    
    echo -e "$COLOR_BLUE""=== 지문 등록 도우미 명령어 ===$COLOR_NC"
    echo ""
    echo -e "$COLOR_GREEN""등록 명령어:$COLOR_NC"
    echo "  register_fingerprints    - 메인 등록 (건조/촉촉 구분)"
    echo "  quick_register          - 특정 상태만 빠르게 등록"
    echo ""
    echo -e "$COLOR_CYAN""테스트 및 진단:$COLOR_NC"
    echo "  test_fingerprint        - 지문 인식 테스트"
    echo "  diagnose_fingerprint    - 상태별 인식 진단"
    echo "  fingerprint_status      - 빠른 상태 확인"
    echo ""
    echo -e "$COLOR_YELLOW""관리 명령어:$COLOR_NC"
    echo "  list_fingerprints       - 등록된 지문 목록"
    echo "  delete_fingerprint      - 특정 지문 삭제"
    echo "  delete_all_fingerprints - 모든 지문 삭제"
    echo "  reset_fingerprint_service - 서비스 재시작"
    echo ""
    echo -e "$COLOR_MAGENTA""도움말:$COLOR_NC"
    echo "  fingerprint_tips        - 사용 팁 표시"
    echo "  fingerprint_help        - 이 도움말 표시"
    echo "  handle_duplicate_enrollment - 중복 문제 해결"
end

# 스크립트 로드 시 안내 메시지
echo -e "$COLOR_CYAN""지문 등록 도우미가 로드되었습니다.$COLOR_NC"
echo "사용 가능한 명령어를 보려면 'fingerprint_help' 를 입력하세요."
