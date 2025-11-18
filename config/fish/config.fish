set -g theme_nerd_fonts yes # if you set by -g once, don't need to set again.
set -g theme_color_scheme gruvbox
set -g fish_color_autosuggestion 999 brblack

# please see ~myoungjin/bin/urxvt
#set -g hostname xps2013 # this is read-only value
set -U RXVT_SOCKET $HOME/.cache/urxvt/urxvtd-$hostname
set BC_ENV_ARGS "-q"

if test -f /etc/debian_version
    abbr AI sudo apt-get install
    abbr AU sudo apt-get update
else if test -f /etc/arch-release
    abbr SP sudo pacman
end

abbr SU sudo -i
abbr SS sudo systemctl

# Set EDITOR based on display environment
if set -q WAYLAND_DISPLAY; or set -q DISPLAY
    set -x EDITOR "emacsclient --socket-name=gui-server"
else
    set -x EDITOR "emacsclient --socket-name=terminal-server -t"
end
set -x IM_MODULE ibus
set -x GTK_IM_MODULE $IM_MODULE
set -x QT_IM_MODULE $IM_MODULE
set -x QT4_IM_MODULE $IM_MODULE
set -x XMODIFERS "@im=$IM_MODULE"
set -x MOZ_ENABLE_WAYLAND 1

set -q GOPATH; and set -x GOPATH $HOME/proj/golib
set -x s_path_basic bin sbin perl5/bin .local/bin .ghcup/bin
set -x s_path_raku  .rakudo/install/bin \
    .rakudo/install/share/perl6/site/bin .local/share/rakudo/bin \
    .local/share/rakudo/share/perl6/site/bin
set -x s_path_golang .local/share/go/bin $GOPATH/bin

for di in $s_path_basic $s_path_raku $s_path_golang
    if test -d $HOME/$di
        elem "$HOME/$di" $PATH; or set -x PATH $HOME/$di $PATH
    end
end

set -x texbin /usr/local/texlive/2023/bin/x86_64-linux

if test -d $texbin
    elem $texbin "$PATH"; or set -x PATH $texbin $PATH
end

if test -d $HOME/perl5
    set -q PERL5LIB; and set -x PERL5LIB $HOME/perl5/lib/perl5:$PERL5LIB;
    set -q PERL5LIB; or set -x PERL5LIB $HOME/perl5/lib/perl5;
    set -q PERL_LOCAL_LIB_ROOT; and set -x PERL_LOCAL_LIB_ROOT $HOME/perl5:$PERL_LOCAL_LIB_ROOT;
    set -q PERL_LOCAL_LIB_ROOT; or set -x PERL_LOCAL_LIB_ROOT $HOME/perl5;
    set -q PERL_INSTALL_ROOT; or set -x PERL_INSTALL_ROOT $HOME/perl5
    set -x PERL_MB_OPT --install_base\ .;
    set -x PERL_MM_OPT INSTALL_BASE=.;
end

set -e MANPATH
set -x MANPATH (manpath)

set -q s_manpath_perl; and set -x s_manpath_perl $HOME/perl5/man
if test -d $s_manpath_perl
    elem $s_manpath_perl "$MANPATH"
    or set -x MANPATH $MANPATH $s_manpath_perl
end


# PERL6LIB use comma(,) as a seporator

for di in $HOME/lib $HOME/proj/myPerl6
    if test -d $di
        if string match "" "$PERL6LIB"
            set -x PERL6LIB $di
        else
            string match -vq $di "$PERL6LIB"; and set -x PERL6LIB $di,$PERL6LIB
        end
    end
end

if test -d $HOME/.wine
    set -x WINEPREFIX $HOME/.wine
    set -x WINEARCH win64
end
