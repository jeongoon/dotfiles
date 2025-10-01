function em --description 'alias em emacs -nw $argv'
	emacs -nw $argv
end

function mc --description 'Emacs terminal client'
    emacsclient --socket-name=terminal-server -tc $argv
end
