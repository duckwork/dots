# bashrc

shopt -s histappend
export HISTIGNORE='&:ls:cd ~:cd ..:[bf]g:exit:h:history'
export HISTCONNTORL=ignoreboth:erasedups
export HISTFILESIZE=50000
export HISTSIZE=5000
export PROMPT_COMMAND='history -a'

if ! echo $PATH | tr ':' '\n' | grep -q "$HOME/bin" -; then
  export PATH="$HOME/bin:$PATH"
fi
alias rbash="source $HOME/.bashrc"

function cleanhist # Remove duplicates from history file
{
  nl $HOME/.bash_history | sort -k 2 | uniq -f 1 | sort -n | cut -f 2 > $HOME/.bash_history;
}

# FANCY ALIASES
function mkdir # don't complain about parents
{
  command mkdir -p "$@";
}
function ls # add distinguishing marks after names
{
  command ls -F --color=auto "$@";
}
function grep # always recursive
{
  command grep -r "$@";
}
