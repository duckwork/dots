# bashrc

shopt -s histappend
export HISTCONNTORL=ignoreboth:erasedups
export HISTFILESIZE=50000
export HISTSIZE=5000
export HISTIGNORE=ls:exit:la:l:pwd
PROMPT_COMMAND='history -a'

# FANCY ALIASES
function mkdir # don't complain about parents
{
  command mkdir -p "$@"
}
function ls # add distinguishing marks after names
{
  command ls -F --color=auto "$@"
}
function grep # always recursive
{
  command grep -r "$@"
}
