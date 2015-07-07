# bashrc

shopt -s histappend
PROMPT_COMMAND='history -a'

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
