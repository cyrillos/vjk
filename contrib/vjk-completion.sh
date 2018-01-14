_vjk()
{
	local cur prev words cword split=false

	if type -t _init_completion >/dev/null; then
		_init_completion -n = || return
	else
		# manual initialization for older bash completion versions
		COMPREPLY=()
		cur="${COMP_WORDS[COMP_CWORD]}"
		prev="${COMP_WORDS[COMP_CWORD-1]}"
	fi

	case "$prev" in
	add)
		COMPREPLY=($(compgen -W '--category --start --tz-start --stop --tz-stop' -- $cur))
		return
		;;
	edit)
		COMPREPLY=($(compgen -W '--category --id --start --stop' -- $cur))
		return
		;;
	delete)
		COMPREPLY=($(compgen -W '--category --id' -- $cur))
		return
		;;
	list)
		COMPREPLY=($(compgen -W '--category --start --stop --summary --long' -- $cur))
		return
		;;
	vjk)
		COMPREPLY=($(compgen -W 'start stop list add edit delete exit' -- $cur))
		return
		;;
	esac

	$split && return
	_filedir
} &&
complete -F _vjk vjk
