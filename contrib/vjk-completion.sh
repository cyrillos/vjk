_vjk()
{
	local cmd cur prev words cword split=false

	if type -t _init_completion >/dev/null; then
		_init_completion -n = || return
	else
		# manual initialization for older bash completion versions
		COMPREPLY=()
		cur="${COMP_WORDS[COMP_CWORD]}"
		prev="${COMP_WORDS[COMP_CWORD-1]}"
	fi

	if [ $COMP_CWORD -gt 1 ] ; then
		cmd="${COMP_WORDS[1]}"
	else
		cmd="$prev"
	fi

	case "$cmd" in
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
		COMPREPLY=($(compgen -W '--merge --category --start --stop --summary --long' -- $cur))
		return
		;;
	restart)
		COMPREPLY=($(compgen -W '--id' -- $cur))
		return
		;;
	vjk)
		COMPREPLY=($(compgen -W 'start restart stop list add edit delete exit' -- $cur))
		return
		;;
	esac

	return
} &&
complete -F _vjk vjk
