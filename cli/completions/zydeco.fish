# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_zydeco_global_optspecs
	string join \n h/help V/version
end

function __fish_zydeco_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_zydeco_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_zydeco_using_subcommand
	set -l cmd (__fish_zydeco_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c zydeco -n "__fish_zydeco_needs_command" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_needs_command" -s V -l version -d 'Print version'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "run" -d 'Run a zydeco program'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "check" -d 'Check a zydeco program'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "build"
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c zydeco -n "__fish_zydeco_using_subcommand run" -l bin -d 'Name of the binary' -r
complete -c zydeco -n "__fish_zydeco_using_subcommand run" -l dry -d 'Dry run (don\'t execute)'
complete -c zydeco -n "__fish_zydeco_using_subcommand run" -s v -l verbose -d 'Level of verbosity'
complete -c zydeco -n "__fish_zydeco_using_subcommand run" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand check" -s v -l verbose -d 'Level of verbosity'
complete -c zydeco -n "__fish_zydeco_using_subcommand check" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -l bin -d 'Name of the binary' -r
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s t -l target -d 'Target architecture' -r
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s b -l build-dir -d 'Build Directory' -r -F
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s r -l runtime-dir -d 'Runtime directory' -r -F
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -l link-existing -d 'Skip dumping assembly and only link existing files'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s x -l execute -d 'Run the program after building'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -l dry -d 'Dry run (don\'t execute)'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s v -l verbose -d 'Level of verbosity'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from run check build help" -f -a "run" -d 'Run a zydeco program'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from run check build help" -f -a "check" -d 'Check a zydeco program'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from run check build help" -f -a "build"
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from run check build help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
