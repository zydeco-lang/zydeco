complete -c zydeco -n "__fish_use_subcommand" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_use_subcommand" -s V -l version -d 'Print version'
complete -c zydeco -n "__fish_use_subcommand" -f -a "run" -d 'Run a zydeco program'
complete -c zydeco -n "__fish_use_subcommand" -f -a "check" -d 'Check a zydeco program'
complete -c zydeco -n "__fish_use_subcommand" -f -a "repl" -d 'Start a REPL'
complete -c zydeco -n "__fish_use_subcommand" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c zydeco -n "__fish_seen_subcommand_from run" -l dry -d 'Dry run (don\'t execute)'
complete -c zydeco -n "__fish_seen_subcommand_from run" -s v -l verbose -d 'Level of verbosity'
complete -c zydeco -n "__fish_seen_subcommand_from run" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_seen_subcommand_from check" -s v -l verbose -d 'Level of verbosity'
complete -c zydeco -n "__fish_seen_subcommand_from check" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_seen_subcommand_from repl" -s v -l verbose -d 'Level of verbosity'
complete -c zydeco -n "__fish_seen_subcommand_from repl" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from run check repl help" -f -a "run" -d 'Run a zydeco program'
complete -c zydeco -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from run check repl help" -f -a "check" -d 'Check a zydeco program'
complete -c zydeco -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from run check repl help" -f -a "repl" -d 'Start a REPL'
complete -c zydeco -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from run check repl help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
