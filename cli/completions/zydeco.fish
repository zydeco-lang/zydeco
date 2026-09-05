# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_zydeco_global_optspecs
    string join \n lint-types h/help V/version
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

complete -c zydeco -n "__fish_zydeco_needs_command" -l lint-types -d 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
complete -c zydeco -n "__fish_zydeco_needs_command" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_needs_command" -s V -l version -d 'Print version'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "fmt" -d 'Format Zydeco source files in place'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "run" -d 'Run a zydeco program'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "check" -d 'Check a zydeco program'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "repl" -d 'Start the declaration-free terminal REPL'
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "build"
complete -c zydeco -n "__fish_zydeco_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c zydeco -n "__fish_zydeco_using_subcommand fmt" -l check -d 'Report files that would change without writing them, and exit unsuccessfully when at least one file would change'
complete -c zydeco -n "__fish_zydeco_using_subcommand fmt" -l lint-types -d 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
complete -c zydeco -n "__fish_zydeco_using_subcommand fmt" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand run" -l dry -d 'Dry run (don\'t execute)'
complete -c zydeco -n "__fish_zydeco_using_subcommand run" -l lint-types -d 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
complete -c zydeco -n "__fish_zydeco_using_subcommand run" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand check" -l lint-types -d 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
complete -c zydeco -n "__fish_zydeco_using_subcommand check" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand repl" -l lint-types -d 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
complete -c zydeco -n "__fish_zydeco_using_subcommand repl" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -l target-os -d 'Target OS (defaults to host OS)' -r -f -a "linux\t''
macos\t''"
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -l target-arch -d 'Target architecture (defaults to host architecture)' -r -f -a "x86-64\t''
aarch64\t''"
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s t -l target -d 'Target backend' -r -f -a "zir\t''
zasm\t''
asm\t''
wasm-am\t''
wasm-sps\t''
exe\t''"
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s b -l build-dir -d 'Build Directory' -r -F
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s r -l runtime-dir -d 'Runtime directory' -r -F
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s x -l execute -d 'Run the program after building'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -l lint-types -d 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
complete -c zydeco -n "__fish_zydeco_using_subcommand build" -s h -l help -d 'Print help'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from fmt run check repl build help" -f -a "fmt" -d 'Format Zydeco source files in place'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from fmt run check repl build help" -f -a "run" -d 'Run a zydeco program'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from fmt run check repl build help" -f -a "check" -d 'Check a zydeco program'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from fmt run check repl build help" -f -a "repl" -d 'Start the declaration-free terminal REPL'
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from fmt run check repl build help" -f -a "build"
complete -c zydeco -n "__fish_zydeco_using_subcommand help; and not __fish_seen_subcommand_from fmt run check repl build help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
