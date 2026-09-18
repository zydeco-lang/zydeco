
use builtin;
use str;

set edit:completion:arg-completer[zydeco] = {|@words|
    fn spaces {|n|
        builtin:repeat $n ' ' | str:join ''
    }
    fn cand {|text desc|
        edit:complex-candidate $text &display=$text' '(spaces (- 14 (wcswidth $text)))$desc
    }
    var command = 'zydeco'
    for word $words[1..-1] {
        if (str:has-prefix $word '-') {
            break
        }
        set command = $command';'$word
    }
    var completions = [
        &'zydeco'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
            cand -V 'Print version'
            cand --version 'Print version'
            cand show 'List the project''s packages and relationships without checking or executing code'
            cand passes 'List optional compiler passes or explain a selected high-SPS plan'
            cand fmt 'Format Zydeco source files in place'
            cand run 'Run a zydeco program'
            cand check 'Check a source package and its code dependencies, including its declared executable role'
            cand test 'Run explicitly selected test packages with empty stdin'
            cand repl 'Start the declaration-free terminal REPL'
            cand build 'Build a Zydeco program for the selected target'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;show'= {
            cand -p 'Select declared packages by name; omit to list all packages'
            cand --pkg 'Select declared packages by name; omit to list all packages'
            cand --package 'Select declared packages by name; omit to list all packages'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;passes'= {
            cand --sps-passes 'Explain `default`, `none`, or a comma-separated list such as normalize,normalize'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;fmt'= {
            cand --check 'Report files that would change without writing them, and exit unsuccessfully when at least one file would change'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;run'= {
            cand -p 'Select declared packages by name; repeat for multiple packages'
            cand --pkg 'Select declared packages by name; repeat for multiple packages'
            cand --package 'Select declared packages by name; repeat for multiple packages'
            cand -t 'Execution backend'
            cand --target 'Execution backend'
            cand -r 'Native runtime sources, used by exe'
            cand --runtime-dir 'Native runtime sources, used by exe'
            cand --link-library 'Resolve a compiled library through its checked artifact manifest (repeatable)'
            cand --dry 'Dry run (don''t execute)'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;check'= {
            cand -p 'Select declared packages by name; repeat for multiple packages'
            cand --pkg 'Select declared packages by name; repeat for multiple packages'
            cand --package 'Select declared packages by name; repeat for multiple packages'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;test'= {
            cand -p 'Select declared packages by name; repeat for multiple packages'
            cand --pkg 'Select declared packages by name; repeat for multiple packages'
            cand --package 'Select declared packages by name; repeat for multiple packages'
            cand -t 'Execution backend or all; repeat to test multiple backends in order'
            cand --target 'Execution backend or all; repeat to test multiple backends in order'
            cand -r 'Native runtime sources, used by exe'
            cand --runtime-dir 'Native runtime sources, used by exe'
            cand --link-library 'Resolve a compiled library through its checked artifact manifest (repeatable)'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;repl'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;build'= {
            cand -p 'Select declared packages by name; repeat for multiple packages'
            cand --pkg 'Select declared packages by name; repeat for multiple packages'
            cand --package 'Select declared packages by name; repeat for multiple packages'
            cand --target-os 'Target OS (defaults to host OS)'
            cand --target-arch 'Target architecture (defaults to host architecture)'
            cand -t 'Target backend'
            cand --target 'Target backend'
            cand --representation 'Local representation policy for native, ZASM, and Wasm targets'
            cand --sps-passes 'High-SPS passes: default, none, or a comma-separated list; order and duplicates are preserved'
            cand -b 'Build Directory'
            cand --build-dir 'Build Directory'
            cand -r 'Runtime directory'
            cand --runtime-dir 'Runtime directory'
            cand --link-library 'Resolve a compiled library through its checked artifact manifest (repeatable)'
            cand --trace-passes 'Trace each selected high-SPS pass and its execution time on stderr'
            cand --verify-passes 'Verify high-SPS invariants before and after each selected pass'
            cand --dump-passes 'Print high-SPS IR before and after each selected pass on stderr'
            cand --print-ids 'Follow every name in IR listings with its arena id, as `acc[54#2257]`, instead of disambiguating rebound names with a prime suffix'
            cand -x 'Run the program after building'
            cand --execute 'Run the program after building'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;help'= {
            cand show 'List the project''s packages and relationships without checking or executing code'
            cand passes 'List optional compiler passes or explain a selected high-SPS plan'
            cand fmt 'Format Zydeco source files in place'
            cand run 'Run a zydeco program'
            cand check 'Check a source package and its code dependencies, including its declared executable role'
            cand test 'Run explicitly selected test packages with empty stdin'
            cand repl 'Start the declaration-free terminal REPL'
            cand build 'Build a Zydeco program for the selected target'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;help;show'= {
        }
        &'zydeco;help;passes'= {
        }
        &'zydeco;help;fmt'= {
        }
        &'zydeco;help;run'= {
        }
        &'zydeco;help;check'= {
        }
        &'zydeco;help;test'= {
        }
        &'zydeco;help;repl'= {
        }
        &'zydeco;help;build'= {
        }
        &'zydeco;help;help'= {
        }
    ]
    $completions[$command]
}
