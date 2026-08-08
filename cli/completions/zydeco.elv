
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
            cand -h 'Print help'
            cand --help 'Print help'
            cand -V 'Print version'
            cand --version 'Print version'
            cand fmt 'Format Zydeco source files in place'
            cand run 'Run a zydeco program'
            cand check 'Check a zydeco program'
            cand build 'build'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;fmt'= {
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;run'= {
            cand --dry 'Dry run (don''t execute)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;check'= {
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;build'= {
            cand --target-os 'Target OS (defaults to host OS)'
            cand --target-arch 'Target architecture (defaults to host architecture)'
            cand -t 'Target backend'
            cand --target 'Target backend'
            cand -b 'Build Directory'
            cand --build-dir 'Build Directory'
            cand -r 'Runtime directory'
            cand --runtime-dir 'Runtime directory'
            cand -x 'Run the program after building'
            cand --execute 'Run the program after building'
            cand --no-cps 'Skip CPS translation in the StackIR pipeline'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;help'= {
            cand fmt 'Format Zydeco source files in place'
            cand run 'Run a zydeco program'
            cand check 'Check a zydeco program'
            cand build 'build'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;help;fmt'= {
        }
        &'zydeco;help;run'= {
        }
        &'zydeco;help;check'= {
        }
        &'zydeco;help;build'= {
        }
        &'zydeco;help;help'= {
        }
    ]
    $completions[$command]
}
