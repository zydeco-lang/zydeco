
using namespace System.Management.Automation
using namespace System.Management.Automation.Language

Register-ArgumentCompleter -Native -CommandName 'zydeco' -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)

    $commandElements = $commandAst.CommandElements
    $command = @(
        'zydeco'
        for ($i = 1; $i -lt $commandElements.Count; $i++) {
            $element = $commandElements[$i]
            if ($element -isnot [StringConstantExpressionAst] -or
                $element.StringConstantType -ne [StringConstantType]::BareWord -or
                $element.Value.StartsWith('-') -or
                $element.Value -eq $wordToComplete) {
                break
        }
        $element.Value
    }) -join ';'

    $completions = @(switch ($command) {
        'zydeco' {
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('show', 'show', [CompletionResultType]::ParameterValue, 'List the project''s packages and relationships without checking or executing code')
            [CompletionResult]::new('passes', 'passes', [CompletionResultType]::ParameterValue, 'List optional compiler passes or explain a selected high-SPS plan')
            [CompletionResult]::new('__doc-example-worker', '__doc-example-worker', [CompletionResultType]::ParameterValue, '__doc-example-worker')
            [CompletionResult]::new('doc', 'doc', [CompletionResultType]::ParameterValue, 'Read, search, generate, or verify project documentation')
            [CompletionResult]::new('fmt', 'fmt', [CompletionResultType]::ParameterValue, 'Format Zydeco source files in place')
            [CompletionResult]::new('run', 'run', [CompletionResultType]::ParameterValue, 'Run a zydeco program')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check a source package and its code dependencies, including its declared executable role')
            [CompletionResult]::new('test', 'test', [CompletionResultType]::ParameterValue, 'Run a test package or the selected package''s direct test companions with empty stdin')
            [CompletionResult]::new('repl', 'repl', [CompletionResultType]::ParameterValue, 'Start the declaration-free terminal REPL')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'Build a Zydeco program for the selected target')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'zydeco;show' {
            [CompletionResult]::new('-p', '-p', [CompletionResultType]::ParameterName, 'Select declared packages by name; omit to list all packages')
            [CompletionResult]::new('--pkg', '--pkg', [CompletionResultType]::ParameterName, 'Select declared packages by name; omit to list all packages')
            [CompletionResult]::new('--package', '--package', [CompletionResultType]::ParameterName, 'Select declared packages by name; omit to list all packages')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;passes' {
            [CompletionResult]::new('--sps-passes', '--sps-passes', [CompletionResultType]::ParameterName, 'Explain `default`, `none`, or a comma-separated list such as normalize,normalize')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;__doc-example-worker' {
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;doc' {
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('show', 'show', [CompletionResultType]::ParameterValue, 'Show a public subject''s classifier and complete documentation')
            [CompletionResult]::new('search', 'search', [CompletionResultType]::ParameterValue, 'Search exposed names and documentation prose')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'Generate a self-contained searchable HTML reference without executing examples')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check links and explicitly verified examples in the entry and its dependencies')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'zydeco;doc;show' {
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;doc;search' {
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;doc;build' {
            [CompletionResult]::new('-o', '-o', [CompletionResultType]::ParameterName, 'o')
            [CompletionResult]::new('--output', '--output', [CompletionResultType]::ParameterName, 'output')
            [CompletionResult]::new('--title', '--title', [CompletionResultType]::ParameterName, 'title')
            [CompletionResult]::new('--guide', '--guide', [CompletionResultType]::ParameterName, 'Explicit guide pages; public links use `zydeco:member:./field`')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;doc;check' {
            [CompletionResult]::new('--guide', '--guide', [CompletionResultType]::ParameterName, 'guide')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;doc;help' {
            [CompletionResult]::new('show', 'show', [CompletionResultType]::ParameterValue, 'Show a public subject''s classifier and complete documentation')
            [CompletionResult]::new('search', 'search', [CompletionResultType]::ParameterValue, 'Search exposed names and documentation prose')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'Generate a self-contained searchable HTML reference without executing examples')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check links and explicitly verified examples in the entry and its dependencies')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'zydeco;doc;help;show' {
            break
        }
        'zydeco;doc;help;search' {
            break
        }
        'zydeco;doc;help;build' {
            break
        }
        'zydeco;doc;help;check' {
            break
        }
        'zydeco;doc;help;help' {
            break
        }
        'zydeco;fmt' {
            [CompletionResult]::new('--check', '--check', [CompletionResultType]::ParameterName, 'Report files that would change without writing them, and exit unsuccessfully when at least one file would change')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;run' {
            [CompletionResult]::new('-p', '-p', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--pkg', '--pkg', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--package', '--package', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('-t', '-t', [CompletionResultType]::ParameterName, 'Execution backend')
            [CompletionResult]::new('--target', '--target', [CompletionResultType]::ParameterName, 'Execution backend')
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Native runtime sources, used by exe')
            [CompletionResult]::new('--runtime-dir', '--runtime-dir', [CompletionResultType]::ParameterName, 'Native runtime sources, used by exe')
            [CompletionResult]::new('--link-library', '--link-library', [CompletionResultType]::ParameterName, 'Resolve a compiled library through its checked artifact manifest (repeatable)')
            [CompletionResult]::new('--dry', '--dry', [CompletionResultType]::ParameterName, 'Dry run (don''t execute)')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;check' {
            [CompletionResult]::new('-p', '-p', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--pkg', '--pkg', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--package', '--package', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;test' {
            [CompletionResult]::new('-p', '-p', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--pkg', '--pkg', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--package', '--package', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('-t', '-t', [CompletionResultType]::ParameterName, 'Execution backend or all; repeat to test multiple backends in order')
            [CompletionResult]::new('--target', '--target', [CompletionResultType]::ParameterName, 'Execution backend or all; repeat to test multiple backends in order')
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Native runtime sources, used by exe')
            [CompletionResult]::new('--runtime-dir', '--runtime-dir', [CompletionResultType]::ParameterName, 'Native runtime sources, used by exe')
            [CompletionResult]::new('--link-library', '--link-library', [CompletionResultType]::ParameterName, 'Resolve a compiled library through its checked artifact manifest (repeatable)')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;repl' {
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;build' {
            [CompletionResult]::new('-p', '-p', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--pkg', '--pkg', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--package', '--package', [CompletionResultType]::ParameterName, 'Select declared packages by name; repeat for multiple packages')
            [CompletionResult]::new('--target-os', '--target-os', [CompletionResultType]::ParameterName, 'Target OS (defaults to host OS)')
            [CompletionResult]::new('--target-arch', '--target-arch', [CompletionResultType]::ParameterName, 'Target architecture (defaults to host architecture)')
            [CompletionResult]::new('-t', '-t', [CompletionResultType]::ParameterName, 'Target backend')
            [CompletionResult]::new('--target', '--target', [CompletionResultType]::ParameterName, 'Target backend')
            [CompletionResult]::new('--representation', '--representation', [CompletionResultType]::ParameterName, 'Local representation policy for native, ZASM, and Wasm targets')
            [CompletionResult]::new('--sps-passes', '--sps-passes', [CompletionResultType]::ParameterName, 'High-SPS passes: default, none, or a comma-separated list; order and duplicates are preserved')
            [CompletionResult]::new('-b', '-b', [CompletionResultType]::ParameterName, 'Build Directory')
            [CompletionResult]::new('--build-dir', '--build-dir', [CompletionResultType]::ParameterName, 'Build Directory')
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Runtime directory')
            [CompletionResult]::new('--runtime-dir', '--runtime-dir', [CompletionResultType]::ParameterName, 'Runtime directory')
            [CompletionResult]::new('--link-library', '--link-library', [CompletionResultType]::ParameterName, 'Resolve a compiled library through its checked artifact manifest (repeatable)')
            [CompletionResult]::new('--trace-passes', '--trace-passes', [CompletionResultType]::ParameterName, 'Trace each selected high-SPS pass and its execution time on stderr')
            [CompletionResult]::new('--verify-passes', '--verify-passes', [CompletionResultType]::ParameterName, 'Verify high-SPS invariants before and after each selected pass')
            [CompletionResult]::new('--dump-passes', '--dump-passes', [CompletionResultType]::ParameterName, 'Print high-SPS IR before and after each selected pass on stderr')
            [CompletionResult]::new('-x', '-x', [CompletionResultType]::ParameterName, 'Run the program after building')
            [CompletionResult]::new('--execute', '--execute', [CompletionResultType]::ParameterName, 'Run the program after building')
            [CompletionResult]::new('--lint-types', '--lint-types', [CompletionResultType]::ParameterName, 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;help' {
            [CompletionResult]::new('show', 'show', [CompletionResultType]::ParameterValue, 'List the project''s packages and relationships without checking or executing code')
            [CompletionResult]::new('passes', 'passes', [CompletionResultType]::ParameterValue, 'List optional compiler passes or explain a selected high-SPS plan')
            [CompletionResult]::new('__doc-example-worker', '__doc-example-worker', [CompletionResultType]::ParameterValue, '__doc-example-worker')
            [CompletionResult]::new('doc', 'doc', [CompletionResultType]::ParameterValue, 'Read, search, generate, or verify project documentation')
            [CompletionResult]::new('fmt', 'fmt', [CompletionResultType]::ParameterValue, 'Format Zydeco source files in place')
            [CompletionResult]::new('run', 'run', [CompletionResultType]::ParameterValue, 'Run a zydeco program')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check a source package and its code dependencies, including its declared executable role')
            [CompletionResult]::new('test', 'test', [CompletionResultType]::ParameterValue, 'Run a test package or the selected package''s direct test companions with empty stdin')
            [CompletionResult]::new('repl', 'repl', [CompletionResultType]::ParameterValue, 'Start the declaration-free terminal REPL')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'Build a Zydeco program for the selected target')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'zydeco;help;show' {
            break
        }
        'zydeco;help;passes' {
            break
        }
        'zydeco;help;__doc-example-worker' {
            break
        }
        'zydeco;help;doc' {
            [CompletionResult]::new('show', 'show', [CompletionResultType]::ParameterValue, 'Show a public subject''s classifier and complete documentation')
            [CompletionResult]::new('search', 'search', [CompletionResultType]::ParameterValue, 'Search exposed names and documentation prose')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'Generate a self-contained searchable HTML reference without executing examples')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check links and explicitly verified examples in the entry and its dependencies')
            break
        }
        'zydeco;help;doc;show' {
            break
        }
        'zydeco;help;doc;search' {
            break
        }
        'zydeco;help;doc;build' {
            break
        }
        'zydeco;help;doc;check' {
            break
        }
        'zydeco;help;fmt' {
            break
        }
        'zydeco;help;run' {
            break
        }
        'zydeco;help;check' {
            break
        }
        'zydeco;help;test' {
            break
        }
        'zydeco;help;repl' {
            break
        }
        'zydeco;help;build' {
            break
        }
        'zydeco;help;help' {
            break
        }
    })

    $completions.Where{ $_.CompletionText -like "$wordToComplete*" } |
        Sort-Object -Property ListItemText
}
