# Files, byte streams, and standard I/O

The first standard-I/O interface treats text, terminal input, and files as unrelated host operations. That makes
simple examples convenient, but it leaves no reusable abstraction for copying data, buffering output, or handling
failures. It also assumes every external resource contains valid UTF-8 text. A filesystem interface should instead
begin with bytes and expose terminals and files through the same stream operations.

This design introduces opaque `Bytes`, `Reader`, and `Writer` value types. `Bytes` represents arbitrary octets;
`Reader` and `Writer` are capabilities owned by the runtime. All operations that observe or mutate a capability end
in `OS`, so opening a file or reading a stream cannot be mistaken for a pure computation.

## Module boundaries

The standard library exposes three modules with distinct responsibilities.

- `io` owns byte conversion, stream operations, and structured I/O errors.
- `fs` opens file-backed readers and writers and provides whole-file convenience operations.
- `stdio` supplies the process's standard streams and text-oriented terminal conveniences.
- `process` owns arguments, random numbers, exit, panic, and halt. These operations are no longer mixed into I/O.

`stdio` and `fs` both call the operations in `io`; they do not have independent read or write implementations. This
keeps partial writes, UTF-8 validation, EOF, flushing, and errors consistent across terminals and files.

## Values and capabilities

The core types have the following roles.

```text
Bytes   arbitrary byte sequences
Reader  a readable runtime resource
Writer  a writable runtime resource
```

`Bytes` is immutable at the language boundary. The initial interface provides an empty value, byte length,
concatenation, UTF-8 encoding from `String`, and checked UTF-8 decoding to `String`. Files remain byte-oriented;
the `read_text` and `write_text` conveniences make the encoding boundary explicit.

`Reader` and `Writer` are opaque. A value contains a runtime-managed handle identifier, never a native pointer or an
operating-system file descriptor. Copying a Zydeco value can therefore copy the identifier safely. Closing removes
the corresponding resource from the runtime's handle table, and later operations report `Closed` instead of
dereferencing freed storage. The standard input, output, and error handles are reserved process capabilities and are
not closed by ordinary library operations.

The first implementation uses blocking streams. This leaves room for future buffered, asynchronous, or seekable
capabilities without changing the distinction between bytes and text.

## Results, errors, and EOF

Fallible operations return `Result A IoError` to their continuation. `IoError` contains a stable library-level kind
and a human-readable message. The initial kinds are:

```text
NotFound  PermissionDenied  AlreadyExists  InvalidInput
InvalidData  BrokenPipe  Closed  Other
```

The runtime maps host errors to these kinds. Programs may branch on the kind and may display the message, but should
not parse the message to recover state.

End-of-file is a normal stream state rather than an error. `read` returns an empty byte sequence when the stream is
at EOF, while `read_line` returns `Ok None`. A final line without a trailing newline is returned as `Ok (Some line)`.
The line operation removes `\n` and an immediately preceding `\r`, matching terminal-oriented expectations without
changing arbitrary byte reads.

## Public operations

The shared `io` layer is byte-oriented. The exact surface types use the standard library's continuation-passing
`OS` convention:

```text
io.read       : Reader -> Int -> Thk (Result Bytes IoError -> OS) -> OS
io.read_line  : Reader -> Thk (Result (Option Bytes) IoError -> OS) -> OS
io.read_all   : Reader -> Thk (Result Bytes IoError -> OS) -> OS
io.write_all  : Writer -> Bytes -> Thk (Result Unit IoError -> OS) -> OS
io.flush      : Writer -> Thk (Result Unit IoError -> OS) -> OS
io.close_reader : Reader -> Thk (Result Unit IoError -> OS) -> OS
io.close_writer : Writer -> Thk (Result Unit IoError -> OS) -> OS
```

`read` rejects negative byte counts. `write_all` either writes the complete buffer or reports an error; exposing a
partial-write primitive would force every caller to duplicate the same retry loop.

The `fs` module uses a typed `Path` wrapper around a UTF-8 `String`. This wrapper prevents ordinary text from being
passed accidentally where the host expects a path, while preserving the current language's portable UTF-8 model.
It does not claim that every native path can be represented on every operating system; a future platform-specific
path representation can replace the wrapper without changing stream operations.

```text
fs.path          : String -> Path
fs.path_string   : Path -> String
fs.open_reader   : Path -> Thk (Result Reader IoError -> OS) -> OS
fs.create_writer : Path -> Thk (Result Writer IoError -> OS) -> OS
fs.append_writer : Path -> Thk (Result Writer IoError -> OS) -> OS
fs.read_bytes    : Path -> Thk (Result Bytes IoError -> OS) -> OS
fs.read_text     : Path -> Thk (Result String IoError -> OS) -> OS
fs.write_bytes   : Path -> Bytes -> Thk (Result Unit IoError -> OS) -> OS
fs.write_text    : Path -> String -> Thk (Result Unit IoError -> OS) -> OS
```

`create_writer` creates a missing file and truncates an existing one. `append_writer` creates a missing file and
places every write at the end. Whole-file helpers open, operate, and close internally. If the data operation fails,
that error wins; otherwise a close error is returned.

The `stdio` module exposes `stdin`, `stdout`, and `stderr` as capabilities. Its `read_line` checks UTF-8 and returns
`Result (Option String) IoError`; its `write`, `write_line`, and error-stream variants encode `String` to `Bytes`,
delegate to `io.write_all`, and preserve write or flush failures.

## Backend contract

The interpreter and native runtime implement the same primitive ABI. Both keep monotonically allocated handle IDs,
distinguish reader and writer operations in the typed ABI, validate each identifier against its resource table, and
translate host errors to the shared error-kind codes. The interpreter reserves capabilities for its injected input
and output streams so tests and embedding continue to control standard I/O. The native runtime reserves capabilities
backed by the process's actual standard streams.

Primitive callbacks carry either a successful value or an error kind plus message. Standard-library code converts
those branches into `Result`, keeping runtime representations out of user programs. This division makes the host
responsible for resource safety and the standard library responsible for ergonomic composition.
