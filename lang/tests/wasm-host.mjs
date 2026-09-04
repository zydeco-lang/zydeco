import fs from "node:fs";

const WORD_BITS = 64;
const IMMEDIATE_SIGNED_MIN = -(0x4000_0000_0000_0000n);
const IMMEDIATE_SIGNED_MAX = 0x3fff_ffff_ffff_ffffn;
const IMMEDIATE_UNSIGNED_MAX = 0x7fff_ffff_ffff_ffffn;

class ExitSignal extends Error {
  constructor(code) {
    super(`Zydeco exited with status ${code}`);
    this.code = code;
  }
}

function compareBytes(first, second) {
  const shared = Math.min(first.length, second.length);
  for (let index = 0; index < shared; index += 1) {
    if (first[index] !== second[index]) {
      return first[index] < second[index] ? -1 : 1;
    }
  }
  return Math.sign(first.length - second.length);
}

class RuntimeWords {
  constructor(memory) {
    this.memory = memory;
  }

  static signed(word) {
    return BigInt.asIntN(WORD_BITS, word);
  }

  static unsigned(word) {
    return BigInt.asUintN(WORD_BITS, word);
  }

  static isImmediate(word) {
    return (RuntimeWords.unsigned(word) & 1n) === 1n;
  }

  static immediateSigned(value) {
    if (value < IMMEDIATE_SIGNED_MIN || value > IMMEDIATE_SIGNED_MAX) {
      throw new RangeError(`signed runtime value ${value} does not fit an immediate`);
    }
    return RuntimeWords.signed((RuntimeWords.unsigned(value) << 1n) | 1n);
  }

  static immediateUnsigned(value) {
    if (value < 0n || value > IMMEDIATE_UNSIGNED_MAX) {
      throw new RangeError(`unsigned runtime value ${value} does not fit an immediate`);
    }
    return RuntimeWords.signed((value << 1n) | 1n);
  }

  static decodeImmediateSigned(word) {
    if (!RuntimeWords.isImmediate(word)) {
      throw new TypeError("expected an immediate runtime word");
    }
    return RuntimeWords.signed(word) >> 1n;
  }

  static decodeImmediateUnsigned(word) {
    if (!RuntimeWords.isImmediate(word)) {
      throw new TypeError("expected an immediate runtime word");
    }
    return RuntimeWords.unsigned(word) >> 1n;
  }

  view() {
    return new DataView(this.memory().buffer);
  }

  loadBits(word) {
    if (RuntimeWords.isImmediate(word)) {
      throw new TypeError("expected a boxed runtime scalar");
    }
    const address = Number(RuntimeWords.unsigned(word));
    if (address === 0) {
      throw new TypeError("boxed runtime scalar pointer is null");
    }
    return this.view().getBigUint64(address, true);
  }

  storeBits(spare, bits) {
    const address = spare >>> 0;
    if (address === 0) {
      throw new TypeError("wide scalar operation did not receive a spare box");
    }
    this.view().setBigUint64(address, BigInt.asUintN(WORD_BITS, bits), true);
    return BigInt(address);
  }

  decodeSigned(word, width) {
    const bits = RuntimeWords.isImmediate(word)
      ? RuntimeWords.decodeImmediateSigned(word)
      : BigInt.asIntN(WORD_BITS, this.loadBits(word));
    return BigInt.asIntN(width, bits);
  }

  decodeUnsigned(word, width) {
    const bits = RuntimeWords.isImmediate(word)
      ? RuntimeWords.decodeImmediateUnsigned(word)
      : this.loadBits(word);
    return BigInt.asUintN(width, bits);
  }

  encodeSigned(value, width, spare) {
    const wrapped = BigInt.asIntN(width, value);
    return wrapped >= IMMEDIATE_SIGNED_MIN && wrapped <= IMMEDIATE_SIGNED_MAX
      ? RuntimeWords.immediateSigned(wrapped)
      : this.storeBits(spare, BigInt.asUintN(width, wrapped));
  }

  encodeUnsigned(value, width, spare) {
    const wrapped = BigInt.asUintN(width, value);
    return wrapped <= IMMEDIATE_UNSIGNED_MAX
      ? RuntimeWords.immediateUnsigned(wrapped)
      : this.storeBits(spare, wrapped);
  }
}

class HostValues {
  constructor() {
    this.next = -2n;
    this.values = new Map();
  }

  store(kind, value) {
    const handle = this.next;
    this.next -= 2n;
    this.values.set(handle, { kind, value });
    return handle;
  }

  load(kind, handle) {
    const entry = this.values.get(RuntimeWords.signed(handle));
    if (entry?.kind !== kind) {
      throw new TypeError(`expected host ${kind} handle ${handle}`);
    }
    return entry.value;
  }

  string(value) {
    return this.store("string", value);
  }

  getString(handle) {
    return this.load("string", handle);
  }

  bytes(value) {
    return this.store("bytes", Uint8Array.from(value));
  }

  getBytes(handle) {
    return this.load("bytes", handle);
  }
}

class Transfers {
  static withoutArguments(closure) {
    return [0n, closure, 0n, 0n];
  }

  static withOneArgument(closure, argument) {
    return [1n, closure, argument, 0n];
  }

  static withTwoArguments(closure, first, second) {
    return [2n, closure, first, second];
  }
}

class InputBuffer {
  constructor(bytes) {
    this.bytes = Uint8Array.from(bytes);
    this.offset = 0;
  }

  read(count) {
    const end = Math.min(this.offset + count, this.bytes.length);
    const bytes = this.bytes.slice(this.offset, end);
    this.offset = end;
    return bytes;
  }

  readAll() {
    return this.read(this.bytes.length - this.offset);
  }

  readLine() {
    const newline = this.bytes.indexOf(0x0a, this.offset);
    const end = newline < 0 ? this.bytes.length : newline + 1;
    const bytes = this.bytes.slice(this.offset, end);
    this.offset = end;
    if (bytes.at(-1) === 0x0a) {
      const withoutNewline = bytes.slice(0, -1);
      return withoutNewline.at(-1) === 0x0d ? withoutNewline.slice(0, -1) : withoutNewline;
    }
    return bytes;
  }

  get eof() {
    return this.offset >= this.bytes.length;
  }
}

class HostIo {
  constructor(stdin) {
    this.stdin = stdin;
    this.nextReader = 1;
    this.nextWriter = 2;
    this.readers = new Map();
    this.writers = new Map();
  }

  static encodeHandle(handle) {
    return RuntimeWords.immediateUnsigned(BigInt(handle));
  }

  static decodeHandle(handle) {
    return Number(RuntimeWords.decodeImmediateUnsigned(handle));
  }

  reader(handle) {
    const decoded = HostIo.decodeHandle(handle);
    if (decoded === 0) {
      return this.stdin;
    }
    const reader = this.readers.get(decoded);
    if (reader === undefined) {
      throw Object.assign(new Error("I/O capability is closed"), { code: "ZYDECO_CLOSED" });
    }
    return reader;
  }

  write(handle, bytes) {
    const decoded = HostIo.decodeHandle(handle);
    if (decoded === 0) {
      process.stdout.write(bytes);
      return;
    }
    if (decoded === 1) {
      process.stderr.write(bytes);
      return;
    }
    const descriptor = this.writers.get(decoded);
    if (descriptor === undefined) {
      throw Object.assign(new Error("I/O capability is closed"), { code: "ZYDECO_CLOSED" });
    }
    fs.writeSync(descriptor, bytes);
  }

  flush(handle) {
    const decoded = HostIo.decodeHandle(handle);
    if (decoded > 1) {
      const descriptor = this.writers.get(decoded);
      if (descriptor === undefined) {
        throw Object.assign(new Error("I/O capability is closed"), { code: "ZYDECO_CLOSED" });
      }
      fs.fsyncSync(descriptor);
    }
  }

  openReader(path) {
    const handle = this.nextReader++;
    this.readers.set(handle, new InputBuffer(fs.readFileSync(path)));
    return HostIo.encodeHandle(handle);
  }

  openWriter(path, append) {
    const handle = this.nextWriter++;
    this.writers.set(handle, fs.openSync(path, append ? "a" : "w"));
    return HostIo.encodeHandle(handle);
  }

  closeReader(handle) {
    const decoded = HostIo.decodeHandle(handle);
    if (decoded !== 0 && !this.readers.delete(decoded)) {
      throw Object.assign(new Error("I/O capability is closed"), { code: "ZYDECO_CLOSED" });
    }
  }

  closeWriter(handle) {
    const decoded = HostIo.decodeHandle(handle);
    if (decoded <= 1) {
      return;
    }
    const descriptor = this.writers.get(decoded);
    if (descriptor === undefined) {
      throw Object.assign(new Error("I/O capability is closed"), { code: "ZYDECO_CLOSED" });
    }
    fs.closeSync(descriptor);
    this.writers.delete(decoded);
  }
}

class ZydecoHost {
  constructor(arguments_, stdin) {
    this.arguments = arguments_;
    this.input = new InputBuffer(stdin);
    this.values = new HostValues();
    this.io = new HostIo(this.input);
    this.instance = undefined;
    this.words = new RuntimeWords(() => this.memory());
    this.utf8 = new TextEncoder();
    this.utf8Decoder = new TextDecoder("utf-8", { fatal: true });
    this.floatScratch = new DataView(new ArrayBuffer(8));
  }

  memory() {
    const memory = this.instance?.exports.memory;
    if (!(memory instanceof WebAssembly.Memory)) {
      throw new Error("generated module has not exported its memory yet");
    }
    return memory;
  }

  imports() {
    const functions = new Map();
    functions.set("string_literal", (offset, length) => this.stringLiteral(offset, length));
    this.installNumeric(functions);
    this.installText(functions);
    this.installIo(functions);
    this.installProcess(functions);
    return {
      zydeco: new Proxy(
        {},
        {
          get: (_target, name) => {
            const implementation = functions.get(name);
            if (implementation !== undefined) {
              return implementation;
            }
            return () => {
              throw new Error(`WASM test host does not implement zydeco.${String(name)}`);
            };
          },
        },
      ),
    };
  }

  stringLiteral(offset, length) {
    const bytes = new Uint8Array(this.memory().buffer, offset >>> 0, length >>> 0);
    return this.values.string(this.utf8Decoder.decode(bytes));
  }

  installNumeric(functions) {
    const integerTypes = [
      ["int8", 8, true],
      ["int16", 16, true],
      ["int32", 32, true],
      ["int64", 64, true],
      ["uint8", 8, false],
      ["uint16", 16, false],
      ["uint32", 32, false],
      ["uint64", 64, false],
    ];
    for (const [name, width, signed] of integerTypes) {
      const decode = (word) =>
        signed ? this.words.decodeSigned(word, width) : this.words.decodeUnsigned(word, width);
      const encode = (value, spare) =>
        signed
          ? this.words.encodeSigned(value, width, spare)
          : this.words.encodeUnsigned(value, width, spare);
      const arithmetic = {
        add: (left, right) => left + right,
        sub: (left, right) => left - right,
        mul: (left, right) => left * right,
        div: (left, right) => left / right,
        mod: (left, right) => left % right,
      };
      for (const [operation, apply] of Object.entries(arithmetic)) {
        functions.set(`${name}_${operation}`, (first, second, spare) =>
          encode(apply(decode(first), decode(second)), spare),
        );
      }
      const comparisons = {
        eq: (left, right) => left === right,
        lt: (left, right) => left < right,
        gt: (left, right) => left > right,
      };
      for (const [operation, predicate] of Object.entries(comparisons)) {
        functions.set(`${name}_${operation}_branch`, (first, second, whenTrue, whenFalse) =>
          Transfers.withoutArguments(predicate(decode(first), decode(second)) ? whenTrue : whenFalse),
        );
      }
      functions.set(`${name}_to_string`, (word) => this.values.string(decode(word).toString()));
    }

    const floats = [
      ["float32", 32],
      ["float64", 64],
    ];
    for (const [name, width] of floats) {
      const decode = (word) => this.decodeFloat(word, width);
      const encode = (value, spare) => this.encodeFloat(value, width, spare);
      const round = (value) => (width === 32 ? Math.fround(value) : value);
      const arithmetic = {
        add: (left, right) => left + right,
        sub: (left, right) => left - right,
        mul: (left, right) => left * right,
        div: (left, right) => left / right,
      };
      for (const [operation, apply] of Object.entries(arithmetic)) {
        functions.set(`${name}_${operation}`, (first, second, spare) =>
          encode(round(apply(decode(first), decode(second))), spare),
        );
      }
      const comparisons = {
        eq: (left, right) => left === right,
        lt: (left, right) => left < right,
        gt: (left, right) => left > right,
      };
      for (const [operation, predicate] of Object.entries(comparisons)) {
        functions.set(`${name}_${operation}_branch`, (first, second, whenTrue, whenFalse) =>
          Transfers.withoutArguments(predicate(decode(first), decode(second)) ? whenTrue : whenFalse),
        );
      }
      functions.set(`${name}_to_string`, (word) => this.values.string(String(decode(word))));
    }
  }

  decodeFloat(word, width) {
    if (width === 32) {
      const bits = Number(RuntimeWords.decodeImmediateUnsigned(word));
      this.floatScratch.setUint32(0, bits, true);
      return this.floatScratch.getFloat32(0, true);
    }
    this.floatScratch.setBigUint64(0, this.words.loadBits(word), true);
    return this.floatScratch.getFloat64(0, true);
  }

  encodeFloat(value, width, spare) {
    if (width === 32) {
      this.floatScratch.setFloat32(0, value, true);
      return RuntimeWords.immediateUnsigned(BigInt(this.floatScratch.getUint32(0, true)));
    }
    this.floatScratch.setFloat64(0, value, true);
    return this.words.storeBits(spare, this.floatScratch.getBigUint64(0, true));
  }

  installText(functions) {
    functions.set("str_scalar_length", (string) =>
      RuntimeWords.immediateSigned(BigInt([...this.values.getString(string)].length)),
    );
    functions.set("str_byte_length", (string) =>
      RuntimeWords.immediateSigned(BigInt(this.utf8.encode(this.values.getString(string)).length)),
    );
    functions.set("str_append", (first, second) =>
      this.values.string(this.values.getString(first) + this.values.getString(second)),
    );
    functions.set("str_get_branch", (string, index, whenNone, whenSome) => {
      const characters = [...this.values.getString(string)];
      const decoded = this.words.decodeSigned(index, 64);
      if (decoded < 0n || decoded >= BigInt(characters.length)) {
        return Transfers.withoutArguments(whenNone);
      }
      return Transfers.withOneArgument(
        whenSome,
        RuntimeWords.immediateUnsigned(BigInt(characters[Number(decoded)].codePointAt(0))),
      );
    });
    functions.set("str_split_once_branch", (string, separator, whenNone, whenSome) => {
      const value = this.values.getString(string);
      const character = String.fromCodePoint(Number(RuntimeWords.decodeImmediateUnsigned(separator)));
      const index = value.indexOf(character);
      return index < 0
        ? Transfers.withoutArguments(whenNone)
        : Transfers.withTwoArguments(
            whenSome,
            this.values.string(value.slice(0, index)),
            this.values.string(value.slice(index + character.length)),
          );
    });
    functions.set("str_split_at_branch", (string, index, whenNone, whenSome) => {
      const characters = [...this.values.getString(string)];
      const decoded = this.words.decodeSigned(index, 64);
      if (decoded < 0n || decoded > BigInt(characters.length)) {
        return Transfers.withoutArguments(whenNone);
      }
      const offset = Number(decoded);
      return Transfers.withTwoArguments(
        whenSome,
        this.values.string(characters.slice(0, offset).join("")),
        this.values.string(characters.slice(offset).join("")),
      );
    });
    functions.set("str_eq_branch", (first, second, whenTrue, whenFalse) =>
      Transfers.withoutArguments(
        this.values.getString(first) === this.values.getString(second) ? whenTrue : whenFalse,
      ),
    );
    functions.set("char_to_str", (character) =>
      this.values.string(
        String.fromCodePoint(Number(RuntimeWords.decodeImmediateUnsigned(character))),
      ),
    );
    functions.set("char_codepoint", (character) =>
      RuntimeWords.immediateSigned(RuntimeWords.decodeImmediateUnsigned(character)),
    );
    functions.set("char_from_codepoint_branch", (codepoint, whenNone, whenSome) => {
      const decoded = this.words.decodeSigned(codepoint, 64);
      const valid =
        decoded >= 0n &&
        decoded <= 0x10ffffn &&
        !(decoded >= 0xd800n && decoded <= 0xdfffn);
      return valid
        ? Transfers.withOneArgument(whenSome, RuntimeWords.immediateUnsigned(decoded))
        : Transfers.withoutArguments(whenNone);
    });
    functions.set("str_parse_int_branch", (string, whenNone, whenSome, spare) => {
      const source = this.values.getString(string);
      if (!/^[+-]?[0-9]+$/.test(source)) {
        return Transfers.withoutArguments(whenNone);
      }
      const parsed = BigInt(source);
      if (parsed < -(1n << 63n) || parsed > (1n << 63n) - 1n) {
        return Transfers.withoutArguments(whenNone);
      }
      return Transfers.withOneArgument(whenSome, this.words.encodeSigned(parsed, 64, spare));
    });
    functions.set("bytes_empty", () => this.values.bytes([]));
    functions.set("bytes_length", (bytes) =>
      RuntimeWords.immediateSigned(BigInt(this.values.getBytes(bytes).length)),
    );
    functions.set("bytes_append", (first, second) =>
      this.values.bytes([...this.values.getBytes(first), ...this.values.getBytes(second)]),
    );
    functions.set("bytes_from_str", (string) =>
      this.values.bytes(this.utf8.encode(this.values.getString(string))),
    );
    functions.set("bytes_to_str_branch", (bytes, whenInvalid, whenValid) => {
      try {
        return Transfers.withOneArgument(
          whenValid,
          this.values.string(this.utf8Decoder.decode(this.values.getBytes(bytes))),
        );
      } catch (error) {
        if (error instanceof TypeError) {
          return Transfers.withoutArguments(whenInvalid);
        }
        throw error;
      }
    });
    functions.set("bytes_get_branch", (bytes, index, whenNone, whenSome) => {
      const view = this.values.getBytes(bytes);
      const decoded = this.words.decodeSigned(index, 64);
      if (decoded < 0n || decoded >= BigInt(view.length)) {
        return Transfers.withoutArguments(whenNone);
      }
      return Transfers.withOneArgument(
        whenSome,
        RuntimeWords.immediateUnsigned(BigInt(view[Number(decoded)])),
      );
    });
    functions.set("bytes_slice_branch", (bytes, start, length, whenNone, whenSome) => {
      const view = this.values.getBytes(bytes);
      const from = this.words.decodeSigned(start, 64);
      const span = this.words.decodeSigned(length, 64);
      if (from < 0n || span < 0n || from + span > BigInt(view.length)) {
        return Transfers.withoutArguments(whenNone);
      }
      const offset = Number(from);
      return Transfers.withOneArgument(
        whenSome,
        this.values.bytes(view.subarray(offset, offset + Number(span))),
      );
    });
    functions.set("bytes_singleton", (octet) =>
      this.values.bytes([Number(RuntimeWords.decodeImmediateUnsigned(octet))]),
    );
    functions.set("bytes_eq_branch", (first, second, whenTrue, whenFalse) =>
      Transfers.withoutArguments(
        compareBytes(this.values.getBytes(first), this.values.getBytes(second)) === 0
          ? whenTrue
          : whenFalse,
      ),
    );
    functions.set("bytes_lt_branch", (first, second, whenTrue, whenFalse) =>
      Transfers.withoutArguments(
        compareBytes(this.values.getBytes(first), this.values.getBytes(second)) < 0
          ? whenTrue
          : whenFalse,
      ),
    );
  }

  installIo(functions) {
    functions.set("stdin", () => HostIo.encodeHandle(0));
    functions.set("stdout", () => HostIo.encodeHandle(0));
    functions.set("stderr", () => HostIo.encodeHandle(1));
    functions.set("io_read", (reader, count, whenError, whenSuccess) =>
      this.ioControl(whenError, () => {
        const decoded = this.words.decodeSigned(count, 64);
        if (decoded < 0n || decoded > BigInt(Number.MAX_SAFE_INTEGER)) {
          throw Object.assign(new Error("byte count is outside the host range"), {
            code: "ERR_INVALID_ARG_VALUE",
          });
        }
        return Transfers.withOneArgument(
          whenSuccess,
          this.values.bytes(this.io.reader(reader).read(Number(decoded))),
        );
      }),
    );
    functions.set("io_read_line", (reader, whenError, whenEof, whenLine) =>
      this.ioControl(whenError, () => {
        const input = this.io.reader(reader);
        return input.eof
          ? Transfers.withoutArguments(whenEof)
          : Transfers.withOneArgument(whenLine, this.values.bytes(input.readLine()));
      }),
    );
    functions.set("io_read_all", (reader, whenError, whenSuccess) =>
      this.ioControl(whenError, () =>
        Transfers.withOneArgument(whenSuccess, this.values.bytes(this.io.reader(reader).readAll())),
      ),
    );
    functions.set("io_write_all", (writer, bytes, whenError, whenSuccess) =>
      this.ioUnit(whenError, whenSuccess, () => this.io.write(writer, this.values.getBytes(bytes))),
    );
    functions.set("io_flush", (writer, whenError, whenSuccess) =>
      this.ioUnit(whenError, whenSuccess, () => this.io.flush(writer)),
    );
    functions.set("io_close_reader", (reader, whenError, whenSuccess) =>
      this.ioUnit(whenError, whenSuccess, () => this.io.closeReader(reader)),
    );
    functions.set("io_close_writer", (writer, whenError, whenSuccess) =>
      this.ioUnit(whenError, whenSuccess, () => this.io.closeWriter(writer)),
    );
    functions.set("fs_open_reader", (path, whenError, whenSuccess) =>
      this.ioControl(whenError, () =>
        Transfers.withOneArgument(whenSuccess, this.io.openReader(this.values.getString(path))),
      ),
    );
    functions.set("fs_create_writer", (path, whenError, whenSuccess) =>
      this.ioControl(whenError, () =>
        Transfers.withOneArgument(
          whenSuccess,
          this.io.openWriter(this.values.getString(path), false),
        ),
      ),
    );
    functions.set("fs_append_writer", (path, whenError, whenSuccess) =>
      this.ioControl(whenError, () =>
        Transfers.withOneArgument(
          whenSuccess,
          this.io.openWriter(this.values.getString(path), true),
        ),
      ),
    );
    functions.set("read_line", (continuation) =>
      Transfers.withOneArgument(continuation, this.values.string(this.readLegacyLine())),
    );
    functions.set("read_line_as_int_branch", (whenInvalid, whenValid, spare) => {
      const line = this.readLegacyLine();
      if (!/^[+-]?[0-9]+$/.test(line)) {
        return Transfers.withoutArguments(whenInvalid);
      }
      const parsed = BigInt(line);
      return parsed < -(1n << 63n) || parsed > (1n << 63n) - 1n
        ? Transfers.withoutArguments(whenInvalid)
        : Transfers.withOneArgument(whenValid, this.words.encodeSigned(parsed, 64, spare));
    });
    functions.set("read_till_eof", (continuation) =>
      Transfers.withOneArgument(
        continuation,
        this.values.string(this.utf8Decoder.decode(this.input.readAll())),
      ),
    );
    functions.set("write_str", (string, continuation) => {
      process.stdout.write(this.values.getString(string));
      return Transfers.withoutArguments(continuation);
    });
    functions.set("write_int", (integer, continuation) => {
      process.stdout.write(this.words.decodeSigned(integer, 64).toString());
      return Transfers.withoutArguments(continuation);
    });
    functions.set("write_line", (string, continuation) => {
      process.stdout.write(`${this.values.getString(string)}\n`);
      return Transfers.withoutArguments(continuation);
    });
  }

  readLegacyLine() {
    return this.utf8Decoder.decode(this.input.readLine());
  }

  ioUnit(whenError, whenSuccess, operation) {
    return this.ioControl(whenError, () => {
      operation();
      return Transfers.withoutArguments(whenSuccess);
    });
  }

  ioControl(whenError, operation) {
    try {
      return operation();
    } catch (error) {
      return Transfers.withTwoArguments(
        whenError,
        RuntimeWords.immediateSigned(BigInt(this.ioErrorKind(error))),
        this.values.string(error instanceof Error ? error.message : String(error)),
      );
    }
  }

  ioErrorKind(error) {
    switch (error?.code) {
      case "ENOENT":
        return 0;
      case "EACCES":
      case "EPERM":
        return 1;
      case "EEXIST":
        return 2;
      case "EINVAL":
      case "ERR_INVALID_ARG_VALUE":
        return 3;
      case "EILSEQ":
        return 4;
      case "EPIPE":
        return 5;
      case "ZYDECO_CLOSED":
        return 6;
      default:
        return 7;
    }
  }

  installProcess(functions) {
    functions.set("arg_fold", (whenEmpty, whenItem) => {
      if (this.arguments.length === 0) {
        return Transfers.withoutArguments(whenEmpty);
      }
      if (this.arguments.length === 1) {
        return Transfers.withTwoArguments(
          whenItem,
          this.values.string(this.arguments[0]),
          whenEmpty,
        );
      }
      throw new Error(
        "zydeco.arg_fold cannot represent a host-owned tail for two or more process arguments",
      );
    });
    functions.set("random_int", (continuation, spare) =>
      Transfers.withOneArgument(continuation, this.words.encodeSigned(0n, 64, spare)),
    );
    functions.set("exit", (code) => {
      const status = Number(BigInt.asIntN(32, this.words.decodeSigned(code, 64)));
      throw new ExitSignal(status);
    });
  }
}

class WasmProgram {
  static async run(modulePath, arguments_) {
    const stdin = fs.readFileSync(0);
    const host = new ZydecoHost(arguments_, stdin);
    const module = await WebAssembly.compile(fs.readFileSync(modulePath));
    host.instance = await WebAssembly.instantiate(module, host.imports());
    try {
      host.instance.exports.entry();
      throw new Error("Zydeco WASM entry returned without calling process/exit");
    } catch (error) {
      if (error instanceof ExitSignal) {
        return error.code;
      }
      throw error;
    }
  }
}

const [modulePath, ...arguments_] = process.argv.slice(2);
if (modulePath === undefined) {
  throw new Error("usage: node wasm-host.mjs MODULE.wasm [ARGUMENT ...]");
}

try {
  process.exitCode = await WasmProgram.run(modulePath, arguments_);
} catch (error) {
  console.error(error instanceof Error ? error.stack : error);
  process.exitCode = 255;
}
