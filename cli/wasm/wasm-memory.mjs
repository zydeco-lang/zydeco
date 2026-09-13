// Checked allocation model for the WebAssembly host. Data addresses are
// virtual host addresses; this host does not export borrowed C pointers.
export class MemoryFault extends Error {
  constructor(code) {
    super(`memory fault ${code}`);
    this.code = code;
  }
}

export class CheckedMemory {
  constructor() {
    this.nextBase = 65536n;
  }

  allocate(length, alignment, initialized) {
    const base = (this.nextBase + alignment - 1n) & -alignment;
    const end = base + length;
    if (end >= 0x1_0000_0000_0000_0000n) throw new RangeError("address space exhausted");
    const buffer = {
      bytes: new Uint8Array(Number(length)),
      initialized: new Uint8Array(Number(length)).fill(initialized ? 1 : 0),
      alignment,
      base,
      pointers: new Map(),
      frozen: false,
    };
    this.nextBase = end + 1n;
    return buffer;
  }

  static grant(buffer, start, length, permission) {
    if (buffer.bytes === null || buffer.frozen) throw new MemoryFault(0n);
    const end = start + length;
    if (start < 0n || length < 0n || end > BigInt(buffer.bytes.length)) throw new MemoryFault(1n);
    if (![1n, 2n, 3n].includes(permission)) throw new MemoryFault(6n);
    return { buffer, start, end, permission, live: true, frozen: false };
  }

  static freeze(buffer) {
    if (buffer.bytes === null || buffer.frozen) throw new MemoryFault(0n);
    if (!buffer.initialized.every((value) => value !== 0)) throw new MemoryFault(5n);
    buffer.frozen = true;
    return { buffer, start: 0n, end: BigInt(buffer.bytes.length), permission: 1n, live: true, frozen: true };
  }

  static immutableLength(access) {
    CheckedMemory.live(access);
    if (!access.frozen) throw new MemoryFault(2n);
    return access.end - access.start;
  }

  import(bytes) {
    const buffer = this.allocate(BigInt(bytes.length), 1n, true);
    buffer.bytes.set(bytes);
    return CheckedMemory.freeze(buffer);
  }

  static live(access) {
    if (!access.live || access.buffer.bytes === null || access.frozen !== access.buffer.frozen) throw new MemoryFault(0n);
  }

  static base(access) {
    CheckedMemory.live(access);
    return { buffer: access.buffer, offset: access.start };
  }

  static revoke(access) {
    if (access.frozen) throw new MemoryFault(2n);
    if (!access.live) throw new MemoryFault(0n);
    access.live = false;
  }

  static granted(access, address) {
    CheckedMemory.live(access);
    if (address.buffer !== access.buffer || address.offset < access.start || address.offset > access.end) {
      throw new MemoryFault(1n);
    }
  }

  static offset(access, address, displacement) {
    CheckedMemory.granted(access, address);
    const offset = address.offset + displacement;
    if (offset < 0n || offset > 0xffff_ffff_ffff_ffffn) throw new MemoryFault(3n);
    if (offset < access.start || offset > access.end) throw new MemoryFault(1n);
    return { buffer: address.buffer, offset };
  }

  static check(access, address, size, alignment, write = false) {
    CheckedMemory.granted(access, address);
    if ((write && access.permission === 1n) || (!write && access.permission === 2n)) throw new MemoryFault(2n);
    if (size < 0n || address.offset + size > access.end) throw new MemoryFault(1n);
    if (alignment <= 0n || (alignment & (alignment - 1n)) !== 0n || (address.buffer.base + address.offset) % alignment !== 0n) {
      throw new MemoryFault(4n);
    }
  }

  static initialized(buffer, start, end) {
    return buffer.initialized.subarray(Number(start), Number(end)).every((value) => value !== 0);
  }

  static invalidate(buffer, start, end) {
    if (start === end) return;
    for (const offset of buffer.pointers.keys()) {
      if (offset + 8n > start && offset < end) buffer.pointers.delete(offset);
    }
    buffer.initialized.fill(1, Number(start), Number(end));
  }

  static load(access, address, size) {
    CheckedMemory.check(access, address, size, size);
    const start = address.offset;
    if (!CheckedMemory.initialized(address.buffer, start, start + size)) throw new MemoryFault(5n);
    return new DataView(address.buffer.bytes.buffer, Number(start), Number(size));
  }

  static readMemory(access, address, size) {
    CheckedMemory.check(access, address, size, 1n);
    if (!CheckedMemory.initialized(address.buffer, address.offset, address.offset + size)) throw new MemoryFault(5n);
    return address.buffer.bytes.subarray(Number(address.offset), Number(address.offset + size));
  }

  static writeMemory(access, address, bytes) {
    const size = BigInt(bytes.length);
    CheckedMemory.check(access, address, size, 1n, true);
    CheckedMemory.invalidate(address.buffer, address.offset, address.offset + size);
    address.buffer.bytes.set(bytes, Number(address.offset));
  }

  static loadI64(access, address) {
    return CheckedMemory.load(access, address, 8n).getBigInt64(0, true);
  }

  static loadU8(access, address) {
    return CheckedMemory.load(access, address, 1n).getUint8(0);
  }

  static loadAddress(access, address) {
    CheckedMemory.load(access, address, 8n);
    const target = address.buffer.pointers.get(address.offset);
    if (target === undefined) throw new MemoryFault(6n);
    return target;
  }

  static store(access, address, size) {
    CheckedMemory.check(access, address, size, size, true);
    CheckedMemory.invalidate(address.buffer, address.offset, address.offset + size);
    return new DataView(address.buffer.bytes.buffer, Number(address.offset), Number(size));
  }

  static storeI64(access, address, value) {
    CheckedMemory.store(access, address, 8n).setBigInt64(0, value, true);
  }

  static storeU8(access, address, value) {
    CheckedMemory.store(access, address, 1n).setUint8(0, value);
  }

  static storeAddress(access, address, value) {
    CheckedMemory.check(access, address, 8n, 8n, true);
    if (value.buffer.bytes === null) throw new MemoryFault(0n);
    const pointer = value.buffer.base + value.offset;
    CheckedMemory.store(access, address, 8n).setBigUint64(0, pointer, true);
    address.buffer.pointers.set(address.offset, value);
  }
}
