// Unmanaged virtual addresses for the Wasm host. The host emulates raw memory;
// native pointer transport remains unavailable on this execution profile.
export class MemoryFault extends Error {
  constructor(code) { super(`memory fault ${code}`); this.code = code; }
}
export class ManualMemory {
  constructor() { this.nextBase = 0x1_0000_0000n; this.allocations = new Map(); }
  layout(length, alignment) {
    if (length < 0n || alignment <= 0n || (alignment & (alignment - 1n)) !== 0n ||
        ((length + alignment - 1n) & -alignment) > 0x7fff_ffff_ffff_ffffn)
      throw new MemoryFault(0n);
  }
  allocate(length, alignment) {
    this.layout(length, alignment);
    if (length > BigInt(Number.MAX_SAFE_INTEGER)) throw new MemoryFault(1n);
    const base = (this.nextBase + alignment - 1n) & -alignment;
    const end = base + length;
    if (end >= 0x1_0000_0000_0000_0000n) throw new MemoryFault(1n);
    let bytes;
    try { bytes = new Uint8Array(Number(length)); }
    catch (error) { if (error instanceof RangeError) throw new MemoryFault(1n); throw error; }
    this.allocations.set(base, { bytes, length, alignment, retained: false });
    this.nextBase = end + 1n;
    return base;
  }
  allocation(base, length, alignment) {
    this.layout(length, alignment);
    const block = this.allocations.get(base);
    if (!block || block.length !== length || block.alignment !== alignment || block.retained)
      throw new RangeError("invalid raw allocation ownership");
    return block;
  }
  free(base, length, alignment) {
    this.allocation(base, length, alignment);
    this.allocations.delete(base);
  }
  retain(base, length, alignment) { this.allocation(base, length, alignment).retained = true; }
  import(bytes) {
    const base = this.allocate(BigInt(bytes.length), 1n);
    this.write(base, bytes);
    this.retain(base, BigInt(bytes.length), 1n);
    return base;
  }
  bytes(address, length) {
    if (length === 0n) return new Uint8Array(0);
    if (length < 0n) throw new RangeError("invalid raw memory extent");
    for (const [base, block] of this.allocations) {
      if (base <= address && address + length <= base + block.length)
        return block.bytes.subarray(Number(address - base), Number(address - base + length));
    }
    throw new RangeError("invalid raw memory range");
  }
  write(address, bytes) { this.bytes(address, BigInt(bytes.length)).set(bytes); }
  copy(source, destination, length) { this.bytes(destination, length).set(this.bytes(source, length)); }
  fill(address, length, octet) { this.bytes(address, length).fill(octet); }
  loadAddress(address) {
    const bytes = this.bytes(address, 8n);
    return new DataView(bytes.buffer, bytes.byteOffset, 8).getBigUint64(0, true);
  }
  storeAddress(address, value) {
    const bytes = this.bytes(address, 8n);
    new DataView(bytes.buffer, bytes.byteOffset, 8).setBigUint64(0, value, true);
  }
}
