export interface SourceEdit {
  rangeOffset: number;
  rangeLength: number;
  text: string;
}

/** A pin follows one source occurrence. Overlapping edits invalidate its identity. */
export class DocumentationAnchor {
  constructor(readonly start: number, readonly end: number, readonly cursor = start) {}

  after(edits: readonly SourceEdit[]): DocumentationAnchor | undefined {
    let shift = 0;
    for (const edit of edits) {
      const end = edit.rangeOffset + edit.rangeLength;
      if (end <= this.start && edit.rangeOffset < this.start) {
        shift += edit.text.length - edit.rangeLength;
      } else if (edit.rangeOffset < this.end || edit.rangeOffset === this.start) {
        return undefined;
      }
    }
    return new DocumentationAnchor(this.start + shift, this.end + shift, this.cursor + shift);
  }
}
