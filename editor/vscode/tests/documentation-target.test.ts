import { strict as assert } from "node:assert";
import { test } from "node:test";
import { DocumentationAnchor } from "../src/documentation-target";

test("pins follow edits before a UTF-16 source range and ignore later edits", () => {
  const anchor = new DocumentationAnchor(10, 15);
  const moved = anchor.after([
    { rangeOffset: 20, rangeLength: 2, text: "later" },
    { rangeOffset: 0, rangeLength: 2, text: "😀prefix" },
  ]);
  assert.deepEqual(moved, new DocumentationAnchor(16, 21));
  assert.deepEqual(anchor.after([{ rangeOffset: 15, rangeLength: 0, text: "suffix" }]), anchor);
});

test("pins invalidate when replaced, deleted, or split by an insertion", () => {
  const anchor = new DocumentationAnchor(10, 15);
  for (const edit of [
    { rangeOffset: 10, rangeLength: 5, text: "other" },
    { rangeOffset: 0, rangeLength: 30, text: "" },
    { rangeOffset: 12, rangeLength: 0, text: "new" },
    { rangeOffset: 10, rangeLength: 0, text: "prefix" },
  ]) { assert.equal(anchor.after([edit]), undefined); }
});

test("a projection pin retains the cursor inside its receiver-and-field span", () => {
  const projection = new DocumentationAnchor(10, 23, 18);
  const moved = projection.after([{ rangeOffset: 0, rangeLength: 0, text: "prefix" }]);
  assert.deepEqual(moved, new DocumentationAnchor(16, 29, 24));
});
