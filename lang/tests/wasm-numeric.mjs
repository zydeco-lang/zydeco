/** The decimal spelling shared by the interpreter and native numeric builtins. */
export class FloatText {
  static render(value, width) {
    if (Number.isNaN(value)) return "NaN";
    if (value === Infinity) return "inf";
    if (value === -Infinity) return "-inf";
    if (Object.is(value, -0)) return "-0";
    if (value === 0) return "0";

    const magnitude = Math.abs(value);
    const round = width === 32 ? Math.fround : (number) => number;
    const maximumDigits = width === 32 ? 9 : 17;
    let shortest;
    for (let digits = 1; digits <= maximumDigits; digits += 1) {
      shortest = magnitude.toPrecision(digits);
      if (round(Number(shortest)) === magnitude) break;
    }

    // Rust's Display uses the shortest round-tripping decimal without exponent notation.
    const [mantissa, exponent = "0"] = shortest.split("e");
    const [integer, fraction = ""] = mantissa.split(".");
    const digits = (integer + fraction).replace(/0+$/, "");
    const point = integer.length + Number(exponent);
    let decimal;
    if (point <= 0) {
      decimal = `0.${"0".repeat(-point)}${digits}`;
    } else if (point >= digits.length) {
      decimal = digits + "0".repeat(point - digits.length);
    } else {
      decimal = `${digits.slice(0, point)}.${digits.slice(point)}`;
    }
    return value < 0 ? `-${decimal}` : decimal;
  }
}
