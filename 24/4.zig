const std = @import("std");

// painfully low-level. painfully. reminds me of assembly.
// partially my fault, since I don't know what to reach for.

fn xmas(x: u8, m: u8, a: u8, s: u8) bool {
    return
        x == 'X' and
        m == 'M' and
        a == 'A' and
        s == 'S';
}

fn mas(m: u8, a: u8, s: u8) bool {
    return
        a == 'A' and
        (
            (m == 'M' and s == 'S') or
            (m == 'S' and s == 'M')
        );
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("4.input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    // no idea if this is idiomatic
    var x: usize = 0;
    var buf: [1024][1024]u8 = undefined;
    var lens: [1024]usize = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf[x], '\n')) |line| {
        std.debug.print("{s}\n", .{line});
        lens[x] = line.len;
        x += 1;
    }

    // In a higher-level language, I would place the rows, cols, and diagonals
    // each in their own string arrays, then count forwards and backwards.
    // We'll use the buggier alternative here.

    const n = lens[0]; // assume all same length
    var res: usize = 0;

    for (0..n) |i| {
        for (0..n) |j| {
            // left
            if (j >= 3) {
                res += if (xmas(buf[i][j], buf[i][j - 1], buf[i][j - 2], buf[i][j - 3])) 1 else 0;

                // up-left
                if (i >= 3) {
                    res += if (xmas(buf[i][j], buf[i - 1][j - 1], buf[i - 2][j - 2], buf[i - 3][j - 3])) 1 else 0;
                }

                // down-left
                if (i + 3 < n) {
                    res += if (xmas(buf[i][j], buf[i + 1][j - 1], buf[i + 2][j - 2], buf[i + 3][j - 3])) 1 else 0;
                }
            }

            // right
            if (j + 3 < n) {
                res += if (xmas(buf[i][j], buf[i][j + 1], buf[i][j + 2], buf[i][j + 3])) 1 else 0;

                // up-right
                if (i >= 3) {
                    res += if (xmas(buf[i][j], buf[i - 1][j + 1], buf[i - 2][j + 2], buf[i - 3][j + 3])) 1 else 0;
                }

                // down-right
                if (i + 3 < n) {
                    res += if (xmas(buf[i][j], buf[i + 1][j + 1], buf[i + 2][j + 2], buf[i + 3][j + 3])) 1 else 0;
                }
            }

            // up
            if (i >= 3) {
                res += if (xmas(buf[i][j], buf[i - 1][j], buf[i - 2][j], buf[i - 3][j])) 1 else 0;
            }

            // down
            if (i + 3 < n) {
                res += if (xmas(buf[i][j], buf[i + 1][j], buf[i + 2][j], buf[i + 3][j])) 1 else 0;
            }
        }
    }

    // Part 2:
    // Could use stencil in APL... pointers is arguably the best solution here.

    var res2: usize = 0;
    for (0..(n - 2)) |i| {
        for (0..(n - 2)) |j| {
            res2 += if (
                mas(buf[i][j], buf[i + 1][j + 1], buf[i + 2][j + 2]) and
                mas(buf[i + 2][j], buf[i + 1][j + 1], buf [i][j + 2])
            ) 1 else 0;
        }
    }

    std.debug.print("{}\n{}\n", .{res, res2});
}

