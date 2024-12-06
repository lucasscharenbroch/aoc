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
        lens[x] = line.len;
        x += 1;
    }

    // In a higher-level language, I would place the rows, cols, and diagonals
    // each in their own string arrays, then count forwards and backwards.
    // (hoped to do this in zig, but it turns out to be more low-level then I thought)
    // We'll use the buggier alternative here.

    const n = lens[0]; // assume all same length
    var res: usize = 0;

    const d = [_]i32{-1, 0, 1}; // can't do this with ranges?

    for (d) |di| {
        for (d) |dj| {
            const lo_i: usize = switch(di) { -1 => 3, else => 0};
            const hi_i: usize = switch(di) { 1 => n - 3, else => n};
            const lo_j: usize = switch(dj) { -1 => 3, else => 0};
            const hi_j: usize = switch(dj) { 1 => n - 3, else => n};

            for (lo_i..hi_i) |i_usize| {
                for (lo_j..hi_j) |j_usize| {
                    const i: i32 = @intCast(i_usize);
                    const j: i32 = @intCast(j_usize);

                    res += if (
                        xmas(
                            buf[@intCast(i + 0 * di)][@intCast(j + 0 * dj)],
                            buf[@intCast(i + 1 * di)][@intCast(j + 1 * dj)],
                            buf[@intCast(i + 2 * di)][@intCast(j + 2 * dj)],
                            buf[@intCast(i + 3 * di)][@intCast(j + 3 * dj)]
                        )
                    ) 1 else 0;
                }
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

