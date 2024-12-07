#!/bin/awk -f
{
    sub(/:$/, "", $1) # (mutate $1, removing the trailing colon)
    # for each bitmask b
    for (b = 0; b < 2 ** (NF - 2); b++) {
        x = $2
        for (i = 3; i <= NF; i++) {
            # hacked >> and & with / and %
            if (int(b / (2 ** (i - 3))) % 2 == 1) {
                x += $i
            } else {
                x *= $i
            }
        }

        if (x == $1) {
            sum += x
            break
        }
    }

    # part 2: do a tritmask instead
    for (t = 0; t < 3 ** (NF - 2); t++) {
        x = $2
        for (i = 3; i <= NF; i++) {
            # Looks like the hack comes in handy. Very nice.
            op = int(t / (3 ** (i - 3))) % 3
            if (op == 0) {
                x += $i
            } else if (op == 1) {
                x *= $i
            } else {
                x = x $i
            }
        }

        if (x == $1) {
            sum2 += x
            break
        }
    }
}
END {
    print sum
    print sum2
}
