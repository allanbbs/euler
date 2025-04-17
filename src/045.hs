triangle n = div (n * (n + 1)) 2

pentagonal n = div (n * (3 * n - 1)) 2

hexagonal n = div (n * (2 * n - 1)) 2

result = head [a |
            t <- [286 .. ],
            let a = triangle t,
            p <- [166 .. ],
            let b = pentagonal p,
            h <- [144 .. ],
            let c = hexagonal h,
            a == b && b == c]

main = print result