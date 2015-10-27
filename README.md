Are you sick and tired of accidentally applying `length` to a tuple
and getting back `1` instead of a compilation error?

At last, there is a solution -- `safeLength`. For zero easy payments
of 0 dollars and 00 cents, you can get the type safety you long for!

Before if you accidentally wrote:

    main :: IO ()
    main = print $ length ('a', 'b')

you would get:

    *Main> main
    1

But with the new and improved `safeLength` you can rest easy. If you write:

    main :: IO ()
    main = print $ safeLength (Proxy :: Proxy [Char]) ('a', 'b')

You now get the glorious error:

    *Main> :load example/Tuple.hs
    [1 of 2] Compiling Data.Length      ( src/Data/Length.hs, interpreted )
    [2 of 2] Compiling Main             ( example/Tuple.hs, interpreted )
    
    example/Tuple.hs:7:51:
        Couldn't match type ‘(,) Char’ with ‘[]’
        Expected type: [Char]
          Actual type: (Char, Char)
        In the second argument of ‘safeLength’, namely ‘('a', 'b')’
        In the second argument of ‘($)’, namely
          ‘safeLength (Proxy :: Proxy [Char]) ('a', 'b')’
        In the expression:
          print $ safeLength (Proxy :: Proxy [Char]) ('a', 'b')
    Failed, modules loaded: Data.Length.

But wait! There's more! What if you had nested lists and you
accidentally take the length of the wrong list. For example, what if
you want the length of the outer list, but you accidentally write:

    import GHC.OldList as OldList
    
    main :: IO ()
    main = print $ OldList.length (head [['a','b','c']])

Oh the horror! It compiles and we get:

    *Main> main
    3

But with `safeLength` all is well! If we accidentally write:

    main :: IO ()
    main = print $ safeLength (Proxy :: Proxy [[Char]]) (head [['a', 'b', 'c']])

We get not 1, not 2, but *3* type errors

    *Main> :load example/NestedList.hs
    [1 of 2] Compiling Data.Length      ( src/Data/Length.hs, interpreted )
    [2 of 2] Compiling Main             ( example/NestedList.hs, interpreted )
    
    example/NestedList.hs:7:61:
        Couldn't match expected type ‘[Char]’ with actual type ‘Char’
        In the expression: 'a'
        In the expression: ['a', 'b', 'c']
        In the first argument of ‘head’, namely ‘[['a', 'b', 'c']]’
    
    example/NestedList.hs:7:66:
        Couldn't match expected type ‘[Char]’ with actual type ‘Char’
        In the expression: 'b'
        In the expression: ['a', 'b', 'c']
        In the first argument of ‘head’, namely ‘[['a', 'b', 'c']]’
    
    example/NestedList.hs:7:71:
        Couldn't match expected type ‘[Char]’ with actual type ‘Char’
        In the expression: 'c'
        In the expression: ['a', 'b', 'c']
        In the first argument of ‘head’, namely ‘[['a', 'b', 'c']]’
    Failed, modules loaded: Data.Length.

