{
{-# OPTIONS -w  #-} -- Suppress warnings from alex-generated code
module Text.Highlighting.Illuminate.Diff (lexer) where
}

%wrapper "illuminate"

tokens :-

<0> {
 ^ "---" .*         { tok OldFile `andBegin` u }
 ^ "***" .*         { tok OldFile `andBegin` old }
 ^ [0-9] .*         { tok Plain  `andBegin` n }
 ^ .*               { tok Plain }
}

-- diff with no options
<n> {
 ^ \> .*            { tok NewFile }
 ^ \< .*            { tok OldFile }
 ^ [0-9] .*         { tok Plain   }
 ^ .+               { tok Comment }
}

-- diff -u
<u> {
 ^ \@ .*            { tok Plain   }
 ^ \+ .*            { tok NewFile }
 ^ \- .*            { tok OldFile }
 ^ [^\-\+\@] .*     { tok Comment }
}

-- diff -c, old file
<old> {
 ^ $white .*        { tok Comment }
 ^ "---" .*         { tok NewFile `andBegin` new }
 ^ .+               { tok OldFile }
}

-- diff -c, new file
<new> {
 ^ $white .*        { tok Comment }
 ^ "***" .*         { tok OldFile `andBegin` old }
 ^ .+               { tok NewFile }
}

 \n                 { tok Whitespace }

{
lexer :: Lexer
lexer = Lexer { name = "Diff"
              , aliases = ["diff"]
              , filenames = ["*.diff","*.patch","*.dpatch"]
              , scan = scanner }
}


