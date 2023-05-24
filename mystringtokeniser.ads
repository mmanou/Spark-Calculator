with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

    type TokenExtent is record
        Start : Positive;
        Length : Natural;
    end record;

    type TokenArray is array(Positive range <>) of TokenExtent;

    function Is_Whitespace(Ch : Character) return Boolean is
      (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
         Ch = Ada.Characters.Latin_1.HT);


    -- This procecure takes an immutable String S,
    --  mutates a TokenArray Tokens, and
    --  returns a Natural number Count
    procedure Tokenise(S: in String; Tokens: in out TokenArray; Count: out Natural) with
      -- Ensure that if the string is not empty, then the position of its first element
      --  is at least as great as the position of the last element.
      --  Ensure that Tokens has a positive length with valid indices.
      Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,

      -- Ensure that our output, Count, has a value of at most the post-mutation length of Tokens
      -- This allows callers to use Count as the right-bound of a range when indexing TokenArray
      --  without fear of out-of-bounds errors.
      Post => Count <= Tokens'Length and
      -- For each element (token) in tokens, we perform several checks to ensure that the bounds
      --  of the token indices are within the bounds of the string indices, with the following
      --  checks.
      -- This allows callers to use token.Start and (token.Start + token.Length - 1) as the start
      --  and end index when obtaining substring of S without fear of out-of-bounds errors.
      (for all Index in Tokens'First..Tokens'First+(Count-1) =>
           -- Ensure that the first index of each token is no less than
           --  the index of the first character of the string.
           -- This allows Tokens(Index).Start to be used at the start index when obtaining substring
           --  of S.
           (Tokens(Index).Start >= S'First and
                  -- Ensure that the token is non-empty.
                  -- This allows Tokens(Index).Start + Tokens(Index).Length - 1 to be used as the
                  --  end index when obtaining substring of S without the prover complaining that
                  --  the end index is smaller than the start index (as required by pre-condition of
                  --  MyString.Substring).
                  Tokens(Index).Length > 0) and then
             -- If both are true, then we ensure that the index of the last element of the token is
             --  no greater than the upper bound of the string. Avoids index out-of-bounds error.
             -- This allows Tokens(Index).Start + Tokens(Index).Length - 1 to be used as the end
             --  index when obtaining substring of S without the prover complaining that then end
             --  index is beyond the length of S (as required by the pre-condition of
             --  MyString.Substring).
             Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);

end MyStringTokeniser;
