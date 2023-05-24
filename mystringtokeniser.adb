
package body MyStringTokeniser with SPARK_Mode is

    procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
        Index : Positive;
        Extent : TokenExtent;
        OutIndex : Integer := Tokens'First;
    begin
        Count := 0;
        if (S'First > S'Last) then
            return;
        end if;
        Index := S'First;
        while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop
            -- This loop invariant ensure that:
            -- None of the tokens has a start index before the start index of the string.
            -- None of the tokens has a non-positive length.
            -- None of the tokens has a length that makes it go beyond the end of the string.
            pragma Loop_Invariant
              (for all J in Tokens'First..OutIndex-1 =>
                 (Tokens(J).Start >= S'First and
                      Tokens(J).Length > 0) and then
               Tokens(J).Length-1 <= S'Last - Tokens(J).Start);

            -- This loop invariant ensure that:
            -- Count always represent the number of token that has been added to Tokens.
            -- OutIndex is always the next token to be added to Tokens.
            --
            -- This loop invariant is neccessary because:
            -- It ensure that OutIndex >= Tokens'First.
            --   This allows the statement `Tokens(OutIndex) := Extent;` to pass array index check
            --   It also allows OutIndex-1 to pass overflow check in the previous loop invariant.
            -- It allow the post condition of the function to pass. By combining this loop invariant
            --   with the previous one, we can yield an identical condition as the second part of the
            --   function's post condition.
            pragma Loop_Invariant (OutIndex = Tokens'First + Count);

            -- look for start of next token
            while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
                Index := Index + 1;
            end loop;
            if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
                -- found a token
                Extent.Start := Index;
                Extent.Length := 0;

                -- look for end of this token
                -- Ensure Index + Extent.length don't overflow.
                while Positive'Last - Extent.Length >= Index
                   and then
                   -- Ensure Index + Extent.length isn't out of bound.
                   (Index + Extent.Length >= S'First and
                    Index + Extent.Length <= S'Last)
                   and then not Is_Whitespace (S (Index + Extent.Length))
                loop
                    Extent.Length := Extent.Length + 1;
                end loop;

                Tokens(OutIndex) := Extent;
                Count := Count + 1;

                -- check for last possible token, avoids overflow when incrementing OutIndex
                if (OutIndex = Tokens'Last) then
                    return;
                else
                    OutIndex := OutIndex + 1;
                end if;

                -- check for end of string, avoids overflow when incrementing Index
                if S'Last - Extent.Length < Index then
                    return;
                else
                    Index := Index + Extent.Length;
                end if;
            end if;
        end loop;
    end Tokenise;

end MyStringTokeniser;
