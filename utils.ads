package Utils with
   SPARK_Mode
is
    function Invalid_Pin (S : String) return Boolean is
       (S'Length /= 4 or
        not (for all I in S'Range => S (I) >= '0' and S (I) <= '9'));
end Utils;
