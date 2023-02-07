program Main;
Var x : integer;
procedure Alpha(a : integer; b : integer);


   procedure Beta(a: integer; b : integer);
   begin
      x := a * 10 + b * 2;
   end;

begin
   x := (a + b ) * 2;

   Beta(5, 10);      { procedure call }
end;

begin { Main }

   Alpha(3 + 5, 7);  { procedure call }

end.  { Main }
