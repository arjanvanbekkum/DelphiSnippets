function TForm1.HexToDec(aValue: String): LongInt;
var
l: LongInt;
b: Byte;
begin
  Result := 0;
  if Length(aValue) <> 0 then
  begin
    l := 1;
    b := Length(aValue) + 1;
     repeat
     dec(b);
     if aValue[b] <= '9' then Result := Result + (Byte(aValue[b]) - 48) * l
     else Result := Result + (Byte(aValue[b]) - 55) * l;
     l := l * 16;
    until b = 1;
   end;
end;
