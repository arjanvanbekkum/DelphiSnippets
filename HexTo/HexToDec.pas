function TForm1.HexToByte(s : String) : Byte;
const
  cs = '0123456789ABCDEF';
begin
  result := 0;
  if (length(s) = 2) and
     (s[1] in ['0'..'9','A'..'F']) and
     (s[2] in ['0'..'9','A'..'F']) then
    result := ((pos(s[1],cs)-1) *16) + (pos(s[2],cs)-1)
  else raise EConvertError.CreateFmt('%s is not a Hexformatstring',[s]);
end;
