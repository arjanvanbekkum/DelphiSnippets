function TForm1.HexStrToStr(const HexStr: string): string;
var
  tmp: AnsiString;
begin
  Assert(not Odd(Length(HexStr)), 'HexToStr input length must be an even number');
  SetLength(tmp, Length(HexStr) div 2);
  HexToBin(PWideChar(HexStr), @tmp[1], Length(tmp));
  result := tmp;
end;
