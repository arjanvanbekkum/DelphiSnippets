type
 TMyValue = class
   Value : Integer;
   TimeStamp : TDateTime;
 end;

type
  TMyCoolClass = class
    myValues : TList<TMyValue>;
    constructor Create();
  end;

implementation

constructor TMyCoolClass.Create();
var
  Comparison : TComparison<TMyValue>;
begin
  Comparison :=
    function(const Left, Right: TMyValue): Integer
    begin
      Result := TComparer<TDateTime>.Default.Compare(Left.TimeStamp, Right.TimeStamp );
    end;
  // create the list including the sort function
  myValues := TList<TMyValue>.Create(TComparer<TMyValue>.Construct(Comparison));
end;

function mySortFunction();
var 
  coolClass : TMyCoolClass;
  value1, value2, searchValue : TMyValue;
  iFoundIndex : integer;
begin
  coolClass := TMyCoolClass.Create();
  value1 := TMyValue.Create();
  value1.TimeStamp = AddDays(Date, -2);
  value1.Value := 2;
  
  value2 := TMyValue.Create();
  value2.TimeStamp = AddDays(Date, -1);
  value2.Value := 2;

  coolClass.myValues.add(value1);
  coolClass.myValues.add(value2);

  // this uses the Comparison to sort the list
  coolClass.myValues.Sort();

  // use binary search tolocate a value in the list matching the Timestamp, returns True if an item is found
  searchValue := TMyValue.Create();
  searchValue.TimeStamp := AddDays(Date, -2);
  if coolClass.myValues.BinarySearch(searchValue, iFoundIndex ) then
  begin
    // iFoundIndex contains the index of the found item
  end
  else
  begin
    // did not find anything
  end;
end;