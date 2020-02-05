## Sort a list with a custom object
Sorting a list in Delphi with a custom object is done using a `TComparison<T>`, where `T` is you object type.

You can create an instance of the `TComparison<T>` and define the properties used for sorting.

```pascal
Comparison :=
    function(const Left, Right: TMyValue): Integer
    begin
      Result := TComparer<TDateTime>.Default.Compare(Left.TimeStamp, Right.TimeStamp );
    end;
```

Next, create the list using the `TComparison<T>` function 

```pascal
  myValues := TList<TMyValue>.Create(TComparer<TMyValue>.Construct(Comparison));
```

use the `sort()` method of the list to sort.

You can also use this to search the list using the `BinarySearch(T, out index)` method of the list, this will return `True` is the item is found and fill the `index` variable with the index of the item found.