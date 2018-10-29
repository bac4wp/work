'Run the following VBA code in Excel to create your own function called CONCATENATEMULTIPLE:

Function CONCATENATEMULTIPLE(Ref As Range, Separator As String) As String
Dim Cell As Range
Dim Result As String
For Each Cell In Ref
 Result = Result & Cell.Value & Separator
Next Cell
CONCATENATEMULTIPLE = Left(Result, Len(Result) - 1)
End Function
