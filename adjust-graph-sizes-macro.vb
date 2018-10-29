'Auto-adjust the Y-axis of charts in a dashboard


Sub AdjustVerticalAxis()
'PURPOSE: Adjust Y-Axis according to Min/Max of Chart Data

Dim cht As ChartObject
Dim srs As Series
Dim FirstTime  As Boolean
Dim MaxNumber As Double
Dim MinNumber As Double
Dim MaxChartNumber As Double
Dim MinChartNumber As Double
Dim Padding As Double

'Input Padding on Top of Min/Max Numbers (Percentage)
  Padding = 0.1  'Number between 0-1

'Optimize Code
  Application.ScreenUpdating = False
  
'Loop Through Each Chart On ActiveSheet
  For Each cht In ActiveSheet.ChartObjects
    
    'First Time Looking at This Chart?
      FirstTime = True
      
    'Determine Chart's Overall Max/Min From Connected Data Source
      For Each srs In cht.Chart.SeriesCollection
        'Determine Maximum value in Series
          MaxNumber = Application.WorksheetFunction.Max(srs.Values)
        
        'Store value if currently the overall Maximum Value
          If FirstTime = True Then
            MaxChartNumber = MaxNumber
          ElseIf MaxNumber > MaxChartNumber Then
            MaxChartNumber = MaxNumber
          End If
        
        'Determine Minimum value in Series (exclude zeroes)
          MinNumber = Application.WorksheetFunction.Min(srs.Values)
          
        'Store value if currently the overall Minimum Value
          If FirstTime = True Then
            MinChartNumber = MinNumber
          ElseIf MinNumber < MinChartNumber Or MinChartNumber = 0 Then
            MinChartNumber = MinNumber
          End If
        
        'First Time Looking at This Chart?
          FirstTime = False
      Next srs
      
    'Rescale Y-Axis
      cht.Chart.Axes(xlValue).MinimumScale = MinChartNumber * (1 - Padding)
      cht.Chart.Axes(xlValue).MaximumScale = MaxChartNumber * (1 + Padding)
  
  Next cht

'Optimize Code
  Application.ScreenUpdating = True

End Sub


'How to get the above macro to run whenever a certain cell changes


Private Sub Worksheet_Change(ByVal Target As Range)
'PURPOSE: Run AdjustVerticalAxis macro when Cell C2 is changed

If Target = Range("C2") Then Call AdjustVerticalAxis

End Sub
