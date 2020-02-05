procedure OndrawSeries(Sender: TObject; ValueSeries: TChartSeries; Index: integer; aRect : TRect);

procedure TMyForm.FormCreate(Sender: TObject);
begin
  myChart.Legend.Symbol.OnDraw := OndrawSeries;
end;

procedure TMyForm.OndrawSeries(Sender: TObject; Series: TChartSeries; Index: integer; aRect: TRect);
begin
  myChart.Canvas.Brush.Color := Series.Color;
  myChart.Canvas.FillRect(aRect);
end;