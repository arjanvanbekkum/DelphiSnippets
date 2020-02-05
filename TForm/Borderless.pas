Protected
    procedure CreateParams(var Params: TCreateParams); override;

procedure TMyForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_STATICEDGE;
  Params.Style := Params.Style or WS_SIZEBOX;
end;

// add a TPanel to the top of the form, so it stays dragable
procedure TMyForm.pnlTopMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.BeginDrag(True);
end;
