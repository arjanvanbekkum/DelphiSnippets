uses
   UxTheme, Themes;

type
  TMainForm = class(TForm)

  private
    FCloseButtonsRect: array of TRect;
    FCloseButtonMouseDownIndex: Integer;
    FCloseButtonShowPushed: Boolean;
    FMouseKey: SmallInt;
    FMousePoint : TPoint;

  end;

implementation


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  //should be done on every change of the page count
  SetLength(FCloseButtonsRect, pgcntrlMain.PageCount);
  FCloseButtonMouseDownIndex := -1;
end;

procedure TfrmMain.pgcntrlMainDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  CloseBtnSize: Integer;
  PageControl: TPageControl;
  TabCaption: TPoint;
  CloseBtnRect, TabRect: TRect;
  CloseBtnDrawState: Cardinal;
  CloseBtnDrawDetails: TThemedElementDetails;
  Flags: TTextFormatFlags;
begin
  PageControl := Control as TPageControl;

    CloseBtnSize := 14;
    TabCaption.Y := Rect.Top + 3;

    if Active then
    begin
      CloseBtnRect.Top := Rect.Top + 4;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 6;
    end
    else
    begin
      CloseBtnRect.Top := Rect.Top + 3;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 3;
    end;

    CloseBtnRect.Bottom := CloseBtnRect.Top + CloseBtnSize;
    CloseBtnRect.Left := CloseBtnRect.Right - CloseBtnSize;
    FCloseButtonsRect[TabIndex] := CloseBtnRect;

    if not UseThemes then
    begin
      PageControl.Canvas.FillRect(Rect);
      PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y, PageControl.Pages[TabIndex].Caption);

      if (FCloseButtonMouseDownIndex = TabIndex) and FCloseButtonShowPushed then
        CloseBtnDrawState := DFCS_CAPTIONCLOSE + DFCS_PUSHED
      else
        CloseBtnDrawState := DFCS_CAPTIONCLOSE;

      DrawFrameControl(PageControl.Canvas.Handle, FCloseButtonsRect[TabIndex], DFC_CAPTION, CloseBtnDrawState);
    end
    else
    begin
      // draw tab
      TabRect := Rect;
      if Active then
        StyleServices.DrawElement(PageControl.Canvas.Handle, StyleServices.GetElementDetails(ttTabItemSelected), TabRect)
      else
        StyleServices.DrawElement(PageControl.Canvas.Handle, StyleServices.GetElementDetails(ttTabItemNormal), TabRect);

      Flags := DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_NOCLIP;
      StyleServices.DrawText(PageControl.Canvas.Handle, StyleServices.GetElementDetails(ttTabItemNormal),  PageControl.Pages[TabIndex].Caption, tabRect, Flags, clBlack);

      // draw close button
      if TabIndex <> 0 then
      begin
        Dec(FCloseButtonsRect[TabIndex].Left);

        if (FCloseButtonMouseDownIndex = TabIndex) and FCloseButtonShowPushed then
          CloseBtnDrawDetails := StyleServices.GetElementDetails(twCloseButtonPushed)
        else
          CloseBtnDrawDetails := StyleServices.GetElementDetails(twCloseButtonNormal);

        StyleServices.DrawElement(PageControl.Canvas.Handle, CloseBtnDrawDetails, FCloseButtonsRect[TabIndex]);
      end;
    end;
end;

procedure TfrmMain.pgcntrlMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FMouseKey := GetASyncKeyState(VK_LBUTTON);
  if Button = mbLeft then
  begin
    for I := 0 to Length(FCloseButtonsRect) - 1 do
    begin
      if PtInRect(FCloseButtonsRect[I], TPoint.Create(X, Y)) then
      begin
        FCloseButtonMouseDownIndex := I;
        FCloseButtonShowPushed := True;
        PageControl.Repaint;
      end;
    end;
  end;
end;


// if you docking of a form you must use this event, if not you can use the MouseUp. 
procedure TfrmMain.pgcntrlMainMouseLeave(Sender: TObject);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FCloseButtonShowPushed := False;
  if (FMouseKey and $8000 <> 0) then // was down
  begin
    if not (GetASyncKeyState(VK_LBUTTON) and $8000 <> 0) then  // not anymore
    begin
      // detect a click based on the coordinates
      if PtInRect( FCloseButtonsRect[FCloseButtonMouseDownIndex], fMousePoint)  then
      begin
        PageControl.Pages[FCloseButtonMouseDownIndex].Free;
        FCloseButtonMouseDownIndex := -1;
        PageControl.Repaint;
      end;
      FMouseKey := GetASyncKeyState(VK_LBUTTON);
    end;
  end;
  PageControl.Repaint;
end;

// Mouse up if you do not use docking 
procedure TFormMain.PageControlCloseButtonMouseUp(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;

  if (Button = mbLeft) and (FCloseButtonMouseDownIndex >= 0) then
  begin
    // detect a click based on the coordinates
    if PtInRect(FCloseButtonsRect[FCloseButtonMouseDownIndex], Point(X, Y)) then
    begin
      FCloseButtonMouseDownIndex := -1;
      PageControl.Repaint;
    end;
  end;
end;

procedure TfrmMain.pgcntrlMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fMousePoint := TPoint.Create(X, Y);
end;

procedure TfrmMain.pgcntrlMainUnDock(Sender: TObject; Client: TControl;  NewTarget: TWinControl; var Allow: Boolean);
begin
  inherited;
  SetLength(FCloseButtonsRect, pgcntrlMain.PageCount);
end;

end.
