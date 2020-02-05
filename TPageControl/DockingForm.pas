
// Add these lines to the form which need to be docked
DragKind := dkDock

procedure TMyBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TMyBaseForm.FormStartDock(Sender: TObject;  var DragObject: TDragDockObject);
begin
  DragObject:= TTransparentDragDockObject.Create(Self);
end;


// Add this to the form with the pagecontrol
PageControl.DockSite := True;

// Change some properties if needed
procedure TMainForm.pgcntrlMainDockDrop(Sender: TObject; Source: TDragDockObject;  X, Y: Integer);
begin
  inherited;
  SetLength(FCloseButtonsRect, pgcntrlMain.PageCount);
  (Source.Control as TMyBaseForm).pnlTop.Visible := False;
  (Source.Control as TMyBaseForm).tbshtMain.TabVisible := False;
  (Source.Control as TMyBaseForm).pgcntrlRibbon.ActivePageIndex := 0;
  (Source.Control as TMyBaseForm).pgcntrlRibbon.Height := 76;
end;

procedure TMainForm.pgcntrlMainDockOver(Sender: TObject; Source: TDragDockObject;  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  Accept := Source.Control is TMyBaseForm;
end;

procedure TMainForm.pgcntrlMainGetSiteInfo(Sender: TObject; DockClient: TControl;  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited;
  CanDock := DockClient is TMyBaseForm;
end;

// Change some properties if needed
procedure TMainForm.pgcntrlMainUnDock(Sender: TObject; Client: TControl;  NewTarget: TWinControl; var Allow: Boolean);
begin
  inherited;
  SetLength(FCloseButtonsRect, pgcntrlMain.PageCount);
  (Client as TMyBaseForm).pnlTop.Visible := True;
  (Client as TMyBaseForm).tbshtMain.TabVisible := True;
  (Client as TMyBaseForm).pgcntrlRibbon.ActivePageIndex := 0;
  (Client as TMyBaseForm).pgcntrlRibbon.Height := 96;
end;

// Transparent docking Windows
type
  TTransparentForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TTransparentDragDockObject = class(TDragDockObjectEx)
  protected
    function GetEraseWhenMoving: Boolean; override;
    procedure DrawDragDockImage; override;
    procedure EraseDragDockImage; override;
  public
    constructor Create(AControl: TControl); override;
  end;

var
  TransparentForm: TTransparentForm;
  AlphaBlendValue: Integer = 100;

{ TTransparentForm }

procedure TTransparentForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle:= Params.ExStyle or WS_EX_TRANSPARENT;
end;

{ TTransparentDragDockObject }

constructor TTransparentDragDockObject.Create(AControl: TControl);
begin
  inherited;
  if TransparentForm = nil then
  begin
    TransparentForm:= TTransparentForm.CreateNew(Application);
    TransparentForm.AlphaBlend:= True;
    TransparentForm.AlphaBlendValue:= AlphaBlendValue;
    TransparentForm.BorderStyle:= bsNone;
    TransparentForm.Color:= clHighlight;
    TransparentForm.FormStyle:= fsStayOnTop;
  end;
end;

procedure TTransparentDragDockObject.EraseDragDockImage;
begin
  TransparentForm.Hide;
end;

procedure TTransparentDragDockObject.DrawDragDockImage;
begin
  if TransparentForm <> nil then
  begin
    TransparentForm.BoundsRect:= DockRect;
    if not TransparentForm.Visible then
      TransparentForm.Show;
  end;
end;

function TTransparentDragDockObject.GetEraseWhenMoving: Boolean;
begin
  Result:= False;
end;
