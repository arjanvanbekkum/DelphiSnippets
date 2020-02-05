unit USB;

interface

uses  Classes, Windows, SetupApi, USBCommunication;

type TMyUSBport = class(TUSBPort)
  private
    FstrMyBuffer : AnsiString;
    FstrUSBportAlias : string;
    FstrUSBportDeviceDescription : string;
    procedure USBPortDataReceived(objSender: TObject; const pBuffer; intCount: integer);
  public
    constructor Create(hOwner:TComponent);override;
    destructor Destroy;override;
end;

// USB Communication
function GetUSBportIndexByDeviceDesc(strUSBportAlias, strDeviceDesc : string):integer;
function SendToUSB(strUSBportAlias : string; strData: AnsiString; intTimeoutmsecs:integer; var strErrorSource : string):boolean;
function ReceiveFromUSB(strUSBportAlias : string; intTimeout : integer; var strErrorSource : string):AnsiString;
function OpenUSBport(strUSBportAlias : string):boolean;
function InitUSBport(strUSBportAlias : string; strDeviceDesc, strBaudrate,
                      strDatabits, strParity, strStopbits, strInBufferSize, strOutBufferSize,
                      strFlowControl : string;bolDSRSensetivity : boolean):boolean;

implementation

uses Forms, Registry, Sysutils, Functions;

const DIGCF_PRESENT = 002;
      DIGCF_DEVICEINTERFACE = 016;
      SPDRP_DEVICEDESC   = $00000000;
var
  aUSBports : TList;


function GetUSBportIndexByDeviceDesc(strUSBportAlias, strDeviceDesc:string):integer;
var
  intLoop : word;
  bFound : boolean;
  objUSBport : TMyUSBport;
begin
  Result := -1;
  intLoop := 0;
  bFound := False;

  if aUSBports.Count>0 then
  begin

    while (intLoop < aUSBports.Count) and not (bFound) do
    begin
      objUSBport := aUSBports[intLoop];
      if objUSBport.FstrUSBportAlias = strUSBportAlias then
      begin
        Result := intLoop;
        bFound := true
      end
      else
        inc(intLoop);
    end;

    if (not bFound) then
    begin
      intLoop:=0;
      while (intLoop < aUSBports.Count) and not (bFound) do
      begin
        objUSBport := aUSBports[intLoop];
        if (objUSBport.FstrUSBportDeviceDescription = strDeviceDesc) and
              (objUSBport.FstrUSBportAlias = EmptyStr) then
        begin
          objUSBport.FstrUSBportAlias := strUSBportAlias;
          Result := intLoop;
          bFound := True
        end
        else
          inc(intLoop);
      end;
    end;
  end;
end;

function SendToUSB(strUSBportAlias:string; strData: AnsiString;intTimeoutmsecs:integer;var strErrorSource:string):boolean;
var intUSBportIndx : integer;
    objUSBport : TMyUSBport;
    bolTimeoutOccured : boolean;
    objASync : USBCommunication.PAsync;
    intTimeoutTime : integer;
begin
  strErrorSource := strUSBportAlias;
  Result:=false;
  intUSBportIndx := GetUSBportIndexByDeviceDesc(strUSBportAlias, EmptyStr);
  if intUSBportIndx>=0 then
  begin
    objUSBport:=aUSBports[intUSBportIndx];
    objUSBport.ClearBuffer(true,true);  // clear input and outputbuffers
    USBCommunication.InitAsync(objAsync);
    try
      intTimeoutTime:=GetTickCount+intTimeoutmsecs;
      objUSBport.WriteStrAsync(strData,objASync);
      while (objUSBport.OutputCount>0) and (GetTickCount<=intTimeoutTime) do
        Application.ProcessMessages;
      bolTimeoutOccured:=(GetTickCount>intTimeoutTime);
    finally
      // Do this? objComport.ClearBuffer(true,true);  // clear input and outputbuffers
      USBCommunication.DoneAsync(objAsync);
    end;
    Result := not bolTimeoutOccured;
  end;
end;

//----------------------------------------------------------------------
function ReceiveFromUSB(strUSBportAlias:string;intTimeout:integer;var strErrorSource:string):AnsiString;
var
  intComportIndx : integer;
  objUSBport : TMyUSBport;
  intTimeoutTime : integer;
 begin
  strErrorSource := strUSBportAlias;
  Result:='';
  intComportIndx := GetUSBportIndexByDeviceDesc(strUSBportAlias,EmptyStr);
  if intComportIndx>=0 then
  begin
    objUSBport:=aUSBports[intComportIndx];
    intTimeoutTime:=GetTickCount+intTimeout;
    repeat
      Application.ProcessMessages;
      SleepEx(50,true);
      Application.ProcessMessages;

      Result:=Result+objUSBport.FstrMyBuffer;
      objUSBport.FstrMyBuffer:='';
    until (Result<>'') and (objUSBport.InputCount=0) and
          (objUSBport.FstrMyBuffer='') or (GetTickCount>intTimeoutTime);
  end
  else
    Result:='';
end;


function OpenUSBport(strUSBportAlias: string): boolean;

var intComportIndx : integer;
    objUSBport : TMyUSBport;

begin
  intComportIndx := GetUSBportIndexByDeviceDesc(strUSBportAlias, EmptyStr);
  if intComportIndx>=0 then
  begin
    objUSBport := aUSBports[intComportIndx];
    objUSBport.Open;
    sleep(100);  // to initialize comport
    while not objUSBport.Connected do
      Application.ProcessMessages;
    Result:=true;
  end
  else
    Result:=false;
end;

function InitUSBport(strUSBportAlias: string; strDeviceDesc, strBaudrate,
                          strDatabits, strParity, strStopbits, strInBufferSize,strOutBufferSize,
                          strFlowControl: string;bolDSRSensetivity:boolean): boolean;

var intComportIndx : integer;
    objUSBport : TMyUSBport;

begin
  Result := False;
  intComportIndx := GetUSBportIndexByDeviceDesc(strUSBportAlias, strDeviceDesc);
  if intComportIndx >= 0 then
  begin
    objUSBport := aUsbports[intComportIndx];
    if objUSBport.Connected then  // just in case
      objUSBport.Close;

    objUSBport.BaudRate := USBCommunication.StrToBaudRate(strBaudrate);
    objUSBport.DataBits := USBCommunication.StrToDataBits(strDatabits);
    objUSBport.Parity.Bits := USBCommunication.StrToParity(strParity);
    objUSBport.StopBits := USBCommunication.StrToStopBits(strStopbits);
    objUSBport.FlowControl.FlowControl := USBCommunication.StrToFlowControl(strFlowControl);
    objUSBport.FlowControl.DSRSensitivity := bolDSRSensetivity;
    objUSBport.Timeouts.ReadInterval := -1;
    objUSBport.Timeouts.ReadTotalMultiplier := 0;
    objUSBport.Timeouts.ReadTotalConstant := 0;
    Result:=true;
  end;
end;

function GetRegistryPropertyString(PnPHandle: HDEVINFO;
  const DevData: TSPDevInfoData; Prop: DWORD): string;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
  Res: PByte;
begin
  BytesReturned := 0;
  RegDataType := 0;
  Res := AllocMem(255);
  SetupDiGetDeviceRegistryPropertyA(PnPHandle, DevData, Prop,
    RegDataType, Res, 255, BytesReturned);

  Result := PAnsichar(Res);
end;

procedure GetUSBDevice;
const
  cHidGuid: TGUID = '{25dbce51-6c8f-4a72-8a6d-b54c2b4fc835}'; //Win CE Devices
var
  PnPHandle: HDEVINFO;
  DevData: TSPDevInfoData;
  DeviceInterfaceData: TSPDeviceInterfaceData;
  FunctionClassDeviceData: PSPDeviceInterfaceDetailData;
  Success: LongBool;
  Devn: Integer;
  BytesReturned: DWORD;
  FHidGuid : TGuid;
  MyUSBPort : TMyUSBport;
  FDevicePath : String;
begin
  FHidGuid := cHidGuid;
  LoadSetupApi;
  // Get a handle for the Plug and Play node and request currently active HID devices
  PnPHandle := SetupDiGetClassDevs(@FHidGuid, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if PnPHandle = Pointer(INVALID_HANDLE_VALUE) then
    Exit;
  Devn := 0;

  aUSBPorts := TList.Create;

  repeat
    DeviceInterfaceData.cbSize := SizeOf(TSPDeviceInterfaceData);
    // Is there a HID device at this table entry?
    Success := SetupDiEnumDeviceInterfaces(PnPHandle, nil, FHidGuid, Devn, DeviceInterfaceData);
    if Success then
    begin
      DevData.cbSize := SizeOf(DevData);
      BytesReturned := 0;
      SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData, nil, 0, BytesReturned, @DevData);
      if (BytesReturned <> 0) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        FunctionClassDeviceData := AllocMem(BytesReturned);
        try
          FunctionClassDeviceData^.cbSize := SizeOf(TSPDeviceInterfaceDetailData);
          if SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData,
            FunctionClassDeviceData, BytesReturned, BytesReturned, @DevData) then
          begin
            FDevicePath := PChar(@FunctionClassDeviceData.DevicePath);
            MyUSBPort := TMyUSBport.Create(nil);
            MyUSBPort.FstrUSBportDeviceDescription := GetRegistryPropertyString(PnPHandle, DevData, SPDRP_DEVICEDESC);
            MyUSBPort.Port := FDevicePath;
            aUSBports.Add(MyUSBPort);
            Inc(Devn);
          end;
        finally
          FreeMem(FunctionClassDeviceData);
        end;
      end;
    end;
  until not Success;
  SetupDiDestroyDeviceInfoList(PnPHandle);
end;


function Finalize : boolean;
var
  objUSBport : TMyUSBport;
begin
  
  while aUSBports.Count>0 do
  begin
    objUSBport:=aUSBports[aUSBports.Count-1];
    if objUSBport<>nil then
    begin
      if objUSBport.Connected then
        objUSBport.Close;
      objUSBport.Destroy;
    end;
    aUSBports.Delete(aUSBports.Count-1);
  end;
  aUSBports.Destroy;

end;


{ TMyUSBport }
constructor TMyUSBport.Create(hOwner: TComponent);
begin
  inherited Create(hOwner);
  FstrMyBuffer:='';
  SyncMethod := USBCommunication.smThreadSync;
  TriggersOnRxChar := false;
  Events := [USBCommunication.evRxChar, USBCommunication.evRXFlag];
  OnRXBuf := USBportDataReceived;
end;

destructor TMyUSBport.Destroy;
begin
  OnRXBuf := nil; // just in case...
  Close;
  Application.ProcessMessages;
  inherited;
end;

procedure TMyUSBport.USBPortDataReceived(objSender: TObject; const pBuffer;
  intCount: integer);
var intLoop : word;
    objUSBport : TMyUSBport;
    strDummy : AnsiString;
    bolFound : boolean;
begin
  if intCount > 0 then
  begin
    intLoop:=0;
    bolFound:=false;
    while (intLoop<aUSBports.Count) and not(bolFound) do
    begin
      objUSBport:=aUSBports[intLoop];
      if (objUSBport<>nil) then
        if objSender=objUSBport then  // Found the 'right' comport
        begin
          SetString(strDummy, PAnsiChar(@pBuffer), intCount);
          objUSBport.FstrMyBuffer:=objUSBport.FstrMyBuffer+strDummy;
          SleepEx(100,true);
          Application.ProcessMessages;
          bolFound:=true;
        end
        else
          inc(intLoop);
    end;
  end;
end;

initialization
  GetUSBDevice;

finalization
  Finalize;


end.
