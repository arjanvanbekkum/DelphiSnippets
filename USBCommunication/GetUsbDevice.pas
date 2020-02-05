procedure TForm1.GetUSBDevice;
var
  PnPHandle: HDEVINFO;
  DevData: TSPDevInfoData;
  DeviceInterfaceData: TSPDeviceInterfaceData;
  FunctionClassDeviceData: PSPDeviceInterfaceDetailData;
  Success: LongBool;
  Devn: Integer;
  BytesReturned: DWORD;
begin
  FHidGuid := cHidGuid;
  LoadSetupApi;
  // Get a handle for the Plug and Play node and request currently active HID devices
  PnPHandle := SetupDiGetClassDevs(@FHidGuid, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if PnPHandle = Pointer(INVALID_HANDLE_VALUE) then
    Exit;
  Devn := 0;
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
	
            // primary information
            Memo1.Lines.Add ( PChar(@FunctionClassDeviceData.DevicePath ));
            Memo1.Lines.Add ( IntToStr(DevData.DevInst) );
            Memo1.Lines.Add ( IntToStr( GetRegistryPropertyDWord(PnPHandle, DevData, SPDRP_CAPABILITIES)) );
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_CLASS));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_PHYSICAL_DEVICE_OBJECT_NAME) );
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_CLASSGUID) );
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_DEVICEDESC) );
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_DRIVER) );
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_ENUMERATOR_NAME) );
            Memo1.Lines.Add ( IntToStr( GetRegistryPropertyDWord(PnPHandle, DevData, SPDRP_CONFIGFLAGS)));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_FRIENDLYNAME));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_MFG));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_SERVICE));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_ADDRESS));
            Memo1.Lines.Add ( IntToStr( GetRegistryPropertyDWord(PnPHandle, DevData, SPDRP_BUSNUMBER)));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_BUSTYPEGUID));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_CHARACTERISTICS));
            Memo1.Lines.Add ( IntToStr( GetRegistryPropertyDWord(PnPHandle, DevData, SPDRP_DEVTYPE)));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_ENUMERATOR_NAME));
            Memo1.Lines.Add ( IntToStr( GetRegistryPropertyDWord(PnPHandle, DevData, SPDRP_EXCLUSIVE)));
            Memo1.Lines.Add ( IntToStr( GetRegistryPropertyDWord(PnPHandle, DevData, SPDRP_LEGACYBUSTYPE)));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_LOCATION_INFORMATION));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_PHYSICAL_DEVICE_OBJECT_NAME));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_SECURITY_SDS));
            Memo1.Lines.Add ( IntToStr( GetRegistryPropertyDWord(PnPHandle, DevData, SPDRP_UI_NUMBER)));
            Memo1.Lines.Add ( GetRegistryPropertyString(PnPHandle, DevData, SPDRP_UI_NUMBER_DESC_FORMAT));
            Memo1.Lines.Add ( GetRegistryPropertyStringList(PnPHandle, DevData, SPDRP_COMPATIBLEIDS).Text);
            Memo1.Lines.Add ( GetRegistryPropertyStringList(PnPHandle, DevData, SPDRP_HARDWAREID).Text);
            Memo1.Lines.Add ( GetRegistryPropertyStringList(PnPHandle, DevData, SPDRP_LOWERFILTERS).Text);
            Memo1.Lines.Add ( GetRegistryPropertyStringList(PnPHandle, DevData, SPDRP_UPPERFILTERS).Text);
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

function TForm1.GetRegistryPropertyString(PnPHandle: HDEVINFO;
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

function TForm1.GetRegistryPropertyStringList(PnPHandle: HDEVINFO;
  const DevData: TSPDevInfoData; Prop: DWORD): TStringList;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
  Res: PByte;
begin
  BytesReturned := 0;
  RegDataType := 0;
  SetupDiGetDeviceRegistryPropertyA(PnPHandle, DevData, Prop,
    RegDataType, Res, 1064, BytesReturned);
  Result := TStringList.Create;
  Result.Text := PAnsichar(Res);
end;

function TForm1.GetRegistryPropertyDWord(PnPHandle: HDEVINFO;
  const DevData: TSPDevInfoData; Prop: DWORD): DWORD;
var
  BytesReturned: DWORD;
  RegDataType: DWORD;
begin
  BytesReturned := 0;
  RegDataType := 0;
  Result := 0;
  SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop,
    RegDataType, PBYTE(@Result), SizeOf(Result), BytesReturned);
end;