unit USBCommunication;

interface

uses
  Windows, Messages, Classes, SysUtils, IniFiles, Registry;

type
  TUSBExceptions = ( CE_OpenFailed      ,    CE_WriteFailed     ,
                        CE_ReadFailed      ,    CE_InvalidAsync    ,
                        CE_PurgeFailed     ,    CE_AsyncCheck      ,
                        CE_SetStateFailed  ,    CE_TimeoutsFailed  ,
                        CE_SetupComFailed  ,    CE_ClearComFailed  ,
                        CE_ModemStatFailed ,    CE_EscapeComFailed ,
                        CE_TransmitFailed  ,    CE_ConnChangeProp  ,
                        CE_EnumPortsFailed ,    CE_StoreFailed     ,
                        CE_LoadFailed      ,    CE_RegFailed       ,
                        CE_LedStateFailed  ,    CE_ThreadCreated   ,
                        CE_WaitFailed      ,    CE_HasLink         ,
                        CE_RegError        ,    CEPortNotOpen     );




  // various types
  TPort = string;
  TBaudRate = (brCustom, br110, br300, br600, br1200, br2400, br4800, br9600, br14400,
                br19200, br38400, br56000, br57600, br115200, br128000, br256000);
  TStopBits = (sbOneStopBit, sbOne5StopBits, sbTwoStopBits);
  TDataBits = (dbFive, dbSix, dbSeven, dbEight);
  TParityBits = (prNone, prOdd, prEven, prMark, prSpace);
  TDTRFlowControl = (dtrDisable, dtrEnable, dtrHandshake);
  TRTSFlowControl = (rtsDisable, rtsEnable, rtsHandshake, rtsToggle);
  TFlowControl = (fcHardware, fcSoftware, fcNone, fcCustom);
  TUSBEvent = (evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS, evDSR, evError, evRLSD, evRx80Full);
  TUSBEvents = set of TUSBEvent;
  TUSBSignal = (csCTS, csDSR, csRing, csRLSD);
  TUSBSignals = set of TUSBSignal;
  TUSBError = (ceFrame, ceRxParity, ceOverrun, ceBreak, ceIO, ceMode, ceRxOver, ceTxFull);
  TUSBErrors = set of TUSBError;
  TSyncMethod = (smThreadSync, smWindowSync, smNone);
  TStoreType = (stRegistry, stIniFile);
  TStoredProp = (spBasic, spFlowControl, spBuffer, spTimeouts, spParity, spOthers);
  TStoredProps = set of TStoredProp;
  TUSBLinkEvent = (leConn, leCTS, leDSR, leRLSD, leRing, leRx, leTx, leTxEmpty, leRxFlag);
  TRxCharEvent = procedure(Sender: TObject; Count: Integer) of object;
  TRxBufEvent = procedure(Sender: TObject; const Buffer; Count: Integer) of object;
  TUSBErrorEvent = procedure(Sender: TObject; Errors: TUSBErrors) of object;
  TUSBSignalEvent = procedure(Sender: TObject; OnOff: Boolean) of object;
  TUSBExceptionEvent = procedure(Sender:TObject;
                          TUSBException:TUSBExceptions; USBPortMessage:String;
                          WinError:Int64; WinMessage:String) of object;

  // types for asynchronous calls
  TOperationKind = (okWrite, okRead);
  TAsync = record
    Overlapped: TOverlapped;
    Kind: TOperationKind;
    Data: Pointer;
    Size: Integer;
  end;
  PAsync = ^TAsync;

  {$IFNDEF Unicode}
  UnicodeString = Widestring;
  {$ENDIF}

  // TUSBPort component and asistant classes
  TCustomUSBPort = class; // forward declaration

  // class that links TCustomUSBPort events to other components
  TUSBLink = class
  private
    FOnConn: TUSBSignalEvent;
    FOnRxBuf: TRxBufEvent;
    FOnTxBuf: TRxBufEvent;
    FOnTxEmpty: TNotifyEvent;
    FOnRxFlag: TNotifyEvent;
    FOnCTSChange: TUSBSignalEvent;
    FOnDSRChange: TUSBSignalEvent;
    FOnRLSDChange: TUSBSignalEvent;
    FOnRing: TNotifyEvent;
    FOnTx: TUSBSignalEvent;
    FOnRx: TUSBSignalEvent;
  public
    property OnConn: TUSBSignalEvent read FOnConn write FOnConn;
    property OnRxBuf: TRxBufEvent read FOnRxBuf write FOnRxBuf;
    property OnTxBuf: TRxBufEvent read FOnTxBuf write FOnTxBuf;
    property OnTxEmpty: TNotifyEvent read FOnTxEmpty write FOnTxEmpty;
    property OnRxFlag: TNotifyEvent read FOnRxFlag write FOnRxFlag;
    property OnCTSChange: TUSBSignalEvent read FOnCTSChange write FOnCTSChange;
    property OnDSRChange: TUSBSignalEvent read FOnDSRChange write FOnDSRChange;
    property OnRLSDChange: TUSBSignalEvent read FOnRLSDChange write FOnRLSDChange;
    property OnRing: TNotifyEvent read FOnRing write FOnRing;
    property OnTx: TUSBSignalEvent read FOnTx write FOnTx;
    property OnRx: TUSBSignalEvent read FOnRx write FOnRx;
  end;

  // thread for background monitoring of port events
  TUSBThread = class(TThread)
  private
    FUSBPort: TCustomUSBPort;
    FStopEvent: THandle;
    FEvents: TUSBEvents;
  protected
    procedure DispatchComMsg;
    procedure DoEvents;
    procedure Execute; override;
    procedure SendEvents;
    procedure Stop;
  public
    constructor Create(AUSBPort: TCustomUSBPort);
    destructor Destroy; override;
  end;

  // timoeout properties for read/write operations
  TUSBTimeouts = class(TPersistent)
  private
    FUSBPort: TCustomUSBPort;
    FReadInterval: Integer;
    FReadTotalM: Integer;
    FReadTotalC: Integer;
    FWriteTotalM: Integer;
    FWriteTotalC: Integer;
    procedure SetUSBPort(const AUSBPort: TCustomUSBPort);
    procedure SetReadInterval(const Value: Integer);
    procedure SetReadTotalM(const Value: Integer);
    procedure SetReadTotalC(const Value: Integer);
    procedure SetWriteTotalM(const Value: Integer);
    procedure SetWriteTotalC(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property USBPort: TCustomUSBPort read FUSBPort;
  published
    property ReadInterval: Integer read FReadInterval write SetReadInterval default -1;
    property ReadTotalMultiplier: Integer read FReadTotalM write SetReadTotalM default 0;
    property ReadTotalConstant: Integer read FReadTotalC write SetReadTotalC default 0;
    property WriteTotalMultiplier: Integer
      read FWriteTotalM write SetWriteTotalM default 100;
    property WriteTotalConstant: Integer
      read FWriteTotalC write SetWriteTotalC default 1000;
  end;

  // flow control settings
  TUSBFlowControl = class(TPersistent)
  private
    FUSBPort: TCustomUSBPort;
    FOutCTSFlow: Boolean;
    FOutDSRFlow: Boolean;
    FControlDTR: TDTRFlowControl;
    FControlRTS: TRTSFlowControl;
    FXonXoffOut: Boolean;
    FXonXoffIn:  Boolean;
    FDSRSensitivity: Boolean;
    FTxContinueOnXoff: Boolean;
    FXonChar: Char;
    FXoffChar: Char;
    procedure SetUSBPort(const AUSBPort: TCustomUSBPort);
    procedure SetOutCTSFlow(const Value: Boolean);
    procedure SetOutDSRFlow(const Value: Boolean);
    procedure SetControlDTR(const Value: TDTRFlowControl);
    procedure SetControlRTS(const Value: TRTSFlowControl);
    procedure SetXonXoffOut(const Value: Boolean);
    procedure SetXonXoffIn(const Value: Boolean);
    procedure SetDSRSensitivity(const Value: Boolean);
    procedure SetTxContinueOnXoff(const Value: Boolean);
    procedure SetXonChar(const Value: Char);
    procedure SetXoffChar(const Value: Char);
    procedure SetFlowControl(const Value: TFlowControl);
    function GetFlowControl: TFlowControl;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property USBPort: TCustomUSBPort read FUSBPort;
  published
    property FlowControl: TFlowControl read GetFlowControl write SetFlowControl stored False;
    property OutCTSFlow: Boolean read FOutCTSFlow write SetOutCTSFlow;
    property OutDSRFlow: Boolean read FOutDSRFlow write SetOutDSRFlow;
    property ControlDTR: TDTRFlowControl read FControlDTR write SetControlDTR;
    property ControlRTS: TRTSFlowControl read FControlRTS write SetControlRTS;
    property XonXoffOut: Boolean read FXonXoffOut write SetXonXoffOut;
    property XonXoffIn:  Boolean read FXonXoffIn write SetXonXoffIn;
    property DSRSensitivity: Boolean
      read FDSRSensitivity write SetDSRSensitivity default False;
    property TxContinueOnXoff: Boolean
      read FTxContinueOnXoff write SetTxContinueOnXoff default False;
    property XonChar: Char read FXonChar write SetXonChar default #17;
    property XoffChar: Char read FXoffChar write SetXoffChar default #19;
  end;

  // parity settings
  TUSBParity = class(TPersistent)
  private
    FUSBPort: TCustomUSBPort;
    FBits: TParityBits;
    FCheck: Boolean;
    FReplace: Boolean;
    FReplaceChar: Char;
    procedure SetUSBPort(const AUSBPort: TCustomUSBPort);
    procedure SetBits(const Value: TParityBits);
    procedure SetCheck(const Value: Boolean);
    procedure SetReplace(const Value: Boolean);
    procedure SetReplaceChar(const Value: Char);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property USBPort: TCustomUSBPort read FUSBPort;
  published
    property Bits: TParityBits read FBits write SetBits;
    property Check: Boolean read FCheck write SetCheck default False;
    property Replace: Boolean read FReplace write SetReplace default False;
    property ReplaceChar: Char read FReplaceChar write SetReplaceChar default #0;
  end;

  // buffer size settings
  TUSBBuffer = class(TPersistent)
  private
    FUSBPort: TCustomUSBPort;
    FInputSize: Integer;
    FOutputSize: Integer;
    procedure SetUSBPort(const AUSBPort: TCustomUSBPort);
    procedure SetInputSize(const Value: Integer);
    procedure SetOutputSize(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property USBPort: TCustomUSBPort read FUSBPort;
  published
    property InputSize: Integer read FInputSize write SetInputSize default 1024;
    property OutputSize: Integer read FOutputSize write SetOutputSize default 1024;
  end;

  // main component
  TCustomUSBPort = class(TComponent)
  private
    FEventThread: TUSBThread;
    FThreadCreated: Boolean;
    FHandle: THandle;
    FWindow: THandle;
    FUpdateCount: Integer;
    FLinks: TList;
    FTriggersOnRxChar: Boolean;
    FEventThreadPriority: TThreadPriority;
    FHasLink: Boolean;
    FConnected: Boolean;
    FBaudRate: TBaudRate;
    FCustomBaudRate: Integer;
    FPort: TPort;
    FStopBits: TStopBits;
    FDataBits: TDataBits;
    FDiscardNull: Boolean;
    FEventChar: Char;
    FEvents: TUSBEvents;
    FBuffer: TUSBBuffer;
    FParity: TUSBParity;
    FTimeouts: TUSBTimeouts;
    FFlowControl: TUSBFlowControl;
    FSyncMethod: TSyncMethod;
    FStoredProps: TStoredProps;
    FOnRxChar: TRxCharEvent;
    FOnRxBuf: TRxBufEvent;
    FOnTxEmpty: TNotifyEvent;
    FOnBreak: TNotifyEvent;
    FOnRing: TNotifyEvent;
    FOnCTSChange: TUSBSignalEvent;
    FOnDSRChange: TUSBSignalEvent;
    FOnRLSDChange: TUSBSignalEvent;
    FOnError: TUSBErrorEvent;
    FOnRxFlag: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnRx80Full : TNotifyEvent;
    FOnException :TUSBExceptionEvent;
    FCodePage   : Cardinal;
    function GetTriggersOnRxChar: Boolean;
    procedure SetTriggersOnRxChar(const Value: Boolean);
    procedure SetConnected(const Value: Boolean);
    procedure SetBaudRate(const Value: TBaudRate);
    procedure SetCustomBaudRate(const Value: Integer);
    procedure SetPort(const Value: TPort);
    procedure SetStopBits(const Value: TStopBits);
    procedure SetDataBits(const Value: TDataBits);
    procedure SetDiscardNull(const Value: Boolean);
    procedure SetEventChar(const Value: Char);
    procedure SetSyncMethod(const Value: TSyncMethod);
    procedure SetEventThreadPriority(const Value: TThreadPriority);
    procedure SetParity(const Value: TUSBParity);
    procedure SetTimeouts(const Value: TUSBTimeouts);
    procedure SetBuffer(const Value: TUSBBuffer);
    procedure SetFlowControl(const Value: TUSBFlowControl);
    function HasLink: Boolean;
    procedure TxNotifyLink(const Buffer; Count: Integer);
    procedure NotifyLink(FLinkEvent: TUSBLinkEvent);
    procedure SendSignalToLink(Signal: TUSBLinkEvent; OnOff: Boolean);
    procedure CheckSignals(Open: Boolean);
    procedure WindowMethod(var Message: TMessage);
    procedure CallAfterOpen;
    procedure CallAfterClose;
    procedure CallBeforeOpen;
    procedure CallBeforeClose;
    procedure CallRxChar;
    procedure CallTxEmpty;
    procedure CallBreak;
    procedure CallRing;
    procedure CallRxFlag;
    procedure CallCTSChange;
    procedure CallDSRChange;
    procedure CallError;
    procedure CallRLSDChange;
    procedure CallRx80Full;
    procedure CallException(AnException: Word; const WinError: Int64 =0);
  protected
    procedure Loaded; override;
    procedure DoAfterClose; dynamic;
    procedure DoAfterOpen; dynamic;
    procedure DoBeforeClose; dynamic;
    procedure DoBeforeOpen; dynamic;
    procedure DoRxChar(Count: Integer); dynamic;
    procedure DoRxBuf(const Buffer; Count: Integer); dynamic;
    procedure DoTxEmpty; dynamic;
    procedure DoBreak; dynamic;
    procedure DoRing; dynamic;
    procedure DoRxFlag; dynamic;
    procedure DoCTSChange(OnOff: Boolean); dynamic;
    procedure DoDSRChange(OnOff: Boolean); dynamic;
    procedure DoError(Errors: TUSBErrors); dynamic;
    procedure DoRLSDChange(OnOff: Boolean); dynamic;
    procedure DoRx80Full; dynamic;
    procedure StoreRegistry(Reg: TRegistry); virtual;
    procedure StoreIniFile(IniFile: TIniFile); virtual;
    procedure LoadRegistry(Reg: TRegistry); virtual;
    procedure LoadIniFile(IniFile: TIniFile); virtual;
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure ApplyDCB; dynamic;
    procedure ApplyTimeouts; dynamic;
    procedure ApplyBuffer; dynamic;
    procedure SetupUSBPort; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure StoreSettings(StoreType: TStoreType; StoreTo: string);
    procedure LoadSettings(StoreType: TStoreType; LoadFrom: string);
    procedure Open;
    procedure Close;
    function InputCount: Integer;
    function OutputCount: Integer;
    function Signals: TUSBSignals;
    function StateFlags: TComStateFlags;
    procedure SetDTR(OnOff: Boolean);
    procedure SetRTS(OnOff: Boolean);
    procedure SetXonXoff(OnOff: Boolean);
    procedure SetBreak(OnOff: Boolean);
    procedure ClearBuffer(Input, Output: Boolean);
    function LastErrors: TUSBErrors;

    function Write(const Buffer; Count: Integer): Integer;
    function WriteStr(const Str: string): Integer;
    function Read(var Buffer; Count: Integer): Integer;
    function ReadStr(var Str: string; Count: Integer): Integer;
    function WriteAsync(const Buffer; Count: Integer;  var AsyncPtr: PAsync): Integer;
    function WriteStrAsync(const Str: string; var AsyncPtr: PAsync): Integer;
    function ReadAsync(var Buffer; Count: Integer;   var AsyncPtr: PAsync): Integer;
    function ReadStrAsync(var Str: string; Count: Integer;  var AsyncPtr: PAsync): Integer;
    function WriteUnicodeString(const Str: Unicodestring): Integer;
    function ReadUnicodeString(var Str: UnicodeString; Count: Integer): Integer;

    function WaitForAsync(var AsyncPtr: PAsync): Integer;
    function IsAsyncCompleted(AsyncPtr: PAsync): Boolean;
    procedure WaitForEvent(var Events: TUSBEvents; StopEvent: THandle;      Timeout: Integer);
    procedure AbortAllAsync;
    procedure TransmitChar(Ch: Char);
    procedure RegisterLink(AComLink: TUSBLink);
    procedure UnRegisterLink(AComLink: TUSBLink);
    property Handle: THandle read FHandle;
    property TriggersOnRxChar: Boolean read GetTriggersOnRxChar write SetTriggersOnRxChar;
    property EventThreadPriority: TThreadPriority  read FEventThreadPriority write SetEventThreadPriority;
    property StoredProps: TStoredProps read FStoredProps write FStoredProps;
    property Connected: Boolean read FConnected write SetConnected default False;
    property BaudRate: TBaudRate read FBaudRate write SetBaudRate;
    property CustomBaudRate: Integer    read FCustomBaudRate write SetCustomBaudRate;
    property Port: TPort read FPort write SetPort;
    property Parity: TUSBParity read FParity write SetParity;
    property StopBits: TStopBits read FStopBits write SetStopBits;
    property DataBits: TDataBits read FDataBits write SetDataBits;
    property DiscardNull: Boolean read FDiscardNull write SetDiscardNull default False;
    property EventChar: Char read FEventChar write SetEventChar default #0;
    property Events: TUSBEvents read FEvents write FEvents;
    property Buffer: TUSBBuffer read FBuffer write SetBuffer;
    property FlowControl: TUSBFlowControl  read FFlowControl write SetFlowControl;
    property Timeouts: TUSBTimeouts read FTimeouts write SetTimeouts;
    property SyncMethod: TSyncMethod read FSyncMethod write SetSyncMethod default smThreadSync;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforeClose: TNotifyEvent   read FOnBeforeClose write FOnBeforeClose;
    property OnRxChar: TRxCharEvent read FOnRxChar write FOnRxChar;
    property OnRxBuf: TRxBufEvent read FOnRxBuf write FOnRxBuf;
    property OnTxEmpty: TNotifyEvent read FOnTxEmpty write FOnTxEmpty;
    property OnBreak: TNotifyEvent read FOnBreak write FOnBreak;
    property OnRing: TNotifyEvent read FOnRing write FOnRing;
    property OnCTSChange: TUSBSignalEvent read FOnCTSChange write FOnCTSChange;
    property OnDSRChange: TUSBSignalEvent read FOnDSRChange write FOnDSRChange;
    property OnRLSDChange: TUSBSignalEvent  read FOnRLSDChange write FOnRLSDChange;
    property OnRxFlag: TNotifyEvent read FOnRxFlag write FOnRxFlag;
    property OnError: TUSBErrorEvent read FOnError write FOnError;
    property OnRx80Full: TNotifyEvent read FOnRx80Full write FOnRx80Full;
    property OnException: TUSBExceptionEvent read FOnException write FOnException;
    // Translate strings between ANSI charsets
    property CodePage: Cardinal read FCodePage write FCodePage default 0;
  end;

  // publish the properties
  TUSBPort = class(TCustomUSBPort)
    property Connected;
    property BaudRate;
    property Port;
    property Parity;
    property StopBits;
    property DataBits;
    property DiscardNull;
    property EventChar;
    property Events;
    property Buffer;
    property FlowControl;
    property Timeouts;
    property StoredProps;
    property TriggersOnRxChar;
    property SyncMethod;
    property OnAfterOpen;
    property OnAfterClose;
    property OnBeforeOpen;
    property OnBeforeClose;
    property OnRxChar;
    property OnRxBuf;
    property OnTxEmpty;
    property OnBreak;
    property OnRing;
    property OnCTSChange;
    property OnDSRChange;
    property OnRLSDChange;
    property OnRxFlag;
    property OnError;
    property OnRx80Full;
    property OnException;
    property CodePage;
  end;

  TUSBStrEvent = procedure(Sender: TObject; const Str: string) of object;
  TCustPacketEvent = procedure(Sender: TObject; const Str: string;
    var Pos: Integer) of object;

  // component for reading data in packets
  TUSBDataPacket = class(TComponent)
  private
    FComLink: TUSBLink;
    FUSBPort: TCustomUSBPort;
    FStartString: string;
    FStopString: string;
    FMaxBufferSize: Integer;
    FSize: Integer;
    FIncludeStrings: Boolean;
    FCaseInsensitive: Boolean;
    FInPacket: Boolean;
    FBuffer: string;
    FOnPacket: TUSBStrEvent;
    FOnDiscard: TUSBStrEvent;
    FOnCustomStart: TCustPacketEvent;
    FOnCustomStop: TCustPacketEvent;
    procedure SetUSBPort(const Value: TCustomUSBPort);
    procedure SetCaseInsensitive(const Value: Boolean);
    procedure SetSize(const Value: Integer);
    procedure SetStartString(const Value: string);
    procedure SetStopString(const Value: string);
    procedure RxBuf(Sender: TObject; const Buffer; Count: Integer);
    procedure CheckIncludeStrings(var Str: string);
    function Upper(const Str: string): string;
    procedure EmptyBuffer;
    function ValidStop: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoDiscard(const Str: string); dynamic;
    procedure DoPacket(const Str: string); dynamic;
    procedure DoCustomStart(const Str: string; var Pos: Integer); dynamic;
    procedure DoCustomStop(const Str: string; var Pos: Integer); dynamic;
    procedure HandleBuffer; virtual;
    property Buffer: string read FBuffer write FBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddData(const Str: string);
  published
    procedure ResetBuffer;
    property USBPort: TCustomUSBPort read FUSBPort write SetUSBPort;
    property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive default False;
    property IncludeStrings: Boolean read FIncludeStrings write FIncludeStrings default False;
    property MaxBufferSize: Integer read FMaxBufferSize write FMaxBufferSize default 1024;
    property StartString: string read FStartString write SetStartString;
    property StopString: string read FStopString write SetStopString;
    property Size: Integer read FSize write SetSize default 0;
    property OnDiscard: TUSBStrEvent read FOnDiscard write FOnDiscard;
    property OnPacket: TUSBStrEvent read FOnPacket write FOnPacket;
    property OnCustomStart: TCustPacketEvent read FOnCustomStart write FOnCustomStart;
    property OnCustomStop: TCustPacketEvent read FOnCustomStop write FOnCustomStop;
  end;

  // com port stream
  TUSBStream = class(TStream)
  private
    FUSBPort: TCustomUSBPort;
  public
    constructor Create(AUSBPort: TCustomUSBPort);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  // exception class for USBPort Library errors
  EUSBPort = class(Exception)
  private
    FWinCode: Integer;
    FCode: Integer;
  public
    constructor Create(ACode: Integer; AWinCode: Integer);
    constructor CreateNoWinCode(ACode: Integer);
    property WinCode: Integer read FWinCode write FWinCode;
    property Code: Integer read FCode write FCode;
  end;

// aditional procedures
procedure InitAsync(var AsyncPtr: PAsync);
procedure DoneAsync(var AsyncPtr: PAsync);
procedure EnumUSBPorts(Ports: TStrings);

// conversion functions
function StrToBaudRate(Str: string): TBaudRate;
function StrToStopBits(Str: string): TStopBits;
function StrToDataBits(Str: string): TDataBits;
function StrToParity(Str: string): TParityBits;
function StrToFlowControl(Str: string): TFlowControl;
function BaudRateToStr(BaudRate: TBaudRate): string;
function StopBitsToStr(StopBits: TStopBits): string;
function DataBitsToStr(DataBits: TDataBits): string;
function ParityToStr(Parity: TParityBits): string;
function FlowControlToStr(FlowControl: TFlowControl): string;
function ComErrorsToStr(Errors:TUSBErrors):String;

const
  // infinite wait
  WaitInfinite = Integer(INFINITE);

  // error codes
  CError_OpenFailed      = 1;
  CError_WriteFailed     = 2;
  CError_ReadFailed      = 3;
  CError_InvalidAsync    = 4;
  CError_PurgeFailed     = 5;
  CError_AsyncCheck      = 6;
  CError_SetStateFailed  = 7;
  CError_TimeoutsFailed  = 8;
  CError_SetupComFailed  = 9;
  CError_ClearComFailed  = 10;
  CError_ModemStatFailed = 11;
  CError_EscapeComFailed = 12;
  CError_TransmitFailed  = 13;
  CError_ConnChangeProp  = 14;
  CError_EnumPortsFailed = 15;
  CError_StoreFailed     = 16;
  CError_LoadFailed      = 17;
  CError_RegFailed       = 18;
  CError_LedStateFailed  = 19;
  CError_ThreadCreated   = 20;
  CError_WaitFailed      = 21;
  CError_HasLink         = 22;
  CError_RegError        = 23;
  CError_PortNotOpen     = 24;

implementation

uses
   Controls, Forms, WinSpool;

var
  // error messages
  ComErrorMessages: array[1..24] of widestring;

const
  // auxilary constants used not defined in windows.pas
  dcb_Binary           = $00000001;
  dcb_Parity           = $00000002;
  dcb_OutxCTSFlow      = $00000004;
  dcb_OutxDSRFlow      = $00000008;
  dcb_DTRControl       = $00000030;
  dcb_DSRSensivity     = $00000040;
  dcb_TxContinueOnXoff = $00000080;
  dcb_OutX             = $00000100;
  dcb_InX              = $00000200;
  dcb_ErrorChar        = $00000400;
  dcb_Null             = $00000800;
  dcb_RTSControl       = $00003000;
  dcb_AbortOnError     = $00004000;

  // com port window message
  CM_USBPort           = WM_USER + 1;

(*****************************************
 * auxilary functions and procedures     *
 *****************************************)
function ComErrorsToStr(Errors:TUSBErrors):String;
  procedure e(msg:String);
  begin
     if result='' then
        result := msg
     else
        result := result+','+msg;
  end;
begin
   result := '';
   if ceFrame    in Errors then e('Frame');
   if ceRxParity in Errors then e('Parity');
   if ceOverrun  in Errors then e('Overrun');
   if ceBreak    in Errors then e('Break');
   if ceIO       in Errors then e('IO');
   if ceMode     in Errors then e('Mode');
   if ceRxOver   in Errors then e('RxOver');
   if ceTxFull   in Errors then e('TxFull');
   if result = '' then
      result := '<Ok>'
   else
      result := '<ComError:'+result+'>';
end;

// converts TUSBEvents type to Integer
function EventsToInt(const Events: TUSBEvents): Integer;
begin
  Result := 0;
  if evRxChar in Events then
    Result := Result or EV_RXCHAR;
  if evRxFlag in Events then
    Result := Result or EV_RXFLAG;
  if evTxEmpty in Events then
    Result := Result or EV_TXEMPTY;
  if evRing in Events then
    Result := Result or EV_RING;
  if evCTS in Events then
    Result := Result or EV_CTS;
  if evDSR in Events then
    Result := Result or EV_DSR;
  if evRLSD in Events then
    Result := Result or EV_RLSD;
  if evError in Events then
    Result := Result or EV_ERR;
  if evBreak in Events then
    Result := Result or EV_BREAK;
  if evRx80Full in Events then
    Result := Result or EV_RX80FULL;
end;

function IntToEvents(Mask: Integer): TUSBEvents;
begin
  Result := [];
  if (EV_RXCHAR and Mask) <> 0 then
    Result := Result + [evRxChar];
  if (EV_TXEMPTY and Mask) <> 0 then
    Result := Result + [evTxEmpty];
  if (EV_BREAK and Mask) <> 0 then
    Result := Result + [evBreak];
  if (EV_RING and Mask) <> 0 then
    Result := Result + [evRing];
  if (EV_CTS and Mask) <> 0 then
    Result := Result + [evCTS];
  if (EV_DSR and Mask) <> 0 then
    Result := Result + [evDSR];
  if (EV_RXFLAG and Mask) <> 0 then
    Result := Result + [evRxFlag];
  if (EV_RLSD and Mask) <> 0 then
    Result := Result + [evRLSD];
  if (EV_ERR and Mask) <> 0 then
    Result := Result + [evError];
  if (EV_RX80FULL and Mask) <> 0 then
    Result := Result + [evRx80Full];
end;

(*****************************************
 * TUSBThread class                      *
 *****************************************)

// create thread
constructor TUSBThread.Create(AUSBPort: TCustomUSBPort);
begin
  inherited Create(false);
  FStopEvent := CreateEvent(nil, True, False, nil);
  FUSBPort := AUSBPort;
  // set thread priority
  Priority := FUSBPort.EventThreadPriority;
  // select which events are monitored
  SetCommMask(FUSBPort.Handle, EventsToInt(FUSBPort.Events));
  // execute thread
  //{$IFDEF Unicode}Start;  {$ELSE}  Resume;  {$ENDIF}
end;

// destroy thread
destructor TUSBThread.Destroy;
begin
  Stop;
  inherited Destroy;
end;

// thread action
procedure TUSBThread.Execute;
var
  EventHandles: array[0..1] of THandle;
  Overlapped: TOverlapped;
  Signaled, BytesTrans, Mask: DWORD;
begin
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, True, nil);
  EventHandles[0] := FStopEvent;
  EventHandles[1] := Overlapped.hEvent;
  repeat
    // wait for event to occur on serial port
    WaitCommEvent(FUSBPort.Handle, Mask, @Overlapped);
    Signaled := WaitForMultipleObjects(2, @EventHandles, False, INFINITE);
    // if event occurs, dispatch it
    if (Signaled = WAIT_OBJECT_0 + 1)
      and GetOverlappedResult(FUSBPort.Handle, Overlapped, BytesTrans, False)
    then
    begin
      FEvents := IntToEvents(Mask);
      DispatchComMsg;
    end;
  until Signaled <> (WAIT_OBJECT_0 + 1);
  // clear buffers
  SetCommMask(FUSBPort.Handle, 0);
  PurgeComm(FUSBPort.Handle, PURGE_TXCLEAR or PURGE_RXCLEAR);
  CloseHandle(Overlapped.hEvent);
  CloseHandle(FStopEvent);
end;

// stop thread
procedure TUSBThread.Stop;
begin
  SetEvent(FStopEvent);
  Sleep(0);
end;

// dispatch events
procedure TUSBThread.DispatchComMsg;
begin
  case FUSBPort.SyncMethod of
    smThreadSync: Synchronize(DoEvents); // call events in main thread
    smWindowSync: SendEvents; // call events in thread that opened the port
    smNone:       DoEvents; // call events inside monitoring thread
  end;
end;

// send events to TCustomUSBPort component using window message
procedure TUSBThread.SendEvents;
begin
  if evError in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_ERR, 0);
  if evRxChar in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_RXCHAR, 0);
  if evTxEmpty in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_TXEMPTY, 0);
  if evBreak in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_BREAK, 0);
  if evRing in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_RING, 0);
  if evCTS in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_CTS, 0);
  if evDSR in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_DSR, 0);
  if evRxFlag in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_RXFLAG, 0);
  if evRing in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_RLSD, 0);
  if evRx80Full in FEvents then
    SendMessage(FUSBPort.FWindow, CM_USBPort, EV_RX80FULL, 0);
end;

// call events
procedure TUSBThread.DoEvents;
begin
  if evError in FEvents then
    FUSBPort.CallError;
  if evRxChar in FEvents then
    FUSBPort.CallRxChar;
  if evTxEmpty in FEvents then
    FUSBPort.CallTxEmpty;
  if evBreak in FEvents then
    FUSBPort.CallBreak;
  if evRing in FEvents then
    FUSBPort.CallRing;
  if evCTS in FEvents then
    FUSBPort.CallCTSChange;
  if evDSR in FEvents then
    FUSBPort.CallDSRChange;
  if evRxFlag in FEvents then
    FUSBPort.CallRxFlag;
  if evRLSD in FEvents then
    FUSBPort.CallRLSDChange;
  if evRx80Full in FEvents then
    FUSBPort.CallRx80Full;
end;

(*****************************************
 * TUSBTimeouts class                    *
 *****************************************)

// create class
constructor TUSBTimeouts.Create;
begin
  inherited Create;
  FReadInterval := -1;
  FWriteTotalM := 100;
  FWriteTotalC := 1000;
end;

// copy properties to other class
procedure TUSBTimeouts.AssignTo(Dest: TPersistent);
begin
  if Dest is TUSBTimeouts then
  begin
    with TUSBTimeouts(Dest) do
    begin
      FReadInterval := Self.ReadInterval;
      FReadTotalM   := Self.ReadTotalMultiplier;
      FReadTotalC   := Self.ReadTotalConstant;
      FWriteTotalM  := Self.WriteTotalMultiplier;
      FWriteTotalC  := Self.WriteTotalConstant;
    end
  end
  else
    inherited AssignTo(Dest);
end;

// select TCustomUSBPort to own this class
procedure TUSBTimeouts.SetUSBPort(const AUSBPort: TCustomUSBPort);
begin
  FUSBPort := AUSBPort;
end;

// set read interval
procedure TUSBTimeouts.SetReadInterval(const Value: Integer);
begin
  if Value <> FReadInterval then
  begin
    FReadInterval := Value;
    // if possible, apply the changes
    if FUSBPort <> nil then
      FUSBPort.ApplyTimeouts;
  end;
end;

// set read total constant
procedure TUSBTimeouts.SetReadTotalC(const Value: Integer);
begin
  if Value <> FReadTotalC then
  begin
    FReadTotalC := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyTimeouts;
  end;
end;

// set read total multiplier
procedure TUSBTimeouts.SetReadTotalM(const Value: Integer);
begin
  if Value <> FReadTotalM then
  begin
    FReadTotalM := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyTimeouts;
  end;
end;

// set write total constant
procedure TUSBTimeouts.SetWriteTotalC(const Value: Integer);
begin
  if Value <> FWriteTotalC then
  begin
    FWriteTotalC := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyTimeouts;
  end;
end;

// set write total multiplier
procedure TUSBTimeouts.SetWriteTotalM(const Value: Integer);
begin
  if Value <> FWriteTotalM then
  begin
    FWriteTotalM := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyTimeouts;
  end;
end;

(*****************************************
 * TUSBFlowControl class                 *
 *****************************************)

// create class
constructor TUSBFlowControl.Create;
begin
  inherited Create;
  FXonChar := #17;
  FXoffChar := #19;
end;

// copy properties to other class
procedure TUSBFlowControl.AssignTo(Dest: TPersistent);
begin
  if Dest is TUSBFlowControl then
  begin
    with TUSBFlowControl(Dest) do
    begin
      FOutCTSFlow       := Self.OutCTSFlow;
      FOutDSRFlow       := Self.OutDSRFlow;
      FControlDTR       := Self.ControlDTR;
      FControlRTS       := Self.ControlRTS;
      FXonXoffOut       := Self.XonXoffOut;
      FXonXoffIn        := Self.XonXoffIn;
      FTxContinueOnXoff := Self.TxContinueOnXoff;
      FDSRSensitivity   := Self.DSRSensitivity;
      FXonChar          := Self.XonChar;
      FXoffChar         := Self.XoffChar;
    end
  end
  else
    inherited AssignTo(Dest);
end;

// select TCustomUSBPort to own this class
procedure TUSBFlowControl.SetUSBPort(const AUSBPort: TCustomUSBPort);
begin
  FUSBPort := AUSBPort;
end;

// set input flow control for DTR (data-terminal-ready)
procedure TUSBFlowControl.SetControlDTR(const Value: TDTRFlowControl);
begin
  if Value <> FControlDTR then
  begin
    FControlDTR := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set input flow control for RTS (request-to-send)
procedure TUSBFlowControl.SetControlRTS(const Value: TRTSFlowControl);
begin
  if Value <> FControlRTS then
  begin
    FControlRTS := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set ouput flow control for CTS (clear-to-send)
procedure TUSBFlowControl.SetOutCTSFlow(const Value: Boolean);
begin
  if Value <> FOutCTSFlow then
  begin
    FOutCTSFlow := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set output flow control for DSR (data-set-ready)
procedure TUSBFlowControl.SetOutDSRFlow(const Value: Boolean);
begin
  if Value <> FOutDSRFlow then
  begin
    FOutDSRFlow := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set software input flow control
procedure TUSBFlowControl.SetXonXoffIn(const Value: Boolean);
begin
  if Value <> FXonXoffIn then
  begin
    FXonXoffIn := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set software ouput flow control
procedure TUSBFlowControl.SetXonXoffOut(const Value: Boolean);
begin
  if Value <> FXonXoffOut then
  begin
    FXonXoffOut := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set DSR sensitivity
procedure TUSBFlowControl.SetDSRSensitivity(const Value: Boolean);
begin
  if Value <> FDSRSensitivity then
  begin
    FDSRSensitivity := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set transfer continue when Xoff is sent
procedure TUSBFlowControl.SetTxContinueOnXoff(const Value: Boolean);
begin
  if Value <> FTxContinueOnXoff then
  begin
    FTxContinueOnXoff := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set Xon char
procedure TUSBFlowControl.SetXonChar(const Value: Char);
begin
  if Value <> FXonChar then
  begin
    FXonChar := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set Xoff char
procedure TUSBFlowControl.SetXoffChar(const Value: Char);
begin
  if Value <> FXoffChar then
  begin
    FXoffChar := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// get common flow control
function TUSBFlowControl.GetFlowControl: TFlowControl;
begin
  if (FControlRTS = rtsHandshake) and (FOutCTSFlow)
    and (not FXonXoffIn) and (not FXonXoffOut)
  then
    Result := fcHardware
  else
    if (FControlRTS = rtsDisable) and (not FOutCTSFlow)
      and (FXonXoffIn) and (FXonXoffOut)
    then
      Result := fcSoftware
    else
      if (FControlRTS = rtsDisable) and (not FOutCTSFlow)
        and (not FXonXoffIn) and (not FXonXoffOut)
      then
        Result := fcNone
      else
        Result := fcCustom;
end;

// set common flow control
procedure TUSBFlowControl.SetFlowControl(const Value: TFlowControl);
begin
  if Value <> fcCustom then
  begin
    FControlRTS := rtsDisable;
    FOutCTSFlow := False;
    FXonXoffIn := False;
    FXonXoffOut := False;
    case Value of
      fcHardware:
      begin
        FControlRTS := rtsHandshake;
        FOutCTSFlow := True;
      end;
      fcSoftware:
      begin
        FXonXoffIn := True;
        FXonXoffOut := True;
      end;
    end;
  end;
  if FUSBPort <> nil then
    FUSBPort.ApplyDCB;
end;

(*****************************************
 * TUSBParity class                      *
 *****************************************)

// create class
constructor TUSBParity.Create;
begin
  inherited Create;
  FBits := prNone;
end;

// copy properties to other class
procedure TUSBParity.AssignTo(Dest: TPersistent);
begin
  if Dest is TUSBParity then
  begin
    with TUSBParity(Dest) do
    begin
      FBits        := Self.Bits;
      FCheck       := Self.Check;
      FReplace     := Self.Replace;
      FReplaceChar := Self.ReplaceChar;
    end
  end
  else
    inherited AssignTo(Dest);
end;

// select TCustomUSBPort to own this class
procedure TUSBParity.SetUSBPort(const AUSBPort: TCustomUSBPort);
begin
  FUSBPort := AUSBPort;
end;

// set parity bits
procedure TUSBParity.SetBits(const Value: TParityBits);
begin
  if Value <> FBits then
  begin
    FBits := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set check parity
procedure TUSBParity.SetCheck(const Value: Boolean);
begin
  if Value <> FCheck then
  begin
    FCheck := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set replace on parity error
procedure TUSBParity.SetReplace(const Value: Boolean);
begin
  if Value <> FReplace then
  begin
    FReplace := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

// set replace char
procedure TUSBParity.SetReplaceChar(const Value: Char);
begin
  if Value <> FReplaceChar then
  begin
    FReplaceChar := Value;
    if FUSBPort <> nil then
      FUSBPort.ApplyDCB;
  end;
end;

(*****************************************
 * TUSBBuffer class                      *
 *****************************************)

// create class
constructor TUSBBuffer.Create;
begin
  inherited Create;
  FInputSize := 1024;
  FOutputSize := 1024;
end;

// copy properties to other class
procedure TUSBBuffer.AssignTo(Dest: TPersistent);
begin
  if Dest is TUSBBuffer then
  begin
    with TUSBBuffer(Dest) do
    begin
      FOutputSize  := Self.OutputSize;
      FInputSize   := Self.InputSize;
    end
  end
  else
    inherited AssignTo(Dest);
end;

// select TCustomUSBPort to own this class
procedure TUSBBuffer.SetUSBPort(const AUSBPort: TCustomUSBPort);
begin
  FUSBPort := AUSBPort;
end;

// set input size
procedure TUSBBuffer.SetInputSize(const Value: Integer);
begin
  if Value <> FInputSize then
  begin
    FInputSize := Value;
    if (FInputSize mod 2) = 1 then
      Dec(FInputSize);
    if FUSBPort <> nil then
      FUSBPort.ApplyBuffer;
  end;
end;

// set ouput size
procedure TUSBBuffer.SetOutputSize(const Value: Integer);
begin
  if Value <> FOutputSize then
  begin
    FOutputSize := Value;
    if (FOutputSize mod 2) = 1 then
      Dec(FOutputSize);
    if FUSBPort <> nil then
      FUSBPort.ApplyBuffer;
  end;
end;

(*****************************************
 * TCustomUSBPort component              *
 *****************************************)

// create component
constructor TCustomUSBPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // component cannot reside on inheritable forms
  FComponentStyle := FComponentStyle - [csInheritable];
  FLinks := TList.Create;
  FTriggersOnRxChar := True;
  FEventThreadPriority := tpNormal;
  FBaudRate := br9600;
  FCustomBaudRate := 9600;
  FPort := 'COM1';
  FStopBits := sbOneStopBit;
  FDataBits := dbEight;
  FEvents := [evRxChar, evTxEmpty, evRxFlag, evRing, evBreak,
             evCTS, evDSR, evError, evRLSD, evRx80Full];
  FHandle := INVALID_HANDLE_VALUE;
  FStoredProps := [spBasic];
  FParity := TUSBParity.Create;
  FParity.SetUSBPort(Self);
  FFlowControl := TUSBFlowControl.Create;
  FFlowControl.SetUSBPort(Self);
  FTimeouts := TUSBTimeouts.Create;
  FTimeouts.SetUSBPort(Self);
  FBuffer := TUSBBuffer.Create;
  FBuffer.SetUSBPort(Self);
  FCodePage := CP_ACP;//0; // uses default system codepage
end;

// destroy component
destructor TCustomUSBPort.Destroy;
begin
  Close;
  FBuffer.Free;
  FFlowControl.Free;
  FTimeouts.Free;
  FParity.Free;
  inherited Destroy;
  FLinks.Free;
end;

//Handle Exceptions
procedure  TCustomUSBPort.CallException(AnException:Word; const WinError:Int64 =0);
var winmessage:string;
begin
  if Assigned(FOnException) then
  begin
    if WinError > 0 then //get windows error string
    try  Win32Check(winerror = 0);  except on E:Exception do WinMessage:=e.message; end;
    FOnException(self,TUSBExceptions(AnException),ComErrorMessages[AnException],WinError, WinMessage);
  end
    else
      if WinError > 0 then raise EUSBPort.Create(AnException, WinError)
       else raise EUSBPort.CreateNoWinCode(AnException);

end;
// create handle to serial port
procedure TCustomUSBPort.CreateHandle;
begin
  FHandle := CreateFile(PChar(FPort), GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
{
  FHandle := CreateFile(
    PChar('\\.\' + FPort),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_OVERLAPPED,
    0);
}
  if FHandle = INVALID_HANDLE_VALUE then
    //raise EUSBPort.Create
    CallException(CError_OpenFailed, GetLastError);
end;

// destroy serial port handle
procedure TCustomUSBPort.DestroyHandle;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    if CloseHandle(FHandle) then
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

procedure TCustomUSBPort.Loaded;
begin
  inherited Loaded;
  // open port if Connected is True at design-time
  if FConnected and not (csDesigning in ComponentState) then
  begin
    FConnected := False;
    try
      Open;
    except
      Application.HandleException(Self);
    end;
  end;
end;

// call events which have been dispatch using window message
procedure TCustomUSBPort.WindowMethod(var Message: TMessage);
begin
  with Message do
    if Msg = CM_USBPort then
      try
        if InSendMessage then
          ReplyMessage(0);
        if FConnected then
          case wParam of
            EV_RXCHAR:   CallRxChar;
            EV_TXEMPTY:  CallTxEmpty;
            EV_BREAK:    CallBreak;
            EV_RING:     CallRing;
            EV_CTS:      CallCTSChange;
            EV_DSR:      CallDSRChange;
            EV_RXFLAG:   CallRxFlag;
            EV_RLSD:     CallRLSDChange;
            EV_ERR:      CallError;
            EV_RX80FULL: CallRx80Full;
          end
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FWindow, Msg, wParam, lParam);
end;

// prevent from applying changes at runtime
procedure TCustomUSBPort.BeginUpdate;
begin
  FUpdateCount := FUpdateCount + 1;
end;

// apply the changes made since BeginUpdate call
procedure TCustomUSBPort.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount := FUpdateCount - 1;
    if FUpdateCount = 0 then
      SetupUSBPort;
  end;
end;

// open port
procedure TCustomUSBPort.Open;
begin
  // if already connected, do nothing
  if not FConnected and not (csDesigning in ComponentState) then
  begin
    CallBeforeOpen;
    // open port
    CreateHandle;
    FConnected := True;
    try
      // initialize port
      SetupUSBPort;
    except
      // error occured during initialization, destroy handle
      DestroyHandle;
      FConnected := False;
      raise;
    end;
    // if at least one event is set, create special thread to monitor port
    if (FEvents = []) then
      FThreadCreated := False
    else
    begin
      if (FSyncMethod = smWindowSync) then
{$IFDEF DELPHI_6_OR_HIGHER}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
        FWindow := AllocateHWnd(WindowMethod);
{$IFDEF DELPHI_6_OR_HIGHER}
  {$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}
      FEventThread := TUSBThread.Create(Self);
      FThreadCreated := True;
    end;
    // port is succesfully opened, do any additional initialization
    CallAfterOpen;
  end;
end;

// close port
procedure TCustomUSBPort.Close;
begin
  // if already closed, do nothing
  if FConnected and not (csDesigning in ComponentState) then
  begin
    CallBeforeClose;
    // abort all pending operations
    AbortAllAsync;
    // stop monitoring for events
    if FThreadCreated then
    begin
      FEventThread.Free;
      FThreadCreated := False;
      if FSyncMethod = smWindowSync then
{$IFDEF DELPHI_6_OR_HIGHER}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
        DeallocateHWnd(FWindow);
{$IFDEF DELPHI_6_OR_HIGHER}
  {$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}
    end;
    // close port
    DestroyHandle;
    FConnected := False;
    // port is closed, do any additional finalization
    CallAfterClose;
  end;
end;

// apply port properties
procedure TCustomUSBPort.ApplyDCB;
const
  CParityBits: array[TParityBits] of Integer =
    (NOPARITY, ODDPARITY, EVENPARITY, MARKPARITY, SPACEPARITY);
  CStopBits: array[TStopBits] of Integer =
    (ONESTOPBIT, ONE5STOPBITS, TWOSTOPBITS);
  CBaudRate: array[TBaudRate] of Integer =
    (0, CBR_110, CBR_300, CBR_600, CBR_1200, CBR_2400, CBR_4800, CBR_9600,
     CBR_14400, CBR_19200, CBR_38400, CBR_56000, CBR_57600, CBR_115200,
     CBR_128000, CBR_256000);
  CDataBits: array[TDataBits] of Integer = (5, 6, 7, 8);
  CControlRTS: array[TRTSFlowControl] of Integer =
    (RTS_CONTROL_DISABLE shl 12,
     RTS_CONTROL_ENABLE shl 12,
     RTS_CONTROL_HANDSHAKE shl 12,
     RTS_CONTROL_TOGGLE shl 12);
  CControlDTR: array[TDTRFlowControl] of Integer =
    (DTR_CONTROL_DISABLE shl 4,
     DTR_CONTROL_ENABLE shl 4,
     DTR_CONTROL_HANDSHAKE shl 4);

var
  DCB: TDCB;

begin
  // if not connected or inside BeginUpdate/EndUpdate block, do nothing
  if FConnected and (FUpdateCount = 0) and
    not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then
  begin
    DCB.DCBlength := SizeOf(TDCB);
    DCB.XonLim := FBuffer.InputSize div 4;
    DCB.XoffLim := DCB.XonLim;
    DCB.EvtChar := AnsiChar(FEventChar);

    DCB.Flags := dcb_Binary;
    if FDiscardNull then
      DCB.Flags := DCB.Flags or dcb_Null;

    with FFlowControl do
    begin
      DCB.XonChar := AnsiChar(XonChar);
      DCB.XoffChar := AnsiChar(XoffChar);
      if OutCTSFlow then
        DCB.Flags := DCB.Flags or dcb_OutxCTSFlow;
      if OutDSRFlow then
        DCB.Flags := DCB.Flags or dcb_OutxDSRFlow;
      DCB.Flags := DCB.Flags or CControlDTR[ControlDTR]
        or CControlRTS[ControlRTS];
      if XonXoffOut then
        DCB.Flags := DCB.Flags or dcb_OutX;
      if XonXoffIn then
        DCB.Flags := DCB.Flags or dcb_InX;
      if DSRSensitivity then
        DCB.Flags := DCB.Flags or dcb_DSRSensivity;
      if TxContinueOnXoff then
        DCB.Flags := DCB.Flags or dcb_TxContinueOnXoff;
    end;

    DCB.Parity := CParityBits[FParity.Bits];
    DCB.StopBits := CStopBits[FStopBits];
    if FBaudRate <> brCustom then
      DCB.BaudRate := CBaudRate[FBaudRate]
    else
      DCB.BaudRate := FCustomBaudRate;
    DCB.ByteSize := CDataBits[FDataBits];

    if FParity.Check then
    begin
      DCB.Flags := DCB.Flags or dcb_Parity;
      if FParity.Replace then
      begin
        DCB.Flags := DCB.Flags or dcb_ErrorChar;
        DCB.ErrorChar := AnsiChar(FParity.ReplaceChar);
      end;
    end;

    // apply settings
    if not SetCommState(FHandle, DCB) then
      //raise EUSBPort.Create
      CallException(CError_SetStateFailed, GetLastError);
  end;
end;

// apply timeout properties
procedure TCustomUSBPort.ApplyTimeouts;
var
  Timeouts: TCommTimeouts;

  function GetTOValue(const Value: Integer): DWORD;
  begin
    if Value = -1 then
      Result := MAXDWORD
    else
      Result := Value;
  end;

begin
  // if not connected or inside BeginUpdate/EndUpdate block, do nothing
  if FConnected and (FUpdateCount = 0) and
    not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then
  begin
    Timeouts.ReadIntervalTimeout := GetTOValue(FTimeouts.ReadInterval);
    Timeouts.ReadTotalTimeoutMultiplier := GetTOValue(FTimeouts.ReadTotalMultiplier);
    Timeouts.ReadTotalTimeoutConstant := GetTOValue(FTimeouts.ReadTotalConstant);
    Timeouts.WriteTotalTimeoutMultiplier := GetTOValue(FTimeouts.WriteTotalMultiplier);
    Timeouts.WriteTotalTimeoutConstant := GetTOValue(FTimeouts.WriteTotalConstant);

    // apply settings
    if not SetCommTimeouts(FHandle, Timeouts) then
      //raise EUSBPort.Create
      CallException(CError_TimeoutsFailed, GetLastError);
  end;
end;

// apply buffers
procedure TCustomUSBPort.ApplyBuffer;
begin
  // if not connected or inside BeginUpdate/EndUpdate block, do nothing
  if FConnected and (FUpdateCount = 0) and
      not ((csDesigning in ComponentState) or (csLoading in ComponentState))
  then
    //apply settings
    if not SetupComm(FHandle, FBuffer.InputSize, FBuffer.OutputSize) then
      //raise EUSBPort.Create
      CallException(CError_SetupComFailed, GetLastError);
end;

// initialize port
procedure TCustomUSBPort.SetupUSBPort;
begin
  ApplyBuffer;
  ApplyDCB;
  ApplyTimeouts;
end;

// get number of bytes in input buffer
function TCustomUSBPort.InputCount: Integer;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    //raise EUSBPort.Create
    CallException(CError_ClearComFailed, GetLastError);
  Result := ComStat.cbInQue;
end;

// get number of bytes in output buffer
function TCustomUSBPort.OutputCount: Integer;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    //raise EUSBPort.Create
    CallException(CError_ClearComFailed, GetLastError);
  Result := ComStat.cbOutQue;
end;

// get signals which are in high state
function TCustomUSBPort.Signals: TUSBSignals;
var
  Status: DWORD;
begin
  if not GetCommModemStatus(FHandle, Status) then
    //raise EUSBPort.Create
    CallException(CError_ModemStatFailed, GetLastError);
  Result := [];

  if (MS_CTS_ON and Status) <> 0 then
    Result := Result + [csCTS];
  if (MS_DSR_ON and Status) <> 0 then
    Result := Result + [csDSR];
  if (MS_RING_ON and Status) <> 0 then
    Result := Result + [csRing];
  if (MS_RLSD_ON and Status) <> 0 then
    Result := Result + [csRLSD];
end;

// get port state flags
function TCustomUSBPort.StateFlags: TComStateFlags;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    //raise EUSBPort.Create
    CallException(CError_ClearComFailed, GetLastError);
  Result := ComStat.Flags;
end;

// set hardware line break
procedure TCustomUSBPort.SetBreak(OnOff: Boolean);
var
  Act: Integer;
begin
  if OnOff then
    Act := Windows.SETBREAK
  else
    Act := Windows.CLRBREAK;

  if not EscapeCommFunction(FHandle, Act) then
    //raise EUSBPort.Create
    CallException(CError_EscapeComFailed, GetLastError);
end;

// set DTR signal
procedure TCustomUSBPort.SetDTR(OnOff: Boolean);
var
  Act: DWORD;
begin
  if OnOff then
    Act := Windows.SETDTR
  else
    Act := Windows.CLRDTR;

  if not EscapeCommFunction(FHandle, Act) then
    //raise EUSBPort.Create
    CallException(CError_EscapeComFailed, GetLastError);
end;

// set RTS signals
procedure TCustomUSBPort.SetRTS(OnOff: Boolean);
var
  Act: DWORD;
begin
  if OnOff then
    Act := Windows.SETRTS
  else
    Act := Windows.CLRRTS;

  if not EscapeCommFunction(FHandle, Act) then
    //raise EUSBPort.Create
    CallException(CError_EscapeComFailed, GetLastError);
end;

// set XonXoff state
procedure TCustomUSBPort.SetXonXoff(OnOff: Boolean);
var
  Act: DWORD;
begin
  if OnOff then
    Act := Windows.SETXON
  else
    Act := Windows.SETXOFF;

  if not EscapeCommFunction(FHandle, Act) then
    //raise EUSBPort.Create
    CallException(CError_EscapeComFailed, GetLastError);
end;

// clear input and/or output buffer
procedure TCustomUSBPort.ClearBuffer(Input, Output: Boolean);
var
  Flag: DWORD;
begin
  Flag := 0;
  if Input then
    Flag := PURGE_RXCLEAR;
  if Output then
    Flag := Flag or PURGE_TXCLEAR;

  if not PurgeComm(FHandle, Flag) then
    //raise EUSBPort.Create
    CallException(CError_PurgeFailed, GetLastError);
end;

// return last errors on port
function TCustomUSBPort.LastErrors: TUSBErrors;
var
  Errors: DWORD;
  ComStat: TComStat;
begin
  if not ClearCommError(FHandle, Errors, @ComStat) then
    //raise EUSBPort.Create
    CallException(CError_ClearComFailed, GetLastError);
  Result := [];

  if (CE_FRAME and Errors) <> 0 then
    Result := Result + [ceFrame];
  if ((CE_RXPARITY and Errors) <> 0) and FParity.Check then // get around a bug
    Result := Result + [ceRxParity];
  if (CE_OVERRUN and Errors) <> 0 then
    Result := Result + [ceOverrun];
  if (CE_RXOVER and Errors) <> 0 then
    Result := Result + [ceRxOver];
  if (CE_TXFULL and Errors) <> 0 then
    Result := Result + [ceTxFull];
  if (CE_BREAK and Errors) <> 0 then
    Result := Result + [ceBreak];
  if (CE_IOE and Errors) <> 0 then
    Result := Result + [ceIO];
  if (CE_MODE and Errors) <> 0 then
    Result := Result + [ceMode];
end;

// prepare PAsync variable for read/write operation
procedure PrepareAsync(AKind: TOperationKind; const Buffer; Count: Integer; AsyncPtr: PAsync);
begin
  with AsyncPtr^ do
  begin
    Kind := AKind;
    if Data <> nil then
      FreeMem(Data);
    GetMem(Data, Count);
    Move(Buffer, Data^, Count);
    Size := Count;
  end;
end;

// perform asynchronous write operation
function TCustomUSBPort.WriteAsync(const Buffer; Count: Integer; var AsyncPtr: PAsync): Integer;
var
  Success: Boolean;
  BytesTrans: DWORD;
begin
  if AsyncPtr = nil then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_InvalidAsync);
  if FHandle = INVALID_HANDLE_VALUE then
    //raise EUSBPort.Create
    CallException(CError_PortNotOpen, -24);
  PrepareAsync(okWrite, Buffer, Count, AsyncPtr);

  Success := WriteFile(FHandle, Buffer, Count, BytesTrans, @AsyncPtr^.Overlapped)
    or (GetLastError = ERROR_IO_PENDING);

  if not Success then
    //raise EUSBPort.Create
    CallException(CError_WriteFailed, GetLastError);

  SendSignalToLink(leTx, True);
  Result := BytesTrans;
end;

// perform synchronous write operation
function TCustomUSBPort.Write(const Buffer; Count: Integer): Integer;
var
  AsyncPtr: PAsync;
begin
  InitAsync(AsyncPtr);
  try
    WriteAsync(Buffer, Count, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
  finally
    DoneAsync(AsyncPtr);
  end;
end;

// perform asynchronous write operation
function TCustomUSBPort.WriteStrAsync(const Str: string; var AsyncPtr: PAsync): Integer;
var sa : Ansistring;
begin
  if Length(Str) > 0 then
  begin
    sa := AnsiString(str);
    Result := WriteAsync(Sa[1], Length(Str), AsyncPtr)
  end
  else
    Result := 0;
end;
// perform synchronous write operation
function TCustomUSBPort.WriteStr(const Str: string): Integer;
var
  AsyncPtr: PAsync;
begin
  InitAsync(AsyncPtr);
  try
    WriteStrAsync(Str, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
  finally
    DoneAsync(AsyncPtr);
  end;
end;
//Pierre Yager - includes codepage converstion of strings being sent
function TCustomUSBPort.WriteUnicodeString(const Str: Unicodestring): Integer;
var
  l: Integer;
  rb: AnsiString;
begin
  l := WideCharToMultiByte(FCodePage, 0, PWideChar(Str), Length(Str), nil, 0, nil, nil);
  SetLength(rb, l);
  WideCharToMultiByte(FCodePage, 0, PWideChar(Str), Length(Str), PAnsiChar(rb), l, nil, nil);
  Result := WriteStr(string(rb));
end;

//Pierre Yager - includes codepage converstion of strings received
function TCustomUSBPort.ReadUnicodeString(var Str: UnicodeString; Count: Integer): Integer;
var
  rb: AnsiString;
  s : string;
  l: Integer;
begin
  setLength(s,count);
  ReadStr(s, Count);
  {$IFDEF Unicode}rb := UTF8Encode(s);{$ELSE} rb := s;  {$ENDIF}
  l := MultiByteToWideChar(FCodePage, 0, PAnsiChar(rb), Length(rb), nil, 0);
  SetLength(Str, l);
  Result := MultiByteToWideChar(FCodePage, 0, PAnsiChar(rb), Length(rb), PWideChar(Str), l);
end;

// perform asynchronous read operation
function TCustomUSBPort.ReadAsync(var Buffer; Count: Integer; var AsyncPtr: PAsync): Integer;
var
  Success: Boolean;
  BytesTrans: DWORD;
begin
  if AsyncPtr = nil then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_InvalidAsync);
  AsyncPtr^.Kind := okRead;
  if FHandle = INVALID_HANDLE_VALUE then
    //raise EUSBPort.Create
    CallException(CError_PortNotOpen, -24);

  Success := ReadFile(FHandle, Buffer, Count, BytesTrans, @AsyncPtr^.Overlapped)
    or (GetLastError = ERROR_IO_PENDING);

  if not Success then
    //raise EUSBPort.Create
    CallException(CError_ReadFailed, GetLastError);

  Result := BytesTrans;
end;

// perform synchronous read operation
function TCustomUSBPort.Read(var Buffer; Count: Integer): Integer;
var
  AsyncPtr: PAsync;
begin
  InitAsync(AsyncPtr);
  try
    ReadAsync(Buffer, Count, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
  finally
    DoneAsync(AsyncPtr);
  end;
end;

// perform asynchronous read operation
function TCustomUSBPort.ReadStrAsync(var Str: string; Count: Integer; var AsyncPtr: PAsync): Integer;
var sa :ansistring;
begin
  SetLength(Str, Count);
  if Count > 0 then
  begin
    sa := AnsiString(Str);
    Result := ReadAsync(sa[1], Count, AsyncPtr);
    str := String(Sa);
  end
  else
    Result := 0;
end;

// perform synchronous read operation
function TCustomUSBPort.ReadStr(var Str: string; Count: Integer): Integer;
var
  AsyncPtr: PAsync;
begin
  InitAsync(AsyncPtr);
  try
    ReadStrAsync(Str, Count, AsyncPtr);
    Result := WaitForAsync(AsyncPtr);
    SetLength(Str, Result);
  finally
    DoneAsync(AsyncPtr);
  end;
end;

function ErrorCode(AsyncPtr: PAsync): Integer;
begin
  Result := 0;
  case AsyncPtr^.Kind of
    okWrite: Result := CError_WriteFailed;
    okRead:  Result := CError_ReadFailed;
  end;
end;

// wait for asynchronous operation to end
function TCustomUSBPort.WaitForAsync(var AsyncPtr: PAsync): Integer;
var
  BytesTrans, Signaled: DWORD;
  Success: Boolean;
begin
  if AsyncPtr = nil then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_InvalidAsync);

  Signaled := WaitForSingleObject(AsyncPtr^.Overlapped.hEvent, INFINITE);
  Success := (Signaled = WAIT_OBJECT_0) and
      (GetOverlappedResult(FHandle, AsyncPtr^.Overlapped, BytesTrans, False));

  if not Success then
    //raise EUSBPort.Create
    CallException(ErrorCode(AsyncPtr), GetLastError);

  if (AsyncPtr^.Kind = okRead) and (InputCount = 0) then
    SendSignalToLink(leRx, False)
  else
    if AsyncPtr^.Data <> nil then
      TxNotifyLink(AsyncPtr^.Data^, AsyncPtr^.Size);

  Result := BytesTrans;
end;

// abort all asynchronous operations
procedure TCustomUSBPort.AbortAllAsync;
begin
  if not PurgeComm(FHandle, PURGE_TXABORT or PURGE_RXABORT) then
    //raise EUSBPort.Create
    CallException(CError_PurgeFailed, GetLastError);
end;

// detect whether asynchronous operation is completed
function TCustomUSBPort.IsAsyncCompleted(AsyncPtr: PAsync): Boolean;
var
  BytesTrans: DWORD;
begin
  if AsyncPtr = nil then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_InvalidAsync);

  Result := GetOverlappedResult(FHandle, AsyncPtr^.Overlapped, BytesTrans, False);
  if not Result then
    if (GetLastError <> ERROR_IO_PENDING) and (GetLastError <> ERROR_IO_INCOMPLETE) then
      //raise EUSBPort.Create
      CallException(CError_AsyncCheck, GetLastError);
end;

// waits for event to occur on serial port
procedure TCustomUSBPort.WaitForEvent(var Events: TUSBEvents;
  StopEvent: THandle; Timeout: Integer);
var
  Overlapped: TOverlapped;
  Mask: DWORD;
  Success: Boolean;
  Signaled, EventHandleCount: Integer;
  EventHandles: array[0..1] of THandle;
begin
  // cannot call method if event thread is running
  if FThreadCreated then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_ThreadCreated);

  FillChar(Overlapped, SizeOf(TOverlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, False, nil);
  EventHandles[0] := Overlapped.hEvent;
  if StopEvent <> 0 then
  begin
    EventHandles[1] := StopEvent;
    EventHandleCount := 2;
  end
  else
    EventHandleCount := 1;

  try
    SetCommMask(FHandle, EventsToInt(Events));
    // let's wait for event or timeout
    Success := WaitCommEvent(FHandle, Mask, @Overlapped);

    if (Success) or (GetLastError = ERROR_IO_PENDING) then
    begin
      Signaled := WaitForMultipleObjects(EventHandleCount, @EventHandles,
        False, Timeout);
      Success := (Signaled = WAIT_OBJECT_0)
        or (Signaled = WAIT_OBJECT_0 + 1) or (Signaled = WAIT_TIMEOUT);
      SetCommMask(FHandle, 0);
    end;

    if not Success then
      //raise EUSBPort.Create
      CallException(CError_WaitFailed, GetLastError);

    Events := IntToEvents(Mask);
  finally
    CloseHandle(Overlapped.hEvent);
  end;
end;

// transmit char ahead of any pending data in ouput buffer
procedure TCustomUSBPort.TransmitChar(Ch: Char);
begin
  if not TransmitCommChar(FHandle, AnsiChar(Ch)) then
    //raise EUSBPort.Create
    CallException(CError_TransmitFailed, GetLastError);
end;

// some conversion routines
function BoolToStr(const Value: Boolean): string;
begin
  if Value then
    Result := 'Yes'
  else
    Result := 'No';
end;

function StrToBool(const Value: string): Boolean;
begin
  if UpperCase(Value) = 'YES' then
    Result := True
  else
    Result := False;
end;

function DTRToStr(DTRFlowControl: TDTRFlowControl): string;
const
  DTRStrings: array[TDTRFlowControl] of string = ('Disable', 'Enable',
    'Handshake');
begin
  Result := DTRStrings[DTRFlowControl];
end;

function RTSToStr(RTSFlowControl: TRTSFlowControl): string;
const
  RTSStrings: array[TRTSFlowControl] of string = ('Disable', 'Enable',
    'Handshake', 'Toggle');
begin
  Result := RTSStrings[RTSFlowControl];
end;

function StrToRTS(Str: string): TRTSFlowControl;
var
  I: TRTSFlowControl;
begin
  I := Low(TRTSFlowControl);
  while (I <= High(TRTSFlowControl)) do
  begin
    if UpperCase(Str) = UpperCase(RTSToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TRTSFlowControl) then
    Result := rtsDisable
  else
    Result := I;
end;

function StrToDTR(Str: string): TDTRFlowControl;
var
  I: TDTRFlowControl;
begin
  I := Low(TDTRFlowControl);
  while (I <= High(TDTRFlowControl)) do
  begin
    if UpperCase(Str) = UpperCase(DTRToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TDTRFlowControl) then
    Result := dtrDisable
  else
    Result := I;
end;

function StrToChar(Str: string): Char;
var
  A: Integer;
begin
  if Length(Str) > 0 then
  begin
    if (Str[1] = '#') and (Length(Str) > 1) then
    begin
      try
        A := StrToInt(Copy(Str, 2, Length(Str) - 1));
      except
        A := 0;
      end;
      Result := Chr(Byte(A));
    end
    else
      Result := Str[1];
  end
  else
    Result := #0;
end;

function CharToStr(Ch: Char): string;
begin
  {$IFDEF Unicode}
  if CharInSet(ch,[#33..#127]) then
  {$ELSE}
  if Ch in [#33..#127] then {$ENDIF}
    Result := Ch
  else
    Result := '#' + IntToStr(Ord(Ch));
end;

// store settings to ini file
procedure TCustomUSBPort.StoreIniFile(IniFile: TIniFile);
begin
  if spBasic in FStoredProps then
  begin
    IniFile.WriteString(Name, 'Port', Port);
    IniFile.WriteString(Name, 'BaudRate', BaudRateToStr(BaudRate));
    if BaudRate = brCustom then
      IniFile.WriteInteger(Name, 'CustomBaudRate', CustomBaudRate);
    IniFile.WriteString(Name, 'StopBits', StopBitsToStr(StopBits));
    IniFile.WriteString(Name, 'DataBits', DataBitsToStr(DataBits));
    IniFile.WriteString(Name, 'Parity', ParityToStr(Parity.Bits));
    IniFile.WriteString(Name, 'FlowControl', FlowControlToStr(FlowControl.FlowControl));
  end;
  if spOthers in FStoredProps then
  begin
    IniFile.WriteString(Name, 'EventChar', CharToStr(EventChar));
    IniFile.WriteString(Name, 'DiscardNull', BoolToStr(DiscardNull));
  end;
  if spParity in FStoredProps then
  begin
    IniFile.WriteString(Name, 'Parity.Check', BoolToStr(Parity.Check));
    IniFile.WriteString(Name, 'Parity.Replace', BoolToStr(Parity.Replace));
    IniFile.WriteString(Name, 'Parity.ReplaceChar', CharToStr(Parity.ReplaceChar));
  end;
  if spBuffer in FStoredProps then
  begin
    IniFile.WriteInteger(Name, 'Buffer.OutputSize', Buffer.OutputSize);
    IniFile.WriteInteger(Name, 'Buffer.InputSize', Buffer.InputSize);
  end;
  if spTimeouts in FStoredProps then
  begin
    IniFile.WriteInteger(Name, 'Timeouts.ReadInterval', Timeouts.ReadInterval);
    IniFile.WriteInteger(Name, 'Timeouts.ReadTotalConstant', Timeouts.ReadTotalConstant);
    IniFile.WriteInteger(Name, 'Timeouts.ReadTotalMultiplier', Timeouts.ReadTotalMultiplier);
    IniFile.WriteInteger(Name, 'Timeouts.WriteTotalConstant', Timeouts.WriteTotalConstant);
    IniFile.WriteInteger(Name, 'Timeouts.WriteTotalMultiplier', Timeouts.WriteTotalMultiplier);
  end;
  if spFlowControl in FStoredProps then
  begin
    IniFile.WriteString(Name, 'FlowControl.ControlRTS', RTSToStr(FlowControl.ControlRTS));
    IniFile.WriteString(Name, 'FlowControl.ControlDTR', DTRToStr(FlowControl.ControlDTR));
    IniFile.WriteString(Name, 'FlowControl.DSRSensitivity', BoolToStr(FlowControl.DSRSensitivity));
    IniFile.WriteString(Name, 'FlowControl.OutCTSFlow', BoolToStr(FlowControl.OutCTSFlow));
    IniFile.WriteString(Name, 'FlowControl.OutDSRFlow', BoolToStr(FlowControl.OutDSRFlow));
    IniFile.WriteString(Name, 'FlowControl.TxContinueOnXoff', BoolToStr(FlowControl.TxContinueOnXoff));
    IniFile.WriteString(Name, 'FlowControl.XonXoffIn', BoolToStr(FlowControl.XonXoffIn));
    IniFile.WriteString(Name, 'FlowControl.XonXoffOut', BoolToStr(FlowControl.XonXoffOut));
    IniFile.WriteString(Name, 'FlowControl.XoffChar', CharToStr(FlowControl.XoffChar));
    IniFile.WriteString(Name, 'FlowControl.XonChar', CharToStr(FlowControl.XonChar));
  end;
end;

// store settings to registry
procedure TCustomUSBPort.StoreRegistry(Reg: TRegistry);
begin
  if spBasic in FStoredProps then
  begin
    Reg.WriteString('Port', Port);
    Reg.WriteString('BaudRate', BaudRateToStr(BaudRate));
    if BaudRate = brCustom then
      Reg.WriteInteger('CustomBaudRate', CustomBaudRate);
    Reg.WriteString('StopBits', StopBitsToStr(StopBits));
    Reg.WriteString('DataBits', DataBitsToStr(DataBits));
    Reg.WriteString('Parity', ParityToStr(Parity.Bits));
    Reg.WriteString('FlowControl', FlowControlToStr(FlowControl.FlowControl));
  end;
  if spOthers in FStoredProps then
  begin
    Reg.WriteString('EventChar', CharToStr(EventChar));
    Reg.WriteString('DiscardNull', BoolToStr(DiscardNull));
  end;
  if spParity in FStoredProps then
  begin
    Reg.WriteString('Parity.Check', BoolToStr(Parity.Check));
    Reg.WriteString('Parity.Replace', BoolToStr(Parity.Replace));
    Reg.WriteString('Parity.ReplaceChar', CharToStr(Parity.ReplaceChar));
  end;
  if spBuffer in FStoredProps then
  begin
    Reg.WriteInteger('Buffer.OutputSize', Buffer.OutputSize);
    Reg.WriteInteger('Buffer.InputSize', Buffer.InputSize);
  end;
  if spTimeouts in FStoredProps then
  begin
    Reg.WriteInteger('Timeouts.ReadInterval', Timeouts.ReadInterval);
    Reg.WriteInteger('Timeouts.ReadTotalConstant', Timeouts.ReadTotalConstant);
    Reg.WriteInteger('Timeouts.ReadTotalMultiplier', Timeouts.ReadTotalMultiplier);
    Reg.WriteInteger('Timeouts.WriteTotalConstant', Timeouts.WriteTotalConstant);
    Reg.WriteInteger('Timeouts.WriteTotalMultiplier', Timeouts.WriteTotalMultiplier);
  end;
  if spFlowControl in FStoredProps then
  begin
    Reg.WriteString('FlowControl.ControlRTS', RTSToStr(FlowControl.ControlRTS));
    Reg.WriteString('FlowControl.ControlDTR', DTRToStr(FlowControl.ControlDTR));
    Reg.WriteString('FlowControl.DSRSensitivity', BoolToStr(FlowControl.DSRSensitivity));
    Reg.WriteString('FlowControl.OutCTSFlow', BoolToStr(FlowControl.OutCTSFlow));
    Reg.WriteString('FlowControl.OutDSRFlow', BoolToStr(FlowControl.OutDSRFlow));
    Reg.WriteString('FlowControl.TxContinueOnXoff', BoolToStr(FlowControl.TxContinueOnXoff));
    Reg.WriteString('FlowControl.XonXoffIn', BoolToStr(FlowControl.XonXoffIn));
    Reg.WriteString('FlowControl.XonXoffOut', BoolToStr(FlowControl.XonXoffOut));
    Reg.WriteString('FlowControl.XoffChar', CharToStr(FlowControl.XoffChar));
    Reg.WriteString('FlowControl.XonChar', CharToStr(FlowControl.XonChar));
  end;
end;

// load settings from ini file
procedure TCustomUSBPort.LoadIniFile(IniFile: TIniFile);
begin
  if spBasic in FStoredProps then
  begin
    Port := IniFile.ReadString(Name, 'Port', Port);
    BaudRate := StrToBaudRate(IniFile.ReadString(Name, 'BaudRate', BaudRateToStr(BaudRate)));
    if BaudRate = brCustom then
      CustomBaudRate := IniFile.ReadInteger(Name, 'CustomBaudRate', 9600);
    StopBits := StrToStopBits(IniFile.ReadString(Name, 'StopBits', StopBitsToStr(StopBits)));
    DataBits := StrToDataBits(IniFile.ReadString(Name, 'DataBits', DataBitsToStr(DataBits)));
    Parity.Bits := StrToParity(IniFile.ReadString(Name, 'Parity', ParityToStr(Parity.Bits)));
    FlowControl.FlowControl := StrToFlowControl(
      IniFile.ReadString(Name, 'FlowControl', FlowControlToStr(FlowControl.FlowControl)));
  end;
  if spOthers in FStoredProps then
  begin
    EventChar := StrToChar(IniFile.ReadString(Name, 'EventChar', CharToStr(EventChar)));
    DiscardNull := StrToBool(IniFile.ReadString(Name, 'DiscardNull', BoolToStr(DiscardNull)));
  end;
  if spParity in FStoredProps then
  begin
    Parity.Check := StrToBool(IniFile.ReadString(Name, 'Parity.Check', BoolToStr(Parity.Check)));
    Parity.Replace := StrToBool(IniFile.ReadString(Name, 'Parity.Replace', BoolToStr(Parity.Replace)));
    Parity.ReplaceChar := StrToChar(IniFile.ReadString(Name, 'Parity.ReplaceChar', CharToStr(Parity.ReplaceChar)));
  end;
  if spBuffer in FStoredProps then
  begin
    Buffer.OutputSize := IniFile.ReadInteger(Name, 'Buffer.OutputSize', Buffer.OutputSize);
    Buffer.InputSize := IniFile.ReadInteger(Name, 'Buffer.InputSize', Buffer.InputSize);
  end;
  if spTimeouts in FStoredProps then
  begin
    Timeouts.ReadInterval := IniFile.ReadInteger(Name, 'Timeouts.ReadInterval', Timeouts.ReadInterval);
    Timeouts.ReadTotalConstant := IniFile.ReadInteger(Name, 'Timeouts.ReadTotalConstant', Timeouts.ReadTotalConstant);
    Timeouts.ReadTotalMultiplier := IniFile.ReadInteger(Name, 'Timeouts.ReadTotalMultiplier', Timeouts.ReadTotalMultiplier);
    Timeouts.WriteTotalConstant := IniFile.ReadInteger(Name, 'Timeouts.WriteTotalConstant', Timeouts.WriteTotalConstant);
    Timeouts.WriteTotalMultiplier := IniFile.ReadInteger(Name, 'Timeouts.WriteTotalMultiplier', Timeouts.WriteTotalMultiplier);
  end;
  if spFlowControl in FStoredProps then
  begin
    FlowControl.ControlRTS := StrToRTS(IniFile.ReadString(Name, 'FlowControl.ControlRTS', RTSToStr(FlowControl.ControlRTS)));
    FlowControl.ControlDTR := StrToDTR(IniFile.ReadString(Name, 'FlowControl.ControlDTR', DTRToStr(FlowControl.ControlDTR)));
    FlowControl.DSRSensitivity := StrToBool(IniFile.ReadString(Name, 'FlowControl.DSRSensitivity', BoolToStr(FlowControl.DSRSensitivity)));
    FlowControl.OutCTSFlow := StrToBool(IniFile.ReadString(Name, 'FlowControl.OutCTSFlow', BoolToStr(FlowControl.OutCTSFlow)));
    FlowControl.OutDSRFlow := StrToBool(IniFile.ReadString(Name, 'FlowControl.OutDSRFlow', BoolToStr(FlowControl.OutCTSFlow)));
    FlowControl.TxContinueOnXoff := StrToBool(IniFile.ReadString(Name, 'FlowControl.TxContinueOnXoff', BoolToStr(FlowControl.TxContinueOnXoff)));
    FlowControl.XonXoffIn := StrToBool(IniFile.ReadString(Name, 'FlowControl.XonXoffIn', BoolToStr(FlowControl.XonXoffIn)));
    FlowControl.XonXoffOut := StrToBool(IniFile.ReadString(Name, 'FlowControl.XonXoffOut', BoolToStr(FlowControl.XonXoffOut)));
    FlowControl.XoffChar := StrToChar(IniFile.ReadString(Name, 'FlowControl.XoffChar', CharToStr(FlowControl.XoffChar)));
    FlowControl.XonChar := StrToChar(IniFile.ReadString(Name, 'FlowControl.XonChar', CharToStr(FlowControl.XonChar)));
  end;
end;

// load settings from registry
procedure TCustomUSBPort.LoadRegistry(Reg: TRegistry);
begin
  if spBasic in FStoredProps then
  begin
    Port := Reg.ReadString('Port');
    BaudRate := StrToBaudRate(Reg.ReadString('BaudRate'));
    if BaudRate = brCustom then
      CustomBaudRate := Reg.ReadInteger('CustomBaudRate');
    StopBits := StrToStopBits(Reg.ReadString('StopBits'));
    DataBits := StrToDataBits(Reg.ReadString('DataBits'));
    Parity.Bits := StrToParity(Reg.ReadString('Parity'));
    FlowControl.FlowControl := StrToFlowControl(Reg.ReadString('FlowControl'));
  end;
  if spOthers in FStoredProps then
  begin
    EventChar := StrToChar(Reg.ReadString('EventChar'));
    DiscardNull := StrToBool(Reg.ReadString('DiscardNull'));
  end;
  if spParity in FStoredProps then
  begin
    Parity.Check := StrToBool(Reg.ReadString('Parity.Check'));
    Parity.Replace := StrToBool(Reg.ReadString('Parity.Replace'));
    Parity.ReplaceChar := StrToChar(Reg.ReadString('Parity.ReplaceChar'));
  end;
  if spBuffer in FStoredProps then
  begin
    Buffer.OutputSize := Reg.ReadInteger('Buffer.OutputSize');
    Buffer.InputSize := Reg.ReadInteger('Buffer.InputSize');
  end;
  if spTimeouts in FStoredProps then
  begin
    Timeouts.ReadInterval := Reg.ReadInteger('Timeouts.ReadInterval');
    Timeouts.ReadTotalConstant := Reg.ReadInteger('Timeouts.ReadTotalConstant');
    Timeouts.ReadTotalMultiplier := Reg.ReadInteger('Timeouts.ReadTotalMultiplier');
    Timeouts.WriteTotalConstant := Reg.ReadInteger('Timeouts.WriteTotalConstant');
    Timeouts.WriteTotalMultiplier := Reg.ReadInteger('Timeouts.WriteTotalMultiplier');
  end;
  if spFlowControl in FStoredProps then
  begin
    FlowControl.ControlRTS := StrToRTS(Reg.ReadString('FlowControl.ControlRTS'));
    FlowControl.ControlDTR := StrToDTR(Reg.ReadString('FlowControl.ControlDTR'));
    FlowControl.DSRSensitivity := StrToBool(Reg.ReadString('FlowControl.DSRSensitivity'));
    FlowControl.OutCTSFlow := StrToBool(Reg.ReadString('FlowControl.OutCTSFlow'));
    FlowControl.OutDSRFlow := StrToBool(Reg.ReadString('FlowControl.OutDSRFlow'));
    FlowControl.TxContinueOnXoff := StrToBool(Reg.ReadString('FlowControl.TxContinueOnXoff'));
    FlowControl.XonXoffIn := StrToBool(Reg.ReadString('FlowControl.XonXoffIn'));
    FlowControl.XonXoffOut := StrToBool(Reg.ReadString('FlowControl.XonXoffOut'));
    FlowControl.XoffChar := StrToChar(Reg.ReadString('FlowControl.XoffChar'));
    FlowControl.XonChar := StrToChar(Reg.ReadString('FlowControl.XonChar'));
  end;
end;

// initialize registry
procedure SetRegistry(Reg: TRegistry; Key: string; Name: string);
var
  I: Integer;
  Temp: string;
begin
  I := Pos('\', Key);
  if I > 0 then
  begin
    Temp := Copy(Key, 1, I - 1);
    if UpperCase(Temp) = 'HKEY_LOCAL_MACHINE' then
      Reg.RootKey := HKEY_LOCAL_MACHINE
    else
      if UpperCase(Temp) = 'HKEY_CURRENT_USER' then
        Reg.RootKey := HKEY_CURRENT_USER;
    Key := Copy(Key, I + 1, Length(Key) - I);
    if Key[Length(Key)] <> '\' then
      Key := Key + '\';
    Key := Key + Name;
    Reg.OpenKey(Key, True);
  end;
end;

// store settings
procedure TCustomUSBPort.StoreSettings(StoreType: TStoreType; StoreTo: string);
var
  IniFile: TIniFile;
  Reg: TRegistry;
begin
  try
    if StoreType = stRegistry then
    begin
      Reg := TRegistry.Create;
      try
        SetRegistry(Reg, StoreTo, Name);
        StoreRegistry(Reg);
      finally
        Reg.Free;
      end
    end else
    begin
      IniFile := TIniFile.Create(StoreTo);
      try
        StoreIniFile(IniFile);
      finally
        IniFile.Free;
      end
    end;
  except
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_StoreFailed);
  end;
end;

// load settings
procedure TCustomUSBPort.LoadSettings(StoreType: TStoreType; LoadFrom: string);
var
  IniFile: TIniFile;
  Reg: TRegistry;
begin
  BeginUpdate;
  try
    try
      if StoreType = stRegistry then
      begin
        Reg := TRegistry.Create;
        try
          SetRegistry(Reg, LoadFrom, Name);
          LoadRegistry(Reg);
        finally
          Reg.Free;
        end
      end else
      begin
        IniFile := TIniFile.Create(LoadFrom);
        try
          LoadIniFile(IniFile);
        finally
          IniFile.Free;
        end
      end;
    finally
      EndUpdate;
    end;
  except
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_LoadFailed);
  end;
end;

// register link from other component to TCustomUSBPort
procedure TCustomUSBPort.RegisterLink(AComLink: TUSBLink);
begin
  if FLinks.IndexOf(Pointer(AComLink)) > -1 then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_RegFailed)
  else
    FLinks.Add(Pointer(AComLink));
  FHasLink := HasLink;
end;

// unregister link from other component to TCustomUSBPort
procedure TCustomUSBPort.UnRegisterLink(AComLink: TUSBLink);
begin
  if FLinks.IndexOf(Pointer(AComLink)) = -1 then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_RegFailed)
  else
    FLinks.Remove(Pointer(AComLink));
  FHasLink := HasLink;
end;

// default actions on port events

procedure TCustomUSBPort.DoBeforeClose;
begin
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self);
end;

procedure TCustomUSBPort.DoBeforeOpen;
begin
  if Assigned(FOnBeforeOpen) then
    FOnBeforeOpen(Self);
end;

procedure TCustomUSBPort.DoAfterOpen;
begin
  if Assigned(FOnAfterOpen) then
    FOnAfterOpen(Self);
end;

procedure TCustomUSBPort.DoAfterClose;
begin
  if Assigned(FOnAfterClose) then
    FOnAfterClose(Self);
end;

procedure TCustomUSBPort.DoRxChar(Count: Integer);
begin
  if Assigned(FOnRxChar) then
    FOnRxChar(Self, Count);
end;

procedure TCustomUSBPort.DoRxBuf(const Buffer; Count: Integer);
begin
  if Assigned(FOnRxBuf) then
    FOnRxBuf(Self, Buffer, Count);
end;

procedure TCustomUSBPort.DoBreak;
begin
  if Assigned(FOnBreak) then
    FOnBreak(Self);
end;

procedure TCustomUSBPort.DoTxEmpty;
begin
  if Assigned(FOnTxEmpty)
    then FOnTxEmpty(Self);
end;

procedure TCustomUSBPort.DoRing;
begin
  if Assigned(FOnRing) then
    FOnRing(Self);
end;

procedure TCustomUSBPort.DoCTSChange(OnOff: Boolean);
begin
  if Assigned(FOnCTSChange) then
    FOnCTSChange(Self, OnOff);
end;

procedure TCustomUSBPort.DoDSRChange(OnOff: Boolean);
begin
  if Assigned(FOnDSRChange) then
    FOnDSRChange(Self, OnOff);
end;

procedure TCustomUSBPort.DoRLSDChange(OnOff: Boolean);
begin
  if Assigned(FOnRLSDChange) then
    FOnRLSDChange(Self, OnOff);
end;

procedure TCustomUSBPort.DoError(Errors: TUSBErrors);
begin
  if Assigned(FOnError) then
    FOnError(Self, Errors);
end;

procedure TCustomUSBPort.DoRxFlag;
begin
  if Assigned(FOnRxFlag) then
    FOnRxFlag(Self);
end;

procedure TCustomUSBPort.DoRx80Full;
begin
  if Assigned(FOnRx80Full) then
    FOnRx80Full(Self);
end;

// set signals to false on close, and to proper value on open,
// because OnXChange events are not called automatically
procedure TCustomUSBPort.CheckSignals(Open: Boolean);
begin
  if Open then
  begin
    CallCTSChange;
    CallDSRChange;
    CallRLSDChange;
  end else
  begin
    SendSignalToLink(leCTS, False);
    SendSignalToLink(leDSR, False);
    SendSignalToLink(leRLSD, False);
    DoCTSChange(False);
    DoDSRChange(False);
    DoRLSDChange(False);
  end;
end;

// called in response to EV_X events, except CallXClose, CallXOpen

procedure TCustomUSBPort.CallAfterClose;
begin
  SendSignalToLink(leConn, False);
  DoAfterClose;
end;

procedure TCustomUSBPort.CallAfterOpen;
begin
  SendSignalToLink(leConn, True);
  DoAfterOpen;
  CheckSignals(True);
end;

procedure TCustomUSBPort.CallBeforeClose;
begin
  // shutdown com signals manually
  CheckSignals(False);
  DoBeforeClose;
end;

procedure TCustomUSBPort.CallBeforeOpen;
begin
  DoBeforeOpen;
end;

procedure TCustomUSBPort.CallBreak;
begin
  DoBreak;
end;

procedure TCustomUSBPort.CallCTSChange;
var
  OnOff: Boolean;
begin
  OnOff := csCTS in Signals;
  // check for linked components
  SendSignalToLink(leCTS, OnOff);
  DoCTSChange(OnOff);
end;

procedure TCustomUSBPort.CallDSRChange;
var
  OnOff: Boolean;
begin
  OnOff := csDSR in Signals;
  // check for linked components
  SendSignalToLink(leDSR, OnOff);
  DoDSRChange(OnOff);
end;

procedure TCustomUSBPort.CallRLSDChange;
var
  OnOff: Boolean;
begin
  OnOff := csRLSD in Signals;
  // check for linked components
  SendSignalToLink(leRLSD, OnOff);
  DoRLSDChange(OnOff);
end;

procedure TCustomUSBPort.CallError;
var
  Errors: TUSBErrors;
begin
  Errors := LastErrors;
  if Errors <> [] then
    DoError(Errors);
end;

procedure TCustomUSBPort.CallRing;
begin
  NotifyLink(leRing);
  DoRing;
end;

procedure TCustomUSBPort.CallRx80Full;
begin
  DoRx80Full;
end;

procedure TCustomUSBPort.CallRxChar;
var
  Count: Integer;

  // read from input buffer
  procedure PerformRead(var P: Pointer);
  begin
    GetMem(P, Count);
    Read(P^, Count);
    // call OnRxBuf event
    DoRxBuf(P^, Count);
  end;

  // check if any component is linked, to OnRxChar event
  procedure CheckLinks;
  {$WARNINGS OFF}
  var
    I: Integer;
    P: Pointer;
    ComLink: TUSBLink;
    ReadFromBuffer: Boolean;
  begin
    // examine links
    if (Count > 0) and (not TriggersOnRxChar) then
    begin
      ReadFromBuffer := False;
      try
        // cycle through links
        for I := 0 to FLinks.Count - 1 do
        begin
          ComLink := TUSBLink(FLinks[I]);
          if Assigned(ComLink.OnRxBuf) then
          begin
            // link to OnRxChar event found
            if not ReadFromBuffer then
            begin
              // TCustomUSBPort must read from USBPort, so OnRxChar event is
              // not triggered
              ReadFromBuffer := True;
              PerformRead(P);
            end;
            // send data to linked component
            ComLink.OnRxBuf(Self, P^, Count);
          end
        end;
        if (not ReadFromBuffer) and (not FTriggersOnRxChar) then
        begin
          ReadFromBuffer := True;
          PerformRead(P);
        end;
      finally
        if ReadFromBuffer then
        begin
          FreeMem(P);
          // data is already out of buffer, prevent from OnRxChar event to occur
          Count := 0;
        end;
      end;
    end;
  end;

begin
  Count := InputCount;
  if Count > 0 then
    SendSignalToLink(leRx, True);
  CheckLinks;
  if Count > 0 then
    DoRxChar(Count);
end;

procedure TCustomUSBPort.CallRxFlag;
begin
  NotifyLink(leRxFlag);
  DoRxFlag;
end;

procedure TCustomUSBPort.CallTxEmpty;
begin
  SendSignalToLink(leTx, False);
  NotifyLink(leTxEmpty);
  DoTxEmpty;
end;

// returns true if it has least one component linked to OnRxBuf event
function TCustomUSBPort.HasLink: Boolean;
var
  I: Integer;
  ComLink: TUSBLink;
begin
  Result := False;
  // examine links
  if FLinks.Count > 0 then
    for I := 0 to FLinks.Count - 1 do
    begin
      ComLink := TUSBLink(FLinks[I]);
      if Assigned(ComLink.OnRxBuf) then
        Result := True;
    end;
end;

// send TxBuf notify to link
procedure TCustomUSBPort.TxNotifyLink(const Buffer; Count: Integer);
var
  I: Integer;
  ComLink: TUSBLink;
begin
  if (FLinks.Count > 0) then
    for I := 0 to FLinks.Count - 1 do
    begin
      ComLink := TUSBLink(FLinks[I]);
      if Assigned(ComLink.OnTxBuf) then
        ComLink.OnTxBuf(Self, Buffer, Count);
    end;
end;

// send event notification to link
procedure TCustomUSBPort.NotifyLink(FLinkEvent: TUSBLinkEvent);
var
  I: Integer;
  ComLink: TUSBLink;
  Event: TNotifyEvent;
begin
  if (FLinks.Count > 0) then
    for I := 0 to FLinks.Count - 1 do
    begin
      ComLink := TUSBLink(FLinks[I]);
      Event := nil;
      case FLinkEvent of
        leRing: Event := ComLink.OnRing;
        leTxEmpty: Event := ComLink.OnTxEmpty;
        leRxFlag: Event := ComLink.OnRxFlag;
      end;
      if Assigned(Event) then
        Event(Self);
    end;
end;

// send signal to linked components
procedure TCustomUSBPort.SendSignalToLink(Signal: TUSBLinkEvent; OnOff: Boolean);
var
  I: Integer;
  ComLink: TUSBLink;
  SignalEvent: TUSBSignalEvent;
begin
  if (FLinks.Count > 0) then
    // cycle through links
    for I := 0 to FLinks.Count - 1 do
    begin
      ComLink := TUSBLink(FLinks[I]);
      SignalEvent := nil;
      case Signal of
        leCTS: SignalEvent := ComLink.OnCTSChange;
        leDSR: SignalEvent := ComLink.OnDSRChange;
        leRLSD: SignalEvent := ComLink.OnRLSDChange;
        leTx: SignalEvent := ComLink.OnTx;
        leRx: SignalEvent := ComLink.OnRx;
        leConn: SignalEvent := ComLink.OnConn;
      end;
      // if linked, trigger event
      if Assigned(SignalEvent) then
        SignalEvent(Self, OnOff);
    end;
end;

// set connected property, same as Open/Close methods
procedure TCustomUSBPort.SetConnected(const Value: Boolean);
begin
  if not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then
  begin
    if Value <> FConnected then
      if Value then
        Open
      else
        Close;
  end
  else
    FConnected := Value;
end;

// set baud rate
procedure TCustomUSBPort.SetBaudRate(const Value: TBaudRate);
begin
  if Value <> FBaudRate then
  begin
    FBaudRate := Value;
    // if possible, apply settings
    ApplyDCB;
  end;
end;

// set custom baud rate
procedure TCustomUSBPort.SetCustomBaudRate(const Value: Integer);
begin
  if Value <> FCustomBaudRate then
  begin
    FCustomBaudRate := Value;
    ApplyDCB;
  end;
end;

// set data bits
procedure TCustomUSBPort.SetDataBits(const Value: TDataBits);
begin
  if Value <> FDataBits then
  begin
    FDataBits := Value;
    ApplyDCB;
  end;
end;

// set discard null charachters
procedure TCustomUSBPort.SetDiscardNull(const Value: Boolean);
begin
  if Value <> FDiscardNull then
  begin
    FDiscardNull := Value;
    ApplyDCB;
  end;
end;

// set event charachters
procedure TCustomUSBPort.SetEventChar(const Value: Char);
begin
  if Value <> FEventChar then
  begin
    FEventChar := Value;
    ApplyDCB;
  end;
end;

// set port
procedure TCustomUSBPort.SetPort(const Value: TPort);
begin
  // 11.1.2001 Ch. Kaufmann; removed function ComString, because there can be com ports
  // with names other than COMn.
  if Value <> FPort then
  begin
    FPort := Value;
    if FConnected and not ((csDesigning in ComponentState) or
      (csLoading in ComponentState)) then
    begin
      Close;
      Open;
    end;
  end;
end;

// set stop bits
procedure TCustomUSBPort.SetStopBits(const Value: TStopBits);
begin
  if Value <> FStopBits then
  begin
    FStopBits := Value;
    ApplyDCB;
  end;
end;

// set event synchronization method
procedure TCustomUSBPort.SetSyncMethod(const Value: TSyncMethod);
begin
  if Value <> FSyncMethod then
  begin
    if FConnected and not ((csDesigning in ComponentState) or
      (csLoading in ComponentState))
    then
      //raise EUSBPort.CreateNoWinCode
      CallException(CError_ConnChangeProp)
    else
      FSyncMethod := Value;
  end;
end;

// sets RxChar triggering
procedure TCustomUSBPort.SetTriggersOnRxChar(const Value: Boolean);
begin
  if FHasLink then
    //raise EUSBPort.CreateNoWinCode
    CallException(CError_HasLink);
  FTriggersOnRxChar := Value;
end;

// sets event thread priority
procedure TCustomUSBPort.SetEventThreadPriority(const Value: TThreadPriority);
begin
  if Value <> FEventThreadPriority then
  begin
    if FConnected and not ((csDesigning in ComponentState) or
      (csLoading in ComponentState))
    then
      //raise EUSBPort.CreateNoWinCode
      CallException(CError_ConnChangeProp)
    else
      FEventThreadPriority := Value;
  end;
end;

// returns true if RxChar is triggered when data arrives input buffer
function TCustomUSBPort.GetTriggersOnRxChar: Boolean;
begin
  Result := FTriggersOnRxChar and (not FHasLink);
end;

// set flow control
procedure TCustomUSBPort.SetFlowControl(const Value: TUSBFlowControl);
begin
  FFlowControl.Assign(Value);
  ApplyDCB;
end;

// set parity
procedure TCustomUSBPort.SetParity(const Value: TUSBParity);
begin
  FParity.Assign(Value);
  ApplyDCB;
end;

// set timeouts
procedure TCustomUSBPort.SetTimeouts(const Value: TUSBTimeouts);
begin
  FTimeouts.Assign(Value);
  ApplyTimeouts;
end;

// set buffer
procedure TCustomUSBPort.SetBuffer(const Value: TUSBBuffer);
begin
  FBuffer.Assign(Value);
  ApplyBuffer;
end;

(*****************************************
 * TUSBDataPacket component              *
 *****************************************)

// create component
constructor TUSBDataPacket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComLink := TUSBLink.Create;
  FComLink.OnRxBuf := RxBuf;
  FMaxBufferSize := 1024;
end;

// destroy component
destructor TUSBDataPacket.Destroy;
begin
  USBPort := nil;
  FComLink.Free;
  inherited Destroy;
end;

// add custom data to packet buffer
procedure TUSBDataPacket.AddData(const Str: string);
begin
  if ValidStop then
  begin
    Buffer := Buffer + Str;
    HandleBuffer;
  end
  else
    DoPacket(Str);
end;

// remove USBPort property if being destroyed
procedure TUSBDataPacket.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FUSBPort) and (Operation = opRemove) then
    USBPort := nil;
end;

// call OnDiscard
procedure TUSBDataPacket.DoDiscard(const Str: string);
begin
  if Assigned(FOnDiscard) then
    FOnDiscard(Self, Str);
end;

// call OnPacket
procedure TUSBDataPacket.DoPacket(const Str: string);
begin
  if Assigned(FOnPacket) then
    FOnPacket(Self, Str);
end;

// call OnCustomStart
procedure TUSBDataPacket.DoCustomStart(const Str: string;
  var Pos: Integer);
begin
  if Assigned(FOnCustomStart) then
    FOnCustomStart(Self, Str, Pos);
end;

// call OnCustomStop
procedure TUSBDataPacket.DoCustomStop(const Str: string; var Pos: Integer);
begin
  if Assigned(FOnCustomStop) then
    FOnCustomStop(Self, Str, Pos);
end;

// discard start and stop strings
procedure TUSBDataPacket.CheckIncludeStrings(var Str: string);
var
  LenStart, LenStop: Integer;
begin
  if FIncludeStrings then
    Exit;
  LenStart := Length(FStartString);
  LenStop := Length(FStopString);
  // remove start string
  if Pos(Upper(FStartString), Upper(Str)) = 1 then
    Str := Copy(Str, LenStart + 1, Length(Str) - LenStart);
  // remove stop string
  if Pos(Upper(FStopString), Upper(Str)) = (Length(Str) - LenStop + 1) then
    Str := Copy(Str, 1, Length(Str) - LenStop);
end;

// upper case
function TUSBDataPacket.Upper(const Str: string): string;
begin
  if FCaseInsensitive then
    Result := UpperCase(Str)
  else
    Result := Str;
end;

// split buffer in packets
procedure TUSBDataPacket.HandleBuffer;

  procedure DiscardPacketToPos(Pos: Integer);
  var
    Str: string;
  begin
    FInPacket := True;
    if Pos > 1 then
    begin
      Str := Copy(Buffer, 1, Pos - 1); // some discarded data
      Buffer := Copy(Buffer, Pos, Length(Buffer) - Pos + 1);
      DoDiscard(Str);
    end;
  end;

  procedure FormPacket(CutSize: Integer);
  var
    Str: string;
  begin
    Str := Copy(Buffer, 1, CutSize);
    Buffer := Copy(Buffer, CutSize + 1, Length(Buffer) - CutSize);
    CheckIncludeStrings(Str);
    DoPacket(Str);
  end;

  procedure StartPacket;
  var
    Found: Integer;
  begin
    // check for custom start condition
    Found := -1;
    DoCustomStart(Buffer, Found);
    if Found > 0 then
      DiscardPacketToPos(Found);
    if Found = -1 then
    begin
      if Length(FStartString) > 0 then // start string valid
      begin
        Found := Pos(Upper(FStartString), Upper(Buffer));
        if Found > 0 then
          DiscardPacketToPos(Found);
      end
      else
        FInPacket := True;
    end;
  end;

  procedure EndPacket;
  var
    Found, CutSize, Len: Integer;
  begin
    // check for custom stop condition
    Found := -1;
    DoCustomStop(Buffer, Found);
    if Found > 0 then
    begin
      // custom stop condition detected
      CutSize := Found;
      FInPacket := False;
    end
    else
      if Found = -1 then
      begin
        Len := Length(Buffer);
        if (FSize > 0) and (Len >= FSize) then
        begin
          // size stop condition detected
          FInPacket := False;
          CutSize := FSize;
        end
        else
        begin
          Len := Length(FStartString);
          Found := Pos(Upper(FStopString),
            Upper(Copy(Buffer, Len + 1, Length(Buffer) - Len)));
          if Found > 0 then
          begin
            // stop string stop condition detected
            CutSize := Found + Length(FStopString) + Len - 1;
            FInPacket := False;
          end;
        end;
      end;
    if not FInPacket then
      FormPacket(CutSize); // create packet
  end;

  function IsBufferTooLarge: Boolean;
  begin
    Result := (Length(Buffer) >= FMaxBufferSize) and (FMaxBufferSize > 0);
  end;

begin
  try
    if not FInPacket then
      StartPacket;
    if FInPacket then
    begin
      EndPacket;
      if not FInPacket then
        HandleBuffer;
    end;
  finally
    if IsBufferTooLarge then
      EmptyBuffer;
  end;
end;

// is stop condition valid?
function TUSBDataPacket.ValidStop: Boolean;
begin
  Result := (FSize > 0) or (Length(FStopString) > 0)
    or (Assigned(FOnCustomStop));
end;

// receive data
procedure TUSBDataPacket.ResetBuffer;
begin
  EmptyBuffer;
end;

procedure TUSBDataPacket.RxBuf(Sender: TObject; const Buffer; Count: Integer);
var sa:AnsiString; Str: string;

begin
  SetLength(Str, Count);
  SetLength(Sa, Count);
  Move(Buffer, Sa[1], Count);
  Str := String(sa);
  AddData(Str);
end;

// empty buffer
procedure TUSBDataPacket.EmptyBuffer;
begin
  if Buffer <> '' then
  begin
    try
      DoDiscard(Buffer);
    finally
      Buffer := '';
      FInPacket := False;
    end;
  end;
end;

// set com port
procedure TUSBDataPacket.SetUSBPort(const Value: TCustomUSBPort);
begin
  if Value <> FUSBPort then
  begin
    if FUSBPort <> nil then
      FUSBPort.UnRegisterLink(FComLink);
    FUSBPort := Value;
    if FUSBPort <> nil then
    begin
      FUSBPort.FreeNotification(Self);
      FUSBPort.RegisterLink(FComLink);
    end;
  end;
end;

// set case sensitivity
procedure TUSBDataPacket.SetCaseInsensitive(const Value: Boolean);
begin
  if FCaseInsensitive <> Value then
  begin
    FCaseInsensitive := Value;
    if not (csLoading in ComponentState) then
      EmptyBuffer;
  end;
end;

// set packet size
procedure TUSBDataPacket.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    if not (csLoading in ComponentState) then
      EmptyBuffer;
  end;
end;

// set start string
procedure TUSBDataPacket.SetStartString(const Value: string);
begin
  if FStartString <> Value then
  begin
    FStartString := Value;
    if not (csLoading in ComponentState) then
      EmptyBuffer;
  end;
end;

// set stop string
procedure TUSBDataPacket.SetStopString(const Value: string);
begin
  if FStopString <> Value then
  begin
    FStopString := Value;
    if not (csLoading in ComponentState) then
      EmptyBuffer;
  end;
end;

(*****************************************
 * EUSBPort exception                    *
 *****************************************)

// create stream
constructor TUSBStream.Create(AUSBPort: TCustomUSBPort);
begin
  inherited Create;
  FUSBPort := AUSBPort;
end;

// read from stream
function TUSBStream.Read(var Buffer; Count: Integer): Longint;
begin
  FUSBPort.Read(Buffer, Count);
end;

// write to stream
function TUSBStream.Write(const Buffer; Count: Integer): Longint;
begin
  FUSBPort.Write(Buffer, Count);
end;

// seek always to 0
function TUSBStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := 0;
end;

(*****************************************
 * EUSBPort exception                    *
 *****************************************)

// create exception with windows error code
constructor EUSBPort.Create(ACode: Integer; AWinCode: Integer);
begin
  FWinCode := AWinCode;
  FCode := ACode;
  inherited CreateFmt(ComErrorMessages[ACode] + ' (Error: %d)', [AWinCode]);
end;

// create exception
constructor EUSBPort.CreateNoWinCode(ACode: Integer);
begin
  FWinCode := -1;
  FCode := ACode;
  inherited Create(ComErrorMessages[ACode]);
end;

(*****************************************
 * other procedures/functions            *
 *****************************************)

// initialization of PAsync variables used in asynchronous calls
procedure InitAsync(var AsyncPtr: PAsync);
begin
  New(AsyncPtr);
  with AsyncPtr^ do
  begin
    FillChar(Overlapped, SizeOf(TOverlapped), 0);
    Overlapped.hEvent := CreateEvent(nil, True, True, nil);
    Data := nil;
    Size := 0;
  end;
end;

// clean-up of PAsync variable
procedure DoneAsync(var AsyncPtr: PAsync);
begin
  with AsyncPtr^ do
  begin
    CloseHandle(Overlapped.hEvent);
    if Data <> nil then
      FreeMem(Data);
  end;
  Dispose(AsyncPtr);
  AsyncPtr := nil;
end;

procedure EnumUSBPorts(Ports: TStrings);
var
  KeyHandle: HKEY;
  ErrCode, Index: Integer;
  ValueName, Data: string;
  ValueLen, DataLen, ValueType: DWORD;
  TmpPorts: TStringList;
begin
  ErrCode := RegOpenKeyEx(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DEVICEMAP\SERIALCOMM',
    0,
    KEY_READ,
    KeyHandle);

  if ErrCode <> ERROR_SUCCESS then
  begin
    //raise EUSBPort.Create(CError_RegError, ErrCode);
    exit;
  end;

  TmpPorts := TStringList.Create;
  try
    Index := 0;
    repeat
      ValueLen := 256;
      DataLen := 256;
      SetLength(ValueName, ValueLen);
      SetLength(Data, DataLen);
      ErrCode := RegEnumValue(
        KeyHandle,
        Index,
        PChar(ValueName),
        {$IFDEF DELPHI_4_OR_HIGHER}
        Cardinal(ValueLen),
        {$ELSE}
        ValueLen,
          {$ENDIF}
        nil,
        @ValueType,
        PByte(PChar(Data)),
        @DataLen);

      if ErrCode = ERROR_SUCCESS then
      begin
        SetLength(Data, DataLen);
        TmpPorts.Add(trim(Data));
        Inc(Index);
      end
      else
        if ErrCode <> ERROR_NO_MORE_ITEMS then break;
          //raise EUSBPort.Create(CError_RegError, ErrCode);

    until (ErrCode <> ERROR_SUCCESS) ;

    TmpPorts.Sort;
    Ports.Assign(TmpPorts);
  finally
    RegCloseKey(KeyHandle);
    TmpPorts.Free;
  end;

end;

// string to baud rate
function StrToBaudRate(Str: string): TBaudRate;
var
  I: TBaudRate;
begin
  I := Low(TBaudRate);
  while (I <= High(TBaudRate)) do
  begin
    if UpperCase(Str) = UpperCase(BaudRateToStr(TBaudRate(I))) then
      Break;
    I := Succ(I);
  end;
  if I > High(TBaudRate) then
    Result := br9600
  else
    Result := I;
end;

// string to stop bits
function StrToStopBits(Str: string): TStopBits;
var
  I: TStopBits;
begin
  I := Low(TStopBits);
  while (I <= High(TStopBits)) do
  begin
    if UpperCase(Str) = UpperCase(StopBitsToStr(TStopBits(I))) then
      Break;
    I := Succ(I);
  end;
  if I > High(TStopBits) then
    Result := sbOneStopBit
  else
    Result := I;
end;

// string to data bits
function StrToDataBits(Str: string): TDataBits;
var
  I: TDataBits;
begin
  I := Low(TDataBits);
  while (I <= High(TDataBits)) do
  begin
    if UpperCase(Str) = UpperCase(DataBitsToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TDataBits) then
    Result := dbEight
  else
    Result := I;
end;

// string to parity
function StrToParity(Str: string): TParityBits;
var
  I: TParityBits;
begin
  I := Low(TParityBits);
  while (I <= High(TParityBits)) do
  begin
    if UpperCase(Str) = UpperCase(ParityToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TParityBits) then
    Result := prNone
  else
    Result := I;
end;

// string to flow control
function StrToFlowControl(Str: string): TFlowControl;
var
  I: TFlowControl;
begin
  I := Low(TFlowControl);
  while (I <= High(TFlowControl)) do
  begin
    if UpperCase(Str) = UpperCase(FlowControlToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TFlowControl) then
    Result := fcCustom
  else
    Result := I;
end;

// baud rate to string
function BaudRateToStr(BaudRate: TBaudRate): string;
const
  BaudRateStrings: array[TBaudRate] of string = ('Custom', '110', '300', '600',
    '1200', '2400', '4800', '9600', '14400', '19200', '38400', '56000', '57600',
    '115200', '128000', '256000');
begin
  Result := BaudRateStrings[BaudRate];
end;

// stop bits to string
function StopBitsToStr(StopBits: TStopBits): string;
const
  StopBitsStrings: array[TStopBits] of string = ('1', '1.5', '2');
begin
  Result := StopBitsStrings[StopBits];
end;

// data bits to string
function DataBitsToStr(DataBits: TDataBits): string;
const
  DataBitsStrings: array[TDataBits] of string = ('5', '6', '7', '8');
begin
  Result := DataBitsStrings[DataBits];
end;

// parity to string
function ParityToStr(Parity: TParityBits): string;
const
  ParityBitsStrings: array[TParityBits] of string = ('None', 'Odd', 'Even',
    'Mark', 'Space');
begin
  Result := ParityBitsStrings[Parity];
end;

// flow control to string
function FlowControlToStr(FlowControl: TFlowControl): string;
const
  FlowControlStrings: array[TFlowControl] of string = ('Hardware',
    'Software', 'None', 'Custom');
begin
  Result := FlowControlStrings[FlowControl];
end;

initialization
  ComErrorMessages[1]:='Unable to open com port';
  ComErrorMessages[2]:='WriteFile function failed';
  ComErrorMessages[3]:='ReadFile function failed';
  ComErrorMessages[4]:='Invalid Async parameter';
  ComErrorMessages[5]:='PurgeComm function failed';
  ComErrorMessages[6]:='Unable to get async status';
  ComErrorMessages[7]:='SetCommState function failed';
  ComErrorMessages[8]:='SeTUSBmTimeouts failed';
  ComErrorMessages[9]:='SetupComm function failed';
  ComErrorMessages[10]:='ClearCommError function failed';
  ComErrorMessages[11]:='GeTUSBmModemStatus function failed';
  ComErrorMessages[12]:='EscapeCommFunction function failed';
  ComErrorMessages[13]:='TransmitCommChar function failed';
  ComErrorMessages[14]:='Cannot set property while connected';
  ComErrorMessages[15]:='EnumPorts function failed';
  ComErrorMessages[16]:='Failed to store settings';
  ComErrorMessages[17]:='Failed to load settings';
  ComErrorMessages[18]:='Link (un)registration failed';
  ComErrorMessages[19]:='Cannot change led state if USBPort is selected';
  ComErrorMessages[20]:='Cannot wait for event if event thread is created';
  ComErrorMessages[21]:='WaitForEvent method failed';
  ComErrorMessages[22]:='A component is linked to OnRxBuf event';
  ComErrorMessages[23]:='Registry error';
  ComErrorMessages[24]:='Port Not Open';//  CError_PortNotOpen


end.
