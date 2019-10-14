unit pstream;

{
 JSDRIV - PGPLOT driver for Windows
 J. Saroun, 2019, jsaroun@seznam.cz

 Implements objects needed to exchange function calls between the clients and the server.
 This unit is shared by both the jsdrivlib.dll on the client side  and jsdriv-server application.

 PTFpar: ecnapsulates the driver function parameters and write/read operation
 on TStream.

 TJSDrivMsg: a message object sent between the client and server through a TStream.
 It encapsulates the GREXEC function parameters in TFPar with other data
 needed to correctly process and dispatch the messages.
}


{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils;

{procedure DelphiDrv(var IFUNC:integer; var RBUF:TBF;var NBUF:integer;CHR:PChar;
              var LCHR:integer; CLEN:integer); stdcall;
}

const
  sQWord = SizeOf(QWord);
  sTLen = SizeOf(size_t);
  sChar = SizeOf(string[1]);
  sReal = SizeOf(single); // should correspond to Fortran REAL*4

  rcNone:byte = 0;
  rcConnect:byte = 1;
  rcDisconnect = 2;
  rcFunc:byte = 3;
  rcFuncCallback:byte = 4;
  rcReturn = 5;
  MAXRBUF = 16384;
  MAXCHAR = 1024;

  CLIENT_ID='jsdriv_';

type

  TBF=array[0..MAXRBUF-1] of single;

  TDynArray=array of single;
  TStrArray=array of char;

  PTFpar = ^TFpar;
  TFpar = class(TObject)
  public
    id: QWord;
    ifunc:DWord;
    nbuf:size_t;
    nchar:size_t;
    rbuf:TDynArray;
    cbuf:PChar;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(source:TFpar);
    procedure WriteToBuffer(var buf;var lbuf:size_t;chr:PChar;var lchr:size_t);
    procedure ReadFromBuffer(var buf; lbuf:size_t;chr:PChar;lchr:size_t);
    procedure ReadFromStream(S:TStream);
    procedure WriteToStream(S:TStream);
  end;

  {Message object for communtication bewteen JSDRIV clients and the server  }
  TJSDrivMsg = class(TObject)
  private
    fSource:string;
    fAppName: string;
    fType:byte;
    fParam: TFpar;
    fCallback:boolean;
  public
    {GREXEC function call request}
    constructor MsgFunc(clientID:string; fnc: TFpar; isCallback:boolean);
    {Connection request from a client}
    constructor MsgConnect(clientID:string; appname:string);
    {Disconnection request from a client}
    constructor MsgDisconnect(clientID:string);
    destructor Destroy; override;
    procedure Assign(source:TJSDrivMsg);
    procedure ReadFromStream(S:TStream);
    procedure WriteToStream(S:TStream);
    {ID of the calling client}
    property Source:string read fSource;
    {Message type}
    property MsgType:byte read fType;
    {GREXEC Function parameters}
    property Param:TFpar read fParam;
    {Whether return alues are requested}
    property isCallback:boolean read fCallback;
    {name of the calling client application}
    property AppName:string read fAppName;
  end;





procedure ClearList(list:TFPList);
function CreatePar(IFUNC:integer; var RBUF:TBF;NBUF:integer;CHR:PChar;CLEN:integer):TFpar;


implementation
uses math;



 {TJSDrivMsg}

constructor TJSDrivMsg.MsgFunc(clientID:string; fnc: TFpar; isCallback:boolean);
begin
  inherited create;
  fCallback:=isCallback;
  if (fCallback) then fType:=rcFuncCallback else fType:=rcFunc;
  fSource:=clientID;
  fAppName:=' ';
  fParam:=fnc;
end;

constructor TJSDrivMsg.MsgConnect(clientID:string; appname:string);
begin
  inherited create;
  fType:=rcConnect;
  fCallback:=true;
  fSource:=clientID;
  fAppName:=appname;
  fParam:=nil;
end;

constructor TJSDrivMsg.MsgDisconnect(clientID:string);
begin
  inherited create;
  fType:=rcDisconnect;
  fCallback:=false;
  fSource:=clientID;
  fAppName:=' ';
  fParam:=nil;
end;


destructor TJSDrivMsg.Destroy;
begin
  FreeAndNil(fParam);
  inherited Destroy;
end;

procedure TJSDrivMsg.Assign(source:TJSDrivMsg);
begin
  fType:=source.fType;
  fCallback:=source.fCallback;
  fSource:=source.fSource;
  fAppName:=source.fAppName;
  if (source.fParam<>nil) then
  begin
    if (fParam=nil) then fParam:=TFPar.Create;
    fParam.Assign(source.fParam);
  end else
  begin
    FreeAndNil(fParam);
  end;
end;

procedure TJSDrivMsg.ReadFromStream(S:TStream);
var b:byte;
begin
  fType:=S.ReadByte;
  b:=S.ReadByte;
  fCallback:=(b=1);
  fSource:=S.ReadAnsiString;
  if (fType=rcConnect) then fAppName:=S.ReadAnsiString;
  if ((fType=rcFunc) or (fType=rcFuncCallback)) then
  begin
     fParam:=TFpar.Create;
     fParam.ReadFromStream(S);
  end else FreeAndNil(fParam);
end;

procedure TJSDrivMsg.WriteToStream(S:TStream);
var b:byte;
begin
  S.WriteByte(fType);
  if (fCallback) then b:=1 else b:=0;
  S.WriteByte(b);
  S.WriteAnsiString(fSource);
  if (fType=rcConnect) then S.WriteAnsiString(fAppName);
  if (fParam<>nil) then fParam.WriteToStream(S);
end;

 { TFPar }
constructor TFpar.Create;
begin
  SetLength(rbuf,0);
  cbuf:=AllocMem(MAXCHAR);
  ifunc:=0;
  nbuf:=0;
  nchar:=0;
end;

procedure TFpar.Assign(source:TFpar);
begin
  id:=source.id;
  ifunc:=source.ifunc;
  nbuf:=source.nbuf;
  nchar:=source.nchar;
  SetLength(rbuf,source.nbuf);
  Move(rbuf[0], source.rbuf[0], nbuf*sReal);
  StrLCopy(cbuf,source.cbuf,nchar)
end;

Destructor TFpar.Destroy;
begin
  SetLength(rbuf,0);
  FreeMem(cbuf);
  inherited;
end;

procedure TFpar.WriteToBuffer(var buf;var lbuf:size_t;chr:PChar;var lchr:size_t);
begin
   lbuf:=self.nbuf;
   lchr:=self.nchar;
   Move(self.rbuf[0], buf, self.nbuf*sReal);
   StrLCopy(chr,cbuf,lchr)
end;

procedure TFpar.ReadFromBuffer(var buf; lbuf:size_t;chr:PChar;lchr:size_t);
// note: chr may not be properly terminated by #0, it can be defined in a fortran module ...
begin
  SetLength(self.rbuf,lbuf);
  self.nbuf:=lbuf;
  self.nchar:=lchr;
  Move(buf, self.rbuf[0], lbuf*sReal);
  StrLCopy(self.cbuf,chr,min(lchr,MAXCHAR));
end;

procedure TFpar.ReadFromStream(S:TStream);
var text:string;
begin
    id:=S.ReadQWord;
    ifunc:=S.ReadDWord;
    S.ReadBuffer(nbuf,sTLen);
    S.ReadBuffer(nchar,sTLen);
    SetLength(rbuf,nbuf);
    S.ReadBuffer(rbuf[0],nbuf*sReal);
    text:=S.ReadAnsiString;
    StrLCopy(self.cbuf,PChar(text),min(nchar,MAXCHAR));
end;

procedure TFpar.WriteToStream(S:TStream);
begin
  S.WriteQWord(id);
  S.WriteDWord(ifunc);
  S.WriteBuffer(nbuf,sTLen);
  S.WriteBuffer(nchar,sTLen);
  S.WriteBuffer(rbuf[0],nbuf*sReal);
  S.WriteAnsiString(string(cbuf));
end;

{ -------------------- Other methods ------------------------}

// clear list and free its members
procedure ClearList(list:TFPList);
var i:integer;
begin
   list.Pack;
   for i:=0 to list.Count-1 do
       PTFPar(list.Items[i])^.free;
   list.Clear;
end;

procedure FreeMemAndNil(var P);
var
  Tmp: Pointer;
begin
  Tmp := Pointer(P);
  Pointer(P) := nil;
  FreeMem(Tmp);
end;


function CreatePar(IFUNC:integer; var RBUF:TBF;NBUF:integer;CHR:PChar;CLEN:integer):TFpar;
var par:TFpar;
begin
  par:=TFpar.Create;
  par.ifunc:=IFUNC;
  par.id:=0;
  try
    par.ReadFromBuffer(RBUF, NBUF, CHR, CLEN);
  except
    par.free;
    par:=nil;
  end;
  result:=par;
end;


end.

