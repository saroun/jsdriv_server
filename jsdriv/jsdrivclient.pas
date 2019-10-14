unit jsdrivclient;
{
 JSDRIV - PGPLOT driver for Windows
 J. Saroun, 2019, jsaroun@seznam.cz

 This unit implements the PGPLOT driver call (jsdriv, called by GREXEC) on the client side.
 It handles connection to the jsdriv_server, sends client's requests to the server
 and returns back the received data.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, pstream, Process;

const
  SERVER_ID = 'jsdriv_server';
  //CLIENT_ID = 'jsdriv_client';
  SERVER_EXE = 'jsdriv_server.exe';

type

  TDLLIPCServer = class(TSimpleIPCServer)
    private
      fID:string;
    protected
      function getIDString:string;
    public
      constructor create(AOwner:TComponent); override;
      property ClientComm : TIPCServerComm read FIPCComm;
      property ID: string read fID;
      property IDString: string read getIDString;
  end;


var
  ipcServer: TDLLIPCServer; // handles callback from the jsdriv server
  ipcClient: TSimpleIPCClient; // sends requests to the jsdriv server
  isConnected:boolean; // indicates that connection to the server has been established
  isInitialized:boolean; // indicates that initialization has been called
  msgID:QWord;  // "unique" message id: just an incremented QWord


{ Main PGPLOT driver function - this is the exported interface to the GREXEC of PGPLOT. }
procedure jsdriv(var I_FUNC:integer; var RBUF:TBF;var N_BUF:integer;CHR:PChar;
              var L_CHR:integer; CLEN:DWORD); stdcall;


implementation
uses Windows,Dos, jwatlhelp32;

const
  DBG=false;

  {Number of elements expected in rbuf array for each function code.
  NOTE: PGPLOT does not pass correct NBUF value on input.
  Therefore there is no other way how to determine the length of the RBUF array
  which has to be sent to the server.
  Set -1 where PGPLOT ensures correct input value}
  nb:array[1..29] of integer = (
  0,6,3,0,0,4,1,2,2,0,
  2,4,2,0,1,0,2,0,0,2,
  4,1,0,4,0,-1,0,0,4);



var
  Sin, Sout: TMemoryStream;
  serverProc: TProcess;
  ndbg:integer;
  AppName:string;



procedure LogIPCMsg(info:string; qry:TJSDrivMsg);
var fmt, msg:string;

begin
   if (not DBG) then exit;
   if (qry.Param.ifunc=12) then
   begin
     inc(ndbg);
     exit;
   end else
   begin
     if (ndbg>0) then writeln(format('... plot %d lines ...',[ndbg]));
     ndbg:=0;
   end;
   fmt:='[%s] id=%d, fnc=%d, nbuf=%d, lchr=%d, ';
   msg:=format(fmt, [info, qry.Param.id, qry.Param.ifunc, qry.Param.nbuf, qry.Param.nchar]);
   writeln(msg);
   {
   if (qry.Param.ifunc=22) then
   begin
      if (qry.Param.nbuf>0) then
      begin
        fmt:='   rbuf=%f';
        msg:=format(fmt, [qry.Param.rbuf[0]]);
      end else
      begin
        msg:='    error: ifunc=22, nbuf=0 ???';
      end;
      writeln(msg);
   end; }
end;

{Thes should replace Application.ProcessMessages in a DLL ... }
procedure ProcessMessages;
var
  Msg:TMsg;
begin
   while PeekMessage( Msg, 0, 0, 0 , PM_REMOVE ) do
   begin
     TranslateMessage( Msg );
     DispatchMessage(Msg );
   end;
end;


{TDLLIPCServer}

{returns a unique string to be used by JSDRIV server to connect to this application instance}
constructor TDLLIPCServer.create(AOwner:TComponent);
begin
   inherited create(AOwner);
   //fID:=IntToStr(HInstance);
   fID:=IntToStr(GetProcessID);
end;


{returns a unique string to be used by JSDRIV server to connect to this application instance}
function TDLLIPCServer.getIDString:string;
begin
   result:=CLIENT_ID+fID;
end;



{This is an implementation of GREXEC driver for calls which can be handled by the client.
This permits to call the server only when actual plot window has to accessed.
}
function ClientSideDrv(var IFUNC:DWord; var RBUF:TBF; CHR:PChar; var LCHR:size_t; CLEN:DWORD):boolean;
var
   S: string;
   icmd: integer;
begin

   result:=false;
   icmd:=ifunc;

   case icmd of

// Return device type ------------------------------------------------------
   1:
   begin
     S:='JSDRIV  (Windows driver for RESTRAX)';
     LCHR:=min(CLEN,length(S));
     StrLCopy(CHR,PChar(S),LCHR);
     result:=true;
   end;
   // Return maximum dimensions of view surface, and range of color index
   2:
   begin
      TBF(RBUF)[0] := 0;
      TBF(RBUF)[1] := -1;
      TBF(RBUF)[2] := 0;
      TBF(RBUF)[3] := -1;
      TBF(RBUF)[4] := 0;
      TBF(RBUF)[5] := 255;
   end;
// Return device capabilities -------------------------------------------------
   4:
   begin
{    chr[0] = 'I'; /* Interactive device */
    chr[1] = 'C'; /* Cursor is available */
    chr[2] = 'N'; /* No dashed lines */
    chr[3] = 'A'; /* Area fill available */
    chr[4] = 'T'; /* Thick lines */
    chr[5] = 'R'; /* Rectangle fill available */
    chr[6] = 'P'; /* Line of pixels available */
    chr[7] = 'V' /*  Close window (non persistent) */
    chr[8] = 'Y'; /* Can return color representation */
    chr[9] = 'N'; /* Not used */
    chr[10]= 'S'; /* Area-scroll available */

    }
        S:='ICNATRPNYNN';
        LCHR:=min(CLEN,length(S));
        StrLCopy(CHR,PChar(S),LCHR);
        result:=true;
    end;
    5:
   // Return default device/file name --------------------------------------------
    begin
        S:='';   // Default name is ""
        LCHR:=0;
        CHR:=char(0);
        result:=true;
    end;
    7:
    // Return miscellaneous defaults ----------------------------------------------
    begin
         TBF(RBUF)[0] :=1;
         result:=true;
    end;
    // Not implemented:  ----------------------------
    18,19,23,25:
    begin
         result:=true;
    end;
    end;

end;

{ return next "unique" ID number for a function call.
Actually this numbers repeat after full cycle 1 .. FFFFFFFF,
but this is of no practical importance in this application  ...
}
function GetNextMsgID:QWord;
begin
   if (msgID<High(QWord)) then
      inc(msgID)
   else
      msgID:=1;
   result:=msgID;
end;



{ Decides whether given PGPLOT call requires returned value, based on the ifunc number. }
function isCallback(ifnc:DWord):boolean;
begin
   if ((ifnc<10) or (ifnc=17) or (ifnc=29)) then
      result:=true
   else
      result:=false;
end;



{ Post function calls from Sout to the server. }
procedure PostFncCall;
begin
   if ((Sout.Size>0) and (ipcClient<>nil)) then ipcClient.SendMessage(0,Sout);
   Sout.Position:=0;
   Sout.clear;
end;



{ Handler of OnMessage event for the server.
This method should only handle the handshake on startup ...
}
procedure ipcServerMessage(Sender: TObject);
begin


end;


{Conects to the JSDRIV server.}
procedure connect;
const
  MAXT=10;
var
  responded,valid:boolean;
  ic:integer;
  qry, resp:TJSDrivMsg;
begin

 if (not ipcClient.ServerRunning) then raise Exception.Create(SERVER_ID + ' is not running');
 if (not ipcServer.Active) then raise Exception.Create(ipcServer.IDString + ' receiver is not running');
 if (not ipcClient.Active) then
 begin
    valid:=false;
    // try to connect and send a message to the server
   // write('Trying to connect to the jsdriv_server ... ');
    ipcClient.Connect;
    if (not ipcClient.Active) then raise Exception.Create('Cannot connect to '+ SERVER_ID);
    qry:=TJSDrivMsg.MsgConnect(ipcServer.ID, AppName);
    try
      qry.WriteToStream(Sout);
      PostFncCall;
      // wait for server response: it has to connect to this.ipcServer
      ic:=0;
      while ((not valid) and (ic<MAXT)) do
      begin
        ic:=ic+1;
        responded:=ipcServer.PeekMessage(100, true);
        if (responded) then
        begin
          Sin.Position:=0;
          Sin.Clear;
          ipcServer.GetMessageData(Sin);
          Sin.Position:=0;
          resp:=TJSDrivMsg.create;
          try
            resp.ReadFromStream(Sin);
            if (resp.MsgType = rcConnect) then
            begin
              if (AnsiCompareStr(resp.Source,ipcServer.ID)=0) then
              begin
                 valid:=true;
              end else
              begin
                raise Exception.Create(
                format('%s  answered with wrong serverID: [%s], expected [%s]',
                [SERVER_ID,ipcServer.ID,resp.Source]));
              end;
            end;
          finally
            resp.free;
          end;
        end;
      end;
    finally
      qry.free;
    end;
    isConnected := valid;
    if (not valid) then raise Exception.Create(SERVER_ID + ' is not responding while trying to connect.');
 end;
end;


function WaitForServer(maxtime:Qword):boolean;
const STIME=200;
var i:QWord;
    tlimit,trun:QWord;
    timeout:boolean;
begin
 i:=0;
 trun:=0;
 if (maxtime=0) then tlimit:=1000*STIME else tlimit:=maxtime;
 result:= ipcClient.ServerRunning;
 timeout:=false;
 while ( not (result or timeout)) do
 begin
 //  if (i=0) then write('Waiting for jsdriv_server to start... ');
   sleep(STIME);
   ProcessMessages;
   result:= ipcClient.ServerRunning;
   if (result) then write('+') else  write('-');
   inc(i);
   trun:=trun+STIME;
   timeout:= (trun>tlimit);
 end;
end;

 {
function FindServerWindow: HWND;
const
  MsgWndClassName: WideString = 'FPCMsgWindowCls';
begin
  Result := FindWindowW(PWideChar(MsgWndClassName), PWideChar(CLIENT_ID));
end;
}

function StartSrvIfNotRuning:boolean;
var
  path:string;
  i,L:size_t;
begin
   result:= ipcClient.ServerRunning;
   if (not result) then
   begin
      serverProc:=TProcess.create(nil);
      path:=GetEnv('PGPLOT_DIR');
      L:=length(path);
      if (L>0) then
      begin
         i:=LastDelimiter('\/',path);
         if (i<L-1) then
         begin
            path := path+DirectorySeparator;
         end;
      end;
      serverProc.Executable:= path+SERVER_EXE;
      serverProc.Parameters.Add('-stay'); // to keep running after client disconnects
{
TProcessOption = (poRunSuspended,poWaitOnExit,
                  poUsePipes,poStderrToOutPut,
                  poNoConsole,poNewConsole,
                  poDefaultErrorMode,poNewProcessGroup,
                  poDebugProcess,poDebugOnlyThisProcess);
              // e.g.
                  serverProc.Options := serverProc.Options + [poWaitOnExit];
}
      write('Waiting for '+SERVER_ID+' to start... ');
      serverProc.Execute;
      result:= WaitForServer(10000);  // give 10 s to start the server ...
      if (result) then writeln('OK') else writeln('failed')
   end;
end;


{finding a running process}
function processExists(exeFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;




{
from Windows docs:
No window classes registered by a DLL are unregistered when the DLL is unloaded.
A DLL must explicitly unregister its classes when it is unloaded.
}
procedure unregSrv;
const
  MsgWndClassName: WideString = 'FPCMsgWindowCls';
var
  cls: TWndClassW;
  isreg : Boolean;
begin
 isreg:=GetClassInfoW(HInstance,PWideChar(MsgWndClassName),@cls);
 if isreg then
    Windows.UnregisterClassW(PWideChar(MsgWndClassName), HInstance);
end;


{Create and start IPC client/server objects.
Waits for a server until it starts.}
procedure initialize;
begin
   writeln('jsdrivclient initialize ID: '+IntToStr(GetProcessID));
   try
     AppName:=ParamStr(0);
   except
   end;
   isConnected:=false;
   try
     ipcServer:=TDLLIPCServer.Create(nil);
     ipcClient:=TSimpleIPCClient.Create(nil);
     ipcClient.ServerID:=SERVER_ID;
     ipcServer.ServerID:=ipcServer.IDString;
     ipcServer.Global:=true;
     ipcServer.Active:=true;
     ProcessMessages;
     if (not StartSrvIfNotRuning) then
     begin
        raise Exception.create('No JSDRIV server running');
     end;
     //WaitForServer;
     connect;
     isInitialized:=true;
   except
     on E:Exception do
     begin
        ShowException(E, E.ClassInfo);
     end;
   end;
end;


{Cleanup }
procedure finalize;
var qry:TJSDrivMsg;
begin
   if (ipcClient<>nil) then
   begin
        if (ipcClient.Active) then
        begin
           // Send server a message to disconnect
           Sout.Clear;;
           Sout.Position:=0;
           qry:=TJSDrivMsg.MsgDisconnect(ipcServer.ID);
           try
              qry.WriteToStream(Sout);
              PostFncCall;
           finally
             qry.free;
             // disconnect form the server
             ipcClient.Disconnect;
             ipcClient.Active:=false;
           end;
        end;
        FreeAndNil(ipcClient);
   end;
   if (ipcServer<>nil) then
   begin
     ipcServer.Active:=false;
     FreeAndNil(ipcServer);
   end;
   FreeAndNil(Sin);
   FreeAndNil(Sout);
   serverProc.free;
end;

{Create a message instance for function call}
function GetQuery(IFUNC:integer; var RBUF:TBF;NBUF:integer;CHR:PChar;CLEN:integer):TJSDrivMsg;
var par:TFPar;
begin
   par:=CreatePar(IFUNC, RBUF, NBUF, CHR, CLEN);
   par.id:=GetNextMsgID;
   result:=TJSDrivMsg.MsgFunc(ipcServer.ID,par,isCallback(IFUNC));
end;

{ Main PGPLOT driver function - this is the exported interface to the GREXEC of PGPLOT. }
procedure jsdriv(var I_FUNC:integer; var RBUF:TBF;var N_BUF:integer;CHR:PChar;
              var L_CHR:integer; CLEN:DWORD); stdcall;
const
  MAXT = 1000;
var
    responded, valid:boolean;
    ic:integer;
    IFUNC:DWord;
    NBUF,LCHR:size_t;
    qry, resp:TJSDrivMsg;
begin
    // type conversion
    IFUNC:=I_FUNC;
    NBUF:=N_BUF;
    LCHR:=L_CHR;

    if ((NBUF=0) and (IFUNC>0) and (IFUNC<=29)) then NBUF:=nb[IFUNC];

// some commands can be handled on the client side ...
    if (ClientSideDrv(IFUNC,RBUF, CHR,LCHR, CLEN)) then
    begin
       L_CHR:=LCHR;
       exit;
    end;

// if not initialized, do it now
    if (not isInitialized) then initialize;

// could not connect, let it know to the calling module
    if (not isConnected) then
    begin
        // this should tell PGPLOT that the driver could not open the workstation
        if (IFUNC=9) then
        begin
            TBF(RBUF)[0]:=0;
            TBF(RBUF)[1]:=0;
        end;
        exit;
    end;

// create message object
    qry:=GetQuery(IFUNC, RBUF, NBUF, CHR, CLEN);
    LogIPCMsg('call',qry);
    try
// send it to plotter and wait for return values
      if (qry.isCallback) then
      begin
         // send all pending non-callback requests to the server
         PostFncCall;
         // send the new request to the server
         qry.WriteToStream(Sout);
         PostFncCall;
         // wait for response, maximum MAXT trials;
         valid:=false;
         ic:=0;
         while ((not valid) and (ic<MAXT)) do
         begin
           // no timeout when asking for mouse input ....
           if (IFUNC<>17) then ic:=ic+1;
           responded:=ipcServer.PeekMessage(100, true);
           if (responded) then
           begin
                Sin.Position:=0;
                Sin.Clear;
                ipcServer.GetMessageData(Sin);
                Sin.Position:=0;
                resp:=TJSDrivMsg.create;
                try
                   resp.ReadFromStream(Sin);
                   LogIPCMsg('response',resp);
                   if (resp.MsgType = rcFuncCallback) then
                   begin
                     if ((resp.Param.ifunc=qry.Param.ifunc) and (resp.Param.id=qry.Param.id)) then
                     begin
                        resp.Param.WriteToBuffer(RBUF, NBUF, CHR, LCHR);
                        valid:=true;
                     end;
                   end;
                finally
                    resp.free;
                end;
           end;
         end;
         if (not valid) then
         begin
              writeln('JSDRIV client: Timeout while waiting for callback, fnc=',IFUNC);
         end;
  // send it to plotter, return values not required
      end else
      begin
         qry.WriteToStream(Sout);
      // process "End plot" "Flush" or "Close workstation" commands immediately
         if ((IFUNC=14) or (IFUNC=10) or (IFUNC=16)) then PostFncCall;
      end;
    finally
      FreeAndNil(qry);
    end;
    L_CHR:=integer(LCHR);
end;


initialization
  //writeln('sizes: ',sQWord,sTLen,SizeOf(DWord));
  AppName:='JSDRIV plotter';
  isInitialized:=false;
  serverProc:=nil;
  Sin:=TMemoryStream.Create;
  Sout:=TMemoryStream.Create;
  msgID:=0;


finalization
  finalize;
  unregSrv;

end.

