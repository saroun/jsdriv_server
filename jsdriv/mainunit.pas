unit MainUnit;
{
 JSDRIV - PGPLOT driver for Windows
 J. Saroun, 2019, jsaroun@seznam.cz

 Implements the main server window. The window is hidden by default and shows only as a tray icon.
 When open, it enables to see the logs, connected cliets and created devices (instances of TPlotter).

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, Buttons, Grids, pstream,plotwin, jsdriv_comm;

const
  SERVER_ID = 'jsdriv_server';
  SRVTITLE = 'JSDRIV plotter';
  TIMER_CONNECTED=20;  // timer period [ms] when a client is connected
  TIMER_DISCONNECTED=100; // timer period [ms] when there is no client connected

type

  { TCtrlForm }

  TCtrlForm = class(TForm)
    Button1: TButton;
    ExitBtn: TBitBtn;
    LogMemo: TMemo;
    CloseItem: TMenuItem;
    Pages: TPageControl;
    PanelTop: TPanel;
    PanelBottom: TPanel;
    ipcServer: TSimpleIPCServer;
    mnClose: TMenuItem;
    mnSettings: TMenuItem;
    PopupDevices: TPopupMenu;
    SrvBtn: TButton;
    GridConn: TStringGrid;
    GridPlots: TStringGrid;
    TabLog: TTabSheet;
    TabConn: TTabSheet;
    TabDev: TTabSheet;
    TimerUpdate: TTimer;
    TrayPopup: TPopupMenu;
    TimerPeek: TTimer;
    TrayIcon1: TTrayIcon;
    procedure Button1Click(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ipcServerMessage(Sender: TObject);
    procedure mnSettingsClick(Sender: TObject);
    procedure SrvBtnClick(Sender: TObject);
    procedure TimerPeekTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
  private
     Sin:TMemoryStream;
     fClosing:boolean;
  public
    procedure UpdateContent;
    procedure ShowPlot;
    procedure ClosePlot;
    function ReceiveData:QWord;
    function ConnectClient(procid, appname:string):TJSDrivClient;
    function HasInputData:boolean;
    procedure SendCommand(query:TJSDrivMsg);
    property isClosing:boolean read fClosing;


  end;

var
  MainForm: TCtrlForm;
  //CLIENT_ID:string;

procedure LogTxt(msg:string);

implementation
uses jsdrivproc, jsdrivlist;
const
  DBG=false;

{$R *.lfm}

var
  ndbg:integer;




procedure LogTxt(msg:string);
begin
  MainForm.LogMemo.Append(msg);
end;



procedure LogIPCMsg(p:TFpar);
var fmt, msg:string;

begin
   if (not DBG) then exit;
   if (p.ifunc=12) then
   begin
     inc(ndbg);
     exit;
   end else
   begin
     if (ndbg>0) then MainForm.LogMemo.Append(format('... plot %d lines ...',[ndbg]));
     ndbg:=0;
   end;
   fmt:='call[%d] fnc=%d, nbuf=%d, lchr=%d, ';
   msg:=format(fmt, [p.id, p.ifunc, p.nbuf, p.nchar]);
   MainForm.LogMemo.Append(msg);
end;

{ TCtrlForm }

procedure TCtrlForm.ShowPlot;
begin
  // this device can serve only a single plot window
 // if (id>0) then exit;
  Plotter.ShowInactive;
end;


procedure TCtrlForm.ClosePlot;
begin
  Plotter.Hide;
end;

procedure TCtrlForm.UpdateContent;
var s:string;
begin
  if ipcServer.Active then begin
    SrvBtn.Caption := 'Stop server';
    TimerPeek.Enabled := true;
    s := 'running';
  end else
  begin
    SrvBtn.Caption := 'Start server';
    TimerPeek.Enabled := false;
    s := 'stopped';
  end;
  Caption:=SRVTITLE + ' '+s;
end;

procedure TCtrlForm.FormCreate(Sender: TObject);
//var s:string;
var
  YY,MM,DD, h,m,s,ms : Word;
  //i,ip:integer;
begin
  fClosing:=false;
  Sin:=TMemoryStream.create;

  Caption:=SRVTITLE;
  TimerPeek.Interval:=TIMER_DISCONNECTED;
  // read arguments
  {
  ip:=ParamCount;
  if (ip>0) then
  for i:=1 to ip do
  begin
     spar:=ParamStr(i);
     if (spar='-stay') then stayActive:=true;
  end;
   }
  // inti the engine
  InitPlots;
  ipcServer.Global := true;
  ipcServer.ServerID:=SERVER_ID;
  ipcServer.Active:=true;
  UpdateContent;
  DeCodeDate (Now,YY,MM,DD);
  DecodeTime(Now,h,m,s,ms);
  LogMemo.Append(format ('Started on %d/%d/%d - %d:%d:%d',[dd,mm,yy,h,m,s]));
  InitPlots;
end;

procedure TCtrlForm.FormDestroy(Sender: TObject);
begin
  ReleaseAllPlots;
  Sin.free;
end;

procedure TCtrlForm.ExitBtnClick(Sender: TObject);
begin
   //Application.Terminate;
   fClosing:=true;;
   Close;
end;

procedure TCtrlForm.Button1Click(Sender: TObject);
begin
  LogMemo.Clear;
end;

procedure TCtrlForm.CloseItemClick(Sender: TObject);
begin
  devices.FreeClosed;
end;

procedure TCtrlForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if isClosing then
    CloseAction := caFree
  else
    CloseAction := caHide;
end;


{Read message data to Sin stream and return number of bytes received.}
function TCtrlForm.ReceiveData:QWord;
begin
  Sin.Position:=0;
  Sin.Clear;
  if (ipcServer<>nil) then ipcServer.GetMessageData(Sin);
  Sin.Position:=0;
  result:=Sin.Size;
end;

{return true until the inpuut stream is read. }
function TCtrlForm.HasInputData:boolean;
begin
  result:=Sin.Position<Sin.Size;
end;


{Get the named TJSDrivClient and connect it to corresponding server.
the client is created and added to the list if not found.
Retun the TJSDrivClient instance.
}
function TCtrlForm.ConnectClient(procid, appname:string):TJSDrivClient;
var ipcClient:TJSDrivClient;
     qry:TJSDrivMsg;
begin
  ipcClient:=clients.GetClient(procid);
  if (ipcClient<>nil) then
  begin
    ipcClient.AppName:=appname;
    ipcClient.ProcID:=procid;
    if (ipcClient.Active) then ipcClient.Disconnect;
    if (ipcClient.ServerRunning) then
    begin
        ipcClient.Connect;
        if (ipcClient.Active) then
        begin
          qry:=TJSDrivMsg.MsgConnect(ipcClient.ProcID, ipcClient.AppName);
          ipcClient.SendQuery(qry);
        end;
    end;
    if (ipcClient.Active) then result:=ipcClient else result:=nil;
  end;

end;


{Send a command to the client}
procedure TCtrlForm.SendCommand(query:TJSDrivMsg);
var ipcClient:TJSDrivClient;
begin
  ipcClient:=clients.FindClient(query.source);
  if (ipcClient<>nil) then
  begin
     ipcClient.SendQuery(query);
  end;
end;

procedure TCtrlForm.ipcServerMessage(Sender: TObject);
var
    qry:TJSDrivMsg;
    srvID:string;
    ipcClient:TJSDrivClient;
begin
  ReceiveData;
  qry:=TJSDrivMsg.create;
  try

    // read all records found in the stream and add their pointers to the inpList
    while (HasInputData) do
    begin
        qry.ReadFromStream(Sin);

    { on connect request: reconnect to the client and ping back a message with the client ID }
        if (qry.MsgType=rcConnect) then
        begin
          srvID:=CLIENT_ID+qry.Source;
          ipcClient:=ConnectClient(qry.Source,qry.AppName);
          if (ipcClient<>nil) then
          begin
            TimerPeek.Interval:=TIMER_CONNECTED;
            LogMemo.Append('Connected '+'['+ipcClient.ProcID+'] '+ipcClient.AppName);
          end else
          begin
            LogMemo.Append('Connection from  '+srvID+' failed');
          end;
          if (clients.Count=0) then
             TimerPeek.Interval:=TIMER_DISCONNECTED
          else
             TimerPeek.Interval:=TIMER_CONNECTED;
    { on disconnect request: just disconnect }
        end else if (qry.MsgType=rcDisconnect) then
        begin
          ipcClient:=clients.FindClient(qry.Source);
          if (ipcClient<>nil) then
          begin
               LogMemo.Append('Disconnected  '+'['+qry.Source+']' );
               ipcClient:=nil;
               clients.DeleteClient(qry.Source);
          end;
          if (clients.Count=0) then
             TimerPeek.Interval:=TIMER_DISCONNECTED
          else
             TimerPeek.Interval:=TIMER_CONNECTED;
    { function call:
        call GrfExec with given parameters and send back returned values if requested.}
        end else if (qry.Param<>nil) then
        begin
           LogIPCMsg(qry.Param);
           ipcClient:=clients.FindClient(qry.Source);
           // interactive window input
           if (qry.Param.ifunc=17) then
           begin
              if (ipcClient.Selected<>nil) then ipcClient.Selected.StartInput(qry);
           end else
           // other commands
           begin
              GrfExec(ipcClient, qry.Param);
              // response required
              if (qry.isCallback) then SendCommand(qry);
           end;
        end;
     end;
  finally
    qry.free;
  end;
end;

procedure TCtrlForm.mnSettingsClick(Sender: TObject);
begin
  self.Show;
end;

procedure TCtrlForm.SrvBtnClick(Sender: TObject);
begin
  ipcServer.Active := (not ipcServer.Active);
  UpdateContent;
end;

procedure TCtrlForm.TimerPeekTimer(Sender: TObject);
var res:boolean;
begin
  if ipcServer.Active then
  begin
    res:=ipcServer.PeekMessage(20, true);
    if (res) then
    begin
    //  LogMemo.Append('received');
    end;

  end;
end;

procedure TCtrlForm.TimerUpdateTimer(Sender: TObject);
var i:integer;
    info:TClientInfo;
    pinfo:TPlotInfo;
begin
  if (self.visible) then
  //with (Sender as TTimer) do
  begin
      // update connections
      if (Pages.TabIndex=1) then
      begin
         if (GridConn.RowCount<> clients.Count+1) then
         begin
            GridConn.Enabled:=false;
            GridConn.RowCount:=clients.Count+1;
            GridConn.Enabled:=true;
         end;
         for i:=0 to clients.Count-1 do
         begin
            info:=clients.getClientInfo(i);
            GridConn.Cells[0,i+1]:=info.id;
            GridConn.Cells[1,i+1]:=info.status;
            GridConn.Cells[2,i+1]:=info.appname;
         end;
         GridConn.Refresh;
      end else
      // update windows list
      if (Pages.TabIndex=2) then
      begin
        if (GridPlots.RowCount<> devices.Count+1) then
        begin
           GridPlots.Enabled:=false;
           GridPlots.RowCount:=devices.Count+1;
        end;
         for i:=0 to devices.Count-1 do
         begin
            pinfo:=devices.GetPlotInfo(i);
            GridPlots.Cells[0,i+1]:=pinfo.id;
            GridPlots.Cells[1,i+1]:=pinfo.status;
            GridPlots.Cells[2,i+1]:=pinfo.caption;
         end;
         GridConn.Refresh;
      end;
  end;

end;


procedure TCtrlForm.TrayIcon1Click(Sender: TObject);
begin
  Self.Show;
end;


end.

