unit jsdriv_comm;
{
JSDRIV - PGPLOT driver for Windows
J. Saroun, 2019, jsaroun@seznam.cz

This unit manages clients on the server side. It enables to serve plot windows
to multiple client applications.
1. Each application connects by sending a request with its unique ID.
2. A new TJSDrivClient is created on the server side and connected to a TSimpleIPCServer of the calling application.
3. TJSDrivClient is then used to send feedback when requested to the calling application.

TClientList manages the list of TJSDrivClient instances, one for each active connections.
TJSDrivClient also manages a list of open TPlotter windows by each client.
The full list of TPlotter windows (open or closed) is maintained by the unit jsdrivlist,
in the global variable devices:TPlotList.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, pstream, plotwin, jsdrivlist;

type

    TClientInfo = record
      id:string;
      status:string;
      appname:string;
    end;

    TJSDrivClient = class(TSimpleIPCClient)
    protected
      fAppName:string;
      fProcID:string;
      Sout: TMemoryStream;
      {The list of open devices}
      plots: TFPList;
      fSelected: TPlotter;
      {Close given device. Deselect if neceessary.}
      procedure CloseDevice(index:integer);
      {Close all devices open by this client}
      procedure CloseAllDevices;
      {Deselect any previously selected device}
      procedure DeselectDevices;
    public
      constructor create(AOwner: TComponent); override;
      constructor create(SrvName:string);
      destructor destroy; override;
      {Send query to given client}
      procedure SendQuery(query:TJSDrivMsg);
      {Open a new TPlotter, return its PID, or -1 if failed}
      function OpenDevice:integer;
      {Select a device with given PID, must already be open by OpenDevice.
      Return true if successful.}
      function SelectDevice(id:integer):boolean;
      {Close currently selected window}
      procedure CloseSelected;

      property AppName:string read fAppName write fAppName;
      property ProcID:string read fProcID write fProcID;
      property Selected:TPlotter read fSelected;

    end;

    TClientList = class(TStringList)
    public
      function getClientInfo(index:integer):TClientInfo;
      {Find an existing client in the clients list or create a new one}
      function GetClient(AName:string):TJSDrivClient;
      {Get a client at given position or return nil}
      function FindClientI(index:integer):TJSDrivClient;
      {Find an existing client in the clients list or return nil}
      function FindClient(AName:string):TJSDrivClient;
      {Delete and destroy a client from the list}
      procedure DeleteClient(AName:string);
      procedure DeleteAll;
      destructor Destroy; override;
    end;

var
  clients: TClientList;
implementation



{Find an existing client in the clients list or create a new one}
function TClientList.GetClient(AName:string):TJSDrivClient;
var idx:integer;
begin
   if (Find(AName,idx)) then
   begin
     result:=(Objects[idx] as TJSDrivClient);
   end else
   begin
     result:=TJSDrivClient.create(AName);
     AddObject(AName,result);
   end;
end;

//Get client info
function TClientList.GetClientInfo(index:integer):TClientInfo;
var r:TClientInfo;
    c:TJSDrivClient;
begin
   if (index>=0) and (index<count) and (objects[index] is TJSDrivClient) then
   begin
     c:=(Objects[index] as TJSDrivClient);
     r.id:=c.ProcID;
     r.appname:=c.AppName;
     if (c.Active) then r.status:='active' else  r.status:='closed';
   end else
   begin
     r.id:='';
     r.appname:='';
     r.status:='';
   end;
   result:=r;
end;


{Get a client at given position or return nil}
function TClientList.FindClientI(index:integer):TJSDrivClient;
begin
   if (index>=0) and (index<count) and (objects[index] is TJSDrivClient) then
   begin
     result:=(Objects[index] as TJSDrivClient);
   end else result:=nil;
end;

{Find an existing client in the clients list or return nil}
function TClientList.FindClient(AName:string):TJSDrivClient;
var idx:integer;
begin
   if (Find(AName,idx)) then
   begin
     result:=(Objects[idx] as TJSDrivClient);
   end else
   begin
     result:=nil;
   end;
end;

procedure TClientList.DeleteClient(AName:string);
var idx:integer;
    c:TJSDrivClient;
begin
  if (Find(AName,idx)) then
  begin
     if (Objects[idx]<>nil) then
     begin
       c:=(Objects[idx] as TJSDrivClient);
       c.free;
     end;
     Delete(idx);
  end;
end;

procedure TClientList.DeleteAll;
var idx:integer;
    c:TJSDrivClient;
begin
  for idx:=0 to Count-1 do
  begin
    if (Objects[idx]<>nil) then
    begin
        c:=(Objects[idx] as TJSDrivClient);
        if (c.Active) then c.Disconnect;
        c.free;
    end;
  end;
  Clear;
end;

destructor TClientList.Destroy;
begin
  DeleteAll;
  inherited destroy;
end;


constructor TJSDrivClient.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fAppName:='';
  Sout := TMemoryStream.create;
  plots:=TFPList.Create;
end;

constructor TJSDrivClient.create(SrvName:string);
begin
  inherited create(nil);
  ServerID:=CLIENT_ID+SrvName;
  fProcID:=SrvName;
  fAppName:='';
  Sout := TMemoryStream.create;
  plots:=TFPList.Create;
end;

destructor TJSDrivClient.destroy;
begin
  try
     CloseAllDevices;
     if (Active) then Disconnect;
     Sout.free;
  finally
     inherited destroy;
  end;
end;

procedure TJSDrivClient.SendQuery(query:TJSDrivMsg);
begin
  if (Active) then
  begin
    Sout.Position:=0;
    Sout.Clear;
    query.WriteToStream(Sout);
    SendMessage(0,Sout);
  end;
end;

{Open a new TPlotter, return its PID, or -1 if failed}
function TJSDrivClient.OpenDevice:integer;
var
    i:integer;
    PL:TPlotter;
begin
  i:=Devices.OpenWorkstation;
  if (i>=0) then
  begin
    PL:=Devices.FindByID(i);
    plots.Add(PL);
  end;
  result:=i;
end;


{Select a device with given PID, must already be open by OpenDevice.
Return true if successful.}
function TJSDrivClient.SelectDevice(id:integer):boolean;
var
  i:integer;
  PL:TPlotter;
begin
  result:=false;
  PL:=Devices.FindByID(id);
  if (PL=nil) then
      raise Exception.create(format('JSDRIV Window [%d] not found',[id]));
  i:=plots.IndexOf(PL);
  if ((i<0) or (not PL.isOpen)) then
      raise Exception.create(format('JSDRIV Window [%d] was not open by this client application',[id]));
  // deselect previous window
  DeselectDevices;
  // mark it as selected
  fSelected:=PL;
  fSelected.PlotSelect;
  result:=true;
end;

{Deselect any previously selected device}
procedure TJSDrivClient.DeselectDevices;
begin
  if (fSelected<>nil) then fSelected.PlotDeselect;
  fSelected:=nil;
end;

{Close currently selected window}
procedure TJSDrivClient.CloseSelected;
var i:integer;
begin
  if (fSelected<>nil) then
  begin
       i:=plots.IndexOf(fSelected);
       CloseDevice(i);
  end;
end;

{Close device at given index and remove it from the list}
procedure TJSDrivClient.CloseDevice(index:integer);
var
    PL:TPlotter;
begin
  if (index>=0) and (index<plots.count) then
  begin
     PL:=TPlotter(plots.Items[index]);
     if (PL = fSelected) then DeselectDevices;
     PL.PlotClose;
     plots.Delete(index);
     plots.Pack;
  end;
end;

{Close all devices open by this client}
procedure TJSDrivClient.CloseAllDevices;
begin
  while plots.count>0 do CloseDevice(plots.count-1);
end;


initialization
    clients:=TClientList.Create;
    clients.Sorted:=true;
finalization
    clients.free;
end.

