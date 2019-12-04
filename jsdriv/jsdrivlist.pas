unit jsdrivlist;
{
 JSDRIV - PGPLOT driver for Windows
 J. Saroun, 2019, jsaroun@seznam.cz

 This unit manages the list of TPlotter windows in the global variable devices:TPlotList.
 TPlotList performs tasks needed to open or destroy TPlotter windows on request
 of the calling clients.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plotwin;

const
  WINNAME='JSDRIV window';
  MAXWIN=100;

type

    TPlotInfo = record
      id:string;
      status:string;
      caption:string;
    end;

    {Manages the list of plot windows}
    TPlotList = class(TFPList)
    protected
      function IndexOfPlotter(plt:TPlotter):integer;
      function GetUniqueID:integer;
      {Create new plotter window}
      function CreateNewPlot:TPlotter;
    public

      {Destroy plots in the specified list.
      Implicitly removes references from the global list "devices".}
      procedure FreeDeviceList(plotlist:TFPList);

      {Find a plotter window by ID number}
      function FindByID(id:integer):TPlotter;

      {Get client info using the order index}
      function GetPlotInfo(index:integer):TPlotInfo;

      {Show all plots}
      procedure ShowAllPlots;

      {Close all plots}
      procedure CloseAllPlots;

      {Select the first plot window in closed state or create a new one.
      Return its order number as ID}
      function OpenWorkstation:integer;

      {Delete given plot from the list, but do not free it! }
      procedure DeletePlot(plt:TPlotter);

      {Close all plots and delete them from the list. Don't free the plots !}
      procedure DeleteAll;

      {Destroy all unused plots and rearrange list}
      procedure FreeClosed;

      {Destroy all plot windows}
      procedure FreeAll;

      {Destructor with cleanup}
      destructor Destroy; override;

    end;

procedure InitPlots;
//procedure ReleaseallPlots;
function DefaultWinSize:TPoint;



var
    devices:TPlotList;  // list of all TPlotter instances


implementation
uses Forms, Dialogs;
var
  DEF_WIDTH,DEF_HEIGHT:integer; // default window size


{Destroy plots in the specified list.
Implicitly removes references from the global list "devices".}
procedure TPlotList.FreeDeviceList(plotlist:TFPList);
var i:integer;
    PL:TPlotter;
begin
  if (plotlist<>nil) and (plotlist<>devices) then
  for i:=0 to plotlist.count-1 do
  if plotlist.items[i]<>nil then
  begin
    PL:=TPlotter(plotlist.items[i]);
    PL.Release;
    PL.free;
    // If self=devices, then PL is deleted from the list implicitly
    if (self <> devices) then DeletePlot(PL);
  end;
  Pack;
end;

function DefaultWinSize:TPoint;
begin
   result:=Point(DEF_WIDTH, DEF_HEIGHT);
end;

procedure InitPlots;
{Initialize plot system}
begin
{Define default window size according to the screen parameters}
  DEF_HEIGHT:=round(Screen.Height*1/2);
  DEF_WIDTH:=round(DEF_HEIGHT/1.41); // Portrait format
end;

{
procedure ReleaseallPlots;
begin
  devices.FreeAll;
end;
 }
{ TPlotList }

{Create new plotter window}
function TPlotList.CreateNewPlot:TPlotter;
var PL:TPlotter;
begin
  PL:=nil;
  PL:=TPlotter.Create(Application);  // CreateNew ??
  if (PL=nil) then
  begin
    MessageDlg('Can''t create new plot window',mtWarning,[mbOK],0);
      result:=nil;
      exit;
  end;
  with PL do SetBounds(20+Count*20,20+Count*20,Width,Height);
  PL.ShowInactive;
  result:=PL;
end;

//Get client info using the order index
function TPlotList.GetPlotInfo(index:integer):TPlotInfo;
var r:TPlotInfo;
    c:TPlotter;
begin
   if (index>=0) and (index<count) and (items[index]<>nil) then
   begin
     c:=TPlotter(items[index]);
     r.id:=IntToStr(c.PID);
     r.caption:=c.Caption;
     if (c.IsUsed) then
        r.status:='used'
     else
        r.status:='free';
   end else
   begin
     r.id:='';
     r.caption:='';
     r.status:='';
   end;
   result:=r;
end;

{find and return plot window with given ID, or nil}
function TPlotList.FindByID(id:integer):TPlotter;
var i:integer;
begin
  i:=0;
  while (i<Count) and (TPlotter(items[i]).PID<>ID) do i:=i+1;
  if (i>=Count) then result:=nil else result:=TPlotter(items[i]);
end;

{return order index of given plot window}
function TPlotList.IndexOfPlotter(plt:TPlotter):integer;
begin
  result:=IndexOf(plt);
end;

function TPlotList.GetUniqueID:integer;
{get unique ID for a new plot}
var ID:integer;
    PL:TPlotter;
    found:boolean;
begin
  if (count=0) then
  begin
      result:=1;
  end else
  begin
    id:=0;
    found:=false;
    // find the lowest available id
    while (id<MAXWIN) and (not found) do
    begin
       inc(id);
       PL:=FindByID(id);
       found:=(PL=nil);
    end;
    if not found then id:=MAXWIN-1; // this should never happen ..
    result:=id;
  end;
end;

{Show all plots}
procedure TPlotList.ShowAllPlots;
var i:integer;
    PL:TPlotter;
begin
  for i:=0 to Count-1 do
  begin
    PL:=TPlotter(Items[i]);
    PL.ShowInactive;
    PL.BringToFront;
  end;
end;


{Close all plots}
procedure TPlotList.CloseAllPlots;
var i:integer;
    PL:TPlotter;
begin
  for i:=0 to Count-1 do
  begin
    PL:=TPlotter(Items[i]);
    PL.Close;
  end;
end;

{Select the first plot window in closed state or create a new one.
Return its order number as ID}
function TPlotList.OpenWorkstation:integer;
var i:integer;
    PL:TPlotter;
begin
// find th first plotter window which is in closed state
  i:=0;
  while (i<count) and TPlotter(items[i]).isUsed do i:=i+1;
// There is no free plot window left, create a new one and add it to the list
  if i>=count then
  begin
     PL:=CreateNewPlot;
     PL.PID:=GetUniqueID;
     PL.Caption:=format('%s %d',[WINNAME,PL.PID]);
     Add(PL);
  end else
// Choose the first available window
  begin
     PL:=TPlotter(items[i]);
  end;
  PL.PlotOpen; // mark as open and do initialization of colors and other defaults ..
  result:=PL.PID;
end;





{Delete given plot from the list, but do not free it! }
procedure TPlotList.DeletePlot(plt:TPlotter);
var i:integer;
begin
  i:=IndexOf(plt);
  if (i>=0) then
  begin
    TPlotter(items[i]).PlotClose;
    Delete(i);
  end;
end;

{Close all plots and delete them from the list. Don't free the plots !}
procedure TPlotList.DeleteAll;
var i:integer;
begin
  for i:=0 to count-1 do
  begin
    TPlotter(items[i]).PlotClose;
    items[i]:=nil;
  end;
  Pack;
end;


{Destroy all unused plots and rearrange list}
procedure TPlotList.FreeClosed;
var i:integer;
    var rmlist:TFPList;
begin
  if count>0 then
  begin
    rmlist := TFPList.Create;
    for i:=0 to count-1 do
        if not TPlotter(items[i]).isUsed then rmlist.Add(items[i]);
    FreeDeviceList(rmlist);
    rmlist.free;
  end;
end;

{Destroy all plot windows}
procedure TPlotList.FreeAll;
var rmlist:TFPList;
begin
   rmlist := TFPList.Create;
   rmlist.AddList(self);
   FreeDeviceList(rmlist);
   rmlist.free;
end;

{Destructor with cleanup}
destructor TPlotList.Destroy;
begin
   FreeAll;
   inherited destroy;
end;



initialization
  Devices := TPlotList.Create;
  DEF_WIDTH:=480;
  DEF_HEIGHT:=640;

finalization
  Devices.Free;

end.

