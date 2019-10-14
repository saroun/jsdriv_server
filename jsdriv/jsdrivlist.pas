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
    public

      {Find a plotter window by ID number}
      function FindByID(id:integer):TPlotter;

      {Get client info using the order index}
      function GetPlotInfo(index:integer):TPlotInfo;

      {Create new plotter window}
      function CreateNewPlot:TPlotter;

      {Select the first plot window in closed state or create a new one.
      Return its order number as ID}
      function OpenWorkstation:integer;

      {Destroy all plots in closed state and rearrange list - keep IDs of open windows!}
      procedure FreeClosed;

      {Delete given plot from the list, but do not free it! }
      procedure DeletePlot(plt:TPlotter);

      {Close all plots and delete them from the list. Don't free the plots !}
      procedure DeleteAll;

      {Destroy all plot windows}
      procedure FreeAll;

      {Destructor with cleanup}
      destructor Destroy; override;

    end;

procedure InitPlots;
procedure ReleaseallPlots;
function DefaultWinSize:TPoint;

var
    devices:TPlotList;  // list of all TPlotter instances


implementation
uses Forms, Dialogs;
var
  DEF_WIDTH,DEF_HEIGHT:integer; // default window size


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

procedure ReleaseallPlots;
begin
  devices.FreeAll;
end;

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
     if (c.IsOpen) then
        r.status:='open'
     else r.status:='closed';
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


{Select the first plot window in closed state or create a new one.
Return its order number as ID}
function TPlotList.OpenWorkstation:integer;
var i:integer;
    PL:TPlotter;
begin
// find th first plotter window which is in closed state
  i:=0;
  while (i<count) and TPlotter(items[i]).isOpen do i:=i+1;
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


{Destroy all plots in closed state and rearrange list - keep IDs of open windows!}
procedure TPlotList.FreeClosed;
var i:integer;
begin
  for i:=0 to count-1 do
  if not TPlotter(items[i]).isOpen then
  begin
    TPlotter(items[i]).PlotClose;
    TPlotter(items[i]).Release;
    TPlotter(items[i]).free;
    items[i]:=nil;
  end;
  Pack;
end;


{Delete given plot from the list, but do not free it! }
procedure TPlotList.DeletePlot(plt:TPlotter);
var i:integer;
begin
  i:=IndexOf(plt);
  if (i>=0) then
  begin
    TPlotter(items[i]).PlotClose;
    //TPlotter(items[i]).Release;
    items[i]:=nil;
  end;
  Pack;
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



{Destroww all plot windows}
procedure TPlotList.FreeAll;
var i:integer;
begin
  for i:=0 to count-1 do
  begin
    if (items[i]<>nil) then
    begin
      TPlotter(items[i]).PlotClose;
      TPlotter(items[i]).Release;
      TPlotter(items[i]).free;
    end;
    items[i]:=nil;
  end;
  Pack;
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

