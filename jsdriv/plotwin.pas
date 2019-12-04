unit plotwin;
{
 JSDRIV - PGPLOT driver for Windows
 J. Saroun, 2019, jsaroun@seznam.cz

 Implements the graphics device - TPlotter.
 TPlotter is a persisent window, i.e. it stays available even after the calling
 client disconnects. While the client is conected, close action only minimizes the window.
 After client disconnects, TPlotter is set to closed state (isOpen=false) and TPlotter.close
 frees the window and removes it from the global list of created TPlotter instances (jsdrivlist.devices).
}
{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  pstream;

type
  TRGB = array [0..2] of byte;

const
  // scaling factor for RGB values = round((0..1)*CMULT)
  CMULT = 255;
  // number of colors
  CPAL = 256;
  // default PGPLOT palette
  PGPAL: array[0..15] of TRGB = (
    (0, 0, 0), (255, 255, 255), (255, 0, 0), (0, 255, 0),
    (0, 0, 255), (0, 255, 255), (255, 0, 255), (255, 255, 0),
    (255, 128, 0), (128, 255, 0), (0, 255, 128), (0, 128, 255),
    (128, 0, 255), (255, 0, 128), (86, 86, 86), (172, 172, 172));

  {PGPAL: array[0..15] of TRGB = (
    (255, 255, 255), (0, 0, 0),  (255, 0, 0), (0, 255, 0),
    (0, 0, 255), (0, 255, 255), (255, 0, 255), (255, 255, 0),
    (255, 128, 0), (128, 255, 0), (0, 255, 128), (0, 128, 255),
    (128, 0, 255), (255, 0, 128), (86, 86, 86), (172, 172, 172));
   }

  CSIZE = 256; // size of the RGB palette
  DefColorBg = clBlack; // Default background color
  DefColorBr = clWhite; // Default brush (fill) color
  DefColorPen = clWhite; // Default pen color

  //BgColor = clWhite; // Default background color
 // BrColor = clBlack; // Default brush (fill) color
 // PenColor = clBlack; // Default pen color

  DefLineWidth = 1; // Default line width


type
  TRGBPal = array [0..CPAL - 1] of TColor;
  TPlotState = (psOpen, psSelected, psDrawing, psCaptured);
  TPlotStates = set of TPlotState;

  {Record used to handle user input events}
  TInput = class(TObject)
  protected
    CurX, CurY: integer;  // mouse position on MouseUp
    InputKey: string;  // response on KeyPressed
    qry:TJSDrivMsg;  // jsdriv function parameters to be sent back after the input
  public
    constructor create;
    destructor destroy; override;
  end;

  { TPlotter }

  TPlotter = class(TForm)
    Image: TImage;
    Panel1: TPanel;
    fBar: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
     procedure SetImageCursor;
  protected
     fUsed:boolean;
     fPID: integer;
     fBuf: TBitmap; // buffer for bitmap image
     fPlotState: TPlotStates;
     fClBack, fClPen, fClBrush: TColor; // background, pen and brush colors
     fPenWidth: integer;
     fCorner: TPanel; // invisible panel to force form scrolling
     fMargin: TPoint; // picture margins
     fPlotArea: TRect; // actual plotting area without margins
     fPolygonV: array of TPoint; // polygon vertices
     fPolygonN: longint; // number of polygon vertices
     needRefresh: boolean; // flag allowing to refresh image in FlushImage
     fStatic: boolean; // flag for static device: only flush buffer on PGEND (ifunc=10)
     {Setters ...}
     procedure SetPlotState(Value: TPlotStates);
     procedure SetClPen(Value: TColor);
     procedure SetClBrush(Value: TColor);
     procedure SetPenWidth(Value: integer);
     function getIsOpen:boolean;
     function getIsSelected:boolean;

     { ************  Handling of plot area *****************}

     {Set plot area and adjust bitmap buffer size if necessary}
     procedure SetPlotArea(w, h:longint);
     {Adjust client area to the plot size}
     procedure AdjustClientSize;
     {Calls SetPlotArea and clears window. If w || h = 0, adjust plot area to the client size.}
     procedure DefineVieport(w, h:longint);
     {Convert PGPLOt xy coordinates to pixel coordinates on the bitmap}
     function XYToPixel(x,y:longint):TPoint;
     {Convert pixel coordinates on the bitmap to the PGPLOt xy coordinates}
     function PixelToXY(x,y:longint):TPoint;
     {Convert pixel coordinates to the client coordinates}
     function PixelToClient(p:TPoint):TPoint;
     {Calculate required client size from plot area.
     makes client size = plot size + margins + 2 pixels}
     function CalClientSize:TPoint;
     {Calculate required plot area from client size, inverse to CalClientSize}
     function CalPlotSize:TPoint;
     {Clear plot area}
     procedure PlotClr;

     { ************ Handling of visual components ****************}

     {Update info on status bar}
     procedure UpdateStatus(i: integer; content: string);
     procedure InitColors;
  public
     {tracking mouse position on MouseMove}
      MousePos:TPoint;
     {palette colors}
     RGBPal: TRGBPal;
     {Mouse/key input}
     Input:TInput;

     {Driver plot commands - called by GrfExec }

     procedure PlotSelect; // 8: select plot window
     procedure PlotOpen; // 9: open workstation
     procedure PlotClose; // 10: close workstation
     procedure PlotStart(w, h:longint); // 11: Begin plot
     procedure PlotLine(x1, y1, x2, y2: integer);  // 12: plot a line
     procedure PlotDot(x, y: integer);  // 13: plot a dot
     procedure PlotEnd;  // 14: End plot
     procedure SetPenIndx(indx: integer); // 15: Select color index
     procedure FlushImage; // 16: flush image buffer
     procedure StartInput(query:TJSDrivMsg); // 17: start input event
     procedure EndInput; // 17: return inputevent
     procedure PlotPolygon(rec:TDynArray; nbuf: integer); // 20: fill a polygon
     procedure SetColorIndx(indx: integer; red, green, blue: single);  // 21: set color
     procedure PlotRect(x1, y1, x2, y2: integer); // 24: fill a rectangle
     procedure PlotRow(rec:TDynArray; nbuf: integer);  // 26: plot a line of pixels
     procedure GetColorIndx(indx: integer; var red, green, blue: single); // 29: get color

     // driver Open/close actions
     procedure PlotDeselect;
     procedure ShowInactive;


{ properties }
     property PID: integer read fPID write fPID; // plot identifier
     property Margin: TPoint read fMargin write fMargin;
     property Bar: TStatusBar read fBar write fBar;
     // state flags
     property PlotState: TPlotStates read fPlotState write SetPlotState;
     property IsOpen:boolean read getIsOpen;
     property IsSelected:boolean read getIsSelected;
     property IsUsed:boolean read fUsed write fUsed;
     property  IsStatic:boolean read fStatic write fStatic;
     // pen attributes etc.
     property ClBack: TColor read fClBack write fClBack;
     property ClPen: TColor read fClPen write SetClPen;
     property ClBrush: TColor read fClBrush write SetClBrush;
     property PenWidth: integer read fPenWidth write SetPenWidth;
     property PlotArea: TRect read fPlotArea;
  end;



implementation
uses jsdrivlist, jsdriv_comm, Math, MainUnit;

const
  {$IFDEF DEBUG}
    DBG=true;
  {$ELSE}
    DBG=false;
  {$ENDIF}
{$R *.lfm}


function GetStateStr(ST: TPlotStates): string;
begin
  if (psCaptured in ST) then
    Result := 'captured'
  else
  if (psDrawing in ST) then
    Result := 'drawing'
  else
  if (psSelected in ST) then
    Result := 'selected'
  else
  if (psOpen in ST) then
    Result := 'open'
  else
    Result := 'closed';
end;

procedure LogInfo(msg:string);
begin
 if DBG then MainForm.LogMemo.Append(msg);
end;

procedure LogPenInfo(p:TPlotter);
var fmt, msg:string;
begin
   fmt:='Pen width: %d, color: %x, brush: %x';
   msg:=format(fmt, [p.PenWidth, p.ClPen, p.ClBrush]);
   if DBG then MainForm.LogMemo.Append(msg);
end;



function RGB2Color(RC: TRGB): TColor;
begin
  Result := RC[0] + (RC[1] shl 8) + (RC[2] shl 16);
end;

function Color2RGB(C: TColor): TRGB;
var
  RC: TRGB;
begin
  RC[0] := C and $000000FF;
  RC[1] := (C and $0000FF00) shr 8;
  RC[2] := (C and $00FF0000) shr 16;
  Result := RC;
end;

function Int2Color(red, green, blue: single): TColor;
var
  RC: TRGB;
begin
  RC[0] := MIN(CMULT, round(red * CMULT));
  RC[1] := MIN(CMULT, round(green * CMULT));
  RC[2] := MIN(CMULT, round(blue * CMULT));
  Result := RGB2Color(RC);
end;

procedure Color2Int(C: TColor; var red, green, blue: single);
var
  RC: TRGB;
begin
  RC := Color2RGB(C);
  red := RC[0] / CMULT;
  green := RC[1] / CMULT;
  blue := RC[2] / CMULT;
end;

{ TInput }
constructor TInput.create;
begin
  inherited create;
  self.qry:=TJSDrivMsg.Create;
end;

destructor TInput.destroy;
begin
  qry.free;
  inherited destroy;
end;

{ TPlotter }

procedure TPlotter.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var PL:TPlotter;
begin
  if (Sender is TPlotter) then
  begin
    PL:=Sender as TPlotter;
    if (PL.isUsed) then
    begin
      CloseAction := caMinimize;
    end else
    begin
     // devices.DeletePlot(PL);
     // CloseAction := caFree;
      CloseAction := caHide;
    end;
  end;
end;

procedure  TPlotter.InitColors;
var i:integer;
    grey:single;
begin
  // initialize palette
  for i := 0 to 15 do
    RGBPal[i] := RGB2Color(PGPal[i]);
  for i := 16 to CPAL - 1 do
  begin
    grey := 1.0 - (i - 16) / (CPAL - 1 - 16);
    RGBPal[i] := Int2Color(grey, grey, grey);
  end;
end;

procedure TPlotter.FormCreate(Sender: TObject);
var sz:TPoint;
begin
  Input:=TInput.create;
  fMargin := Point(10, 10);
  fPlotArea := Rect(0,0,0,0);
  FormStyle := fsStayOnTop;
  fPID := 0;
  fPlotState := [];
  fStatic := false;
  InitColors;
  // create bitmap buffer
  fBuf := TBitmap.Create;
  fBuf.PixelFormat := pf24bit;
  Color:=clBlack;
  ClBrush:=DefColorBr;
  ClPen:=DefColorPen;
  ClBack:=DefColorBg;
  PenWidth:=DefLineWidth;
  sz:=DefaultWinSize;
  ClientWidth:=sz.x+2*Margin.X;
  ClientHeight:=sz.y+2*Margin.Y;
  DefineVieport(0,0);  // sets plot area to fit the client size and sets buffer bitmap size.
  //FlushImage; // draw empty plot, just background ...
end;

procedure TPlotter.FormDestroy(Sender: TObject);
begin
  devices.DeletePlot(Self);
  Input.free;
end;



{ ----------------------------- Viewport setting ---------------------------- }

{Set plot area and adjust bitmap buffer size if necessary}
procedure TPlotter.SetPlotArea(w, h:longint);
var
  w1, h1:longint;
begin
   fPlotArea:=Rect(fMargin.x, fMargin.y, fMargin.x+w, fMargin.y+h);
   // newly requested buffer size:
   w1:=fPlotArea.Width+2*fMargin.x;
   h1:=fPlotArea.Height+2*fMargin.y;
   if ( (fBuf.width<>w1) or (fBuf.height<>h1) ) then
   begin
      fBuf.width:=w1;
      fBuf.height:=h1;
   end;
end;

{Convert PGPLOt xy coordinates to pixel coordinates on the bitmap}
function TPlotter.XYToPixel(x,y:longint):TPoint;
begin
    result:=Point(fPlotArea.Left + x, fPlotArea.Bottom - y);
end;
{Convert pixel coordinates on the bitmap to the PGPLOt xy coordinates}
function TPlotter.PixelToXY(x,y:longint):TPoint;
begin
    result:=Point(x - fPlotArea.Left, fPlotArea.Bottom - y);
end;

function TPlotter.PixelToClient(p:TPoint):TPoint;
begin
  result.x:=p.x+Image.left;
  result.y:=p.y+Image.top;
end;

{Calculate required client size from plot area.
makes client size = plot size + margins + 2 pixels}
function TPlotter.CalClientSize:TPoint;
begin
  result:=Point(fPlotArea.Width+2*fMargin.x+2,
                fPlotArea.Height+2*fMargin.y+2+fBar.Height);
end;

{Calculate required plot area from client size, inverse to CalClientSize}
function TPlotter.CalPlotSize:TPoint;
begin
  result:=Point(ClientWidth-2*fMargin.x-2, ClientHeight-2*fMargin.y-2-fBar.Height);
end;


procedure TPlotter.AdjustClientSize;
{Make client area equal to the bitmap size + 2 pixels}
var
  cs:TPoint;
  needresize:boolean;
begin
  cs:=CalClientSize;
  needresize := (cs.x<>ClientWidth) or (cs.y<>ClientHeight);
  if (needresize) then
  begin
    ClientWidth:=cs.x;
    ClientHeight:=cs.y;
    invalidate;
  end;
end;

procedure TPlotter.DefineVieport(w, h:longint);
{Set new plot area. width, height = requested plot area without margins.
Adjusts bitmap buffer and clears window.
To be called on ifunc=11 (begin plot)
}
var
  sz:TPoint;
begin
  // w,h = size of image area
  if (w=0) or (h=0) then sz:=CalPlotSize else sz:=Point(w,h);
  SetPlotArea(sz.x,sz.y);
  PlotClr;
end;

{ ----------------------------- Open-close actions ---------------------------- }


procedure TPlotter.ShowInactive;
begin
  WindowState := wsNormal;
  ShowWindow(Self.Handle, SW_SHOWNOACTIVATE);
  Self.Visible := True;
  //BringToFront;
end;


procedure TPlotter.PlotOpen; // 9: open workstation
begin
  InitColors;
  PlotState := PlotState + [psOpen];
end;

procedure TPlotter.PlotSelect; // 8: select plot window
var
  sz:TPoint;
begin
  PlotState := PlotState + [psSelected];
  ShowInactive;
  // adjust plot area for actual client size;
  sz:=CalPlotSize;
  SetPlotArea(sz.x,sz.y);
end;

procedure TPlotter.PlotDeselect; // deselect plot window
begin
  //Hide;
  PlotState := PlotState - [psSelected];
end;

procedure TPlotter.PlotStart(w, h:longint);  // 11: Begin plot
{ Clear the plot when required and set state to 1 }
begin
  //needrefresh:=true;
  LogInfo('PlotStart '+BoolToStr(needrefresh));
  //FlushImage;
  DefineVieport(w, h);
  UpdateStatus(0,format('Plot size: %dx%d',[fBuf.Width,fBuf.Height]));
  PlotState := PlotState + [psDrawing];
end;


procedure TPlotter.PlotEnd; // 14: End plot
{ Flush image and set state to 0}
begin
  AdjustClientSize;
  //needrefresh:=true;
  LogInfo('PlotEnd '+BoolToStr(needrefresh));
  FlushImage;
  PlotState := PlotState - [psDrawing];
  ShowInactive;
  BringToFront;
end;


procedure TPlotter.PlotClose; // 10: close workstation
begin
  PlotState := [];
  //Close;
end;

// ***********************  P R O P E R T Y   A C C E S S ******************

procedure TPlotter.SetImageCursor;
{Determine cursor from the PlotState value}
begin
  // nothing to do on shutdown ...
  if (MainForm.isClosing) then exit;
  try
  if (psCaptured in PlotState) then
    Image.Cursor := crCross
  else
  if (psDrawing in PlotState) then
    Image.Cursor := crHourGlass
  else
  if (psOpen in PlotState) then
    Image.Cursor := crHandPoint
  else
    Image.Cursor := crDefault;
  except
  end;
end;

procedure TPlotter.SetPlotState(Value: TPlotStates);
begin
  fPlotState := Value;
  SetImageCursor;
end;

procedure TPlotter.SetClPen(Value: TColor);
begin
  fClPen := Value;
  fBuf.Canvas.Pen.Color := Value;
 // LogPenInfo(self);
end;

procedure TPlotter.SetClBrush(Value: TColor);
begin
  fClBrush := Value;
  fBuf.Canvas.Brush.Color := Value;
 // LogPenInfo(self);
end;

procedure TPlotter.SetPenWidth(Value: integer);
begin
  fPenWidth := Value;
  fBuf.Canvas.Pen.Width := Value;
//  LogPenInfo(self);
end;


procedure TPlotter.SetColorIndx(indx: integer; red, green, blue: single);
{Set color of the device palette}
var c:TColor;
begin
  c:=Int2Color(red, green, blue);
  //LogInfo(format('SetColorIndx: %d = %x',[indx, c]));
  if (indx >= 0) and (indx < CPAL) then
    RGBPal[indx] := c;
end;

procedure TPlotter.SetPenIndx(indx: integer);
{Set color index for Pen}
begin
 // LogInfo(format('SetPenIndx: %d',[indx]));
  if (indx >= 0) and (indx < CPAL) then
    ClPen := RGBPal[indx];
end;

procedure TPlotter.GetColorIndx(indx: integer; var red, green, blue: single);
{Get RGB intensity of the indx-th color of the device palette}
begin
  if (indx >= 0) and (indx < CPAL) then
    Color2Int(RGBPal[indx], red, green, blue)
  else
    Color2Int(RGBPal[0], red, green, blue);
end;

function TPlotter.getIsOpen:boolean;
begin
  result:= (psOpen in  fPlotState);
end;

function TPlotter.getIsSelected:boolean;
begin
  result:= (psSelected in  fPlotState);
end;



{ ------------------------  D R A W I N G -------------------------------}

procedure TPlotter.FlushImage;
{Assign bitmap buffer to the Image.Picture component}
begin
 // if (needrefresh) then
 //    LogInfo('FlushImage true')
 // else
 //    LogInfo('FlushImage false');
  if (needRefresh) then
  begin
       Image.Picture.Assign(fBuf);
       Image.Invalidate;
       Image.Refresh;
       needRefresh:=false;
       Invalidate;
  end;
end;

procedure TPlotter.PlotClr;
{Fill image buffer with BgColor and flush}
var
  R: TRect;
begin
  R := Rect(0, 0, fBuf.Width, fBuf.Height);
  fBuf.Canvas.Brush.Color := DefColorBg;
  fBuf.Canvas.FillRect(R);
  fBuf.Canvas.Brush.Color := DefColorBr;
  needRefresh:=true;
 // FlushImage;
end;

procedure TPlotter.PlotDot(x, y: integer);
{Plot a spot at (x1,y1) accordinto the current pen width}
var
  pt1,pt2:TPoint;
  dpx:real;
begin
  if PenWidth <= 0 then
  begin
    pt1:=XYToPixel(x,y);
    fBuf.Canvas.Pixels[pt1.x, pt1.y] := ClPen
  end else
  begin
    ClBrush := ClPen;
    dpx:= 0.5*PenWidth;
    pt1:=XYToPixel(round(x - dpx),round(y + dpx));
    pt2:=XYToPixel(round(x + dpx),round(y - dpx));
    fBuf.Canvas.Ellipse(pt1.x, pt1.y, pt2.x, pt2.y);
    ClBrush := ClBack;
  end;
  needRefresh:=true;
end;

procedure TPlotter.PlotLine(x1, y1, x2, y2: integer);
{Plot a line}
var pt1,pt2:TPoint;
begin
  pt1:=XYToPixel(x1, y1);
  pt2:=XYToPixel(x2, y2);
  fBuf.Canvas.MoveTo(pt1);
  fBuf.Canvas.LineTo(pt2);
  needRefresh:=true;
end;

procedure TPlotter.PlotRect(x1, y1, x2, y2: integer);
{Plot a filled rectangle}
var pt1,pt2:TPoint;
    r:Trect;
begin
  ClBrush := ClPen;
  pt1:=XYToPixel(x1, y1);
  pt2:=XYToPixel(x2, y2);
  r:= Rect(min(pt1.x, pt2.x),min(pt1.y, pt2.y),
           max(pt1.x, pt2.x),max(pt1.y, pt2.y));
  fBuf.canvas.FillRect(r);
  ClBrush := ClBack;
  needRefresh:=true;
end;


procedure TPlotter.PlotPolygon(rec:TDynArray; nbuf: integer);
{Polygon is implemented in multiple calls:
1st call sends only the number of vertices. Next calls define vertices.
Polygon is drawn after the last vertex. This is handled by TPlotWin automatically.}
var
  ix, iy:integer;
  isInit,isVertex:boolean;
begin
  isInit:= (nbuf > 0) and (fPolygonN=0);
  isVertex := (nbuf > 1) and (fPolygonN>0) and (length(fPolygonV)>0);

  ix:=round(rec[0]);
  if (isInit) then
  // new polygon requested - set number of vertices and allocate fPolygonV
  begin
     ix:=round(rec[0]);
     SetLength(fPolygonV,ix);
     fPolygonN:=ix;
  end else
  // next vertex received
  if (isVertex) then
  begin
    ix:=round(rec[0]);
    iy:=round(rec[1]);
    fPolygonN:=fPolygonN-1;
    fPolygonV[fPolygonN]:=XYToPixel(ix,iy);
    // on the last vertex, plot it and free buffer
    if (fPolygonN<=0) then
    try
      ClBrush := ClPen;
      fBuf.Canvas.Polygon(fPolygonV);
    finally
      fPolygonN:=0;
      SetLength(fPolygonV,0);
      ClBrush := ClBack;
    end;
  end else
 // something is wrong, free array and set fPolygonN=0
  begin
    SetLength(fPolygonV,0);
    fPolygonN:=0;
  end;
  needRefresh:=true;
end;


procedure TPlotter.PlotRow(rec:TDynArray; nbuf: integer);
{plot a row of pixels.
rec[0] ... starting column, x1
rec[1] ... starting row, y1
rec[i=2..nbuf-1] ... colors to be placed at pixels (x1+i-3,y1+i-3)
}
var
  x1, y1, i, x2, ic: integer;
  B: TBitmap;
  pt,pt0:TPoint;
begin
  if (nbuf >= 3) then
  begin
    x1 := round(rec[0]);
    y1 := round(rec[1]);
    if (y1 >= 0) and (y1 <= fPlotArea.Height) then
    begin
      B := TBitmap.Create;
      B.PixelFormat := fBuf.PixelFormat;
      try
        B.Height := 1;
        x2 := x1 + nbuf - 3;
        if x2 >= fPlotArea.Width then x2 := fPlotArea.Width-1;
        B.Width := x2 - x1 + 1;
        pt0:=XYToPixel(x1,y1);
        B.BeginUpdate(true);
        for i := x1 to x2 do
        begin
          ic := round(rec[i - x1 + 2]);
          pt:=XYToPixel(i,y1);
          B.Canvas.pixels[pt.x-pt0.x, 0] := RGBPal[ic];
        end;
        B.EndUpdate(true);
        fBuf.canvas.Draw(pt0.x, pt0.y, B);
      finally
        B.Free;
      end;
    end;
    needRefresh:=true;
  end;
end;

{-----------------------------  E V E N T S ------------------------------------}

// 17: return input event
procedure TPlotter.EndInput;
var ipcClient:TJSDrivClient;
begin
  try
     //LogTxt('End input');
     Input.qry.Param.rbuf[0]:=Input.CurX;
     Input.qry.Param.rbuf[1]:=Input.CurY;
     Input.qry.Param.nbuf:=2;
     Input.qry.Param.nchar:=1;
     StrLCopy(Input.qry.Param.cbuf,PChar(Input.InputKey),1);
     ipcClient:=clients.FindClient(Input.qry.source);
     if (ipcClient<>nil) then
     begin
          //LogTxt('Input sent');
          ipcClient.SendQuery(Input.qry);
     end;
  finally
     PlotState:=PlotState-[psCaptured];
  end;
end;

 // 17: start input event
procedure TPlotter.StartInput(query:TJSDrivMsg);
var PT:TPoint;
    P:TPoint;
   // Msg:TMsg;
begin
  // ths source query is destroyed by sender, so we need to keep a copy:
  Input.qry.Assign(query);
  // don't do this:  Input.qry:=query;
  Input.CurX:=round(query.Param.rbuf[0]);
  Input.CurY:=round(query.Param.rbuf[1]);

  P:=PixelToClient(XYToPixel(Input.CurX, Input.CurY));
  PT:=ClientToScreen(P);
  SetCursorPos(PT.X,PT.Y);
  Visible:=true;
  BringToFront;
  if (not Focused) then SetFocus;
  PlotState:=PlotState+[psCaptured];
  //LogTxt('Start input');
// wait for user's action
{
    while fCaptured do
    begin
         while PeekMessage( Msg, Self.Handle, 0, 0 , PM_REMOVE ) do
         begin
              TranslateMessage( Msg );
              DispatchMessage(Msg );
         end;
         sleep(10);
    end;
    x1:=CurX;
    x2:=CurY;
    S:=InputKey;
}
end;


procedure TPlotter.FormResize(Sender: TObject);
{ On resize: only update status bar}
begin
  //  Map.Width:=ClientWidth;
  //  Map.Height:=ClientHeight;
  //  Map.Canvas.Draw(0,0,fBuf);
  //  With Image do
  UpdateStatus(1,format('Window size: %dx%d',[ClientWidth, ClientHeight]));
//  UpdateStatus(1,format('Window size: %dx%d',[Map.Width,Map.Height]));
end;

procedure TPlotter.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  MousePos:=PixelToXY(x,y);
  UpdateStatus(1,format('Position:  %dx%d',[MousePos.x,MousePos.y]));
end;

procedure TPlotter.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (psCaptured in PlotState) then
  begin
    // LogTxt('Mouse up');
    MousePos:=PixelToXY(x,y);
    Input.CurX := MousePos.x;
    Input.CurY := MousePos.y;
    case Button of
      mbLeft: Input.InputKey:='A';
      mbRight: Input.InputKey:='D';
      mbMiddle: Input.InputKey:='X';
      else Input.InputKey:='A';
    end;
    EndInput;
    //MainForm.LogMemo.Append(format('Mouse up: (%d, %d), coord=(%d, %d)',[X,Y,MousePos.x,MousePos.y]));
  end;
end;

procedure TPlotter.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (psCaptured in PlotState) then
  begin
      Input.CurX := MousePos.x;
      Input.CurY := MousePos.y;
      Input.InputKey:=Key;
      EndInput;
      //MainForm.LogMemo.Append(format('KeyPressed: coord=(%d, %d)',[MousePos.x,MousePos.y]));
  end;
end;


{can resize only if drawing is completed

procedure TPlotter.CanRes(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  Resize:=(PlotState*[psSelected,psDrawing,psCaptured]=[]);
end;
}

procedure TPlotter.UpdateStatus(i: integer; content: string);
begin
  Bar.Panels[i].Text := content;
  self.Invalidate;
end;




end.

