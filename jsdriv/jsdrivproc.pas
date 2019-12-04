unit jsdrivproc;
{
 JSDRIV - PGPLOT driver for Windows
 J. Saroun, 2019, jsaroun@seznam.cz

 This unit implements the function GrfExec, which executes the PGPLOT driver
 call (jsdriv, called by GREXEC) on the server side.

 handles connection to the jsdriv_server, sends client's requests to the server
 and returns back the data received by the server.
}

{$mode objfpc}{$H+}
interface
 uses Classes, Graphics,Controls,Windows,plotwin, pstream, jsdriv_comm;

 
{Implementation of PGPLOT GREXEC function on the server side. The arguments are:
calling client, function parameters as a pstream.TFPar object }
procedure GrfExec(client:TJSDrivClient;par:TFPar);


implementation
uses  jsdrivlist, Forms, SysUtils, Dialogs;

{Implementation of PGPLOT GREXEC function on the server side}
procedure GrfExec(client:TJSDrivClient;par:TFPar);
var
   S: string;
   I,I2X0,I2Y0,I2X1,I2Y1:integer;
   plt: TPlotter;
   sz:TPoint;
begin
try
   plt:=client.Selected;
   case par.ifunc of

// Return device type ------------------------------------------------------
   1: begin
        S:='JSDRIV  (Graphics server for Windows)';
        par.nchar:=min(par.nchar,length(S));
        StrLCopy(par.cbuf,PChar(S),par.nchar);
      end;

// Return maximum dimensions of view surface, and range of color index
   2: begin
         par.rbuf[0] := 0;
         par.rbuf[1] := -1;
         par.rbuf[2] := 0;
         par.rbuf[3] := -1;
         par.rbuf[4] := 0;
         par.rbuf[5] := CPAL-1;
      end;

// Return device resolution ---------------------------------------------------
   3: begin
         par.rbuf[0]:=Screen.PixelsPerInch;
         par.rbuf[1]:=Screen.PixelsPerInch;
         par.rbuf[2]:=1.0;
      end;

// Return device capabilities -------------------------------------------------
   4: begin

{    chr[0] = 'I'; /* Interactive device */                                 1
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
        //S:='ICNATRPNYNN'; // XWDRIV setting
        par.nchar:=min(par.nchar,length(S));
        StrLCopy(par.cbuf,PChar(S),par.nchar);
      end;

// Return default device/file name --------------------------------------------
   5: begin
        par.nchar:=0;
        par.cbuf:=char(0);
      end;

// Return default size of view surface  ---------------------------------------
   6: if (plt<>nil) then
      begin
        par.rbuf[0] := 0;
        par.rbuf[1] := plt.PlotArea.Width;
        par.rbuf[2] := 0;
        par.rbuf[3] := plt.PlotArea.Height;
      end else
      begin
        sz:=DefaultWinSize;
        par.rbuf[0] := 0;
        par.rbuf[1] := sz.x;
        par.rbuf[2] := 0;
        par.rbuf[3] := sz.y;
      end;

// Return miscellaneous defaults ----------------------------------------------
   7: begin
         par.rbuf[0] :=1;
      end;

// Select plot by ID ----------------------------------------------------------
   8: begin
         {NOTE: the RBUF(0) is the PGPLOT iner identifier. We need to use RBUF(1).
         PGPLOT makes sure this is the value retruned by Open Workstation}
         client.SelectDevice(round(par.rbuf[1]));
      end;

// Open workstation -----------------------------------------------------------
   9: begin

        I:=client.OpenDevice;
        if I<=0 then // opening of a new plot window failed
        begin
            par.rbuf[0]:=0;
            par.rbuf[1]:=0;
            exit;
        end else begin
            par.rbuf[0]:=I; // Number used to select this device
            par.rbuf[1]:=1; // successful
        end;
      end;

// Close workstation ----------------------------------------------------------
   10: begin
         client.CloseSelected;
       end;

// Begin picture --------------------------------------------------------------
   11: if (plt<>nil) then
       begin
         I2X0:=round(par.rbuf[0]);
         I2Y0:=round(par.rbuf[1]);
         plt.PlotStart(I2X0, I2Y0);
       end;

// Plot line ------------------------------------------------------------------
   12: if (plt<>nil) then
       begin
         I2X0:=round(par.rbuf[0]);
         I2Y0:=round(par.rbuf[1]);
         I2X1:=round(par.rbuf[2]);
         I2Y1:=round(par.rbuf[3]);
         plt.PlotLine(I2X0,I2Y0,I2X1,I2Y1);
       end;

// Plot dot -------------------------------------------------------------------
   13: if (plt<>nil) then
       begin
         I2X0:=round(par.rbuf[0]);
         I2Y0:=round(par.rbuf[1]);
         plt.PlotDot(I2X0,I2Y0);
       end;

// End picture ----------------------------------------------------------------
   14: if (plt<>nil) then
       begin
         plt.PlotEnd;
      //   Application.ProcessMessages;
       end;

// Select color index ---------------------------------------------------------
   15: if (plt<>nil) then
       begin
         I:=round(par.rbuf[0]);
         plt.SetPenIndx(I);
       end;

// Flush buffer ---------------------------------------------------------------
   16: if (plt<>nil) then
       begin
         if (not plt.IsStatic) then plt.FlushImage;
//         Application.ProcessMessages;
       end;

// Read cursor ----------------------------------------------------------------
    {
    17: treated internally by TPlotter
    }

// Erase alpha screen.---------------------------------------------------------
// (Not implemented: no alpha screen)
   18: begin
       end;

// Set line style. ------------------------------------------------------------
// (Not implemented: should not be called)
   19: begin
       end;

// Polygon fill --------------------------------------------------------------
   20: if plt<>nil then
       begin
         plt.PlotPolygon(par.rbuf,par.nbuf);
       end;

// Set color representation ---------------------------------------------------
   21: if plt<>nil then
       begin
         I:=round(par.rbuf[0]);
         plt.SetColorIndx(I,par.rbuf[1],par.rbuf[2],par.rbuf[3]);
       end;

// Set line width -------------------------------------------------------------
   22: if plt<>nil then
       begin
         plt.PenWidth:=round(par.rbuf[0]*0.005*Screen.PixelsPerInch);
       end;

// Escape ---------------------------------------------------------------------
// (Not implemented)
   23: begin
       end;

// Rectangle fill -------------------------------------------------------------
   24: if plt<>nil then
       begin
         I2X0:=round(par.rbuf[0]);
         I2Y0:=round(par.rbuf[1]);
         I2X1:=round(par.rbuf[2]);
         I2Y1:=round(par.rbuf[3]);
         plt.PlotRect(I2X0,I2Y1,I2X1,I2Y0);
       end;

// (Not implemented)  ---------------------------------------------------------
   25: begin
       end;

// Line of pixels -------------------------------------------------------------
   26: if plt<>nil then
       begin
          plt.PlotRow(par.rbuf,par.nbuf);
       end;

// Query color representation ------------------------------------------------
   29: if plt<>nil then
       begin
         I:=round(par.rbuf[0]);
         plt.GetColorIndx(I,par.rbuf[1],par.rbuf[2],par.rbuf[3]);
       end;
   end;
except
   on E:Exception do
   begin
     MessageDlg('Error in JSDriver (func='+IntToStr(par.ifunc)+'): '+#13+E.Message,mtError,[mbOK],0);
     client.CloseSelected;
   end;
end;
end;



end.
