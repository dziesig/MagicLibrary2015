unit MagicClock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type
  TPosition = ( cpLeft, cpCenter, cpRight );

  TOption = ( rrcHideCalendar, rrcHideClock );
  TOptions = set of TOption;

  THeartBeatEvent = procedure( Sender : TObject; out Time : TDateTime ) of object;

  { TMagicClock }

  TMagicClock = class(TCustomPanel)
  private
    fAMPM: Boolean;
    fCalendarFormat: String;
    fCalendarPosition: TPosition;
    fClockPosition: TPosition;
    fColorStopped: TColor;
    fColorStoppedText: TColor;
    fColorText: TColor;
    fDateTime: TDateTime;
    fMarginsLR: Integer;
    fOnHeartBeat: THeartBeatEvent;
    fOptions: TOptions;
    fRunning: Boolean;
    fSpeedMultiplier: Word;
    fSpeedVernir: Double;
    procedure SetAMPM(AValue: Boolean);
    procedure SetCalendarFormat(AValue: String);
    procedure SetCalendarPosition(AValue: TPosition);
    procedure SetClockPosition(AValue: TPosition);
    procedure SetColorStopped(AValue: TColor);
    procedure SetColorStoppedText(AValue: TColor);
    procedure SetColorText(AValue: TColor);
    procedure SetDateTime(AValue: TDateTime);
    procedure SetMarginsLR(AValue: Integer);
    procedure SetOptions(AValue: TOptions);
    procedure SetRunning(AValue: Boolean);
    procedure SetSpeedMultiplier(AValue: Word);
    procedure SetSpeedVernir(AValue: Double);
    { Private declarations }
  protected
    { Protected declarations }
    vCalendarTop  : Integer;
    vCalendarLeft : Integer;
    vClockTop     : Integer;
    vClockLeft    : Integer;
    vLastFontSize : Integer;
    vAnimationPos : Integer;
    vAnimationX,
    vAnimationY   : Integer;

    vOneMinute    : TDateTime;

    vAnimationTimer : TTimer;
    vRRTimer      : TTimer;

    vDateStr         : String;

    procedure SetElementPositions;

    procedure Paint; override;
    procedure SizeChanged( Sender : TObject );
    //procedure ToggleColon( Sender : TObject );
    procedure AnimateClock( Sender : TObject );
    procedure IncrementMinutes( Sender : TObject );

    function TimeStr( theTime : TDateTime ) : String;

  public
    { Public declarations }
    constructor Create(theOwner: TComponent); override;
  published
    { Published declarations }
    property Align;
    property AMPM : Boolean read fAMPM write SetAMPM;
    property CalendarFormat   : String    read fCalendarFormat   write SetCalendarFormat;
    property CalendarPosition : TPosition read fCalendarPosition write SetCalendarPosition;
    property Caption;
    property ClockPosition : TPosition read fClockPosition write SetClockPosition;
    property Color; // Clock Running Background
    property ColorStopped     : TColor read fColorStopped     write SetColorStopped default clRed;
    property ColorText        : TColor read fColorText        write SetColorText; // Clock Running Font Color
    property ColorStoppedText : TColor read fColorStoppedText write SetColorStoppedText default clWhite;
    property DateTime         : TDateTime read fDateTime      write SetDateTime;
    property Font;
    property MarginsLR        : Integer read fMarginsLR       write SetMarginsLR;
    property Options          : TOptions read fOptions        write SetOptions;
    property Running          : Boolean read fRunning         write SetRunning;
    property SpeedMultiplier  : Word    read fSpeedMultiplier write SetSpeedMultiplier;
    property SpeedVernir      : Double  read fSpeedVernir     write SetSpeedVernir;

    property OnHeartBeat      : THeartBeatEvent read fOnHeartBeat write fOnHeartBeat;
  end;

procedure Register;

implementation

uses
  StringSubs, Types;

procedure Register;
begin
  {$I magicclock_icon.lrs}
  RegisterComponents('Magic',[TMagicClock]);
end;

{ TMagicClock }

procedure TMagicClock.AnimateClock(Sender: TObject);
const
  X : array[0..3] of Double = (0, 0.7, 1, 0.7 );
  Y : array[0..3] of Double = (1, 0.7, 0, -0.7) ;
var
  //Xc, Yc : Double;
  Xp, Yp : Double;
  L : Double;
begin
  //vAnimationX := Width div 2;
  vAnimationY := Height div 2;
  L := vAnimationY*0.5;

  with Canvas do
    begin
      Pen.Color := Color; // Erase any previous lines;
      Pen.Width := 3;
      Xp := vAnimationX + L*X[vAnimationPos];
      Yp := vAnimationY - L*Y[vAnimationPos];
      MoveTo(Round(Xp),Round(Yp));
      Xp := vAnimationX - L*X[vAnimationPos];
      Yp := vAnimationY + L*Y[vAnimationPos];
      LineTo(Round(Xp),Round(Yp));

      Pen.Color := fColorText; // Draw the new line
      vAnimationPos := (vAnimationPos + 1) mod 4;
      Xp := vAnimationX + L*X[vAnimationPos];
      Yp := vAnimationY - L*Y[vAnimationPos];
      MoveTo(Round(Xp),Round(Yp));
      Xp := vAnimationX - L*X[vAnimationPos];
      Yp := vAnimationY + L*Y[vAnimationPos];
      LineTo(Round(Xp),Round(Yp));
    end;

end;

constructor TMagicClock.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  vOneMinute := EncodeTime( 0, 1, 0, 0 );
  OnResize:=@SizeChanged;
  fColorStopped := clRed;
  fColorStoppedText := clWhite;
  fCalendarFormat := 'mmmm d, yyyy';
  Font.Size := 20;
  fMarginsLR := 10;
  fClockPosition := cpRight;
  Width := 420;
  Color := clLime;
  vAnimationTimer := TTimer.Create( Self );
  vAnimationTimer.OnTimer := @AnimateClock;
  vAnimationTimer.Interval := 500;
  vAnimationTimer.Enabled := False;
  fSpeedMultiplier := 1;
  vDateStr := '';
  vRRTimer := TTimer.Create( Self );
  vRRTimer.OnTimer := @IncrementMinutes;
  vRRTimer.Interval := 60000;  // 1 Minute at 1:1
  vRRTimer.Enabled := False;
  RePaint;
end;

procedure TMagicClock.IncrementMinutes(Sender: TObject);
begin
  fDateTime := fDateTime + vOneMinute;
  RePaint;
  if Assigned( fOnHeartBeat ) then
    fOnHeartBeat( Self, fDateTime );
end;

procedure TMagicClock.Paint;
var
  CR : TRect; // The Clipping Rectangle
  DateStr : String;
  //Bitmap  : TBitmap;
begin
  //inherited Paint;
  //Bitmap := TBitmap.Create;
  //Bitmap.Width := Width;
  //Bitmap.Height := Height;
  try

    if Canvas.Font.Size <> vLastFontSize then
      begin
        vLastFontSize := Canvas.Font.Size;
        SetElementPositions;
        vDateStr := '';
      end;
    CR := Canvas.ClipRect;
    // Set the Background and Text Colors f(Running)
    with Canvas do
      if fRunning then
        begin
          Font.Color := fColorText;
          Brush.Color := Color;
        end
      else
        begin
          Font.Color := fColorStoppedText;
          Brush.Color := ColorStopped;
        end;

    DateStr := FormatDateTime(fCalendarFormat,fDateTime);
    //if VDateStr <> DateStr then
      begin
        vDateStr := DateStr;
        Canvas.Pen.Color := clGray;
        Canvas.FillRect(0,0,CR.Right,CR.Bottom);
        Canvas.Frame( CR );
      end;
    // Draw the calendar date if not hidden
    if not (rrcHideCalendar in fOptions) then
      Canvas.TextOut(vCalendarLeft,vCalendarTop,DateStr);
    // Draw the clock if not hidden
    if not (rrcHideClock in fOptions) then
      begin
        Canvas.TextOut(vClockLeft,vClockTop,TimeStr(fDateTime));
      end;
    //Canvas.Draw(0,0,Bitmap);
  finally
    //Bitmap.Free;
  end;

end;

procedure TMagicClock.SetAMPM(AValue: Boolean);
begin
  if fAMPM=AValue then Exit;
  fAMPM:=AValue;
  SetElementPositions;
  RePaint;
end;

procedure TMagicClock.SetCalendarFormat(AValue: String);
begin
  if fCalendarFormat=AValue then Exit;
  fCalendarFormat:=AValue;
  SetElementPositions;
  RePaint;
end;

procedure TMagicClock.SetCalendarPosition(AValue: TPosition);
begin
  if fCalendarPosition=AValue then Exit;
  fCalendarPosition:=AValue;
  SetElementPositions;
  RePaint;
end;

procedure TMagicClock.SetClockPosition(AValue: TPosition);
begin
  if fClockPosition=AValue then Exit;
  fClockPosition:=AValue;
  SetElementPositions;
  RePaint;
end;

procedure TMagicClock.SetColorStopped(AValue: TColor);
begin
  if fColorStopped=AValue then Exit;
  fColorStopped:=AValue;
  RePaint;
end;

procedure TMagicClock.SetColorStoppedText(AValue: TColor);
begin
  if fColorStoppedText=AValue then Exit;
  fColorStoppedText:=AValue;
  RePaint;
end;

procedure TMagicClock.SetColorText(AValue: TColor);
begin
  if fColorText=AValue then Exit;
  fColorText:=AValue;
end;

procedure TMagicClock.SetDateTime(AValue: TDateTime);
begin
  if fDateTime=AValue then Exit;
  fDateTime:=AValue;
  RePaint;
end;

procedure TMagicClock.SetElementPositions;
var
  TextExtents : TSize;

  function Center( Size, Extent : Integer ) : Integer;
  begin
    Result := ( Extent - Size ) div 2;
  end;

begin
  TextExtents := Canvas.TextExtent(TimeStr(fDateTime));
  TextExtents.cx := TextExtents.cx + TextExtents.cy;
  case fClockPosition of
    cpLeft :
      vClockLeft := fMarginsLR;
    cpCenter :
      vClockLeft := Center( TextExtents.cx, Width );
    cpRight :
      vClockLeft := Width - fMarginsLR - TextExtents.cx;
  end;
  //vAnimationX := vClockLeft + TextExtents.cx + TextExtents.cy;
  vAnimationX := vClockLeft + TextExtents.cx - (TextExtents.cy div 2);
  vClockTop := Center( TextExtents.cy, Height );
  TextExtents := Canvas.TextExtent(FormatDateTime(fCalendarFormat,fDateTime));
  case fCalendarPosition of
    cpLeft :
      vCalendarLeft := fMarginsLR;
    cpCenter :
      vCalendarLeft := Center( TextExtents.cx, Width );
    cpRight :
      vCalendarLeft := Width - fMarginsLR - TextExtents.cx;
  end;
  vCalendarTop := Center( TextExtents.cy, Height );
end;

procedure TMagicClock.SetMarginsLR(AValue: Integer);
begin
  if fMarginsLR=AValue then Exit;
  fMarginsLR:=AValue;
  SetElementPositions;
  Repaint;
end;

procedure TMagicClock.SetOptions(AValue: TOptions);
begin
  if fOptions=AValue then Exit;
  fOptions:=AValue;
  Repaint;
end;

procedure TMagicClock.SetRunning(AValue: Boolean);
begin
  if fRunning=AValue then Exit;
  fRunning:=AValue;
  vDateStr := '';
  vAnimationTimer.Enabled := fRunning;
  //if fRunning then
  //  begin
  //    vHideColon := true;
  //    vColonTimer.Enabled := true;
  //  end
  //else
  //  begin
  //    vColonTimer.Enabled := false;
  //    vHideColon := False;
  //  end;
  vRRTimer.Interval := Trunc((60000 div fSpeedMultiplier)*(1.0 - fSpeedVernir));
  vRRTimer.Enabled := fRunning;
  RePaint;
  // do time change event here
  if Assigned( fOnHeartBeat ) then
    fOnHeartBeat( Self, fDateTime );
end;

procedure TMagicClock.SetSpeedMultiplier(AValue: Word);
begin
  if AValue < 1 then
    AValue := 1;
  if fSpeedMultiplier=AValue then Exit;
  fSpeedMultiplier:=AValue;
end;

procedure TMagicClock.SetSpeedVernir(AValue: Double);
begin
  if fSpeedVernir=AValue then Exit;
  fSpeedVernir:=AValue;
end;

procedure TMagicClock.SizeChanged(Sender: TObject);
begin
  SetElementPositions;
  RePaint;
end;

function TMagicClock.TimeStr(theTime: TDateTime): String;
var
  H, M, S, mS : Word;
  AM : Boolean;
  //function Colon : String;
  //begin
  //  if HideColon then
  //    Result := ' '
  //  else
  //    Result := ':';
  //end;
  function AMPMStr( IsAM : Boolean ) : String;
  begin
    if IsAM then
      Result := ' AM'
    else
      Result := ' PM'
  end;
begin
  DecodeTime( theTime, H, M, S, mS );
  if fAMPM then
    begin
      AM := H < 12;
      H := H mod 12;
      if H = 0 then
        H := 12
    end;
  if fAMPM then
    Result := IntToStr( H ) + ':' + IntToStr( M, 1 ) + AMPMStr( AM )
  else
    Result := IntToStr( H, 1 ) + ':' + IntToStr( M, 1 );
end;

//procedure TMagicClock.ToggleColon(Sender: TObject);
//begin
//  vHideColon := False; // not vHideColon;
//  //RePaint;
//end;

end.
