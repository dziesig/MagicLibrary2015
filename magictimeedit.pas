unit MagicTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TMagicTimeEdit }

  TMagicTimeEdit = class(TCustomPanel)
  private
    fAMPM: Boolean;
    fOnTimeChange: TNotifyEvent;
    function getDateTime: TDateTime;
    function GetTime: String;
    procedure SetAMPM(AValue: Boolean);
    procedure SetDateTime(AValue: TDateTime);
    procedure Settime(AValue: String);
    { Private declarations }
  protected
    { Protected declarations }
    vCreated : Boolean; // True after create completed.
    fHourCombo   : TComboBox;
    fMinuteCombo : TComboBox;
    fAMPMCombo   : TComboBox;
    fColonLabel  : TLabel;

    procedure FontChanged( Sender : TObject ); override;
    procedure DoFontChanged;
    procedure PopulateHours;
    procedure PopulateMinutes;
    procedure PopulateAMPM;
    procedure ReDraw; // Handles changes in font/ampm, etc.
    procedure SetHourMinute( Hour, Minute : Word );
    procedure Loaded; override;
    procedure DoOnTimeChange;
    procedure ComboChange( Sender : TObject );
  public
    { Public declarations }
    constructor Create( theOwner : TComponent ); override;
    destructor  Destroy; override;

    property DateTime      : TDateTime read getDateTime write SetDateTime;

  published
    { Published declarations }

    property AMPM          : Boolean   read fAMPM      write SetAMPM;
    property Font;
    property Time          : String    read GetTime     write Settime;

    property OnTimeChange  : TNotifyEvent read fOnTimeChange write fOnTimeChange;
  end;

procedure Register;

implementation

uses
  CommonDebug, LazLogger,
  Types;

procedure Register;
begin
  {$I magictimeedit_icon.lrs}
  RegisterComponents('Magic',[TMagicTimeEdit]);
end;

//function CSToStr( CS : TComponentState ) : String;
//begin
//  Result := '';
//  if csLoading in CS then Result := Result + 'csLoading, ';
//  if csReading in CS then REsult := Result + 'csReading, ';
//  if csWriting in CS then Result := Result + 'csWriting, ';
//  if csDestroying in CS then Result := Result + 'csDestroying, ';
//  if csDesigning in CS then Result := Result + 'csDesigning, ';
//  if csAncestor in CS then Result := Result + 'csAncestor, ';
//  if csUpdating in CS then Result := Result + 'csUpdating, ';
//  if csFixups in CS then Result := Result + 'csFixups, ';
//  if csFreeNotification in CS then Result := Result + 'csFreeNotification, ';
//  if csInline in CS then Result := Result + 'csInline, ';
//  if csDesignInstance in CS then Result := Result + 'csDesignInstance, ';
//end;
//
{ TMagicTimeEdit }

constructor TMagicTimeEdit.Create(theOwner: TComponent);
begin
  DebugLn('TMagicTimeEdit.Create ' + DbgS(ComponentState) );
  if Assigned( theOwner ) then
    DebugLn('Owner: ' + theOwner.Name );
  vCreated := False;
  inherited Create(theOwner);
  Parent := theOwner as TWinControl;
  Caption := '';
  BevelOuter := bvNone;
  fColonLabel := TLabel.Create( Self );
  fColonLabel.Parent := Self;
  fColonLabel.Caption := ':';
  fColonLabel.AutoSize := True;
  DebugLn('Creating Combos');
  fHourCombo   := TComboBox.Create( self );
  fMinuteCombo := TComboBox.Create( self );
  fAMPMCombo   := TComboBox.Create( self );
  fHourCombo.Parent   := self;
  fMinuteCombo.Parent := self;
  fAMPMCombo.Parent   := self;
  fHourCombo.Name   := 'HourCombo';
  fMinuteCombo.Name := 'MinuteCombo';
  fAMPMCombo.Name   := 'AMPMCombo';
  fHourCombo.Style := csDropDownList;
  fMinuteCombo.Style := csDropDownList;
  fAMPMCombo.Style := csDropDownList;
  fHourCombo.OnChange:= @ComboChange;
  fMinuteCombo.OnChange:= @ComboChange;
  fAMPMCombo.OnChange:= @ComboChange;
  PopulateHours;
  PopulateMinutes;
  PopulateAMPM;
  DebugLn('Combos Created');
  if csDesigning in ComponentState then
    begin
      Font.Name := 'Arial';
      Font.Size := 10;
    end;
  ReDraw;
  vCreated := True;
  DebugLn('End Create');
end;

destructor TMagicTimeEdit.Destroy;
begin
  fHourCombo.Free;
  fMinuteCombo.Free;
  fAMPMCombo.Free;
  fColonLabel.Free;
  inherited Destroy;
end;

procedure TMagicTimeEdit.DoFontChanged;
var
  Size : TSize;
begin
  DebugLn('DoFontChanged ' + DbgS(ComponentState) );
  if (csReading in ComponentState) then exit;
  //Debugln('aaaaaa');
  if not Assigned(fHourCombo) then
    begin
      exit;
    end;
  //Debugln('00000');
  try
  Size := Canvas.TextExtent('00');
  DebugLn('Size %d %d',[Size.CX,Size.CY]);
  Height := Size.CY + 8;
  //Debugln('11111');
  fHourCombo.Font.Assign( Font );
  //Debugln('22222');
  fMinuteCombo.Font.Assign( Font );
  fAMPMCombo.Font.Assign( Font );
  //debugln('33333');
  fHourCombo.Width := Size.CX + 27;
  fMinuteCombo.Width := Size.CX + 27;
  Size := Canvas.TextExtent('AM');
  fAMPMCombo.Width := Size.CX + 27;
  fColonLabel.Width := 5;
  fColonLabel.Font.Assign( Font );
  fColonLabel.Font.Style := [fsBold];
  fColonLabel.Caption := ':';
  except
    //Debugln('zzzzz');
  end;
  //Debugln('44444');
  ReDraw;
end;

procedure TMagicTimeEdit.FontChanged(Sender: TObject);
begin
  //Debugln('Sender:  ' + Dbgs(Pointer(Sender)) );
  inherited FontChanged(Sender);
  DoFontChanged;
end;

procedure TMagicTimeEdit.Loaded;
begin
  //DebugLn('Start Loaded ' + DbgS(ComponentState) );
  if Assigned(Parent) or (ParentWindow <> 0) then
    HandleNeeded;
  if csLoading in ComponentState then
    begin
      //DebugLn('Invoking DoFontChanged');
      DoFontChanged;
    end;
  inherited Loaded;
  //DebugLn('End Loaded');
end;

procedure TMagicTimeEdit.PopulateAMPM;
begin
  fAMPMCombo.Clear;
  fAMPMCombo.Items.Add('AM');
  fAMPMCombo.Items.Add('PM');
  fAMPMCombo.ItemIndex := 0;
end;

procedure TMagicTimeEdit.PopulateHours;
var
  I : Integer;
begin
  fHourCombo.Clear;
  if fAMPM then
    begin
      for I := 0 to 11 do
        begin
          if I = 0 then
            fHourCombo.Items.Add('12')
          else if I < 10 then
            fHourCombo.Items.Add(' ' + IntToStr(I))
          else
            fHourCombo.Items.Add(IntToStr(I));
        end;
      fAMPMCombo.Visible := True;
    end
  else
    begin
      for I := 0 to 23 do
        if I < 10 then
          fHourCombo.Items.Add('0' + IntToStr(I))
        else
          fHourCombo.Items.Add(IntToStr(I));
      fAMPMCombo.Visible := False;
    end;
  fHourCombo.ItemIndex := 0;
end;

procedure TMagicTimeEdit.PopulateMinutes;
var
  I : Integer;
begin
  fMinuteCombo.Clear;
  for I := 0 to 59 do
    if I < 10 then
      fMInuteCombo.Items.Add('0' + IntToStr(I) )
    else
      fMInuteCombo.Items.Add(IntToStr(I));
  fMinuteCombo.ItemIndex := 0;
end;

procedure TMagicTimeEdit.ReDraw;
var
  Sep : Integer;
begin
  Caption := ''; // Just in case.
  Sep := fHourCombo.Width div 3;
  fAMPMCombo.Visible := fAMPM;
  fMinuteCombo.Left := fHourCombo.Width + Sep;
  fAMPMCombo.Left := fMinuteCombo.Left + fMinuteCombo.Width + (Sep div 2);
  fColonLabel.Refresh;
  Sep := Sep - fColonLabel.Width;
  if fAMPM then
    Width := fAMPMCombo.Left + fAMPMCombo.Width
  else
    Width := fMinuteCombo.Left + fMinuteCombo.Width;
  fColonLabel.Left := fHourCombo.Width + (Sep div 2);
  fColonLabel.Top := (Height - fColonLabel.Height) div 2;
  fColonLabel.Visible := True;
end;

procedure TMagicTimeEdit.SetAMPM(AValue: Boolean);
var
  DT : TDateTime;
begin
  //DebugLn('Start SetAMPM ' + DbgS(ComponentState) );
  DT := DateTime;
  fAMPM:=AValue;
  PopulateHours;
  DateTime := DT;
  ReDraw;
end;


function TMagicTimeEdit.getDateTime: TDateTime;
var
  H, M, S, mS : Word;
begin
  Result := 0.0;
  if fAMPM then
    H := fHourCombo.ItemIndex + 12*fAMPMCombo.ItemIndex
  else
    H := fHourCombo.ItemIndex;
  M := fMinuteCombo.ItemIndex;
  S := 0;
  mS := 0;
  try
    Result := EncodeTime( H, M, S, mS );
  except
    if not (csDesigning in ComponentState) then raise;
  end;
end;

function TMagicTimeEdit.GetTime: String;
begin
  Result := TimeToStr( DateTime );
end;

procedure TMagicTimeEdit.ComboChange(Sender: TObject );
begin
  DoOnTimeChange;
end;

procedure TMagicTimeEdit.DoOnTimeChange;
begin
  if Assigned( fOnTimeChange ) then
    fOnTimeChange( Self );
end;

procedure TMagicTimeEdit.SetDateTime(AValue: TDateTime);
var
  H, M, S, mS : Word;
begin
  //DebugLn('Start SetDateTime ' + DbgS(ComponentState) );
  DecodeTime( AValue, H, M, S, mS);
  SetHourMinute( H, M );
end;

procedure TMagicTimeEdit.Settime(AValue: String);
begin
  //DebugLn('Start Settime ' + DbgS(ComponentState) );
  DateTime := StrToTime( AValue );
end;

procedure TMagicTimeEdit.SetHourMinute(Hour, Minute: Word);
begin
  if fAMPM then
    begin
      fHourCombo.ItemIndex := Hour mod 12;
      fAMPMCombo.ItemIndex := Hour div 12;
    end
  else
    fHourCombo.ItemIndex := Hour;
  fMinuteCombo.ItemIndex := Minute;
end;

end.
