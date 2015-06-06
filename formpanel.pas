unit FormPanel;

{$mode objfpc}{$H+}

{.$define DEBUG}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  TFormPanelError = class( Exception);

  TFormPanelCentering = (fpcNone, fpcHoriz, fpcVert, fpcBoth);

  { TPanelForm }
{ TODO 1 -odonz -cImportant : Add CanHide so we can stop changing forms in the middle of something important. }
  TPanelForm = class(TForm)
  public
    procedure Showing; virtual; abstract;
    procedure Hiding;  virtual; abstract;
    procedure PreferencesChanged; virtual;
    function  CanHide : Boolean; virtual;
  end;

  { TCustomFormPanel }

  TCustomFormPanel = class(TCustomPanel)
  private
    fCentering: TFormPanelCentering;
    fForm: TPanelForm;
    procedure SetCentering(AValue: TFormPanelCentering);
    procedure SetForm(AValue: TPanelForm);
    { Private declarations }
  protected
    { Protected declarations }
    vLeft : Integer;
    vRight : Integer;
    vTop  : Integer;
    vBottom : Integer;
    procedure CenteringNone;
    procedure CenteringVert;
    procedure CenteringHoriz;
  public
    { Public declarations }
    constructor Create( aOwner : TComponent ); override;
    property Form      : TPanelForm read fForm write SetForm;
  published
    { Published declarations }
    property Centering : TFormPanelCentering read fCentering write SetCentering;
  end;

  TMagicFormPanel = class(TCustomFormPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property Align;
    property Alignment;
    Property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property ParentColor;
    property Visible;
  end;

procedure Register;

implementation

uses
  LazLogger,
  CommonMath;

procedure Register;
begin
  {$I formpanel_icon.lrs}
  RegisterComponents('Magic',[TMagicFormPanel]);
end;

{ TPanelForm }

function TPanelForm.CanHide: Boolean;
begin
  Result := True; // Default
end;

procedure TPanelForm.PreferencesChanged;
begin
  // Does nothing unless overridden.
end;

{ TCustomFormPanel }

procedure TCustomFormPanel.CenteringHoriz;
var
  W : Integer;
begin
 w := VRight + 2*VLeft;
 VLeft := (Width - W) div 2;
end;

procedure TCustomFormPanel.CenteringNone;
begin
  vLeft := 0;
  vTop  := 0;
end;

procedure TCustomFormPanel.CenteringVert;
var
  H : Integer;
begin
  H := VBottom + 2*vTop;
  vTop := (Height - H) div 2;
end;

constructor TCustomFormPanel.Create(aOwner: TComponent);
begin
  {$ifdef DEBUG}DebugLn('Creating');{$endif}
  inherited Create(aOwner);
  fCentering := fpcBoth;
end;

procedure TCustomFormPanel.SetCentering(AValue: TFormPanelCentering);
begin
  if fCentering=AValue then Exit;
  fCentering:=AValue;
end;

procedure TCustomFormPanel.SetForm(AValue: TPanelForm);
var
  I : Integer;
  C : TControl;
begin
  {$ifdef DEBUG}DebugLn('SetForm');{$endif}
  if fForm=AValue then Exit;
  Visible := False;
  Application.ProcessMessages;
  try
  if Assigned( fForm ) then
    begin
      DebugLn('Hiding form ' + fForm.Name);
      fForm.Hiding;
      {$ifdef DEBUG}DebugLn('ComponentCount 1:  ' + IntToStr(fForm.ComponentCount) );{$endif}
      for I := 0 to pred( fForm.ComponentCount) do
        begin
          if fForm.Components[I] is TControl then
            begin
              C := fForm.Components[I] as TControl;
              if C.Parent = Self then
                begin
                  {$ifdef DEBUG}DebugLn('control ' + IntToStr(I) + ' is ' + C.Name );{$endif}
                  C.Parent := fForm;
                  C.Left := C.Left - vLeft;
                  C.Top  := C.Top - vTop;
                end
            end;
        end;
    end;
  fForm:=AValue;
  if Assigned( fForm ) then
    begin
      //fForm.Visible := False;
      Application.ProcessMessages;
      if (fForm.Height > Height) then
        raise TFormPanelError.CreateFmt('Form %s is too high for %s',[fForm.Name,Name]);
      if (fForm.Width > Width) then
        raise TFormPanelError.CreateFmt('Form %s is too wide for %s',[fForm.Name,Name]);
      vLeft := 65536;
      vRight := 0;
      vTop   := 65536;
      vBottom := 0;
      {$ifdef DEBUG}DebugLn('ComponentCount 2:  ' + IntToStr(fForm.ComponentCount) );{$endif}

      for I := 0 to pred( fForm.ComponentCount ) do
        begin
          if fForm.Components[I] is TControl then
            begin
              C := fForm.Components[I] as TControl;
              {$ifdef DEBUG}DebugLn('control ' + IntToStr(I) + ' is ' + C.Name );{$endif}
              if C.Parent = fForm then
                begin
                  C.Parent := Self;
                  vLeft := Min(vLeft,C.Left);
                  vTop  := Min(vTop,C.Top);
                  vRight := Max(vRight, C.Left + C.Width);
                  vBottom := Max(vBottom,C.Top + C.Height);
                end;
            end;
        end;
      case fCentering of
      fpcNone  : CenteringNone;
      fpcHoriz : CenteringHoriz;
      fpcVert  : CenteringVert;
      fpcBoth :
        begin
          CenteringHoriz;
          CenteringVert;
        end;
      end;
      for I := 0 to pred( fForm.ComponentCount ) do
        begin
          if fForm.Components[I] is TControl then
            begin
              C := fForm.Components[I] as TControl;
              {$ifdef DEBUG}DebugLn('control ' + IntToStr(I) + ' is ' + C.Name );{$endif}
              if C.Parent = Self then
                begin
                  C.Left := C.Left + vLeft;
                  C.Top := C.Top + VTop;
                end;
            end;
        end;
      DebugLn('Showing form ' + fForm.Name);
      Visible := True;
      fForm.Showing;
    end;
  finally
    Visible := True;
    Application.ProcessMessages;
  end;
end;

end.
