unit FontPicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFontPicker }

  TFontPicker = class(TComboBox)
    { Private declarations }
  private
    fAllFonts: Boolean;
    fFontName: String;
    fFontPick: TFont;
    fH: Integer;
    procedure SetAllFonts(AValue: Boolean);
    procedure SetFontName(AValue: String);
    //procedure setFontPick(AValue: TFont);
    procedure setH(AValue: Integer);
  protected
    { Protected declarations }
    procedure DrawItem( Control: TWinControl; Index: Integer; ARect: TRect;
                        State: TOwnerDrawState);
    procedure MeasureItem( Control: TWinControl; Index: Integer;
                           var AHeight: Integer);
  public
    { Public declarations }
    constructor Create( theOwner : TComponent ); override;
    destructor  Destroy; override;
  published
    { Published declarations }
    property AllFonts : Boolean read fAllFonts write SetAllFonts;
    property Height;
    property Text;
    property Width;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I fontpicker_icon.lrs}
  RegisterComponents('Magic',[TFontPicker]);
end;

{ TFontPicker }

constructor TFontPicker.Create(theOwner: TComponent);
var
  I : Integer;
begin
  inherited Create(theOwner);
  Style := csOwnerDrawFixed;
  OnDrawItem := @DrawItem;
  ItemIndex := 0;
  RePaint;
end;

destructor TFontPicker.Destroy;
begin
  inherited Destroy;
end;

procedure TFontPicker.DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with Canvas do
    begin
      Font.Name := Items[Index];
      FillRect(ARect) ;
      TextOut( ARect.Left + 5, ARect.Top, Items[Index] );
    end;
end;

procedure TFontPicker.MeasureItem(Control: TWinControl; Index: Integer;
  var AHeight: Integer);
begin
  Canvas.Font.Name := Items[Index];
  AHeight := Canvas.TextHeight( Items[Index] );
end;

procedure TFontPicker.SetAllFonts(AValue: Boolean);
var
  I : Integer;
begin
  if fAllFonts=AValue then Exit;
  fAllFonts:=AValue;
  if AValue then
    begin
      Items.Clear;
      for I := 0 to pred(Screen.Fonts.Count) do
        Items.Add( Screen.Fonts.Strings[I] );
    end;
end;

procedure TFontPicker.SetFontName(AValue: String);
begin
  if fFontName=AValue then Exit;
  fFontName:=AValue;
end;

procedure TFontPicker.setH(AValue: Integer);
begin
  if fH=AValue then Exit;
  fH:=AValue;
  RePaint;
end;

end.
