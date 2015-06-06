unit testtimeeditframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls,
  MagicTimeEdit, zzz;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    FontDialog1: TFontDialog;
    Label2: TLabel;
    Label3: TLabel;
    MagicTimeEdit1: TMagicTimeEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure MagicTimeEdit1TimeChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.Button1Click(Sender: TObject);
begin
  if FontDialog1.Execute then
    begin
      MagicTimeEdit1.Font := FontDialog1.Font;
    end;
end;

procedure TFrame1.Button2Click(Sender: TObject);
begin
  MagicTimeEdit1.AMPM := not MagicTimeEdit1.AMPM;
end;

procedure TFrame1.MagicTimeEdit1TimeChange(Sender: TObject);
begin
  Label2.Caption := TimeToStr( MagicTimeEdit1.DateTime );
  Label3.Caption := MagicTimeEdit1.Time;
end;

end.

