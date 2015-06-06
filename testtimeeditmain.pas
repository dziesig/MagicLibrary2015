unit TestTimeEditMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MagicTimeEdit, testtimeeditframe;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Frame : TFrame1;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.Button1Click(Sender: TObject);
begin
  if not Assigned( Frame ) then
    begin
      //Frame := TFrame1.Create( Panel1 );
      //Frame.Parent := Panel1;
    end;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

end.

