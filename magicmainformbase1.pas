//Copyright (c) 2012 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of MagicLibrary.
//
//MagicLibrary is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicLibrary is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicLibrary.  If not, see <http://www.gnu.org/licenses/>.

unit magicmainformbase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, {PrintersDlgs,} Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, ComCtrls, ExtCtrls,

  Persists2, CommonStacks, MagicFormFrame1;

type

  { TMagicMainFormBase }

  TMagicMainFormBase = class(TForm)
    FilePreferencesAction: TAction;
    PrimaryFrame: TMagicFormFrame1;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem9: TMenuItem;
    FilePrintAction: TAction;
    FilePrinterSetupAction: TAction;
    FileExitAction: TAction;
    FileSaveAsAction: TAction;
    FileSaveAction: TAction;
    FileOpenAction: TAction;
    FileNewAction: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
{    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;  }
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure FileExitActionExecute(Sender: TObject);
    procedure FileNewActionExecute(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure FilePreferencesActionExecute(Sender: TObject);
    procedure FilePrintActionExecute(Sender: TObject);
    procedure FilePrinterSetupActionExecute(Sender: TObject);
    procedure FileSaveActionExecute(Sender: TObject);
    procedure FileSaveAsActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fCurrentFile: String;

    procedure SetCurrentFile(AValue: String);
    procedure SetData(AValue: TPersists);
    procedure ResizeHack;
  protected
    vAppName     : String;
    vDataName    : String;
    vDefaultExt  : String;
    vTitleLeader : String;
    vFileFilter  : String;

    fLeft, fRight, fTop, fBottom : Integer; // Screen Position/Size

    fData : TPersists;

    CursorStack : TFormCursorStack;

    procedure UpdateData; virtual; abstract; // so we can get the last control data
    procedure FileNew; virtual;
    procedure FileOpen( FileName : String ); virtual; abstract;
    procedure FileSave; virtual; abstract;

  public
    { public declarations }

    property CurrentFile : String    read fCurrentFile write SetCurrentFile;
    property Data        : TPersists read fData        write SetData;

  end;

var
  MagicMainFormBase: TMagicMainFormBase;

const
  NoFile = 'NoFILE';

implementation

{$R *.lfm}

uses
  CommonApp, CommonIni, CommonMath;

{ TMagicMainFormBase }

procedure TMagicMainFormBase.FileNewActionExecute(Sender: TObject);
var
  Ans : Integer;
begin
  UpdateData;
  if Data.Modified then
     begin
       Ans := MessageDlg( CurrentFile + ' has been modified.'#13#10 +
                          'Do you want to save it before creating a new one?',
                          mtConfirmation, [mbYes, mbNo, mbCancel], 0);
       case Ans of
         mrYes:
           begin
             FileSaveActionExecute( Sender );
             FileNew;
           end;
         mrNo:
           FileNew;
         mrCancel: ;
       end;
     end
  else
    FileNew;
end;

procedure TMagicMainFormBase.FileExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMagicMainFormBase.FileOpenActionExecute(Sender: TObject);
  procedure DoOpen;
  begin
    OpenDialog1.InitialDir := DefaultSaveLocation;
    OpenDialog1.DefaultExt := vDefaultExt;
    OpenDialog1.Filter := vFileFilter;
    if OpenDialog1.Execute then
      begin
        CurrentFile := OpenDialog1.FileName;
        FileOpen( CurrentFile );
      end;
  end;
var
  Ans : Integer;
begin
  UpdateData;
  if Data.Modified then
     begin
       Ans := MessageDlg( CurrentFile + ' has been modified.'#13#10 +
                          'Do you want to save it before opening another one?',
                          mtConfirmation, [mbYes, mbNo, mbCancel], 0);
       case Ans of
         mrYes:
           begin
             FileSaveActionExecute( Sender );
             DoOpen;
           end;
         mrNo:
           DoOpen;
         mrCancel: ;
       end;
     end
  else
    DoOpen;
end;

procedure TMagicMainFormBase.FilePreferencesActionExecute(Sender: TObject);
begin

end;

procedure TMagicMainFormBase.FilePrintActionExecute(Sender: TObject);
begin

end;

procedure TMagicMainFormBase.FilePrinterSetupActionExecute(Sender: TObject);
begin

end;

procedure TMagicMainFormBase.FileSaveActionExecute(Sender: TObject);
begin
  if CurrentFile = NoFile then
     FileSaveAsActionExecute(Sender)
  else
    begin
      UpdateData;
      FileSave;
    end;
end;

procedure TMagicMainFormBase.FileSaveAsActionExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := DefaultSaveLocation;
  SaveDialog1.DefaultExt := vDefaultExt;
  SaveDialog1.Filter     := vFileFilter;
  UpdateData;
  if SaveDialog1.Execute then
    begin
      CurrentFile := SaveDialog1.FileName;
      FileSave;
    end;
end;

procedure TMagicMainFormBase.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SetConfig('Position','Top',Top);
  SetConfig('Position','Left',Left);
  SetConfig('Position','Height',Height);
  SetConfig('Position','Width',Width);
  CloseAction := caFree;
end;

procedure TMagicMainFormBase.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  Ans : Integer;
begin
  CursorStack.Push( crHourGlass );
  UpdateData;
  CursorStack.Pop;
  if (Data <> nil) and Data.Modified then
    begin
      Ans := MessageDlg( CurrentFile + ' has been modified'#13#10 +
                         'Do you want to save it before exiting?',
                         mtConfirmation,
                         [mbYes, mbNo, mbCancel], 0 );

      case Ans of
        mrYes:
          begin
            if Assigned(@FileSaveActionExecute) then
              FileSaveActionExecute( Sender );
            CanClose := False;
          end;
        mrNo:
          CanClose := True;
        mrCancel:
          CanClose := False;
      end;
    end
  else
    CanClose := True;

end;

procedure TMagicMainFormBase.FormCreate(Sender: TObject);
var
  T,L : Integer;
  H,W : Integer;
  X, Y : Integer;
begin
  fData := nil;
  CursorStack := TFormCursorStack.Create( self );
  Timer1.Enabled := True;
end;

{ For some strange reason, using a derived form stops resizing during the      }
{ Create constructor.  This code is a hack that uses a timer interrupt (started}
{ in the constructor) to force the resize (and reposition) once everything else}
{ has stabilized.                                                              }
procedure TMagicMainFormBase.ResizeHack;
var
  T,L : Integer;
  H,W : Integer;
begin
  T := Max(GetConfig('Position','Top', 0),0);     // Don't let form go above top
  L := Max(GetConfig('Position','Left', 0),0);    // Don't let form go left of left edge.
  H := GetConfig('Position','Height',640);
  W := GetConfig('Position','Width',1024);
  Top    := T;
  Left   := L;
  Height := Min(Screen.Height,H);
  Width  := Min(Screen.Width,W);
end;

procedure TMagicMainFormBase.SetCurrentFile(AValue: String);
begin
  if fCurrentFile=AValue then Exit;
  fCurrentFile:=AValue;
  StatusBar1.Panels[1].Text := fCurrentFile;
end;

procedure TMagicMainFormBase.SetData(AValue: TPersists);
begin
  Caption := vAppName;// + ' - ' + aValue.Name;
  fData:=AValue;
end;

procedure TMagicMainFormBase.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  ResizeHack;
end;

procedure TMagicMainFormBase.FileNew;
begin
  CurrentFile := NoFile;
end;


end.

