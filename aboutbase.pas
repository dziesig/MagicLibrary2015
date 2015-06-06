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

unit AboutBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TAboutFormBase }

  TAboutFormBase = class(TForm)
    BitBtn1: TBitBtn;
    CopyrightLabel:
TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ConfigFolderLabel: TLabel;
    AppNameLabel: TLabel;
    Label4: TLabel;
    LastBuildLabel: TLabel;
    DefaultSaveFolderLabel: TLabel;
    procedure FormCreate(Sender: TObject); virtual;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutFormBase: TAboutFormBase;

implementation

{$R *.lfm}

uses
  CommonApp;

{ TAboutFormBase }

procedure TAboutFormBase.FormCreate(Sender: TObject);
begin
  ConfigFolderLabel.Caption      := GetAppConfigFile( False, True );
  LastBuildLabel.Caption         := BuildDateTime;
  DefaultSaveFolderLabel.Caption := DefaultSaveLocation;
end;

end.

