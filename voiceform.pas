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

unit VoiceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  VoiceUnit, ComCtrls, Buttons;

type

  { TVoicePreferencesForm }

  TVoicePreferencesForm = class(TForm)
    ButtonAccept: TBitBtn;
    ButtonDefaults: TBitBtn;
    ButtonClose: TBitBtn;
    ButonTest: TBitBtn;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TrackBar1: TTrackBar;
    procedure ButtonAcceptClick(Sender: TObject);
    procedure ButtonDefaultsClick(Sender: TObject);
    procedure ButonTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    fSpeed : Integer;
    fVoice : Integer;
    VoiceQueue : TVoiceQueue;
    procedure SetControls;
  public
    { public declarations }
    procedure Say( What : String );
    procedure TestString( What : String );
  end;

var
  VoicePreferencesForm: TVoicePreferencesForm;

implementation

{$R *.lfm}

uses
  CommonIni;

{ TVoicePreferencesForm }

procedure TVoicePreferencesForm.ButtonDefaultsClick(Sender: TObject);
begin
  fVoice := 0;
  fSpeed := 5;
  SetControls;
end;

procedure TVoicePreferencesForm.ButonTestClick(Sender: TObject);
var
  S : String;
begin
  VoiceQueue.Voice:= ComboBox1.ItemIndex;
  VoiceQueue.Speed := TrackBar1.Position;
  S := Edit1.Text;
  VoiceQueue.Say( S );
end;

procedure TVoicePreferencesForm.ButtonAcceptClick(Sender: TObject);
begin
  SetConfig('Voice','Speaker',ComboBox1.ItemIndex);
  SetConfig('Voice','Speed',TrackBar1.Position);
end;

procedure TVoicePreferencesForm.FormCreate(Sender: TObject);
begin
  fVoice := GetConfig('Voice','Speaker',0);
  fSpeed := GetConfig('Voice','Speed',5);
  VoiceQueue := TVoiceQueue.Create;
  VoiceQueue.Speed := fSpeed;
  VoiceQueue.Voice := fVoice;
  SetControls;
end;

procedure TVoicePreferencesForm.FormShow(Sender: TObject);
begin
  fVoice := GetConfig('Voice','Speaker',0);
  fSpeed := GetConfig('Voice','Speed',5);
  SetControls;
end;

procedure TVoicePreferencesForm.Say(What: String);
begin
  VoiceQueue.Say(What);
end;

procedure TVoicePreferencesForm.SetControls;
begin
  ComboBox1.ItemIndex := fVoice;
  TrackBar1.Position := fSpeed;
end;

procedure TVoicePreferencesForm.TestString(What: String);
begin
  Edit1.Text := What;
end;


end.

