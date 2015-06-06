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

{==============================================================================}
{ The MagicFormFrame is a frame that can host multiple Forms (from TForm )     }
{ such that the user interface can be limited to a single window but the code  }
{ (and components) can be separated into individual files (so you don't have a }
{ single humongous file defining the user interface.                           }
{==============================================================================}

{==============================================================================}
{ WARNING.  This is deprecated due to an error in the Lazarus LCL which causes }
{           strange, non-reproducable behavior if the app-specific frame is    }
{           sub-classed.                                                       }
{           It has been replaced by the MagicFormPanel component.              }
{==============================================================================}

unit MagicFormFrame1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, {StdCtrls,} MagicFrameForm;

type

  { TMagicFormFrame1 }

  TMagicFormFrame1 = class(TFrame)
  private
    { private declarations }
    FForm: TMagicForm;
    FOnAfterFormChanged: TNotifyEvent;
    FOnBeforeFormChanged: TNotifyEvent;
    procedure SetForm(AValue: TMagicForm);
  protected
    { protected declarations }
    procedure AfterFormChanged; virtual;
    procedure BeforeFormChanged; virtual;

    procedure Paint; override;
    procedure Resize; override;
  public
    { public declarations }
    property Form: TMagicForm read FForm write SetForm;

    property OnAfterFormChanged  : TNotifyEvent read FOnAfterFormChanged  write FOnAfterFormChanged;
    property OnBeforeFormChanged : TNotifyEvent read FOnBeforeFormChanged write FOnBeforeFormChanged;
  end;

implementation

{$R *.lfm}

uses
  CommonDebug;

{ TMagicFormFrame1 }

procedure TMagicFormFrame1.SetForm(AValue: TMagicForm);
var
  N : String;
begin
  if fform <> nil then
    N := FForm.ClassName;
  if AValue <> FForm then
    begin
      if (FForm <> nil) and (not FForm.IsValid ) then  // Don't change form is
                                                        // current form is invalid
        Exit;
//      BeforeFormChanged;
      if (FForm <> nil) then { hide current form }
        FForm.Visible := False;
      FForm := AValue;
      if (FForm <> nil) then
        begin
          FForm.Parent := Self;
          Resize;
          FForm.Visible := True;
        end;
//      AfterFormChanged;
    end;
end;

procedure TMagicFormFrame1.AfterFormChanged;
begin
  if Assigned(FOnAfterFormChanged) then FOnAfterFormChanged(Self);
end;

procedure TMagicFormFrame1.BeforeFormChanged;
begin
  if Assigned(FOnBeforeFormChanged) then FOnBeforeFormChanged(Self);
end;

procedure TMagicFormFrame1.Paint;
begin
  inherited Paint;
  if FForm <> nil then
    SetForm(FForm);
end;

procedure TMagicFormFrame1.Resize;
begin
  inherited;
  if (FForm <> nil) and not (csDestroying in ComponentState) then
    begin
      FForm.SetBounds(0, 0, Width, Height);
    end;
end;

end.

