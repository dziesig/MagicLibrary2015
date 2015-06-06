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

unit CommonStacks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms;

type
{==============================================================================}
{ TStack }
{==============================================================================}

  generic TStack<T> = class
  private
    SP : Integer;
    Stack : array of T;
  public
    constructor Create;  virtual;
    destructor  Destroy; override;

    procedure Push( Value : T ); virtual;
    function  Pop : T; virtual;
    function  Top : T; virtual;
    procedure Reset;
  end;

{==============================================================================}
{ TFormCursorStack }
{==============================================================================}

TCursorStack = specialize TStack<TCursor>;

TFormCursorStack = class(TCursorStack)
  private
    TheForm : TControl;
  public
    constructor Create( Form : TForm ); overload;
    destructor  Destroy; override;
    procedure Push( Value : TCursor ); override;
    function  Pop : TCursor; override;
end;

{ TFrameCursorStack }

TFrameCursorStack = class(TCursorStack)
  private
    TheFrame : TFrame;
  public
    constructor Create( aFrame : TFrame ); overload;
    destructor  Destroy; override;
    procedure Push( Value : TCursor ); override;
    function  Pop : TCursor; override;
end;

{ TAppCursorStack }

TAppCursorStack = class(TCursorStack)
  public
    procedure Push( Value : TCursor ); override;
    function  Pop : TCursor; override;
end;

implementation

{ TFrameCursorStack }

constructor TFrameCursorStack.Create(aFrame: TFrame);
begin
  inherited Create;
  TheFrame := aFrame;
end;

destructor TFrameCursorStack.Destroy;
begin
  inherited Destroy;
end;

function TFrameCursorStack.Pop: TCursor;
begin
  Result:=inherited Pop;
  TheFrame.Cursor := Result;
  Application.ProcessMessages;
end;

procedure TFrameCursorStack.Push(Value: TCursor);
begin
  inherited Push(TheFrame.Cursor);
  TheFrame.Cursor := Value;
  Application.ProcessMessages;
end;

{ TAppCursorStack }

function TAppCursorStack.Pop: TCursor;
begin
  Result:=inherited Pop;
  Screen.Cursor := Result;
  Application.ProcessMessages;
end;

procedure TAppCursorStack.Push(Value: TCursor);
begin
  inherited Push(Screen.Cursor);
  Screen.Cursor := Value;
  Application.ProcessMessages;
end;

{==============================================================================}
{ TStack }
{==============================================================================}

constructor TStack.Create;
const
  InitialListSize = 4;
var
  I : Integer;
begin
  SP := -1;
  SetLength(Stack,InitialListSize);
  //for I := 0 to pred(InitialListSize) do
  //  if Stack[i] <> 0 then
  //    raise EListError.Create('non 0 in creation of Stack');
end;

destructor TStack.Destroy;
begin
  SetLength(Stack,0);
  inherited Destroy;
end;

procedure TStack.Push(Value: T);
begin
  Inc(SP);
  if SP >= Length(Stack) then
    SetLength(Stack,Length(Stack)*2);
  Stack[SP] := Value;
end;

procedure TStack.Reset;
begin
  SP := -1;
end;

function TStack.Top: T;
begin
  if SP < 0 then
    raise EListError.Create('Stack Underflow');
  Result := Stack[0];
end;

function TStack.Pop: T;
begin
  if SP < 0 then
    raise EListError.Create('Stack Underflow');
  Result := Stack[SP];
  Dec(SP);
end;

{==============================================================================}
{ TFormCursorStack }
{==============================================================================}

constructor TFormCursorStack.Create(Form: TForm);
begin
  inherited Create;
  TheForm := Form;
end;

destructor TFormCursorStack.Destroy;
begin
//  SetLength( Stack, 0 );
  inherited Destroy;
end;

procedure TFormCursorStack.Push(Value: TCursor);
begin
  inherited Push(TheForm.Cursor);
  TheForm.Cursor := Value;
  Application.ProcessMessages;
end;

function TFormCursorStack.Pop: TCursor;
begin
  Result:=inherited Pop;
  TheForm.Cursor := Result;
  Application.ProcessMessages;
end;

end.

