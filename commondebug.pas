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

unit CommonDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure MessageBox( What : String );
procedure Debug( const Message : String ); overload;
procedure Debug( const Value : Integer ); overload;
procedure Debug( const Value : Double ); overload;
procedure DebugLn( const Message : String = '');
procedure Stub( ForWhat : String );
procedure Stub1( ForWhat : String ); // Only outputs ForWhat once per run.

implementation

uses
  LCLProc, Dialogs;

procedure MessageBox(What: String);
begin
  MessageDlg(What,mtInformation,[mbOk],0);
end;

procedure Debug(const Message: String);
begin
{$ifdef WIN32}
  WriteLn( Message );
  //OutputDebugString( PChar(Message) );
{$else}
 DbgOut( Message + #13); //#10 );
{$endif}
end;

procedure Debug(const Value: Integer);
begin
  DbgOut( IntToStr( Value ) );
end;

procedure Debug(const Value: Double);
begin
  DbgOut( FloatToStr( Value ) );
end;

procedure DebugLn(const Message: String);
begin
  LCLProc.DebugLn( Message );
end;

procedure Stub(ForWhat: String);
begin
  MessageBox( ForWhat + ' not implemented.' );
end;

var
  StubList : TStringList;

procedure Stub1(ForWhat: String);
var
  Temp : Integer;
begin
  if StubList.Find( ForWhat, Temp ) then exit;
  { TODO 4 -odonz -cDebugging Aide : Add critical section here to avoid recursion. }
  Stub(ForWhat);
  StubList.Add( ForWhat );
end;

initialization
  StubList := TStringList.Create;
  StubList.Sorted := True;

finalization
  StubList.Free;
end.

