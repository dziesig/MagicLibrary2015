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

unit CommonLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCommonLog }

  TCommonLog = class

  private
    fActive: Boolean;
    fFileName : String;
    vFile     : TextFile;
    procedure SetActive(AValue: Boolean);
  public
    constructor Create(theFileName : String = '');

    procedure PutLn( Str : String );
    procedure FormatLn( Str : String; Args : array of const );
    procedure Timing( Start : TDateTime; What : String = '' );

    property Active : Boolean read fActive write SetActive;
    property FileName : String read fFileName;

  end;

var
  Log : TCommonLog;

implementation

uses
  CommonAPP, Stringsubs;

{ TCommonLog }

constructor TCommonLog.Create(theFileName: String);
begin
  if Empty( theFileName ) then
    fFileName := DefaultSaveLocation(ApplicationName,'Logs')+'IronMikeLog.txt'
  else
    fFileName := theFileName;
  AssignFile( vFile, fFileName );
  Rewrite( vFile );
  CloseFile( vFile );
end;

procedure TCommonLog.FormatLn(Str: String; Args: array of const);
begin
  Append( vFile );
  WriteLn( vFile, Format(Str,Args) );
  CloseFile( vFile );
end;

procedure TCommonLog.SetActive(AValue: Boolean);
begin
  if fActive=AValue then Exit;
  fActive:=AValue;
end;

procedure TCommonLog.Timing(Start: TDateTime; What : String);
var
  DT : TDateTime;
begin
  DT := Now - Start;
  PutLn( 'Time:  ' + FormatDateTime('s.zzz',DT) + ' -- ' + What );
end;

procedure TCommonLog.PutLn(Str: String);
begin
  Append( vFile );
  WriteLn( vFile, Str );
  CloseFile( vFile );
end;

initialization

  Log := TCommonLog.Create;

finalization

  Log.Destroy;

end.

