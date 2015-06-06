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

unit CommonApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Returns (after creating, if necessary) the path to the Default Save Location }
{ for this App.                                                                }
function DefaultSaveLocation( AppName : String = ''; WorkingDir : String = '' ): string;

{ This is here because even after using TurboPascal from version 1.0 til now,  }
{ I never can remember if it is ParamStr(0) or ParamStr[0].                    }
function ExePath : String;

{ Strips File Type (extension) if necessary }
function ExeName : String;

function BuildDateTime : String;


implementation

uses
  Stringsubs;

function DefaultSaveLocation( AppName : String;  WorkingDir : String ): string;
begin
  Result := GetUserDir;
  if Empty( AppName ) then
    Result := Result + ApplicationName + DirectorySeparator
  else
    Result := Result + AppName + DirectorySeparator;
  if not Empty( WorkingDir ) then
    Result := Result + WorkingDir + DirectorySeparator;

  if not DirectoryExists( Result ) then
    ForceDirectories( Result );
end;

function ExePath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function ExeName: String;
var
  P : Integer;
begin
  Result := ExtractFileName(ParamStr(0));
  P := Pos('.',Result);
  if P > 0 then
    Result := Copy(Result,1,P-1);
end;

function BuildDateTime: String;
var
  fa : LongInt;
  BD : TDateTime;
begin
  fa:=FileAge(ParamStr(0));
  BD := FileDateTodateTime(fa);
  Result := FormatDateTime('dddddd tt',bd);
end;

end.

