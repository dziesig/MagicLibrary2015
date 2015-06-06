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

unit CommonIni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{==============================================================================}
{ Config (.ini) file support.  Slow but easy (and isn't used that often).      }
{==============================================================================}
  procedure SetConfig( const Section, Ident : String; const Value : String ); overload;
  function  GetConfig( const Section, Ident : String; const Default : String ) : String; overload;
  procedure SetConfig( const Section, Ident : String; const Value : Integer ); overload;
  function  GetConfig( const Section, Ident : String; const Default : Integer ) : Integer; overload;
  procedure SetConfig( const Section, Ident : String; const Value : Double ); overload;
  function  GetConfig( const Section, Ident : String; const Default : Double ) : Double; overload;
  procedure SetConfig( const Section, Ident : String; const Value : Boolean ); overload;
  function  GetConfig( const Section, Ident : String; const Default : Boolean ) : Boolean; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TDateTime ); overload;
  function  GetConfig( const Section, Ident : String; const Default : TDateTime ) : TDateTime; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TDate ); overload;
  function  GetConfig( const Section, Ident : String; const Default : TDate ) : TDate; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TTime ); overload;
  function  GetConfig( const Section, Ident : String; const Default : TTime ) : TTime; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TStream ); overload;
  function  GetConfig( const Section, Ident : String; Value : TStream ) : Integer; overload;

  procedure DelConfig( const Section, Ident : String );

implementation

uses
  IniFiles;

function OpenIniFile : TIniFile; // Local to this unit
var
  IniFileName : String;
  IniFilePath : String;
begin
  IniFileName := GetAppConfigFile( False, True );
  IniFilePath := ExtractFilePath( IniFileName );
  if not FileExists( IniFilePath ) then
    ForceDirectories( IniFilePath );
  Result := TIniFile.Create( IniFileName );
end;

procedure SetConfig(const Section, Ident: String; const Value: String);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteString( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: String): String;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadString( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: Integer);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteInteger( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: Integer
  ): Integer;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadInteger( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: Double);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteFloat( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: Double): Double;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadFloat( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: Boolean);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteBool( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: Boolean
  ): Boolean;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadBool( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TDateTime);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteDateTime( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: TDateTime
  ): TDateTime;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadDateTime( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TDate);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteDate( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: TDate): TDate;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadDate( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TTime);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteTime( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: TTime): TTime;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadTime( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TStream);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteBinaryStream( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; Value : TStream
  ): Integer;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadBinaryStream( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

procedure DelConfig(const Section, Ident: String);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.DeleteKey( Section, Ident );
  finally
    IniFile.Free;
  end;
end;

end.

