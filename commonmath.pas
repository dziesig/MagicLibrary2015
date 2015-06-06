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


unit CommonMath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function Min( V0, V1 : Double ) : Double; overload;
function Min( V0, V1 : Integer ) : Integer; overload;
function Min( V0, V1 : Cardinal ) : Cardinal; overload;
function Max( V0, V1 : Double ) : Double; overload;
function Max( V0, V1 : Integer ) : Integer; overload;
function Max( V0, V1 : Cardinal ) : Cardinal; overload;

procedure Exchange( var V0, V1 : Double ); overload;
procedure Exchange( var V0, V1 : Integer ); overload;
procedure Exchange( var V0, V1 : Cardinal ); overload;

function RealMod (x, y : extended) : extended;

// Compare Between Limits... I inclusive L <= V <= U
//                           G gt        L <= V <  U
//                           L lt        L <  V <= U
//                           X exclusive L <  V <  U

function CBLI( L, V, U : extended ) : Boolean; overload;
function CBLI( L, V, U : Integer ) : Boolean; overload;
function CBLG( L, V, U : extended ) : Boolean; overload;
function CBLG( L, V, U : Integer ) : Boolean; overload;
function CBLL( L, V, U : extended ) : Boolean; overload;
function CBLL( L, V, U : Integer ) : Boolean; overload;
function CBLX( L, V, U : extended ) : Boolean; overload;
function CBLX( L, V, U : Integer ) : Boolean; overload;

{------------------------------------------------------------------------------}
{ Function type definitions for UnitTest driver program                        }
{------------------------------------------------------------------------------}

type
  TMinMaxFloatTest            = function( V0, V1 : Extended ) : Extended of object;
  TMinMaxIntegerTest          = function( V0, V1 : Integer ) : Integer of object;

implementation

uses
  CommonLog;

function Min(V0, V1: Double): Double;
begin
  if V0 > V1 then
    Result := V1
  else
    Result := V0;
end;

function Min(V0, V1: Integer): Integer;
begin
  if V0 > V1 then
    Result := V1
  else
    Result := V0;
end;

function Min(V0, V1: Cardinal): Cardinal;
begin
  if V0 > V1 then
    Result := V1
  else
    Result := V0;
end;

function Max(V0, V1: Double): Double;
begin
  if V0 < V1 then
    Result := V1
  else
    Result := V0;
end;

function Max(V0, V1: Integer): Integer;
begin
  if V0 < V1 then
    Result := V1
  else
    Result := V0;
end;

function Max(V0, V1: Cardinal): Cardinal;
begin
  if V0 < V1 then
    Result := V1
  else
    Result := V0;
end;

procedure Exchange(var V0, V1: Double);
var
  V : Double;
begin
  V := V0;
  V0 := V1;
  V1 := V;
end;

procedure Exchange(var V0, V1: Integer);
var
  V : Integer;
begin
  V := V0;
  V0 := V1;
  V1 := V;
end;

procedure Exchange(var V0, V1: Cardinal);
var
  V : Cardinal;
begin
  V := V0;
  V0 := V1;
  V1 := V;
end;

function RealMod (x, y : extended) : extended;
begin
   Result := x - y * Trunc(x/y);
end;

function CBLI(L, V, U: extended): Boolean;
begin
  Result := (L <= V) and (V <= U);
  //Log.FormatLn('%d = (%f <= %f) and (%f <= %f )',[ord(result),L,V,V,U]);
end;

function CBLI(L, V, U: Integer): Boolean;
begin
  Result := (L <= V) and (V <= U);
end;

function CBLG(L, V, U: extended): Boolean;
begin
  Result := (L <= V) and (V <  U);
end;

function CBLG(L, V, U: Integer): Boolean;
begin
  Result := (L <= V) and (V <  U);
end;

function CBLL(L, V, U: extended): Boolean;
begin
  Result := (L <  V) and (V <= U);
end;

function CBLL(L, V, U: Integer): Boolean;
begin
  Result := (L <  V) and (V <= U);
end;

function CBLX(L, V, U: extended): Boolean;
begin
  Result := (L <  V) and (V <  U);
end;

function CBLX(L, V, U: Integer): Boolean;
begin
  Result := (L <  V) and (V <  U);
end;

end.

