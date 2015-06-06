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

unit CommonMisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function AreYouSure( Prompt : String; withCancel : Boolean = False ) : Integer;

function SaveIfModified( Doing, What : String ) : Integer;

function MonthName( Index : Integer ) : String;

implementation

uses
  Dialogs;

function AreYouSure( Prompt : String; withCancel : Boolean ) : Integer;
begin
  if withCancel then
    Result := MessageDlg( Prompt + #13#10'Are You Sure?',
                          mtConfirmation, [mbYes, mbNO, mbCancel ], 0)
  else
    Result := MessageDlg( Prompt + #13#10'Are You Sure?',
                          mtConfirmation, [mbYes, mbNO ], 0);
end;

function SaveIfModified( Doing, What : String ) : Integer;
begin
  Result := MessageDlg( What + ' has been modified.'#13#10 +
                     'Do you want to save it before ' + Doing + ' a new one?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

function MonthName(Index: Integer): String;
type
  TMonths = array[0..11] of String[3];
const
  MonthNames : TMonths = ( 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' );
begin
  if Index in [0..11] then
    Result := MonthNames[Index]
  else
    Result := '';
end;


end.

