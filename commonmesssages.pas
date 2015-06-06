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

unit CommonMesssages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function SaveModified( Doing, What : String ) : Integer;

implementation

uses
  Dialogs;

function SaveModified( Doing, What : String ) : Integer;
begin
  Result := MessageDlg( What + ' has been modified.'#13#10 +
                     'Do you want to save it before ' + Doing + ' a new one?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

end.

