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
{ WARNING.  This is deprecated due to an error in the Lazarus LCL which causes }
{           strange, non-reproducable behavior if the app-specific frame is    }
{           sub-classed.                                                       }
{           It has been replaced by the MagicFormPanel component.              }
{==============================================================================}

unit MagicFrameForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Persists2;

type

  { TMagicForm }

  TMagicForm = class(TForm)
  private
    fData: TPersists;
    procedure SetData(AValue: TPersists);
    public
      function  IsValid : Boolean; virtual; abstract;
      procedure ControlsLoad; virtual; abstract;

      property Data : TPersists read fData write SetData;
  end;

implementation

{ TMagicForm }

procedure TMagicForm.SetData(AValue : TPersists);
begin
  fData := AValue;
end;


end.

