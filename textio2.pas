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

unit textio2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTextIO }

  TTextIO = class( TObject )
    private
      fOut        : Boolean;
      fLineNo     : Integer;
      fFile       : Text;
      fPath       : String;

    public
      constructor Create( aPath : String; Output : Boolean );
      destructor  Destroy; override;

      function    Readln( var Line : String; MultiLine : Boolean = False ) : Integer; overload;
      function    Readln( var Int  : Integer ) : Integer;  overload;
      function    Readln( var Dbl  : Double ) : Integer;  overload;
      function    Readln( var Bool : Boolean ) : Integer; overload;
      function    ReadLn( var Card : Cardinal ) : Integer;   overload;

      procedure   Writeln( Line : String; MultiLine : Boolean = False );    overload;
      procedure   Writeln( Int  : Integer );   overload;
      procedure   Writeln( Dbl  : Double );     overload;
      procedure   Writeln( Bool : Boolean );    overload;
      procedure   WriteLn( Card : Cardinal );   overload;

      property    LineNo : Integer read fLineNo;
      property    Path : String read fPath;

  end;

implementation

{ TTextIO }

{uses
  Common1;  }

constructor TTextIO.Create( aPath : String; Output : Boolean);
begin
  inherited Create;
  fLineNo := 0;
  fPath := aPath;
  AssignFile( fFile, aPath );
  fOut := Output;
  if Output then
    Rewrite( fFile )
  else
    Reset( fFile );
end;

destructor TTextIO.Destroy;
begin
  CloseFile( fFile );
end;



function TTextIO.Readln(var Line: String; MultiLine : Boolean): Integer;
var
  Lines : Integer;
  Str   : String;
  I     : Integer;
begin
  if MultiLine then
    begin
      ReadLn( Lines );
      for I := 0 to pred( Lines ) do
        begin
          ReadLn( Str );
          Line := Line + Str + #10;
        end;
    end
  else
    begin
      Inc(fLineNo);
      Result := fLineNo;
      if fOut then
        raise Exception.CreateFmt( 'Attempt to read from output file %s at line %d',
                                   [ExtractFileName( fPath ),fLineNo]);
      System.ReadLn( fFile, Line );
    end;
end;

procedure TTextIO.Writeln(Bool: Boolean);
var
  Txt : String;
begin
  Inc(fLineNo);
  if not fOut then
    raise Exception.CreateFmt( 'Attempt to write to input file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  if Bool then
    Txt := 'TRUE'
  else
    Txt := 'FALSE';
  System.Writeln( fFile, Txt );
end;

procedure TTextIO.WriteLn(Card: Cardinal);
begin
  Inc( fLineNo );
  if not fOut then
    raise Exception.CreateFmt( 'Attempt to write to input file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  System.Writeln( fFile, Card );
end;

function TTextIO.Readln(var Int: Integer): Integer;
var
  S : String;
begin
  Inc(fLineNo);
  Result := fLineNo;
  if fOut then
    raise Exception.CreateFmt( 'Attempt to read from output file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  try
 //   System.ReadLn( fFile, Int );
    System.ReadLn( fFile, S );
    Int := StrToInt( S );
  except
    raise EFormatError.CreateFmt('Invalid numeric (Integer) format "%s" at line %d.',[s,fLineNo]);
  end;
end;

function TTextIO.Readln(var Dbl: Double): Integer;
begin
  Inc(fLineNo);
  Result := fLineNo;
  if fOut then
    raise Exception.CreateFmt( 'Attempt to read from output file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  System.ReadLn( fFile, Dbl );
end;

procedure TTextIO.Writeln(Line: String; MultiLine : Boolean);
var
  Lines : Integer;
  Len   : Integer;
  I     : Integer;
begin
  if MultiLine then
    begin
      Lines := 1;   // This is correct.  WriteLn appends NewLine.
      Len   := Length( Line );
      if Len > 0 then
        begin
          if Line[Len] = #10 then
            begin
              Dec(Len);
              Line := Copy(Line,1,Len);
            end;
          for I := 1 to Len do
            if Line[I] = #10 then
              Inc(Lines);
        end;
      WriteLn( Lines );
      WriteLn( Line );
    end
  else
    begin
      Inc( fLineNo );
      if not fOut then
        raise Exception.CreateFmt( 'Attempt to write to input file %s at line %d',
                                   [ExtractFileName( fPath ),fLineNo]);
      System.Writeln( fFile, Line );
    end;
end;

procedure TTextIO.Writeln(Int: Integer);
begin
  Inc( fLineNo );
  if not fOut then
    raise Exception.CreateFmt( 'Attempt to write to input file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  System.Writeln( fFile, Int );
end;

procedure TTextIO.Writeln(Dbl: Double);
begin
  Inc( fLineNo );
  if not fOut then
    raise Exception.CreateFmt( 'Attempt to write to input file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  System.Writeln( fFile, Dbl );
end;

function TTextIO.Readln(var Bool: Boolean): Integer;
var
  Txt : String;
begin
  Inc(fLineNo);
  Result := fLineNo;
  if fOut then
    raise Exception.CreateFmt( 'Attempt to read from output file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  System.ReadLn( fFile, Txt );
  Bool := Txt = 'TRUE';
end;

function TTextIO.ReadLn(var Card: Cardinal): Integer;
begin
  Inc(fLineNo);
  Result := fLineNo;
  if fOut then
    raise Exception.CreateFmt( 'Attempt to read from output file %s at line %d',
                               [ExtractFileName( fPath ),fLineNo]);
  try
    System.ReadLn( fFile, Card );
  except
    raise EFormatError.CreateFmt('Invalid numberic (Cardinal) format at line %d.',[fLineNo]);
  end;
end;

end.

