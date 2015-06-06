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

unit Persists2;

{$mode objfpc}{$H+}
{$M+} // Enables RTTI

interface

uses
  Classes, SysUtils, TextIO2;

type

{ TPersists }

{
  TPersists is the root of streamable objects, which may be created by
  TObjectFactory (primarily for loading from the stream).

  These objects write and read a version field which supports upgrading by
  allowing older objects to be read from the stream and created as the most
  recent object (see MakeNew).

  These objects also maintain a Modified status and provide the means of
  passing this status to their parent (if any).  This automates what used to
  be an error-prone (for Don Ziesig) approach to manually tracking modifications
  made by the user.
}

  TPersists = class
  private
    fOnChange : TNotifyEvent;
    fParent   : TPersists;

    function GetVersion: Cardinal;
    procedure SetOnChange(AValue: TNotifyEvent);

    procedure SaveHeader( TextIO : TTextIO );
    procedure SaveTrailer( TextIO : TTextIO );
  protected
    fModified : Boolean;
    fVersion  : Cardinal; // Version of child object .. should be set by
                          // constructor to be a value > 0
    function  IsModified : Boolean; virtual;
  public
    DebugString : String;
    constructor Create( aParent : TPersists = nil); virtual;
    destructor  Destroy; override;
    procedure   MakeNew; virtual; // Initialize new instances.  Particularly useful
                                  // for handling reading older versions of the object
                                  // which do not have the newer data elements.

    class function Load( TextIO : TTextIO; aParent : TPersists = nil ) : TPersists; // Calls read after
                                                         // creating object
    procedure Store( TextIO : TTextIO ); virtual; // Calls write after
                                                  // SaveHeader and
                                                  // before SaveTrailer;

    procedure Read( TextIO : TTextIO; aVersion : Integer ); virtual; abstract;
    procedure Write( TextIO : TTextIO ); virtual; abstract;

    function  ToString : String; virtual;

    procedure DumpToFile( var F : Text; Path : String = '' ); virtual; abstract;

    procedure Assign( Source : TPersists ); virtual;
    procedure AssignTo( Dest : TPersists ); virtual;

    // Consistent approach to setting the modified flag.

    procedure Update( var Data : Integer;  NewValue : Integer );  overload;
    procedure Update( var Data : Cardinal; NewValue : Cardinal ); overload;
    procedure Update( var Data : Double;   NewValue : Double );   overload;
    procedure Update( var Data : String;   NewValue : String );   overload;
    procedure Update( var Data : Boolean;  NewValue : Boolean );  overload;

    procedure Modify; virtual;
    procedure UNMODIFY; virtual; // LOOK HERE there are only a few places where
                                 // this is valid }
    property Version  : Cardinal read GetVersion;
    property Modified : Boolean read IsModified;
    property Parent   : TPersists read fParent write fParent;
    property OnChange : TNotifyEvent read fOnChange write SetOnChange; // Needed to
                                                                       // monitor mods.
  end;

  { TPersistsListItem }

  TPersistsListItem = class(TPersists)
  private
    procedure SetId(aValue: Cardinal);
  protected
    fID       : Cardinal; // When used with PersistsListsN, is the 0-based order of
                          // addition to or insertion in the list.
                          // This is NOT the index in the list since items may
                          // be inserted, deleted or exchanged.
  public
    procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
    procedure Write( TextIO : TTextIO ); override;

    procedure DumpToFile( var F : Text; Path : String = '' ); virtual; abstract;

    procedure Assign( Source : TPersists ); override;
    procedure AssignTo( Dest : TPersists ); override;

    function Compare( ToItem : TPersistsListItem; Index : Integer ) : Integer;  virtual;
    class function  IndexCount : Integer; virtual;
    class function  IndexName( Index : Integer ) : String ; virtual;
    class function  GetIndex( Name : String ) : Integer;

    function  ToString : String; override;

    property Id : Cardinal read FId write SetId;
  end;

  EModifingID       = Exception;
  ENonExistentIndex = Exception;
  EEndOfClass       = Exception;
  EStartOfClass     = Exception;

implementation

uses
  ObjectFactory1;

{ TPersistsListItem }

procedure TPersistsListItem.Assign(Source: TPersists);
begin
  inherited Assign(Source);
end;

procedure TPersistsListItem.AssignTo(Dest: TPersists);
begin
  inherited AssignTo(Dest);
end;

function TPersistsListItem.Compare(ToItem: TPersistsListItem; Index: Integer): Integer;
begin
  if Index > 0 then
    raise Exception.CreateFmt('Invalid Index %d.  TPersistsListItem expects 0.',[Index]);
  Result := Id - ToItem.Id;
end;

class function TPersistsListItem.GetIndex(Name: String): Integer;
var
  I : Integer;
begin
  for I := 0 to pred(IndexCount) do
    if IndexName(I) = Name then
      begin
        Result := I;
        exit;
      end;
  raise ENonExistentIndex.CreateFmt( 'Attempt to retrieve non-existent Index "%s"',[Name]);
end;

class function TPersistsListItem.IndexCount: Integer;
begin
  Result := 1;  // If not overridden, there is only ONE index, namely the Id
                // of the object.
end;

class function TPersistsListItem.IndexName(Index: Integer): String;
begin
  if Index = 0 then
    Result := 'Id'
  else
    raise ENonExistentIndex.CreateFmt( 'Attempt to retrieve non-existent IndexName for index %d.',
                                       [Index] );
end;

procedure TPersistsListItem.Read(TextIO: TTextIO; aVersion: Integer);
begin
  // This only works for TPersistsListItem version 1.
  // Unfortunately, if it becomes necessary to update
  // TPersistsListItem, all existing files become obsolete. :(
  MakeNew;
  if aVersion >= 1 then
    begin
      TextIO.ReadLn( fId );
    end;
end;

procedure TPersistsListItem.SetId(aValue : Cardinal);
begin
  if (fId <> 0) and (fId <> aValue) then
    raise EModifingID.CreateFmt( 'Attempt to re-set an object''s Id from %d to %d',
                                 [fId,aValue] );
  fId := aValue;
end;

function TPersistsListItem.ToString: String;
begin
  Result := inherited + 'Id: ' + IntToStr(fId) + ', ';
end;

procedure TPersistsListItem.Write(TextIO: TTextIO);
begin
  TextIO.WriteLn( fId );
end;


{ TPersists }

procedure TPersists.Assign(Source: TPersists);
begin
  fModified := false;
  fParent := Source.Parent;
end;

procedure TPersists.AssignTo(Dest: TPersists);
begin
  Dest.fModified := False;
  Dest.Parent := Parent;
end;

constructor TPersists.Create(aParent: TPersists);
begin
  fParent := aParent;
  fOnChange := nil;
  if Assigned(fOnChange) then
    raise Exception.Create('fOnChange not nil after Create');
  MakeNew;
end;

destructor TPersists.Destroy;
begin
  inherited Destroy;
end;

function TPersists.GetVersion: Cardinal;
begin
  if fVersion = 0 then
    raise Exception.Create('Getting unset Version');
  Result := fVersion;
end;

function TPersists.IsModified: Boolean;
begin
  Result := fModified;
end;

class function TPersists.Load(TextIO: TTextIO; aParent : TPersists ): TPersists;
  var
    ClsName  : String;
    S        : String;
    aVersion : Integer;

    procedure CheckEndClass(FileClass, ExpectedClass: String; TextIO : TTextIO);
    var
      Cls : String;
      Len : Integer;
      Line : Integer;
      H, T : String;
    begin
      Len := Length( FileClass );
      Cls := Copy( FileClass,3,Len-3);
      Line := TextIO.LineNo;
      H := Copy( FileClass,1,2);
      T := Copy( FileClass,Len,1);
      if (H <> '</') or (T <> '>') then
        raise EEndOfClass.CreateFmt( 'Invalid End of Class format [%s], expecting [%s] at line %d.',
                                     [FileClass,ExpectedClass,Line] );
      if Cls <> ExpectedClass then
        raise EEndOfClass.CreateFmt( 'End of Class mismatch.  [%s] found, [%s] expected at line %d.',
                                     [Cls, ExpectedClass, Line] );
    end;
    procedure CheckStartClass(var FileClass : String; TextIO : TTextIO);
    var
      Cls : String;
      Len : Integer;
    begin
      Len := Length( FileClass );
      Cls := Copy( FileClass,2,Len-2);
      if (Copy( FileClass,1,1) <> '<') or (Copy( FileClass,Len,1) <> '>') then
        raise EStartOfClass.CreateFmt( 'Invalid Start of Class format [%s] at line %d.',
                                       [FileClass, TextIO.LineNo] );
      FileClass := Cls;
    end;

  begin
    aVersion := 0;
    S := '';
    ClsName := self.ClassName;    // Get the expected class name
    TextIO.ReadLn(S);             // Read the start of class
    CheckStartClass(S, TextIO);   // Assert they are correct and of correct format
    ClsName := S;
    TextIO.Readln(aVersion);       // Read the Object's version
    Result := ObjectFactory.MakeObject( ClsName ) as TPersists;
    Result.Parent := aParent;
    Result.fVersion := aVersion;  // This sets the version of the newly created object.
    Result.Read( TextIO, aVersion );
    TextIO.Readln(S);             // Read the end of class
    CheckEndClass(S,ClsName, TextIO);     // Assert end of class is correct and of correct format
    Result.UNMODIFY;              // make sure this was NOT modified by the load.
end;

procedure TPersists.MakeNew;
begin
  fModified := false;
end;

procedure TPersists.SetOnChange(AValue : TNotifyEvent);
begin
  fOnChange := AValue;
end;

procedure TPersists.Store(TextIO: TTextIO);
begin
  SaveHeader( TextIO  );
  Write( TextIO );
  SaveTrailer( TextIO );
end;

function TPersists.ToString: String;
begin
  Result := '';
end;


procedure TPersists.Modify;
begin
  if not Assigned(self) then
    exit;
  if (not fModified) and (fParent <> nil) then
    fParent.Modify;
  fModified := true;
  //if fParent <> nil then
  //  fParent.Modify;
  if Assigned(fOnChange) then
    fOnChange( self as TObject );
end;

procedure TPersists.SaveHeader(TextIO: TTextIO);
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class
  TextIO.Writeln( Version );
end;

procedure TPersists.SaveTrailer(TextIO: TTextIO);
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  UNMODIFY;                     // if it were modified, it isn't any more.
end;

procedure TPersists.UNMODIFY;
begin
  fModified := false;  // The original version of TPersists UNMODIFIED the Parent
                       // but that doesn't make sense since other parts of the
                       // parent tree might still be modified.  It only makes
                       // sense to set UNMODIFIED from the root.
end;

procedure TPersists.Update(var Data: Boolean; NewValue: Boolean);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: Cardinal; NewValue: Cardinal);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: Double; NewValue: Double);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: Integer; NewValue: Integer);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: String; NewValue: String);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

end.

