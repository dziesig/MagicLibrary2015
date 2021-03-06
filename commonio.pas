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

unit CommonIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{==============================================================================}
{ File and Directory Support stuff.                                            }
{==============================================================================}

type
  PBoolean = ^Boolean;

  TProcessFileObject = procedure (       BasePath, RelPath : String;
                                   const SR                : TSearchRec;
                                   const Depth             : Integer;
                                         UserData          : TObject = nil) of object;
  TProcessFile       = procedure (       BasePath, RelPath : String;
                                   const SR                : TSearchRec;
                                   const Depth             : Integer;
                                         UserData          : TObject = nil);

{ TODO 3 -odonz -cTest : Implement Unit Test }
procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                             Attributes              : Integer;
                             SubDirsFirst                 : Boolean;
                             DoSomething             : TProcessFile;
                             UserData                : TObject = nil;
                             Stop                    : PBoolean = nil ); overload;

procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                              Attributes              : Integer;
                              SubDirsFirst                 : Boolean;
                              DoSomething             : TProcessFile;
                              UserData                : TObject;
                              Depth                   : Integer;
                              Stop                    : PBoolean = nil ); overload;

{ TODO 3 -odonz -cTest : Implement Unit Test }
procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                             Attributes              : Integer;
                             SubDirsFirst                 : Boolean;
                             DoSomething             : TProcessFileObject;
                             UserData                : TObject = nil;
                             Stop                    : PBoolean = nil ); overload;

procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                              Attributes              : Integer;
                              SubDirsFirst                 : Boolean;
                              DoSomething             : TProcessFileObject;
                              UserData                : TObject;
                              Depth                   : Integer;
                              Stop                    : PBoolean = nil ); overload;

{==============================================================================}
{ File name and path processing.                                               }
{==============================================================================}
function  CountFilesOrDirectories( Path : String ) : Integer;

function  RemoveExt( FilePath : String ) : String;

function UniqueFile( FilePath : String ) : String;

{------------------------------------------------------------------------------}
{ RTrimPath removes Count directories from FilePath (e.g.                      }
{ RTrimFilePath( 'C:\ABC\def\ghi\MyFile.dat', 1 ) returns                      }
{ C:\ABC\def\                                                                  }
{ RTrimFilePath( 'C:\ABC\def\ghi\MyFile.dat', 2 ) returns                      }
{ C:\ABC\                                                                      }
{------------------------------------------------------------------------------}

function  RTrimPath( FilePath : String; Count : Integer = 1 ) :String;

{------------------------------------------------------------------------------}
{ ExtractFileOrDirectory allows the extraction of the Nth file or directory in }
{ the Path string such that:  Loc = 0, returns the left most file/directory    }
{ name, Loc = 1, returns the second left-most, ... Loc = -1, returns the right }
{ most file/directory name, Loc = -2, returns the second right-most file/      }
{ directory name.                                                              }
{------------------------------------------------------------------------------}
function ExtractFileOrDirectory( Loc : Integer; Path : String ) : String;

{==============================================================================}
{ File handling routines                                                       }
{==============================================================================}

procedure RmFiles( Base, Rel, FileMask : String ); overload;
procedure RmFiles(       Base, Rel : String;      { TODO 3 -odonz -cTest : Implement Unit Test }
                   const SR        : TSearchRec;
                   const Depth     : Integer ); overload;

procedure CopyFile(Src, Dst : String; RmSrc : Boolean = False);

procedure CopyFiles(FromPath, ToPath, Wildcard : String; Force : Boolean = false );
procedure MoveFiles(FromPath, ToPath, Wildcard : String );


function  CompareFiles( Src, Dst : String;  // This is tested during CopyFile test
                        IgnoreWhiteSpace : Boolean = False ) : Boolean;

function FileSize( Path : String ) : Int64;

{==============================================================================}
{ Procedures to read/write boolean from/to a text file                         }
{==============================================================================}
procedure ReadBool( var F : TextFile; var Value : Boolean );
procedure WriteBool( var F : TextFile; Value : Boolean );


{==============================================================================}
{ Procedure to append string list to a file.                                   }
{==============================================================================}

procedure AppendStringListToFile( Path : String; StringList : TStringList );

{==============================================================================}
{ Blob to/from Hex String                                                      }
{==============================================================================}

type
  TByteArray = array of Byte;

function  HexStringToByteArray( S : String ) : TByteArray;
function  ByteArrayToHexString( A : array of Byte) : String;

function  TStreamToHexString( Stream : TStream ) : String;
procedure HexStringToTStream( Stream : TStream; S : String);

{==============================================================================}
{ Shell Execute and Wait                                                       }
{==============================================================================}
{$ifdef WINDOWS}
procedure ExecuteAndWait( FilePath : String );
{$endif}
{------------------------------------------------------------------------------}
{ Function type definitions for Test driver program                            }
{------------------------------------------------------------------------------}

type
  TCopyFileTest               = function( F0, F1 : String )  : Boolean of object;
  TWalkDirectoryTreeTest      = function( SubDirsFirst : Boolean ) : Boolean of object;
  TExtractFileOrDirectoryTest = function( Loc : Integer; Value : String ) : String of object;
  TCopyFilesTest              = function( Src, Dst : String ) : Boolean of object;
  TRemoveExtTest              = function( Path, NameOnly : String ) : Boolean of object;
  TReadBoolTest               = function( Src : String; Line : Integer ) : Boolean of object;

implementation

uses
  FileUtil, StrUtils
{$ifdef WINDOWS}
  , Windows, ShellAPI
{$endif};

procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                             Attributes              : Integer;
                             SubDirsFirst            : Boolean;
                             DoSomething             : TProcessFile;
                             UserData                : TObject;
                             Stop                    : PBoolean );
var
  Depth : Integer;
begin
  Depth := 0;
  WalkDirectoryTree1( BasePath, RelPath, Mask, Attributes, SubDirsFirst,
                      DoSomething, UserData, depth, Stop );
end;

procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                              Attributes              : Integer;
                              SubDirsFirst            : Boolean;
                              DoSomething             : TProcessFile;
                              UserData                : TObject;
                              Depth                   : Integer;
                              Stop                    : PBoolean );
var
  SR: TSearchRec;
  SearchPath : String;
  NewRelPath : String; // For debug and test

  function Go : Boolean;
  begin
    if Assigned(Stop) then
      begin
//        Application.ProcessMessages;
        Result := not  Stop^;
      end
    else
      Result := True;
  end;
  procedure NormalFiles( SearchPath : String; Attributes : Integer; var SR : TSearchRec );
  begin
    if (FindFirst( SearchPath, Attributes - faDirectory, SR) = 0) and Go then
      begin
        DoSomething(BasePath,RelPath,SR,Depth,UserData);

        while (FindNext( SR ) = 0) and Go do
          DoSomething( BasePath,RelPath,SR,Depth,UserData);
      end;
    Sysutils.FindClose( SR );
  end;
  { TODO 3 -odonz -cPossible error : Figure out why SearchPath is not used in the procedure. }
  procedure Directories( SearchPath, RelPath : String; var SR : TSearchRec );
  begin
    if (SR.Attr and faDirectory) = faDirectory then
      if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          NewRelPath := RelPath + SR.Name + DirectorySeparator;
          if SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
          WalkDirectoryTree1( BasePath,NewRelPath,
                             Mask,
                             Attributes,
                             SubDirsFirst,
                             DoSomething,
                             UserData,
                             Depth+1,
                             Stop );
          if not SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
        end;
  end;

begin
  if BasePath[Length(BasePath)] <> DirectorySeparator then
    BasePath := BasePath + DirectorySeparator;
  SearchPath := BasePath + RelPath;
  if SearchPath[Length(SearchPath)] <> DirectorySeparator then
    SearchPath := SearchPath + DirectorySeparator;
  SearchPath := SearchPath + Mask;

// Handle the normal files first if requested
  if not SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
// Now handle the directories
  if (FindFirst( SearchPath, faDirectory, SR ) = 0) and Go then
    begin
      Directories( SearchPath, RelPath, SR );
      while (FindNext(SR) = 0) and go do
        begin
          Directories( SearchPath, RelPath, SR );
        end;
    end;

  Sysutils.FindClose( SR );
  // Handle the normal files if requested
  if SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
end;  { procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String; ... }

procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                             Attributes              : Integer;
                             SubDirsFirst            : Boolean;
                             DoSomething             : TProcessFileObject;
                             UserData                : TObject;
                             Stop                    : PBoolean );
var
  Depth : Integer;
begin
  Depth := 0;
  WalkDirectoryTree1( BasePath, RelPath, Mask, Attributes, SubDirsFirst,
                      DoSomething, UserData, depth, Stop );
end;

procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                              Attributes              : Integer;
                              SubDirsFirst            : Boolean;
                              DoSomething             : TProcessFileObject;
                              UserData                : TObject;
                              Depth                   : Integer;
                              Stop                    : PBoolean );
var
  SR: TSearchRec;
  SearchPath : String;
  NewRelPath : String; // For debug and test
  function Go : Boolean;
  begin
    if Assigned(Stop) then
      begin
//        Application.ProcessMessages;
        Result := not  Stop^;
      end
    else
      Result := True;
  end;
  procedure NormalFiles( SearchPath : String; Attributes : Integer; var SR : TSearchRec );
  begin
    if (FindFirst( SearchPath, Attributes - faDirectory, SR) = 0) and Go then
      begin
        DoSomething(BasePath,RelPath,SR,Depth,UserData);

        while (FindNext( SR ) = 0) and Go do
          DoSomething( BasePath,RelPath,SR,Depth,UserData);
      end;
    Sysutils.FindClose( SR );
  end;
  procedure Directories( SearchPath, RelPath : String; var SR : TSearchRec );
  begin
    if (SR.Attr and faDirectory) = faDirectory then
      if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          NewRelPath := RelPath + SR.Name + DirectorySeparator;
          if SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
          WalkDirectoryTree1( BasePath,NewRelPath,
                             Mask,
                             Attributes,
                             SubDirsFirst,
                             DoSomething,
                             UserData,
                             Depth+1,
                             Stop );
          if not SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
        end;
  end;

begin
  if BasePath[Length(BasePath)] <> DirectorySeparator then
    BasePath := BasePath + DirectorySeparator;
  SearchPath := BasePath + RelPath;
  if SearchPath[Length(SearchPath)] <> DirectorySeparator then
    SearchPath := SearchPath + DirectorySeparator;
  SearchPath := SearchPath + Mask;

// Handle the normal files first if requested
  if not SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
// Now handle the directories
  if (FindFirst( SearchPath, faDirectory, SR ) = 0) and Go then
    begin
      Directories( SearchPath, RelPath, SR );
      while (FindNext( SR ) = 0) and Go do
        Directories( SearchPath, RelPath, SR );
    end;
  Sysutils.FindClose( SR );
  if SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
end;  { procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String; ... }

{==============================================================================}

function RTrimPath(FilePath: String; Count: Integer): String;
var
  I, P : Integer;
begin
  Result := ExtractFileDir( FilePath );
  for I := 1 to Count do
    begin
      P := RPos( DirectorySeparator, Result );
      Result := Copy( Result, 1, P);
    end;
  Result := Result + DirectorySeparator;
end;

function ExtractFileOrDirectory(Loc: Integer; Path: String): String;
  function RPos( Str : String ) : Integer;
  var
    I : Integer;
  begin
    for I := Length(Str) downto 1 do
      if Str[I] = DirectorySeparator then
        begin
          Result := I;
          exit;
        end;
    Result := 0;
  end;
var
  Temp : String;
  I    : Integer;
  S    : Integer;
begin
  Temp := Path;
  Result := '';
  if Loc >= 0 then
    begin
      for I := 0 to Loc do
        begin
          S := Pos( DirectorySeparator, Temp );
          if S = 1 then
            Temp := Copy(Temp,2,MaxPathLen);
          S := Pos( DirectorySeparator, Temp );
          Result := Copy(Temp,1,S-1);
          Temp := Copy( Temp, S+1,MaxPathLen );
        end;
    end
  else
    begin
      for I := 1 to -Loc do
        begin
          S := RPos( Temp );
          if S = Length(Temp) then
            Temp := Copy(Temp,1,S-1);
          S := RPos(  Temp );
          Result := Copy(Temp,S+1,MaxPathLen);
          Temp := Copy(Temp,1,S-1);
        end;
    end;
end;  { function ExtractFileOrDirectory(Loc: Integer; Path: String): String; }

procedure RmFiles(Base, Rel, FileMask: String);
var
  SR : TSearchRec;
  Depth : Integer;
begin
  SR.Name := FileMask;
  Depth   := 0;
  RmFiles(Base,Rel,SR,Depth);
end;

procedure RmFiles(Base, Rel: String; const SR: TSearchRec; const Depth: Integer
  );
var
  Path : String;
begin
  Path := Base + Rel + DirectorySeparator + SR.Name;
//  Debug('Deleting:  [' + Path + ']');
  if SR.Attr = faDirectory then
    begin
     if (Depth > 0) and not ((SR.Name = '.') or (SR.Name = '..')) then
       RmDir( Path)
    end
  else
    SysUtils.DeleteFile( Path );
end;

procedure CopyFile(Src, Dst : String; RmSrc : Boolean );
var
  PathToDirectory : String;
begin
  PathToDirectory := ExtractFilePath(Dst);
  if not DirectoryExists(PathToDirectory) then
    ForceDirectories(PathToDirectory);
  if Src <> Dst then
    FileUtil.Copyfile( Src, Dst );
  if RmSrc then
    FileUtil.DeleteFileUTF8( Src );
end; { CopyFile(Src, Dst : String); }


type TCopyFilesData = class(TObject)
  FromPath : String;
  ToPath   : String;
  Force    : Boolean;
  RmSrc    : Boolean;
  constructor Create( aFromPath, aToPath : String; aForce, aRmSrc : Boolean );
end;

constructor TCopyFilesData.Create( aFromPath, aToPath : String; aForce, aRmSrc : Boolean );
begin
  FromPath := aFromPath;
  ToPath   := aToPath;
  Force    := aForce;
  RmSrc    := aRmSrc;
end;

procedure CP(       BasePath, RelPath : String;
              const SR                : TSearchRec;
              const Depth             : Integer;
                    UserData          : TObject );
var
  CPData : TCopyFilesData;
  Src, Dst : String;
begin
  // Ignore compiler warning about BasePath and Depth not used.
  CPData := UserData as TCopyFilesData;
  Src := CPData.FromPath + DirectorySeparator + RelPath;
  Dst := CPData.ToPath + DirectorySeparator + RelPath;
  if (SR.Attr and faDirectory) = faDirectory then
    begin
      ForceDirectories( Dst + DirectorySeparator + SR.Name );
    end
  else
    begin
      Src := Src + SR.Name;
      Dst := Dst + SR.Name;
      if FileExists( Dst ) and not CPData.Force then
        raise Exception.CreateFmt( 'Overwrite of file %s by %s not allowed by "Force".',
                                   [Dst,Src]);
      CopyFile( Src, Dst );
      if CPData.RmSrc then
        FileUtil.DeleteFileUTF8( Src );
    end;
end;

procedure CopyFiles( FromPath, ToPath, Wildcard : String; Force : Boolean = false );
var
  UserData : TCopyFilesData;
begin
  UserData := TCopyFilesData.Create( FromPath, ToPath, Force, false );

  WalkDirectoryTree( FromPath, '', Wildcard, faAnyFile, True, @CP, UserData);
end;  { CopyFiles( FromPath, ToPath, Wildcard : String; ... }

procedure MoveFiles(FromPath, ToPath, Wildcard: String);
var
  UserData : TCopyFilesData;
begin
  UserData := TCopyFilesData.Create( FromPath, ToPath, true, true );

  WalkDirectoryTree( FromPath, '', Wildcard, faAnyFile, True, @CP, UserData);
end;

function CompareFiles(Src, Dst: String; IgnoreWhiteSpace: Boolean): Boolean;
  function IgnoreWhite ( Str : String ) : String;
  var
    I : Integer;
  begin
    Result := '';
    for I := 1 to Length( Str ) do
      if not((Str[I] = ' ') or (Str[I] = #9)) then
        Result := Result + Str[I];
  end;
var
  FSrc, FDst : Text;
  SrcLine, DstLine : String;
begin
  Result := true;
  AssignFile( FSrc, Src );
  Reset(FSrc);
  AssignFile( FDst, Dst );
  Reset( FDst );
  while not Eof(FSrc) do
    begin
      Readln(FSrc, SrcLine);
      Readln(FDst, DstLine);
      if IgnoreWhiteSpace then
        begin
          if IgnoreWhite( SrcLine ) <> IgnoreWhite( DstLine ) then Result := False;
        end
      else
        begin
          if SrcLine <> DstLine then Result := False;
        end;
    end;
  if not Eof(FDst) then Result := False;
  CloseFile( FSrc );
  CloseFile( FDst );
end;

function FileSize(Path: String): Int64;
var
  SR : TSearchRec;
begin
  Result := 0;
  if FindFirst( Path, faAnyFile, SR ) = 0  then
    Result := SR.Size;
  Sysutils.FindClose(SR);
end;

procedure ReadBool(var F: TextFile; var Value: Boolean);
var
  S0, S1 : String;
begin
  Readln(F,S0);
  S1 := uppercase(trim(S0));
  if S1 = 'TRUE' then
    Value := True
  else if S1 = 'FALSE' then
    Value := False
  else
    raise Exception.CreateFMT('Invalid boolean "%s"',[S0]);
end;

procedure WriteBool(var F: TextFile; Value: Boolean);
begin
  if Value then
    WriteLn(F,'TRUE')
  else
    WriteLn(F,'FALSE');
end;

function CountFilesOrDirectories( Path : String ) : Integer;
var
  I : Integer;
begin
  Result := 0;
  if Path[1] <> DirectorySeparator then
    Inc(Result);
  if Path[Length(path)] = DirectorySeparator then
    Dec(Result);
  for I := 1 to Length(Path) do
    if Path[I] = DirectorySeparator then
      Inc(Result);
end;

function RemoveExt( FilePath : String ) : String;
var
  Ext : String;
  P   : Integer;
begin
  Ext := ExtractFileExt( FilePath );
  P := Pos(Ext,FilePath);
  if P = 0 then
    Result := FilePath
  else
    Result := Copy(FilePath,1,P-1);
end;

function UniqueFile(FilePath: String): String;
var
  Path, Name, Ext : String;
  Version : Integer;
begin
  Path := ExtractFilePath( FilePath );
  Name := RemoveExt(ExtractFileName( FilePath ));
  Ext  := ExtractFileExt( FilePath );
  Result := Path +  Name + Ext;
  Version := 1;
  while FileExists(Result) do
    begin
      Inc(Version);
      Result := Path + Name + '(' + IntToStr(Version) + ')' + Ext;
    end;
end;

procedure AppendStringListToFile(Path: String; StringList: TStringList);
var
  FileStringList : TStringList;
begin
  FileStringList := TStringList.Create;
  try
    if FileExists( Path ) then
      FileStringList.LoadFromFile( Path );
    FileStringList.AddStrings( StringList );
    FileStringList.SaveToFile( Path );
  finally
    FileStringList.Free;
  end;
end;

function  HexStringToByteArray( S : String ) : TByteArray;
var
  Size : Integer;
  I    : Integer;
  B    : Byte;
  Q    : String;
begin
  Size := Length(S);
  SetLength( Result, Size div 2 );
  for I := 0 to pred(Size div 2) do
    begin
      Q := Copy(S,I*2+1,2);
      B := Hex2Dec( Q );
      Result[I] := B;
    end;
end;

//procedure HexStringToByteArray( var A : array of Byte; S : String );
//var
//  Size : Integer;
//  I    : Integer;
//  B    : Byte;
//  Q    : String;
//begin
//  Size := Length(S);
////  SetLength(A,Size div 2);
//  for I := 0 to pred(Size div 2) do
//    begin
//      Q := Copy(S,I*2+1,2);
//      B := Hex2Dec( Q );
//      A[I] := B;
//    end;
//end;

function  ByteArrayToHexString( A : array of Byte) : String;
var
 Size : Integer;
 I    : Integer;
begin
  Size := Length(A);
  Result := '';
  for I := 0 to pred(Size) do
    Result := Result + IntToHex(A[I],2);
end;

function  TStreamToHexString( Stream : TStream ) : String;
var
  Size : Integer;
  BA   : TByteArray;
  I    : Integer;
  C    : Char;
begin
  Size := Stream.Size;
  SetLength( BA, Size );
  Stream.Seek(0,soFromBeginning);
  //Debug('MyVariable has ' + IntToStr( Length(BA) ) + ' elements');
  //Debug('Its range is ' + IntToStr( Low(BA) ) + ' to ' + IntToStr( High(BA)));
  Stream.ReadBuffer( Pointer(BA)^, Size );
  //Debug('MyVariable has ' + IntToStr( Length(BA) ) + ' elements');
  //Debug('Its range is ' + IntToStr( Low(BA) ) + ' to ' + IntToStr( High(BA)));
  for I := 0 to pred(Size) do
    C := Char(BA[I]);
  Result := ByteArrayToHexString( BA );
end;

procedure HexStringToTStream( Stream : TStream; S : String );
var
  BA : TByteArray;
  Size : Integer;
begin
  BA := HexStringToByteArray( S );
  Size := Length( BA );
  Stream.Seek(0,soFromBeginning);
  Stream.WriteBuffer( Pointer(BA)^, Size );
  Stream.Seek(0,soFromBeginning);
end;

{$ifdef WINDOWS}
procedure ExecuteAndWait(FilePath: String);
var
  Info : TShellExecuteInfo;
begin
  FillChar( Info, Sizeof( Info ) , 0);
  Info.cbSize := Sizeof( Info );
  Info.fMask := SEE_MASK_NOCLOSEPROCESS;
//  Info.Wnd := Handle;
  Info.lpVerb := ' open ';
//  Info.nShow: = SW_SHOWNORMAL;
  Info.lpFile := PChar(FilePath);
  Info.lpDirectory := PChar(ExtractFileDir(FilePath));
//  ShellExecuteEx(@Info as LPSHELLEXECUTEINFOA);
  WaitforSingleObject (Info.hProcess, INFINITE);
end;
{$endif}

{
procedure TForm1.btn1Click (Sender: TObject);
var
p2: TShellExecuteInfo;
begin
if OpenDialog1.execute then
begin
FillChar (p2, SizeOf (p2), 0);
With p2
do
Begin
cbSize: = SizeOf (p2);
fMask: = SEE_MASK_NOCLOSEPROCESS;
Wnd: = Handle;
lpVerb: = ' open ';
nShow: = SW_SHOWNORMAL;
lpFile: = PChar (OpenDialog1.FileName);
lpDirectory: = PChar (ExtractFileDir (OpenDialog1.FileName));
end;
ShellExecuteEx (@p2);
WaitforSingleObject (p2.hProcess, INFINITE);
ShowMessage (' It is ready! ');
end;
end;
}

end.

