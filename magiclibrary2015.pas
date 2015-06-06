{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MagicLibrary2015;

interface

uses
  MagicFrameForm, VoiceForm, CommonApp, CommonMath, magicmainformbase1, 
  ObjectFactory1, CommonIO, AboutBase, CommonIni, MagicFormFrame1, StringSubs, 
  CommonCS, Persists2, textio2, CommonStacks, CommonDebug, CommonMesssages, 
  CommonMisc, CommonLog, VoiceUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MagicLibrary2015', @Register);
end.
