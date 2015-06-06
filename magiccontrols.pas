{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MagicControls;

interface

uses
  FontPicker, MealyFiniteStateMachine, FormPanel, MagicTimeEdit, 
  MagicStringGrid, MagicClock, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FontPicker', @FontPicker.Register);
  RegisterUnit('MealyFiniteStateMachine', @MealyFiniteStateMachine.Register);
  RegisterUnit('FormPanel', @FormPanel.Register);
  RegisterUnit('MagicTimeEdit', @MagicTimeEdit.Register);
  RegisterUnit('MagicStringGrid', @MagicStringGrid.Register);
  RegisterUnit('MagicClock', @MagicClock.Register);
end;

initialization
  RegisterPackage('MagicControls', @Register);
end.
