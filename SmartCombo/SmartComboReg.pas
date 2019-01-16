unit SmartComboReg;

interface

uses
  SmartCombo;

procedure Register;

implementation

uses
  Classes;

procedure Register;
begin
  RegisterComponents('SmartComboBox', [TSmartComboBox]);
end;

end.