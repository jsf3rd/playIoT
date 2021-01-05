// *******************************************************
//
// Real Singleton Templete
//
// Copyright(c) 2020.
//
// jsf3rd@nate.com
//
// *******************************************************

unit JdcSingleton;

interface

uses System.Classes, System.SysUtils;

type
  // 생성자 외부 접근 차단
  // Constructor Block external access
  THideConstructor = class abstract
  strict protected
    constructor Create; virtual; abstract;
  end;

  // 생성자 Overload를 통해서 Create; 함수 접근을 TObject에서 THideConstructor로 전환
  // Create함수를 procedure로 선언해서 class 호출 막음 - TMySingle.Create('string') 호출 불가

  // Switching the access to the Create function THideConstructor in TObject through the constructor Overloading
  // Declaring Create Method as a procedure to prevent class call-TMySingle.Create('string') call impossible
  TOverloadConstructor = class(THideConstructor)
  public
    procedure Create(s: string); reintroduce; overload; deprecated 'null method';
  end;

  TMySingleton = class sealed(TOverloadConstructor)
  private
    class var MyObj: TMySingleton;
  strict protected
    // TOberloadConstructor.Create(s: string); 숨김
    // THideConstructor.Create 구체화

    // Hiding TOberloadConstructor.Create(s: string);
    // Implement THideConstructor.Create
    constructor Create; override;
  public
    class function Obj: TMySingleton;
    function Echo(const value: string): String;
  end;

implementation

{ TMySingleton }

constructor TMySingleton.Create;
begin
  // TODO
end;

function TMySingleton.Echo(const value: string): String;
begin
  result := value;
end;

class function TMySingleton.Obj: TMySingleton;
begin
  if MyObj = nil then
    MyObj := Self.Create;
  result := MyObj;
end;

{ TOverloadContructor }

procedure TOverloadConstructor.Create(s: string);
begin
  // null method
end;

initialization

TMySingleton.MyObj := nil;

finalization

if Assigned(TMySingleton.MyObj) then
  FreeAndNil(TMySingleton.MyObj);

end.
