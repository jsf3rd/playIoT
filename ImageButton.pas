//
// ImageButton.pas
//
// Author: GilGil(gilgil1973@hanmail.net)
// Last Modified : 2007.08.29
//
unit ImageButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  // ----------------------------------------------------------------------------
  // TDrawType
  // ----------------------------------------------------------------------------
  TDrawType = (dtDisable, dtDown, dtOver, dtUp);

  // ----------------------------------------------------------------------------
  // TImageButton
  // ----------------------------------------------------------------------------
  TImageButton = class(TImage)
  private
    { Private declarations }
    // 그려 진거 다시 그려지 지 않도록 내부적으로 관리되는 변수
    FDrawType: TDrawType;
    // Enabled가 false일 때는 FPictureDisable만을 보여 준다.
    FEnabled: Boolean;
    // 버튼이 비활성화될 때 보여지는 그림
    FPictureDisable: TPicture;
    // 버튼이 눌렸을 때 보여지는 그림
    FPictureDown: TPicture;
    // 마우스가 위로 이동할 때 보여지는 그림
    FPictureOver: TPicture;
    // 마우스가 위에 있지 않을 때 보여지는 그림
    FPictureUp: TPicture;
    // 마우스가 누른 다음 딱! 놀 때
    procedure WmLButtonUp(var Msg: TMessage); message WM_LBUTTONUP;
    // 마우스가 눌릴 때
    procedure WmLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    // 마우스가 들어 올 때
    procedure CmMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    // 마우스가 나갈 때
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    //
    // property용 메소드
    //
    procedure SetPictureDisable(Value: TPicture);
    procedure SetPictureDown(Value: TPicture);
    procedure SetPictureOver(Value: TPicture);
    procedure SetPictureUp(Value: TPicture);
  protected
    { Protected declarations }
    procedure SetEnabled(Value: Boolean); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // CM_MOUSE_LEAVE 이벤트가 발생하지 않을 수도 있다.
    // 이런 경우 프로그램에서 직접 MouseLeave를 호출하도록 한다.
    procedure MouseLeave;
    // Image에는 Click Method가 없다. 그래서 넣어 줬다.
    procedure Click; override;
  published
    { Published declarations }
    property Enabled: Boolean read FEnabled write SetEnabled;
    property PictureDisable: TPicture read FPictureDisable write SetPictureDisable;
    property PictureDown: TPicture read FPictureDown write SetPictureDown;
    property PictureOver: TPicture read FPictureOver write SetPictureOver;
    property PictureUp: TPicture read FPictureUp write SetPictureUp;
  end;

procedure Register;
procedure PictureToPictureUp(ImageButton: TImageButton);
procedure PictureToPictureUpAll(Parent: TComponent); overload;
procedure PictureToPictureUpAll(Parent: TWinControl); overload;

implementation

procedure Register;
begin
  RegisterComponents('GilGil', [TImageButton]);
end;

procedure PictureToPictureUp(ImageButton: TImageButton);
begin
  ImageButton.PictureUp := ImageButton.Picture;
end;

procedure PictureToPictureUpAll(Parent: TComponent);
var
  i: Integer;
  ImageButton: TImageButton;
begin
  for i := 0 to Parent.ComponentCount - 1 do
  begin
    if Parent.Components[i] is TImageButton then
    begin
      ImageButton := TImageButton(Parent.Components[i] as TImageButton);
      PictureToPictureUp(ImageButton);
    end;
  end;
end;

procedure PictureToPictureUpAll(Parent: TWinControl); overload;
var
  i: Integer;
  ImageButton: TImageButton;
begin
  for i := 0 to Parent.ControlCount - 1 do
  begin
    if Parent.Controls[i] is TImageButton then
    begin
      ImageButton := TImageButton(Parent.Controls[i] as TImageButton);
      PictureToPictureUp(ImageButton);
    end;
  end;
end;

// ----------------------------------------------------------------------------
// TImageButton
// ----------------------------------------------------------------------------
procedure TImageButton.WmLButtonUp(var Msg: TMessage);
begin
  inherited;
  if not FEnabled then
    exit;
  if FDrawType = dtDisable then
    exit;
  FDrawType := dtDisable;
  if assigned(FPictureUp) then
    Picture := FPictureUp;
end;

procedure TImageButton.WmLButtonDown(var Msg: TMessage);
begin
  inherited;
  if not FEnabled then
    exit;
  if FDrawType = dtDown then
    exit;
  FDrawType := dtDown;
  if assigned(FPictureDown) then
    Picture := FPictureDown;
end;

procedure TImageButton.CmMouseEnter(var Msg: TMessage);
begin
  inherited;
  if not FEnabled then
    exit;
  if FDrawType = dtOver then
    exit;
  FDrawType := dtOver;
  if assigned(FPictureOver) then
    Picture := FPictureOver;
end;

procedure TImageButton.CmMouseLeave(var Msg: TMessage);
begin
  inherited;
  if not FEnabled then
    exit;
  if FDrawType = dtUp then
    exit;
  FDrawType := dtUp;
  if assigned(FPictureUp) then
    Picture := FPictureUp;
end;

procedure TImageButton.SetPictureDown(Value: TPicture);
begin
  FPictureDown.Assign(Value);
end;

procedure TImageButton.SetPictureOver(Value: TPicture);
begin
  FPictureOver.Assign(Value);
end;

procedure TImageButton.SetPictureDisable(Value: TPicture);
begin
  FPictureDisable.Assign(Value);
  if not FEnabled then
    Picture := Value;
end;

procedure TImageButton.SetPictureUp(Value: TPicture);
begin
  // Up 이미지를 처음에 Picture에 대입시켜서 처음화면에 보여 질 수 있도록 한다.
  FPictureUp.Assign(Value);
  if FEnabled then
    Picture := Value;
end;

procedure TImageButton.SetEnabled(Value: Boolean);
begin
  inherited;
  FEnabled := Value;
  if FEnabled and assigned(FPictureUp) then
  begin
    FDrawType := dtUp;
    Picture := FPictureUp;
  end;
  if not FEnabled and assigned(FPictureDisable) then
  begin
    FDrawType := dtDisable;
    Picture := FPictureDisable;
  end;
end;

constructor TImageButton.Create(AOwner: TComponent);
begin
  inherited;
  FDrawType := dtUp;
  FEnabled := true;
  FPictureDisable := TPicture.Create;
  FPictureDown := TPicture.Create;
  FPictureOver := TPicture.Create;
  FPictureUp := TPicture.Create;
end;

destructor TImageButton.Destroy;
begin
  FPictureDisable.Free;
  FPictureDown.Free;
  FPictureOver.Free;
  FPictureUp.Free;
  inherited;
end;

procedure TImageButton.MouseLeave;
var
  Msg: TMessage;
begin
  CmMouseLeave(Msg);
end;

procedure TImageButton.Click;
begin
  inherited Click;
end;

end.
