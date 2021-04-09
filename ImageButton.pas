//
// ImageButton.pas
//
// Author: GilGil(gilgil1973@hanmail.net)
// Last Modified : 2007.08.29
//
unit ImageButton;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
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
    // �׷� ���� �ٽ� �׷��� �� �ʵ��� ���������� �����Ǵ� ����
    FDrawType: TDrawType;
    // Enabled�� false�� ���� FPictureDisable���� ���� �ش�.
    FEnabled: Boolean;
    // ��ư�� ��Ȱ��ȭ�� �� �������� �׸�
    FPictureDisable: TPicture;
    // ��ư�� ������ �� �������� �׸�
    FPictureDown: TPicture;
    // ���콺�� ���� �̵��� �� �������� �׸�
    FPictureOver: TPicture;
    // ���콺�� ���� ���� ���� �� �������� �׸�
    FPictureUp: TPicture;
    // ���콺�� ���� ���� ��! �� ��
    procedure WmLButtonUp(var Msg: TMessage); message WM_LBUTTONUP;
    // ���콺�� ���� ��
    procedure WmLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    // ���콺�� ��� �� ��
    procedure CmMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    // ���콺�� ���� ��
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    //
    // property�� �޼ҵ�
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
    // CM_MOUSE_LEAVE �̺�Ʈ�� �߻����� ���� ���� �ִ�.
    // �̷� ��� ���α׷����� ���� MouseLeave�� ȣ���ϵ��� �Ѵ�.
    procedure MouseLeave;
    // Image���� Click Method�� ����. �׷��� �־� ���.
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
  // Up �̹����� ó���� Picture�� ���Խ��Ѽ� ó��ȭ�鿡 ���� �� �� �ֵ��� �Ѵ�.
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
