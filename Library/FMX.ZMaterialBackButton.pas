unit FMX.ZMaterialBackButton;

{
  author: Ravil (ZuBy) Zaripov

  email: rzaripov1990@gmail.com

  http://github.com/rzaripov1990

  2017
}

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts,
  FMX.Objects, System.UITypes;

type
  TZMaterialBackButtonKind = (Menu, Back, Close);
  TZMaterialBackButtonAnimationType = (None, MenuToBack, BackToMenu, MenuToClose, CloseToMenu);

  TZMaterialBackButton = class(TLayout)
  private
    FLayout: TLayout;
    FTopLine: TRectangle;
    FMiddleLine: TRectangle;
    FBottomLine: TRectangle;

    FKind: TZMaterialBackButtonKind;
    FAnimationType: TZMaterialBackButtonAnimationType;
    FColor: TAlphaColor;
    FTopLinePosY: Single;
    FMiddleLinePosY: Single;
    FBottomLinePosY: Single;

    { Private declarations }
    procedure RestoreToMenuKind;
    procedure StopAllAnimations;
    procedure SetKind(const Value: TZMaterialBackButtonKind);

    procedure DoAnimateMenuToBack;
    procedure DoAnimateBackToMenu;
    procedure DoAnimateMenuToClose;
    procedure DoAnimateCloseToMenu;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }

    procedure Animate(const aAnimationType: TZMaterialBackButtonAnimationType);
  published
    { Published declarations }
    property Kind: TZMaterialBackButtonKind read FKind write SetKind default TZMaterialBackButtonKind.Menu;
  end;

procedure Register;

implementation

uses
  FMX.Graphics, System.Types, FMX.Ani;

procedure Register;
begin
  RegisterComponents('ZMaterial', [TZMaterialBackButton]);
end;

{ TZMaterialBackButton }

constructor TZMaterialBackButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAnimationType := TZMaterialBackButtonAnimationType.None;
  FKind := TZMaterialBackButtonKind.Menu;
  FColor := TAlphaColorRec.White;

  FLayout := TLayout.Create(Self);
  FLayout.Parent := Self;
  FLayout.Stored := false;

  FTopLine := TRectangle.Create(FLayout);
  FTopLine.Parent := FLayout;
  FTopLine.Stored := false;

  FMiddleLine := TRectangle.Create(FLayout);
  FMiddleLine.Parent := FLayout;
  FMiddleLine.Stored := false;

  FBottomLine := TRectangle.Create(FLayout);
  FBottomLine.Parent := FLayout;
  FBottomLine.Stored := false;

  RestoreToMenuKind;
end;

destructor TZMaterialBackButton.Destroy;
begin
{$IFDEF AUTOREFCOUNT}
  FBottomLine.DisposeOf;
  FBottomLine := nil;
  FMiddleLine.DisposeOf;
  FMiddleLine := nil;
  FTopLine.DisposeOf;
  FTopLine := nil;
  FLayout.DisposeOf;
  FLayout := nil;
{$ELSE}
  FreeAndNil(FBottomLine);
  FreeAndNil(FMiddleLine);
  FreeAndNil(FTopLine);
  FreeAndNil(FLayout);
{$ENDIF}
  inherited Destroy;
end;

procedure TZMaterialBackButton.DoAnimateMenuToBack;
begin
  FLayout.RotationAngle := 0;
  TAnimator.AnimateFloat(FLayout, 'RotationAngle', 180, 0.25);
  TAnimator.AnimateFloat(FTopLine, 'RotationAngle', 45, 0.25);
  TAnimator.AnimateFloat(FBottomLine, 'RotationAngle', -45, 0.25);
  TAnimator.AnimateFloat(FTopLine, 'Width', 12, 0.25);
  TAnimator.AnimateFloat(FBottomLine, 'Width', 12, 0.25);
  TAnimator.AnimateFloat(FMiddleLine, 'Width', 16 - FMiddleLine.Height / 4, 0.25);
  TAnimator.AnimateFloat(FMiddleLine, 'Position.X', 2, 0.25);
  TAnimator.AnimateFloat(FTopLine, 'Position.Y', FMiddleLine.Position.Y + FMiddleLine.Height / 4 - 12 * 0.5 *
    cos((45 * pi) / 180), 0.25);
  TAnimator.AnimateFloat(FBottomLine, 'Position.Y', FMiddleLine.Position.Y - FMiddleLine.Height / 4 + 12 * 0.5 *
    cos((45 * pi) / 180), 0.25);
  TAnimator.AnimateFloat(FTopLine, 'Position.X', 9, 0.25);
  TAnimator.AnimateFloatWait(FBottomLine, 'Position.X', 9, 0.25);
end;

procedure TZMaterialBackButton.DoAnimateBackToMenu;
begin
  TAnimator.AnimateFloatDelay(FLayout, 'RotationAngle', 360, 0.25);
  TAnimator.AnimateFloat(FTopLine, 'RotationAngle', 0, 0.25);
  TAnimator.AnimateFloat(FBottomLine, 'RotationAngle', 0, 0.25);
  TAnimator.AnimateFloat(FTopLine, 'Width', 18, 0.25);
  TAnimator.AnimateFloat(FBottomLine, 'Width', 18, 0.25);
  TAnimator.AnimateFloat(FMiddleLine, 'Width', 18, 0.25);
  TAnimator.AnimateFloat(FMiddleLine, 'Position.X', 0, 0.25);
  TAnimator.AnimateFloat(FTopLine, 'Position.Y', 0, 0.25);
  TAnimator.AnimateFloat(FBottomLine, 'Position.Y', 12, 0.25);
  TAnimator.AnimateFloat(FTopLine, 'Position.X', 0, 0.25);
  TAnimator.AnimateFloatWait(FBottomLine, 'Position.X', 0, 0.25);
end;

procedure TZMaterialBackButton.DoAnimateMenuToClose;
begin
  FLayout.RotationAngle := 0;
  TAnimator.AnimateFloat(FLayout, 'RotationAngle', 90, 0.125);
  TAnimator.AnimateFloat(FTopLine, 'Position.Y', FMiddleLinePosY, 0.125);
  TAnimator.AnimateFloatWait(FBottomLine, 'Position.Y', FMiddleLinePosY, 0.125);
  TAnimator.AnimateFloat(FLayout, 'RotationAngle', 180, 0.125);
  FMiddleLine.Visible := false;
  TAnimator.AnimateFloat(FTopLine, 'RotationAngle', -45, 0.125);
  TAnimator.AnimateFloatWait(FBottomLine, 'RotationAngle', 45, 0.125);
end;

procedure TZMaterialBackButton.DoAnimateCloseToMenu;
begin
  TAnimator.AnimateFloat(FLayout, 'RotationAngle', 270, 0.125);
  TAnimator.AnimateFloat(FTopLine, 'RotationAngle', 0, 0.125);
  TAnimator.AnimateFloatWait(FBottomLine, 'RotationAngle', 0, 0.125);
  FMiddleLine.RotationAngle := 0;
  FMiddleLine.SetBounds(0, 6, FLayout.Width, 2);
  FMiddleLine.Visible := True;
  TAnimator.AnimateFloat(FLayout, 'RotationAngle', 360, 0.125);
  TAnimator.AnimateFloat(FTopLine, 'Position.Y', FTopLinePosY, 0.125);
  TAnimator.AnimateFloatWait(FBottomLine, 'Position.Y', FBottomLinePosY, 0.125);
end;

procedure TZMaterialBackButton.Animate(const aAnimationType: TZMaterialBackButtonAnimationType);
begin
  if (aAnimationType = TZMaterialBackButtonAnimationType.None) or (FAnimationType = aAnimationType) then
    exit;

  FAnimationType := aAnimationType;
  case FAnimationType of
    TZMaterialBackButtonAnimationType.MenuToBack:
      DoAnimateMenuToBack;
    TZMaterialBackButtonAnimationType.BackToMenu:
      DoAnimateBackToMenu;
    TZMaterialBackButtonAnimationType.MenuToClose:
      DoAnimateMenuToClose;
    TZMaterialBackButtonAnimationType.CloseToMenu:
      DoAnimateCloseToMenu;
  end;
end;

procedure TZMaterialBackButton.RestoreToMenuKind;
const
  DefaultBounds: TRectF = (Left: 19; Top: 21; Right: 18; Bottom: 13);
begin
  StopAllAnimations;

  FLayout.RotationAngle := 0;
  FLayout.Align := TAlignLayout.Center;
  FLayout.HitTest := false;
  FLayout.SetBounds(0, 0, DefaultBounds.Right, DefaultBounds.Bottom);

  FTopLine.Align := TAlignLayout.None;
  FTopLine.Stroke.Kind := TBrushKind.None;
  FTopLine.HitTest := false;
  FTopLine.RotationAngle := 0;
  FTopLine.SetBounds(0, 0, FLayout.Width, 2);
  FTopLine.Fill.Color := FColor;

  FMiddleLine.Align := TAlignLayout.None;
  FMiddleLine.Stroke.Kind := TBrushKind.None;
  FMiddleLine.HitTest := false;
  FMiddleLine.RotationAngle := 0;
  FMiddleLine.SetBounds(0, 6, FLayout.Width, 2);
  FMiddleLine.Fill.Color := FColor;
  FMiddleLine.Visible := True;

  FBottomLine.Align := TAlignLayout.None;
  FBottomLine.Stroke.Kind := TBrushKind.None;
  FBottomLine.HitTest := false;
  FBottomLine.RotationAngle := 0;
  FBottomLine.SetBounds(0, 12, FLayout.Width, 2);
  FBottomLine.Fill.Color := FColor;

  FTopLinePosY := FTopLine.Position.Y;
  FMiddleLinePosY := FMiddleLine.Position.Y;
  FBottomLinePosY := FBottomLine.Position.Y;
end;

procedure TZMaterialBackButton.SetKind(const Value: TZMaterialBackButtonKind);
begin
  if FKind = Value then
    exit;

  RestoreToMenuKind;
  FKind := Value;

  case FKind of
    TZMaterialBackButtonKind.Menu:
      { RestoreToMenuKind };
    TZMaterialBackButtonKind.Back:
      begin
        FLayout.RotationAngle := 180;

        FTopLine.RotationAngle := 45;
        FTopLine.Width := 12;
        FTopLine.Position.X := 9;
        FTopLine.Position.Y := FMiddleLine.Position.Y + FMiddleLine.Height / 4 - 12 * 0.5 * cos((45 * pi) / 180);

        FMiddleLine.Width := 16 - FMiddleLine.Height / 4;
        FMiddleLine.Position.X := 2;

        FBottomLine.RotationAngle := -45;
        FBottomLine.Width := 12;
        FBottomLine.Position.X := 9;
        FBottomLine.Position.Y := FMiddleLine.Position.Y - FMiddleLine.Height / 4 + 12 * 0.5 * cos((45 * pi) / 180);
      end;
    TZMaterialBackButtonKind.Close:
      begin
        FLayout.RotationAngle := 180;

        FTopLine.RotationAngle := -45;
        FTopLine.Position.Y := FMiddleLinePosY;

        FMiddleLine.Visible := false;

        FBottomLine.RotationAngle := 45;
        FBottomLine.Position.Y := FMiddleLinePosY;
      end;
  end;
end;

procedure TZMaterialBackButton.StopAllAnimations;
begin
  FLayout.StopPropertyAnimation('RotationAngle');

  FTopLine.StopPropertyAnimation('RotationAngle');
  FTopLine.StopPropertyAnimation('Width');
  FTopLine.StopPropertyAnimation('Position.Y');
  FTopLine.StopPropertyAnimation('Position.X');

  FMiddleLine.StopPropertyAnimation('RotationAngle');
  FMiddleLine.StopPropertyAnimation('Width');
  FMiddleLine.StopPropertyAnimation('Position.Y');
  FMiddleLine.StopPropertyAnimation('Position.X');

  FBottomLine.StopPropertyAnimation('RotationAngle');
  FBottomLine.StopPropertyAnimation('Width');
  FBottomLine.StopPropertyAnimation('Position.Y');
  FBottomLine.StopPropertyAnimation('Position.X');
end;

end.
