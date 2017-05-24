unit FMX.ZMaterialActionButton;

{
  author: Ravil (ZuBy) Zaripov
  email: rzaripov1990@gmail.com
  http://github.com/rzaripov1990

  2017
}

interface

uses
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.Effects,
  FMX.ZMaterialTypes, FMX.ZNativeDrawFigure;

type

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZMaterialActionButton = class(TZNativeDrawFigureTemplateWithEvents)
  private
    { Private declarations }
    FColor: TAlphaColor;
    FShadow: TShadowEffect;

    procedure SetColor(const Value: TAlphaColor);
    function GetShadow: Boolean;
    procedure SetShadow(const Value: Boolean);
    function GetShadowDistance: Single;
    procedure SetShadowDistance(const Value: Single);
    function GetShadowSoftness: Single;
    procedure SetShadowSoftness(const Value: Single);
    function GetShadowOpacity: Single;
    procedure SetShadowOpacity(const Value: Single);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Color: TAlphaColor read FColor write SetColor;
    property Shadow: Boolean read GetShadow write SetShadow default true;
    property ShadowDistance: Single read GetShadowDistance write SetShadowDistance;
    property ShadowSoftness: Single read GetShadowSoftness write SetShadowSoftness;
    property ShadowOpacity: Single read GetShadowOpacity write SetShadowOpacity;
  end;

procedure Register;

implementation

uses
  FMX.Graphics, FMX.Platform;

procedure Register;
begin
  RegisterComponents('ZMaterial', [TZMaterialActionButton]);
end;

{ TZMaterialActionButton }

constructor TZMaterialActionButton.Create(AOwner: TComponent);
var
  IScreen: IFMXScreenService;
begin
  inherited Create(AOwner);

  FShadow := TShadowEffect.Create(Self);
  FShadow.Parent := Self;
  FShadow.Stored := false;
  FShadow.Direction := 90;
  FShadow.Softness := 0.9;
  FShadow.Distance := 2;
  FShadow.Opacity := 0.4;

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IScreen) then
    FShadow.Distance := IScreen.GetScreenScale;

  Kind := TZNativeDrawFigureKind.Circle;
  FColor := $FFE91E63;
  Fill.Color := FColor;
  Stroke.Kind := TBrushKind.None;

  SetSize(56, 56);
end;

destructor TZMaterialActionButton.Destroy;
begin
  FShadow.Parent := nil;
  FShadow.Free;

  inherited;
end;

function TZMaterialActionButton.GetShadow: Boolean;
begin
  Result := FShadow.Enabled;
end;

function TZMaterialActionButton.GetShadowDistance: Single;
begin
  Result := FShadow.Distance;
end;

function TZMaterialActionButton.GetShadowOpacity: Single;
begin
  Result := FShadow.Opacity;
end;

function TZMaterialActionButton.GetShadowSoftness: Single;
begin
  Result := FShadow.Softness;
end;

procedure TZMaterialActionButton.SetColor(const Value: TAlphaColor);
begin
  if FColor = Value then
    Exit;

  FColor := Value;
  Fill.Color := FColor;
end;

procedure TZMaterialActionButton.SetShadow(const Value: Boolean);
begin
  FShadow.Enabled := Value;
end;

procedure TZMaterialActionButton.SetShadowDistance(const Value: Single);
begin
  FShadow.Distance := Value;
end;

procedure TZMaterialActionButton.SetShadowOpacity(const Value: Single);
begin
  FShadow.Opacity := Value;
end;

procedure TZMaterialActionButton.SetShadowSoftness(const Value: Single);
begin
  FShadow.Softness := Value;
end;

end.
