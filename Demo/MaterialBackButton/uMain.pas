unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Layouts, FMX.Effects, FMX.Objects,
  FMX.ZMaterialBackButton;

type
  TForm2 = class(TForm)
    Rectangle1: TRectangle;
    ShadowEffect1: TShadowEffect;
    zmbKind: TZMaterialBackButton;
    zmbBack: TZMaterialBackButton;
    zmbClose: TZMaterialBackButton;
    sbMenu: TSpeedButton;
    sbBack: TSpeedButton;
    sbClose: TSpeedButton;
    procedure zmbBackClick(Sender: TObject);
    procedure zmbCloseClick(Sender: TObject);
    procedure zmbKindClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.zmbBackClick(Sender: TObject);
begin
  if zmbBack.Tag = 0 then
    zmbBack.Animate(TZMaterialBackButtonAnimationType.BackToMenu)
  else
    zmbBack.Animate(TZMaterialBackButtonAnimationType.MenuToBack);
  zmbBack.Tag := zmbBack.Tag xor 1;
end;

procedure TForm2.zmbCloseClick(Sender: TObject);
begin
  if zmbClose.Tag = 0 then
    zmbClose.Animate(TZMaterialBackButtonAnimationType.CloseToMenu)
  else
    zmbClose.Animate(TZMaterialBackButtonAnimationType.MenuToClose);
  zmbClose.Tag := zmbClose.Tag xor 1;
end;

procedure TForm2.zmbKindClick(Sender: TObject);
begin
  case zmbKind.Kind of
    TZMaterialBackButtonKind.Menu:
      zmbKind.Kind := TZMaterialBackButtonKind.Back;
    TZMaterialBackButtonKind.Back:
      zmbKind.Kind := TZMaterialBackButtonKind.Close;
    TZMaterialBackButtonKind.Close:
      zmbKind.Kind := TZMaterialBackButtonKind.Menu;
  end;
end;

end.
