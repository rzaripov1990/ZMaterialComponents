unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ZMaterialActionButton,
  FMX.Effects, FMX.ZNativeDrawFigure, FMX.Objects;

type
  TForm1 = class(TForm)
    ZNativeDrawFigureLine1: TZNativeDrawFigureLine;
    ZNativeDrawFigureRectangle1: TZNativeDrawFigureRectangle;
    ZNativeDrawFigureRoundRect1: TZNativeDrawFigureRoundRect;
    ZNativeDrawFigureEllipse1: TZNativeDrawFigureEllipse;
    ZNativeDrawFigureCircle1: TZNativeDrawFigureCircle;
    ZNativeDrawFigureArc1: TZNativeDrawFigureArc;
    ZMaterialActionButton1: TZMaterialActionButton;
    Text1: TText;
    Timer1: TTimer;
    Circle1: TCircle;
    Text2: TText;
    procedure ZMaterialActionButton1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ZMaterialBackButton1Click(Sender: TObject);
    procedure ZMaterialBackButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormResize(Sender: TObject);
begin
  ZMaterialActionButton1.Position.X := ClientWidth - 16 - ZMaterialActionButton1.Width;
  ZMaterialActionButton1.Position.Y := ClientHeight - 16 - ZMaterialActionButton1.Height;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if ZNativeDrawFigureArc1.EndAngle >= 350 then
    ZNativeDrawFigureArc1.EndAngle := 0;

  ZNativeDrawFigureArc1.EndAngle := ZNativeDrawFigureArc1.EndAngle + 10;
end;

procedure TForm1.ZMaterialActionButton1Click(Sender: TObject);
begin
  ShowMessage('Floating Action Button Clicked');
end;

procedure TForm1.ZMaterialBackButton1Click(Sender: TObject);
begin
  if ZMaterialBackButton1.Tag = 0 then
    ZMaterialBackButton1.Animate(TZMaterialBackButtonAnimationType.MenuToBack)
  else
    ZMaterialBackButton1.Animate(TZMaterialBackButtonAnimationType.BackToMenu);
  ZMaterialBackButton1.Tag := ZMaterialBackButton1.Tag xor 1;
end;

procedure TForm1.ZMaterialBackButton2Click(Sender: TObject);
begin
  if ZMaterialBackButton2.Tag = 0 then
    ZMaterialBackButton2.Animate(TZMaterialBackButtonAnimationType.CloseToMenu)
  else
    ZMaterialBackButton2.Animate(TZMaterialBackButtonAnimationType.MenuToClose);
  ZMaterialBackButton2.Tag := ZMaterialBackButton2.Tag xor 1;
end;

end.
