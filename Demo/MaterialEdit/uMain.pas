unit uMain;

{
  author: ZuBy

  https://github.com/rzaripov1990
  http://blog.rzaripov.kz

  2017
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ZMaterialEdit, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    zmeEmail: TZMaterialEdit;
    zmePassword: TZMaterialEdit;
    zmeLogin: TZMaterialEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  zmeEmail.Text := 'rzaripov1990@gmail.com';
  zmeLogin.Text := 'ZuBy';
  zmePassword.Text := '123456789!@';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  zmeEmail.Text := '';
  zmeLogin.Text := '';
  zmePassword.Text := '';
end;

end.
