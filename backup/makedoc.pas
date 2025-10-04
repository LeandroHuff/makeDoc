unit makeDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn;

type

  { TformMain }

  TformMain = class(TForm)
    btnClose: TButton;
	btnRun: TButton;
	editSource: TFileNameEdit;
	editDestine: TFileNameEdit;
    procedure btnCloseClick(Sender: TObject);
  private

  public

  end;

var
  formMain: TformMain;

implementation

{$R *.lfm}

{ TformMain }

procedure TformMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

