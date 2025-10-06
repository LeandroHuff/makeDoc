program _makeDoc;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    {$IFDEF HASAMIGA}
    athreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, makeDoc
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:= True;
    Application. Title:='makeDoc';
    Application. Scaled:= True;
    {$PUSH}{$WARN 5044 OFF}
    Application. MainFormOnTaskbar:= True;
    {$POP}
    Application. Initialize;
    Application. CreateForm( TFormMain, FormMain);
    Application. Run;
end.

