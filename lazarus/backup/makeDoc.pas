//D> <a id="makeDoc"></a>
//D> # makeDoc
//D>
//D> |               | Description                                                               |
//D> | ------------: | ------------------------------------------------------------------------- |
//D> |               | Lazarus/Free Pascal project to export documentation lines with tag marker |
//D> | **File**      | makeDoc.pas                                                               |
//D> | **Author**    | [Leandro](mailto:leandrohuff@email.com)                                   |
//D> | **Date**      | _2025-10-06_                                                              |
//D> | **Version**   | _1.0.0_ / _2025-10-06_                                                    |
//D> | **Copyright** | CC01 1.0 Universal                                                        |
//D> | **Copyright** | **Copyright Icons** [Uicons by Flaticon](https://www.flaticon.com/uicons) |
//D>
//D> **Note**: Changes in this document will be discarded on next build,  
//D> any changes should be made on source code documentation instead.  
//D>
//D> <a id="Details"></a>
//D> ## Details
//D>
//D> Save formatted lines from source code into documentation file.  
//D> Read source code line-by-line and save prefixed lines by tag ??D to file.  
//D> C/C++/Free Pascal source codes lines start with tag //D and Bash lines start with tag //D.  
//D> Only those lines started by tags are exportedd to documentation files.  
//D> Mixed commented lines can co-exist at same source code, one for local  
//D> documentation purpose and another to be exported to apropriate documentation file.  
//D> All lines are documented using Markdown format, the exported document can be read  
//D> by an Markdown program reader.  
//D>
//D> <a id="Index"></a>
//D> ## Index
//D>
//D> | Index                             | Description                                         |
//D> | :-------------------------------- | :-------------------------------------------------- |
//D> | [Top](#makeDoc)                   |                                                     |
//D> | [Details](#Details)               |                                                     |
//D> | [Unit](#Unit)                     |                                                     |
//D> | [Type](#Type)                     |                                                     |
//D> | [Interface](#Interface)           |                                                     |
//D> | [Uses](#Uses)                     |                                                     |
//D> | [Constants](#Constants)           |                                                     |
//D> | [Variables](#Variables)           |                                                     |
//D> | [Implementation](#Implementation) |                                                     |
//D> | [btnAboutClick](#btnAboutClick)   |                                                     |
//D> | [btnCloseClick](#btnCloseClick)   |                                                     |
//D> | [btnRunClick](#btnRunClick)       |                                                     |
//D> | [OnClick](#OnClick)               |                                                     |
//D> | [OnCreate](#OnCreate)             |                                                     |
//D> | [Bottom](#updateEditDestine)      |                                                     |
//D>
//D> [⬅️](#makeDoc) | [⬆️](#Index)
//D>
//D> <a id="Unit"></a>
//D> ## Unit

unit makeDoc;

{$mode objfpc}{$H+}

//D>
//D> <a id="Interface"></a>
//D> ## Interface

interface

//D>
//D> <a id="Uses"></a>
//D> ## Uses
//D>
//D> | Name     | Description |
//D> | -------: | ----------- |
//D> | LCLType  | Key codes   |
//D> | Classes  | Abstract and concrete classes for object-oriented programming |
//D> | Forms    | Creating and managing user interface Forms |
//D> | Controls | Common controls, such as TButton, TEdit, TMemo, TComboBox, and TListBox |
//D> | Graphics | Native graphics classes like TCanvas, TFont, and TBitmap |
//D> | Dialogs  | Standard dialog components and functions like ShowMessage and dialogs for file opening/saving, color selection, and more |
//D> | StdCtrls | Standard components like TButton, TLabel, TEdit, TMemo, TCheckBox, and TRadioButton |
//D> | EditBtn  | Edit box with an associated button, is found in the LCLBase unit |
//D> | SysUtils | Part of the Free Pascal Run-Time Library (RTL) |
//D> | RegExpr  | Working with regular expressions is RegExpr |
//D>
//D> [⬅️](#Details) | [⬆️](#Index)

uses
    LCLType, Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
    ComCtrls, SysUtils, RegExpr, Dos;

//D>
//D> <a id="Type"></a>
//D> ## Type
//D>
//D> | Type            | Name                  | Description                     |
//D> | --------------: | --------------------: | ------------------------------- |
//D> | _TFormMain_     | **FormMain**          | Main form application           |
//D> | _TButton_       | **btnClose**          | Button close                    |
//D> | _TButton_       | **btnAbout**          | Show about box                  |
//D> | _TButton_       | **btnRun**            | Execute documentation procedure |
//D> | _TFileNameEdit_ | **editSource**        | Source file edit box            |
//D> | _TFileNameEdit_ | **editDestine**       | Destine file edit box           |
//D> | _TLabel_        | **lblStatus**         | Status processing               |
//D> | _TLabel_        | **lblDestine**        | Source file static label        |
//D> | _TLabel_        | **lblSource**         | Destine file static label       |
//D> | _procedure_     | **btnAboutClick**     | Treat About button click event  |
//D> | _procedure_     | **btnCloseClick**     | Treat Close button click event  |
//D> | _procedure_     | **btnRunClick**       | Treat Run button click event    |
//D> | _procedure_     | **OnClick**           | Treat edit destine click event  |
//D> | _procedure_     | **OnCreate**          | Treat on create form event      |
//D>
//D> [⬅️](#Uses) | [⬆️](#Index)

type

{ TFormMain }

    TFormMain = class(TForm)
        btnClose: TButton;
        btnAbout: TButton;
        btnRun: TButton;
        editSource: TFileNameEdit;
        editDestine: TFileNameEdit;
        lblStatus: TLabel;
        lblDestine: TLabel;
        lblSource: TLabel;
        procedure btnAboutClick(Sender: TObject);
        procedure btnCloseClick( Sender: TObject);
        procedure btnRunClick( Sender: TObject);
        procedure OnClick( Sender: TObject);
        procedure OnCreate( Sender: TObject);
    private
        currentDir : string;
        docDir : string;
    public

end;

//D>
//D> <a id="Constants"></a>
//D> ## Constants
//D>
//D> | Type | Name | Description |
//D> | :--: | :--: | :---------: |
//D> |  -   |  -   |      -      |
//D>
//D> <a id="Variables"></a>
//D> ## Variables
//D>
//D> | Type      | Name     | Description           |
//D> | --------: | -------: | --------------------- |
//D> | TFormMain | FormMain | Main form application |
//D>
//D> [⬅️](#Type) | [⬆️](#Index)

var
    FormMain: TFormMain;

//D>
//D> <a id="Implementation"></a>
//D> ## Implementation

implementation

{$R *.lfm}

{ TFormMain }

//D>
//D> <a id="btnAboutClick"></a>
//D> ### btnAboutClick
//D>
//D> | Return | Name | Parameters | Result |
//D> | ------ | ---- | ---------- | ------ |
//D> | _boolean_ | **btnAboutClick** | _TObject_ **Sender** | _None_ |
//D>
//D> Treat About button click event.  
//D>
//D> **Parameter**:  
//D> _TObject_ **Sender**: Objects and Events from main window.  
//D>
//D> **Result**:  
//D> _None_: Create and show a formatted about form window.  
//D>
//D> **Return**:  
//D> _boolean_  
//D>
//D> [⬅️](#Variables) | [⬆️](#Index)

procedure TFormMain.btnAboutClick(Sender: TObject);
begin
    ShowMessage('Export Documentation from taged lines.' + sLineBreak +
                'File: makeDoc' + sLineBreak +
                'Version: 1.0.0 / 2025-10-05' + sLineBreak +
                'Author: Leandro' + sLineBreak +
                'Copyright: CC01  1.0 Universal');
end;

//D>
//D> <a id="btnCloseClick"></a>
//D> ### btnCloseClick
//D>
//D> | Return | Name | Parameters | Result |
//D> | ------ | ---- | ---------- | ------ |
//D> | _boolean_ | **btnCloseClick** | _TObject_ **Sender** | _None_ |
//D>
//D> Treat Close button click event.  
//D>
//D> **Parameter**:  
//D> _TObject_ **Sender** : Objects and Events from main window.  
//D>
//D> **Result**:  
//D> _TFormMain_ : End and close main form window.  
//D>
//D> **Return**:  
//D> _None_
//D>
//D> [⬅️](#btnAboutClick) | [⬆️](#Index)

procedure TFormMain.btnCloseClick( Sender: TObject);
begin
    Close;
end;

//D>
//D> <a id="btnRunClick"></a>
//D> ### btnRunClick
//D>
//D> | Return | Name | Parameters | Result |
//D> | ------ | ---- | ---------- | ------ |
//D> | _None_ | **btnRunClick** | _TObject_ **Sender** | _None_ |
//D>
//D> Treat Run button click event.  
//D>
//D> **Parameter**:  
//D> _TObject_ **Sender**: Objects and Events from main window.  
//D>
//D> **Result**:  
//D> _TFormMain_: Run documentation procedure.  
//D>
//D> **Return**:  
//D> _None_  
//D>
//D> [⬅️](#btnCloseClick) | [⬆️](#Index)

procedure TFormMain.btnRunClick( Sender: TObject);
var

    inputFile, outputFile : TextFile;
    line : string;
    strList : TStringList;
    enableDoc : boolean;
    reAll, reDoc, reBegin, reEnd, reValidFilename : TRegExpr;

begin
    lblStatus.Caption := 'Running...';
    reValidFilename := TRegExpr.Create('([A-Za-z0-9_]+.?[A-Za-z0-9_]*)$');
    if FileExists (editSource.Text) then
    begin
        if not reValidFilename.Exec(editSource.Text) then
        begin
            ShowMessage('Source filename is not valid.');
            reValidFilename.Free;
            Exit;
        end;
        if not FileExists(editSource.Text) then
        begin
            ShowMessage('Source filename not found.');
            reValidFilename.Free;
            Exit;
        end;
        if DirectoryExists(editSource.Text) then
        begin
            ShowMessage('Source filename is a directory.' + sLineBreak +
            'Select a file and try again.');
            reValidFilename.Free;
            Exit;
        end;
        if not reValidFilename.Exec(editDestine.Text) then
        begin
            ShowMessage('Destine filename is not valid.');
            reValidFilename.Free;
            Exit;
        end;
        if DirectoryExists(editDestine.Text) then
        begin
            ShowMessage('Destine filename is a directory.' + sLineBreak +
            'Select a file and try again.');
            reValidFilename.Free;
            Exit;
        end;
        if FileExists(editDestine.Text) then
            if MessageDlg('Output filename already exist.' + sLineBreak +
                          'Do you LET OVERWRITE it?',
                          mtWarning,
                          [mbYes,
                          mbCancel],
                          0) = mrCancel then
            begin
                reValidFilename.Free;
                Exit;
            end;
        FormMain.Update;
        try
            AssignFile(inputFile, editSource.Text);
            Reset(inputFile);
            AssignFile(outputFile, editDestine.Text);
            Rewrite(outputFile);
            reAll    := TRegExpr.Create('^ *(#+|[/%-]{2,}[DBE])>? *');
            reDoc    := TRegExpr.Create('^ *(#+|[/%-]{2,}D)>? *');
            reBegin  := TRegExpr.Create('^ *(#+|[/%-]{2,}B)>? *');
            reEnd    := TRegExpr.Create('^ *(#+|[/%-]{2,}E)>? *');
            strList := TStringList.Create;
            enableDoc := true;
            try
                while not eof(inputFile) do
                begin
                    ReadLn(inputFile, line);
                    if reAll.Exec(line) then
                    begin
                        if enableDoc then
                        begin
                            if reEnd.Exec(line) then enableDoc := false
                            else if reDoc.Exec(line) then
                            begin
                                strList.Clear;
                                reDoc.Split(line, strList);
                                WriteLn(outputFile, strList.Strings[1]);
                            end
                        end
                        else if reBegin.Exec(line) then enableDoc := true;
                    end;
                end;
                Flush(outputFile);
            except
                on E: EInOutError do
                    ShowMessage( 'File Error: '+ E.ClassName + #10 + E.Message );
            end;
        finally
            if strList <> nil then strList.Free;
            if reAll <> nil then reAll.Free;
            if reDoc <> nil then reDoc.Free;
            if reBegin <> nil then reBegin.Free;
            if reEnd <> nil then reEnd.Free;
            if reValidFilename <> nil then reValidFilename.Free;
            CloseFile(inputFile);
            CloseFile(outputFile);
        end;
    end
    else
        Application.MessageBox ('Source file does not exist.', 'Error', mb_Ok or mb_IconHand);
    Sleep(1000);
    lblStatus.Caption := 'Finished, choose source and destine directory+filename.';
end;

//D>
//D> <a id="OnClick"></a>
//D> ### OnClick
//D>
//D> | Return  | Name        | Parameters           | Result |
//D> |  ------ | ----------- | -------------------- | ------ |
//D> | _None_  | **OnClick** | _TObject_ **Sender** | _None_ |
//D>
//D> Treat edit destine click event.
//D>
//D> **Parameter**:
//D> _TObject_ **Sender**: Objects and Events from main window.
//D>
//D> **Result**:
//D> _TFormMain_: Prepare destine edit text to show path + filename.
//D>
//D> [⬅️](btnRunClick) | [⬆️](#Index)

procedure TFormMain. OnClick( Sender: TObject);
var
    strFilename : TStringList;
begin
    try
        strFilename := TStringList.Create;
        strFilename.Clear;
        strFilename.Delimiter := '/';
        strFilename.StrictDelimiter := True;
        strFilename.DelimitedText := editSource.Text;
        if strFilename.Count > 0 then
        begin
            editDestine.FileName := strFilename.Strings[strFilename.Count - 1] + '.md';
            editDestine.Text := editDestine.InitialDir + '/' + editDestine.FileName;
        end;
    finally
        if strFilename <> nil then strFilename.Free;
    end;
end;

//D>
//D> <a id="OnCreate"></a>
//D> ### OnCreate
//D>
//D> | Return  | Name         | Parameters           | Result |
//D> |  ------ | ------------ | -------------------- | ------ |
//D> | _None_  | **OnCreate** | _TObject_ **Sender** | _None_ |
//D>
//D> Treat form create event.
//D>
//D> **Parameter**:
//D> _TObject_ **Sender**: Objects and Events from main window.
//D>
//D> **Result**:
//D> _TFormMain_: Prepare main form edit box to load and show current directory.
//D>
//D> [⬅️](OnClick) | [⬆️](#Index)

procedure TFormMain. OnCreate( Sender: TObject);
begin
    currentDir := GetCurrentDir;
    if DirectoryExists(currentDir + '/doc') then
        docDir := currentDir + '/doc'
    else
        docDir := currentDir;
    editSource.InitialDir := currentDir;
    editDestine.InitialDir := docDir;
end;

end.
