//H>

//D> <h1 id="top">makeDoc</h1>

//D> <table id="description">
//D><tr><td align=right></td><td></td><td>Lazarus/Free Pascal project to export documentation lines with tag marker.</td></tr>
//D><tr><td align=right><b>File</b></td><td>:</td><td>makeDoc.pas</td></tr>
//D><tr><td align=right><b>Date</b></td><td>:</td><td>2025-10-06</td></tr>
//D><tr><td align=right><b>Author</b></td><td>:</td><td><a href="mailto:leandrohuff@email.com">Leandro</a></td></tr>
//D><tr><td align=right><b>Version</b></td><td>:</td><td>1.0.0 / 2025-10-06</td></tr>
//D><tr><td align=right><b>Copyright App</b></td><td>:</td><td>CC01 1.0 Universal</td></tr>
//D><tr><td align=right><b>Copyright Icons</b></td><td>:</td><td>Uicons by <a href="https://www.flaticon.com/uicons">Flaticon</a></td></tr>
//D></table>

//D><br>
//D><b>Note</b>: Changes in this document will be discarded on next build, any changes should be made on source code documentation instead.<br>

//D><h2 id="details">Details</h2>

//D><p>
//D>Save formatted lines from source code into documentation file. <br>
//D>Read source code line-by-line and save prefixed lines by tag ??D to file. <br>
//D>C/C++/Free Pascal source codes lines start with tag //D and Bash lines start with tag //D. <br>
//D>Only those lines started by tags are exportedd to documentation files. <br>
//D>Mixed commented lines can co-exist at same source code, one for local documentation purpose and another to be exported to apropriate documentation file. <br>
//D>All lines are documented using Markdown format, the exported document can be read by an Markdown program reader. <br>
//D></p>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

//D><h2 id="index">Index</h2>

//D><table>
//D><tr><td  align=left><a href="#top">Top</a>                      </td><td> </td></tr>
//D><tr><td  align=left><a href="#details">Details</a>              </td><td> </td></tr>
//D><tr><td  align=left><a href="#glossary">Glossary</a>            </td><td> </td></tr>
//D><tr><td  align=left><a href="#unit">Unit</a>                    </td><td> </td></tr>
//D><tr><td  align=left><a href="#interface">Interface</a>          </td><td> </td></tr>
//D><tr><td  align=left><a href="#uses">Uses</a>                    </td><td> </td></tr>
//D><tr><td  align=left><a href="#type">Type</a>                    </td><td> </td></tr>
//D><tr><td  align=left><a href="#variables">Variables</a>          </td><td> </td></tr>
//D><tr><td align=right><a href="#implementation">Implementation</a></td><td> </td></tr>
//D><tr><td align=right><a href="#saveHeader">saveHeader</a>        </td><td>Save a formatted html header to parameter file descriptor.</td></tr>
//D><tr><td align=right><a href="#saveFooter">saveFooter</a>        </td><td>Save a formatted html footer to parameter file descriptor.</td></tr>
//D><tr><td align=right><a href="#btnAboutClick">btnAboutClick</a>  </td><td>Treat About button click event.</td></tr>
//D><tr><td align=right><a href="#btnCloseClick">btnCloseClick</a>  </td><td>Treat Close button click event.</td></tr>
//D><tr><td align=right><a href="#btnRunClick">btnRunClick</a>      </td><td>Treat Run button click event.</td></tr>
//D><tr><td align=right><a href="#FormShow">FormShow</a>            </td><td>Treat form show event.</td></tr>
//D><tr><td  align=left><a href="#bottom">Bottom</a>                </td><td> </td></tr>
//D></table>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

//D><h2 id="glossary">Glossary</h2>

//D><table>
//D><tr><th align=right>Use       </th><th> </th><th align=left>Description                                                        </th></tr>
//D><tr><td align=right>Constants </td><td>:</td><td>Memory space for read only data.                                              </td></tr>
//D><tr><td align=right>Variables </td><td>:</td><td>Memory space for read/write data.                                             </td></tr>
//D><tr><td align=right>Functions </td><td>:</td><td>fileSOURCE/Executable statement code, can be called anywhere from source code.</td></tr>
//D><tr><td align=right>Parameters</td><td>:</td><td>Data passed to functions.                                                     </td></tr>
//D><tr><td align=right>Result    </td><td>:</td><td>Functions result after execution.                                             </td></tr>
//D><tr><td align=right>Return    </td><td>:</td><td>Allways an integer returned from function to inform success or failure.       </td></tr>
//D><tr><td align=right>none      </td><td>:</td><td>Is similar as a void type, no parameter, no result or no return from function.</td></tr>
//D><tr><td align=right>string    </td><td>:</td><td>Char vector to store a group of characters.                                   </td></tr>
//D><tr><td align=right>type[]    </td><td>:</td><td>Memory vector space to store contigous data type.                             </td></tr>
//D></table>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

//D><br>
//D><table>
//D><tr><th align=right>Tags</th><th> </th><th align=left>Programming Language Documentation         </th></tr>
//D><tr><td align=right>//D></td><td>:</td><td>C/C++, C#, Java, JavaScript, Pascal/Object Pascal.    </td></tr>
//D><tr><td align=right>//D></td><td>:</td><td>Bash, Zsh, Python, Perl, Ruby, Go, Swift, Kotlin, Rust</td></tr>
//D><tr><td align=right>--D></td><td>:</td><td>SQL, Ada, Haskell.                                    </td></tr>
//D><tr><td align=right>''D></td><td>:</td><td>Visual Basic, VBScript.                               </td></tr>
//D><tr><td align=right>%%D></td><td>:</td><td>LaTex, MATLAB.                                        </td></tr>
//D><tr><td align=right>??E></td><td>:</td><td>End, stop documentation until ??B> tag.               </td></tr>
//D><tr><td align=right>??B></td><td>:</td><td>Begin, start documentation until ??E> tag.            </td></tr>
//D><tr><td align=right>??M></td><td>:</td><td>Macro, evaluate lines to show variables values.       </td></tr>
//D></table>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

//D><h2 id="unit">Unit makeDoc</h2>
unit makeDoc;

{$mode objfpc}{$H+}

//D><h2 id="interface">Interface</h2>

interface

//D><h2 id="uses">Uses</h3>

//D><table>
//D><tr><th align=left>Name</th><th align=left>Description</th></tr>
//D><tr><td>LCLType</td><td>Key codes</td></tr>
//D><tr><td>Classes</td><td>Abstract and concrete classes for object-oriented programming</td></tr>
//D><tr><td>Forms</td><td>Creating and managing user interface Forms</td></tr>
//D><tr><td>Controls</td><td>Common controls, such as TButton, TEdit, TMemo, TComboBox, and TListBox</td></tr>
//D><tr><td>Graphics</td><td>Native graphics classes like TCanvas, TFont, and TBitmap</td></tr>
//D><tr><td>Dialogs</td><td>Standard dialog components and functions like ShowMessage and dialogs for file opening/saving, color selection, and more</td></tr>
//D><tr><td>StdCtrls</td><td>Standard components like TButton, TLabel, TEdit, TMemo, TCheckBox, and TRadioButton</td></tr>
//D><tr><td>EditBtn</td><td>Edit box with an associated button, is found in the LCLBase unit</td></tr>
//D><tr><td>SysUtils</td><td>Part of the Free Pascal Run-Time Library (RTL)</td></tr>
//D><tr><td>RegExpr</td><td>Working with regular expressions is RegExpr</td></tr>
//D></table>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

uses
    LCLType, Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn, SysUtils, RegExpr;

//D><h2 id="type">Type</h2>

//D><table>
//D><tr><th align=center>Type</th><th align=center>Name</th><th align=center>Description</th></tr>
//D><tr><td align=right> <i>TFormMain</i></td><td align=right><b>FormMain</b></td><td>Main form application</td> </tr>
//D><tr><td align=right> <i>TButton</i></td><td align=right><b>btnClose</b></td><td>Button close</td> </tr>
//D><tr><td align=right> <i>TButton</i></td><td align=right><b>btnAbout</b></td><td>Show about box</td> </tr>
//D><tr><td align=right> <i>TButton</i></td><td align=right><b>btnRun</b></td><td>Execute documentation procedure</td> </tr>
//D><tr><td align=right> <i>TFileNameEdit</i></td><td align=right><b>editSource</b></td><td>Source file edit box</td> </tr>
//D><tr><td align=right> <i>TFileNameEdit</i></td><td align=right><b>editDestine</b></td><td>Destine file edit box</td> </tr>
//D><tr><td align=right> <i>TLabel</i></td><td align=right><b>lblDestine</b></td><td>Source file static label</td> </tr>
//D><tr><td align=right> <i>TLabel</i></td><td align=right><b>lblSource</b></td><td>Destine file static label</td> </tr>
//D><tr><td align=right> <i>procedure</i></td><td align=right><b>btnAboutClick</b></td><td>Treat About button click event</td> </tr>
//D><tr><td align=right> <i>procedure</i></td><td align=right><b>btnCloseClick</b></td><td>Treat Close button click event</td> </tr>
//D><tr><td align=right> <i>procedure</i></td><td align=right><b>btnRunClick</b></td><td>Treat Run button click event</td> </tr>
//D><tr><td align=right> <i>procedure</i></td><td align=right><b>FormShow</b></td><td>Treat form show event</td> </tr>
//D></table>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

type

{ TFormMain }

    TFormMain = class(TForm)
        btnClose: TButton;
        btnAbout: TButton;
        btnRun: TButton;
        editSource: TFileNameEdit;
        editDestine: TFileNameEdit;
        lblDestine: TLabel;
        lblSource: TLabel;
        procedure btnAboutClick(Sender: TObject);
        procedure btnCloseClick( Sender: TObject);
        procedure btnRunClick( Sender: TObject);
        procedure FormShow( Sender: TObject);
        procedure updateEditDestine( Sender: TObject);
    private

    public

end;

//D><h2 id="variables">Variables</h2>

//D><table>
//D><tr><th align=center>Type</th><th align=center>Name</th><th align=left>Description</th></tr>
//D><tr><td align=right> <i>TFormMain</i></td><td align=right><b>FormMain</b></td><td>Main form application</td></tr>
//D></table>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>


var
    FormMain: TFormMain;

//D><h2 id="implementation">Implementation</h2>

implementation

//D><h3 id="saveHeader">saveHeader( )</h3>

//D><i>boolean</i> <b>saveHeader</b>( <i>TextFile</i> <b>fd</b> ) : <i>string</i> <br>
//D>&ensp;Save a formatted html header to parameter file descriptor. <br>
//D><br>
//D><b>Parameter</b>: <br>
//D>&ensp;<i>TextFile</i>: <b>fd</b> - Target file descripton. <br>
//D><br>
//D><b>Result</b>: <br>
//D>&ensp;<i>string</i>: HTML header. <br>
//D><br>
//D><b>Return</b>: <br>
//D>&ensp;<i>boolean</i>: <b>true</b> - Success <br>
//D>&ensp;<i>boolean</i>: <b>false</b> - Error <br>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

function saveHeader(var fd : TextFile) : boolean;
const
    textDoc : string = '<!DOCTYPE html>' + sLineBreak +
                        '<html>' + sLineBreak +
                        '  <head>' + sLineBreak +
                        '    <title>Documentation</title>' + sLineBreak +
                        '    <style>' + sLineBreak +
                        '      table {' + sLineBreak +
                        '        border: hidden;' + sLineBreak +
                        '        border-collapse: collapse;' + sLineBreak +
                        '        padding-left: 10px;' + sLineBreak +
                        '        padding-right: 10px; }' + sLineBreak +
                        '      th { background-color: #E0E0E0; }' + sLineBreak +
                        '    </style>' + sLineBreak +
                        '  </head>' + sLineBreak +
                        '  <body>'+ sLineBreak;
begin
    saveHeader := false;
    try
        WriteLn(fd, textDoc);
        saveHeader := true;
    except
        on E: EInOutError do
        ShowMessage( 'File Error: '+ E.ClassName + #13#10 + E.Message );
    end;
end;

//D><h3 id="saveFooter">saveFooter( )</h3>

//D><i>boolean</i> <b>saveFooter</b>( <i>TextFile</i> <b>fd</b> ) : <i>string</i> <br>
//D>&ensp;Save a formatted html footer to parameter file descriptor. <br>
//D><br>
//D><b>Parameter</b>: <br>
//D>&ensp;<i>TextFile</i>: <b>fd</b> - Target file descripton. <br>
//D><br>
//D><b>Result</b>: <br>
//D>&ensp;<i>string</i>: HTML footer. <br>
//D><br>
//D><b>Return</b>: <br>
//D>&ensp;<i>boolean</i>: <b>true</b> - Success <br>
//D>&ensp;<i>boolean</i>: <b>false</b> - Error <br>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

function saveFooter(var fd : TextFile) : boolean;
const
    textDoc : string = sLineBreak + '  </body>' + sLineBreak + '</html>' + sLineBreak;
begin
    saveFooter := false;
    try
        WriteLn(fd, textDoc);
        saveFooter := true;
    except
        on E: EInOutError do
        ShowMessage( 'File Error: '+ E.ClassName + #13#10 + E.Message );
    end;
end;

{$R *.lfm}

{ TFormMain }

//D><h3 id="btnAboutClick">btnAboutClick( )</h3>

//D><i>boolean</i> <b>btnAboutClick</b>( <i>TObject</i> <b>Sender</b> ) : <i>None</i> <br>
//D>&ensp;Treat About button click event. <br>
//D><br>
//D><b>Parameter</b>: <br>
//D>&ensp;<i>TObject</i>: <b>Sender</b> - Objects and Events from main window. <br>
//D><br>
//D><b>Result</b>: <br>
//D>&ensp;<i>TFormMain</i>: Create and show a formatted about form window. <br>
//D><br>
//D><b>Return</b>: <br>
//D>&ensp;<i>None</i> <br>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

procedure TFormMain.btnAboutClick(Sender: TObject);
begin
    ShowMessage('Export Documentation from taged lines.' + sLineBreak +
                'File: makeDoc' + sLineBreak +
                'Version: 1.0.0 / 2025-10-05' + sLineBreak +
                'Author: Leandro' + sLineBreak +
                'Copyright: CC01  1.0 Universal');
end;

//D><h3 id="btnCloseClick">btnCloseClick( )</h3>

//D><i>boolean</i> <b>btnCloseClick</b>( <i>TObject</i> <b>Sender</b> ) : <i>None</i> <br>
//D>&ensp;Treat Close button click event. <br>
//D><br>
//D><b>Parameter</b>: <br>
//D>&ensp;<i>TObject</i>: <b>Sender</b> - Objects and Events from main window. <br>
//D><br>
//D><b>Result</b>: <br>
//D>&ensp;<i>TFormMain</i>: End and close main form window. <br>
//D><br>
//D><b>Return</b>: <br>
//D>&ensp;<i>None</i> <br>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

procedure TFormMain.btnCloseClick( Sender: TObject);
begin
    Close;
end;

//D><h3 id="btnRunClick">btnRunClick( )</h3>

//D><i>boolean</i> <b>btnRunClick</b>( <i>TObject</i> <b>Sender</b> ) : <i>None</i> <br>
//D>&ensp;Treat Run button click event. <br>
//D><br>
//D><b>Parameter</b>: <br>
//D>&ensp;<i>TObject</i>: <b>Sender</b> - Objects and Events from main window. <br>
//D><br>
//D><b>Result</b>: <br>
//D>&ensp;<i>TFormMain</i>: Run documentation procedure. <br>
//D><br>
//D><b>Return</b>: <br>
//D>&ensp;<i>None</i> <br>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

procedure TFormMain.btnRunClick( Sender: TObject);

var

    inputFile, outputFile : TextFile;
    line : string;
    strList : TStringList;
    enableDoc : boolean;
    reAll, reDoc, reHeader, reFooter, reBegin, reEnd, reValidFilename : TRegExpr;

begin
    reValidFilename := TRegExpr.Create('/([A-Za-z0-9_.]+[A-Za-z0-9_.]*)$');
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
        editDestine.Text:= editSource.Text + '.html';
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
        try
            AssignFile(inputFile, editSource.Text);
            Reset(inputFile);
            AssignFile(outputFile, editDestine.Text);
            Rewrite(outputFile);
            reAll    := TRegExpr.Create('^ *([#/%-]{2}[DMBEHF]>?) *');
            reDoc    := TRegExpr.Create('^ *([#/%-]{2}[DM]>?) *');
            reHeader := TRegExpr.Create('^ *([#/%-]{2}H>?) *');
            reFooter := TRegExpr.Create('^ *([#/%-]{2}F>?) *');
            reBegin  := TRegExpr.Create('^ *([#/%-]{2}B>?) *');
            reEnd    := TRegExpr.Create('^ *([#/%-]{2}E>?) *');
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
                            else if reHeader.Exec(line) then saveHeader(outputFile)
                            else if reFooter.Exec(line) then saveFooter(outputFile);
                        end
                        else if reBegin.Exec(line) then enableDoc := true;
                    end;
                end;
                Flush(outputFile);
            except
                on E: EInOutError do
                    ShowMessage( 'File Error: '+ E.ClassName + #13#10 + E.Message );
            end;
        finally
            strList.Free;
            reAll.Free; reDoc.Free; reHeader.Free; reFooter.Free; reBegin.Free; reEnd.Free; reValidFilename.Free;
            CloseFile(inputFile); CloseFile(outputFile);
        end;
    end
    else
        Application.MessageBox ('Source file does not exist.', 'Error', mb_Ok or mb_IconHand);
end;

//D><h3 id="FormShow">FormShow( )</h3>

//D><i>boolean</i> <b>FormShow</b>( <i>TObject</i> <b>Sender</b> ) : <i>None</i> <br>
//D>&ensp;Treat form show event. <br>
//D><br>
//D><b>Parameter</b>: <br>
//D>&ensp;<i>TObject</i>: <b>Sender</b> - Objects and Events from main window. <br>
//D><br>
//D><b>Result</b>: <br>
//D>&ensp;<i>TFormMain</i>: Prepare main form edit box to load and show current directory. <br>
//D><br>
//D><b>Return</b>: <br>
//D>&ensp;<i>None</i> <br>

//D><br>
//D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] <br>

procedure TFormMain.FormShow( Sender: TObject);
var
    currentDir : string;
begin
    currentDir := GetCurrentDir;
    editSource.InitialDir := currentDir + '/*';
    editSource.Text:= currentDir + '/*';
    if DirectoryExists(currentDir + '/doc') then currentDir := currentDir + '/doc/*';
    editDestine.InitialDir := currentDir;
    editDestine.Text:= currentDir;
end;

procedure TFormMain.updateEditDestine( Sender: TObject);
begin
    editDestine.Text:= editSource.Text + '.html';
end;

end.

//F>
