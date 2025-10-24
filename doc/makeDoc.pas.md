<a id="makeDoc"></a>
# makeDoc

|               | Description                                                               |
| ------------: | ------------------------------------------------------------------------- |
|               | Lazarus/Free Pascal project to export documentation lines with tag marker |
| **File**      | makeDoc.pas                                                               |
| **Author**    | [Leandro](mailto:leandrohuff@email.com)                                   |
| **Date**      | _2025-10-06_                                                              |
| **Version**   | _1.0.0_ / _2025-10-06_                                                    |
| **Copyright** | CC01 1.0 Universal                                                        |
| **Copyright** | **Copyright Icons** [Uicons by Flaticon](https://www.flaticon.com/uicons) |

**Note**: Changes in this document will be discarded on next build,  
any changes should be made on source code documentation instead.  

<a id="Details"></a>
## Details

Save formatted lines from source code into documentation file.  
Read source code line-by-line and save prefixed lines by tag ??D to file.  
C/C++/Free Pascal source codes lines start with tag //D and Bash lines start with tag //D.  
Only those lines started by tags are exportedd to documentation files.  
Mixed commented lines can co-exist at same source code, one for local  
documentation purpose and another to be exported to apropriate documentation file.  
All lines are documented using Markdown format, the exported document can be read  
by an Markdown program reader.  

<a id="Index"></a>
## Index

| Index                             | Description                                         |
| :-------------------------------- | :-------------------------------------------------- |
| [Top](#makeDoc)                   |                                                     |
| [Details](#Details)               |                                                     |
| [Unit](#Unit)                     |                                                     |
| [Type](#Type)                     |                                                     |
| [Interface](#Interface)           |                                                     |
| [Uses](#Uses)                     |                                                     |
| [Constants](#Constants)           |                                                     |
| [Variables](#Variables)           |                                                     |
| [Implementation](#Implementation) |                                                     |
| [btnAboutClick](#btnAboutClick)   |                                                     |
| [btnCloseClick](#btnCloseClick)   |                                                     |
| [btnRunClick](#btnRunClick)       |                                                     |
| [OnClick](#OnClick)               |                                                     |
| [OnCreate](#OnCreate)             |                                                     |
| [Bottom](#OnCreate)               |                                                     |

[⬅️](#makeDoc) | [⬆️](#Index)

<a id="Unit"></a>
## Unit

<a id="Interface"></a>
## Interface

<a id="Uses"></a>
## Uses

| Name     | Description |
| -------: | ----------- |
| LCLType  | Key codes   |
| Classes  | Abstract and concrete classes for object-oriented programming |
| Forms    | Creating and managing user interface Forms |
| Controls | Common controls, such as TButton, TEdit, TMemo, TComboBox, and TListBox |
| Graphics | Native graphics classes like TCanvas, TFont, and TBitmap |
| Dialogs  | Standard dialog components and functions like ShowMessage and dialogs for file opening/saving, color selection, and more |
| StdCtrls | Standard components like TButton, TLabel, TEdit, TMemo, TCheckBox, and TRadioButton |
| EditBtn  | Edit box with an associated button, is found in the LCLBase unit |
| SysUtils | Part of the Free Pascal Run-Time Library (RTL) |
| RegExpr  | Working with regular expressions is RegExpr |

[⬅️](#Details) | [⬆️](#Index)

<a id="Type"></a>
## Type

| Type            | Name                  | Description                     |
| --------------: | --------------------: | ------------------------------- |
| _TFormMain_     | **FormMain**          | Main form application           |
| _TButton_       | **btnClose**          | Button close                    |
| _TButton_       | **btnAbout**          | Show about box                  |
| _TButton_       | **btnRun**            | Execute documentation procedure |
| _TFileNameEdit_ | **editSource**        | Source file edit box            |
| _TFileNameEdit_ | **editDestine**       | Destine file edit box           |
| _TLabel_        | **lblStatus**         | Status processing               |
| _TLabel_        | **lblDestine**        | Source file static label        |
| _TLabel_        | **lblSource**         | Destine file static label       |
| _procedure_     | **btnAboutClick**     | Treat About button click event  |
| _procedure_     | **btnCloseClick**     | Treat Close button click event  |
| _procedure_     | **btnRunClick**       | Treat Run button click event    |
| _procedure_     | **OnClick**           | Treat edit destine click event  |
| _procedure_     | **OnCreate**          | Treat on create form event      |

[⬅️](#Uses) | [⬆️](#Index)

<a id="Constants"></a>
## Constants

| Type | Name | Description |
| :--: | :--: | :---------: |
|  -   |  -   |      -      |

<a id="Variables"></a>
## Variables

| Type      | Name     | Description           |
| --------: | -------: | --------------------- |
| TFormMain | FormMain | Main form application |

[⬅️](#Type) | [⬆️](#Index)

<a id="Implementation"></a>
## Implementation

<a id="btnAboutClick"></a>
### btnAboutClick

| Return | Name | Parameters | Result |
| ------ | ---- | ---------- | ------ |
| _boolean_ | **btnAboutClick** | _TObject_ **Sender** | _None_ |

Treat About button click event.  

**Parameter**:  
_TObject_ **Sender**: Objects and Events from main window.  

**Result**:  
_None_: Create and show a formatted about form window.  

**Return**:  
_boolean_  

[⬅️](#Variables) | [⬆️](#Index)

<a id="btnCloseClick"></a>
### btnCloseClick

| Return | Name | Parameters | Result |
| ------ | ---- | ---------- | ------ |
| _boolean_ | **btnCloseClick** | _TObject_ **Sender** | _None_ |

Treat Close button click event.  

**Parameter**:  
_TObject_ **Sender** : Objects and Events from main window.  

**Result**:  
_TFormMain_ : End and close main form window.  

**Return**:  
_None_

[⬅️](#btnAboutClick) | [⬆️](#Index)

<a id="btnRunClick"></a>
### btnRunClick

| Return | Name | Parameters | Result |
| ------ | ---- | ---------- | ------ |
| _None_ | **btnRunClick** | _TObject_ **Sender** | _None_ |

Treat Run button click event.  

**Parameter**:  
_TObject_ **Sender**: Objects and Events from main window.  

**Result**:  
_TFormMain_: Run documentation procedure.  

**Return**:  
_None_  

[⬅️](#btnCloseClick) | [⬆️](#Index)

<a id="OnClick"></a>
### OnClick

| Return  | Name        | Parameters           | Result |
|  ------ | ----------- | -------------------- | ------ |
| _None_  | **OnClick** | _TObject_ **Sender** | _None_ |

Treat edit destine click event.

**Parameter**:
_TObject_ **Sender**: Objects and Events from main window.

**Result**:
_TFormMain_: Prepare destine edit text to show path + filename.

[⬅️](btnRunClick) | [⬆️](#Index)

<a id="OnCreate"></a>
### OnCreate

| Return  | Name         | Parameters           | Result |
|  ------ | ------------ | -------------------- | ------ |
| _None_  | **OnCreate** | _TObject_ **Sender** | _None_ |

Treat form create event.

**Parameter**:
_TObject_ **Sender**: Objects and Events from main window.

**Result**:
_TFormMain_: Prepare main form edit box to load and show current directory.

[⬅️](OnClick) | [⬆️](#Index)
