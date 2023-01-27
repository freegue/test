{-------------------------------------------------------------------------------
  Copyright (C) 2022
  Project  :
  Author   : kyungmun
  Date     : 2022-07-06
  Comment  : 스나이퍼 프로퍼티 데이터 기본 데이터 타입 인터페이스
-------------------------------------------------------------------------------}

unit uPropertyDesignIntf;

interface

uses
  SysUtils, System.Types, Windows, Messages, Classes, Graphics, Controls, StdCtrls, DesignIntf,
  Menus, TypInfo, uPropertyResource, uDefineType;

type
  TSniperPropertyAttribute = (paValueList, paSortList, paDialog, paMultiSelect, paRevertable, paSubProperties, paReadOnly, paCustomDropDown, paDisplayReadOnly);
  TSniperPropertyAttributes = set of TSniperPropertyAttribute;
  TSniperPropertyItem = class;

  TSniperPropertyEditor = class(TObject)
  private
    FCompList: TList;
    FPropList: TList;
    FItemHeight: Integer;
    FValues: TStrings;
    function GetPropInfo: PPropInfo;
    function GetComponent: TPersistent;
    function GetSubComponent: TPersistent;
    function GetPropInfoTypeKind: TTypeKind;
    function GetPropInfoTypeName: String;
    function GetPropCount: Integer;
  protected
    procedure GetStrProc(const s: String);
    function GetFloatValue: Extended;
    function GetOrdValue: Integer;
    function GetStrValue: String;
    function GetVarValue: Variant;
    procedure SetFloatValue(Value: Extended);
    procedure SetOrdValue(Value: Integer);
    procedure SetStrValue(const Value: String);
    procedure SetVarValue(Value: Variant);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Edit: Boolean; virtual;
    function GetAttributes: TSniperPropertyAttributes; virtual;
    function GetName: String; virtual;
    function GetExtraLBSize: Integer; virtual;
    function GetValue: String; virtual;
    procedure GetValues; virtual;
    procedure SetValue(const Value: String); virtual;
    procedure OnDrawLBItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState); virtual;
    procedure OnDrawItem(Canvas: TCanvas; ARect: TRect); virtual;
    property Component: TPersistent read GetComponent;
    property SubComponent: TPersistent read GetSubComponent;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property PropInfo: PPropInfo read GetPropInfo;
    property Value: String read GetValue write SetValue;
    property Values: TStrings read FValues;
    property PropTypeKind: TTypeKind read GetPropInfoTypeKind;
    property PropTypeName: String read GetPropInfoTypeName;
    property PropCount: Integer read GetPropCount;
  end;

  TDataValidateEvent = procedure(Sender: TSniperPropertyEditor; const AValue : string; var Valid : boolean) of object;

  TSniperPropertyEditorClass = class of TSniperPropertyEditor;

  TSniperIntegerProperty = class(TSniperPropertyEditor)
  public
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TSniperFloatProperty = class(TSniperPropertyEditor)
  public
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TSniperStringProperty = class(TSniperPropertyEditor)
  public
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TSniperEnumProperty = class(TSniperPropertyEditor)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function GetValue: String; override;
    procedure GetValues; override;
    procedure SetValue(const Value: String); override;
  end;

  TSniperBooleanProperty = class(TSniperEnumProperty)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    procedure OnDrawLBItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState); override;
    procedure OnDrawItem(Canvas: TCanvas; ARect: TRect); override;
  end;

  TSniperSetProperty = class(TSniperPropertyEditor)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function GetValue: String; override;
  end;

  TSniperSetElementProperty = class(TSniperPropertyEditor)
  private
    FElement: Integer;
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function GetName: String; override;
    function GetValue: String; override;
    procedure GetValues; override;
    procedure SetValue(const Value: String); override;
   end;

  TSniperNameProperty = class(TSniperStringProperty)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
  end;

  TSniperClassProperty = class(TSniperPropertyEditor)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function GetValue: String; override;
  end;

  TSniperComponentProperty = class(TSniperPropertyEditor)
  public
    function GetAttributes: TSniperPropertyAttributes; override;
    function GetValue: String; override;
  end;


  TSniperDateProperty = class(TSniperPropertyEditor)
  public
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TSniperTimeProperty = class(TSniperPropertyEditor)
  public
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

{ Internal classes used by Object Inspector }

  TSniperPropertyList = class;

  TSniperPropertyItem = class(TCollectionItem)
  private
    FEditor: TSniperPropertyEditor;
    FExpanded: Boolean;
    FSubProperty: TSniperPropertyList;
  public
    destructor Destroy; override;
    function GetPropertyComponent : TPersistent;
    function GetSubPropertyComponent : TPersistent;
    function GetSub2PropertyComponent: TPersistent;
    property Editor: TSniperPropertyEditor read FEditor;
    property Expanded: Boolean read FExpanded write FExpanded;
    property SubProperty: TSniperPropertyList read FSubProperty;
  end;

  TSniperPropertyList = class(TCollection)
  private
    FComponent: TPersistent;
    FParent: TSniperPropertyList;
    procedure AddProperties(PropertyList: TSniperPropertyList);
    procedure FillProperties(AClass: TPersistent);
    procedure FillCommonProperties(PropertyList: TSniperPropertyList);
    procedure SetComponent(Value: TPersistent);
    function GetPropertyItem(Index: Integer): TSniperPropertyItem;
  public
    constructor Create;
    function Add: TSniperPropertyItem;
    property Component: TPersistent read FComponent write SetComponent;
    property Items[Index: Integer]: TSniperPropertyItem read GetPropertyItem; default;
    property Parent: TSniperPropertyList read FParent;
  end;

  TSniperPropertyEditorItem = class(TCollectionItem)
  public
    PropertyType: PTypeInfo;
    ComponentClass: TClass;
    PropertyName: String;
    EditorClass: TSniperPropertyEditorClass;
  end;

  TSniperPropertyEditorCollection = class(TCollection)
  private
    FEventEditorItem: Integer;
    function GetPropertyEditorItem(Index: Integer): TSniperPropertyEditorItem;
  public
    constructor Create;
    procedure Register(PropertyType: PTypeInfo; ComponentClass: TClass;
      const PropertyName: String; EditorClass: TSniperPropertyEditorClass);
    procedure RegisterEventEditor(EditorClass: TSniperPropertyEditorClass);
    procedure UnRegister(EditorClass: TSniperPropertyEditorClass);
    function GetPropertyEditor(PropertyType: PTypeInfo; Component: TPersistent;
      PropertyName: String): Integer;
    property Items[Index: Integer]: TSniperPropertyEditorItem
      read GetPropertyEditorItem; default;
  end;


{ internal methods }

function CreatePropertyList(ComponentList: TList): TSniperPropertyList;
procedure HideProperties(ComponentClass: TClass; const Properties: String);
function PropertyEditors: TSniperPropertyEditorCollection;

implementation

uses
  Consts, Forms, Dialogs;

type
  TIntegerSet = set of 0..31;

var
  FPropertyEditors: TSniperPropertyEditorCollection = nil;

{ Routines }

procedure HideProperties(ComponentClass: TClass; const Properties: String);
var
  i: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Properties;

    for i := 0 to sl.Count - 1 do
      PropertyEditors.Register(nil, ComponentClass, sl[i], nil);
  finally
    sl.Free;
  end;
end;

procedure ViewProperties(ComponentClass: TClass; const Properties: String);
var
  i: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Properties;

    //for i := 0 to sl.Count - 1 do
    //  PropertyEditors.Register(ComponentClass, nil, sl[i], ComponentClass);
      //PropertyEditors.Register(TypeInfo(TDate), nil, '', TSniperDateProperty);

  finally
    sl.Free;
  end;
end;

function CreatePropertyList(ComponentList: TList): TSniperPropertyList;
var
  i: Integer;
  p: TSniperPropertyList;
  l: TList;
begin
  if ComponentList.Count = 0 then
  begin
    Result := nil;
    Exit;
  end;

  l := TList.Create;
  for i := 0 to ComponentList.Count - 1 do
  begin
    p := TSniperPropertyList.Create;
    l.Add(p);
    p.Component := ComponentList[i];
  end;

  Result := l[0];
  for i := 1 to ComponentList.Count - 1 do
    Result.FillCommonProperties(TSniperPropertyList(l[i]));

  for i := 1 to ComponentList.Count - 1 do
  begin
    TSniperPropertyList(l[i]).FillCommonProperties(Result);
    Result.AddProperties(TSniperPropertyList(l[i]));
    TSniperPropertyList(l[i]).Free;
  end;

  l.Free;
end;


function frStrToFloat(s: String): Extended;
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    if CharInSet(s[i], [',', '.']) then
      s[i] := FormatSettings.DecimalSeparator;

  Result := StrToFloat(Trim(s));
end;


{ TPropertyEditor }

constructor TSniperPropertyEditor.Create;
begin
  FCompList := TList.Create;
  FPropList := TList.Create;
  FValues := TStringList.Create;
end;

destructor TSniperPropertyEditor.Destroy;
begin
  FCompList.Free;
  FPropList.Free;
  FValues.Free;
  inherited;
end;

function TSniperPropertyEditor.GetAttributes: TSniperPropertyAttributes;
begin
  //Result := [paMultiSelect, paRevertable];
  //if PropInfo.SetProc = nil then
  //  Result := Result + [paReadOnly, paDisplayReadOnly];
  Result := [paMultiSelect];
end;

function TSniperPropertyEditor.GetName: String;
begin
  Result := String(PropInfo.Name);
end;

function TSniperPropertyEditor.GetComponent: TPersistent;
begin
  Result := FCompList[0];
end;

function TSniperPropertyEditor.GetPropCount: Integer;
begin
  result := FCompList.Count;
end;

function TSniperPropertyEditor.GetPropInfo: PPropInfo;
begin
  Result := FPropList[0];
end;

function TSniperPropertyEditor.GetPropInfoTypeKind: TTypeKind;
begin
  result := PropInfo.PropType^.Kind;
end;

function TSniperPropertyEditor.GetPropInfoTypeName: String;
begin
  result := String(PropInfo.PropType^.Name);
end;

function TSniperPropertyEditor.GetValue: String;
begin
  Result := '(Unknown)';
end;

procedure TSniperPropertyEditor.SetValue(const Value: String);
begin
  { empty method }
end;

function TSniperPropertyEditor.GetFloatValue: Extended;
begin
  Result := GetFloatProp(Component, PropInfo);
end;

function TSniperPropertyEditor.GetOrdValue: Integer;
begin
  Result := GetOrdProp(Component, PropInfo);
end;

function TSniperPropertyEditor.GetStrValue: String;
begin
  Result := GetStrProp(Component, PropInfo);
end;

function TSniperPropertyEditor.GetSubComponent: TPersistent;
begin
  result := TPersistent(GetOrdProp(Component, PropInfo));
end;

function TSniperPropertyEditor.GetVarValue: Variant;
begin
  Result := GetVariantProp(Component, PropInfo);
end;

procedure TSniperPropertyEditor.SetFloatValue(Value: Extended);
var
  i: Integer;
begin
  for i := 0 to FCompList.Count - 1 do
    if (PPropInfo(FPropList[i]).SetProc <> nil) then
      SetFloatProp(TObject(FCompList[i]), PPropInfo(FPropList[i]), Value);
end;

procedure TSniperPropertyEditor.SetOrdValue(Value: Integer);
var
  i: Integer;
begin
  for i := 0 to FCompList.Count - 1 do
    if (PPropInfo(FPropList[i]).SetProc <> nil) then
      SetOrdProp(TObject(FCompList[i]), PPropInfo(FPropList[i]), Value);
end;

procedure TSniperPropertyEditor.SetStrValue(const Value: String);
var
  i: Integer;
begin
  for i := 0 to FCompList.Count - 1 do
    if (PPropInfo(FPropList[i]).SetProc <> nil) then
      SetStrProp(TObject(FCompList[i]), PPropInfo(FPropList[i]), Value);
end;

procedure TSniperPropertyEditor.SetVarValue(Value: Variant);
var
  i: Integer;
begin
  for i := 0 to FCompList.Count - 1 do
    if (PPropInfo(FPropList[i]).SetProc <> nil) then
      SetVariantProp(TObject(FCompList[i]), PPropInfo(FPropList[i]), Value);
end;

procedure TSniperPropertyEditor.GetValues;
begin
  FValues.Clear;
  TStringList(FValues).Sorted := paSortList in GetAttributes;
end;

procedure TSniperPropertyEditor.GetStrProc(const s: String);
begin
  FValues.Add(s);
end;

function TSniperPropertyEditor.Edit: Boolean;
var
  i: Integer;
begin
  Result := False;
  GetValues;
  if FValues.Count > 0 then
  begin
    i := FValues.IndexOf(Value) + 1;
    if i = FValues.Count then
      i := 0;
    Value := FValues[i];
    Result := True;
  end;
end;

procedure TSniperPropertyEditor.OnDrawLBItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with TListBox(Control).Canvas do
  begin
    FillRect(ARect);
    TextOut(ARect.Left + 2, ARect.Top, TListBox(Control).Items[Index]);
    Pen.Color := clGray;
  end;
end;

procedure TSniperPropertyEditor.OnDrawItem(Canvas: TCanvas; ARect: TRect);
begin
  Canvas.TextOut(ARect.Left + FItemHeight - 2, ARect.Top, Value);
  Canvas.Pen.Color := clGray;
end;

function TSniperPropertyEditor.GetExtraLBSize: Integer;
begin
  Result := FItemHeight + 2;
end;

{ TPropertyList }

constructor TSniperPropertyList.Create;
begin
  inherited Create(TSniperPropertyItem);
end;

function TSniperPropertyList.GetPropertyItem(Index: Integer): TSniperPropertyItem;
begin
  Result := TSniperPropertyItem(inherited Items[Index]);
end;

function TSniperPropertyList.Add: TSniperPropertyItem;
begin
  Result := TSniperPropertyItem(inherited Add);
end;

procedure TSniperPropertyList.SetComponent(Value: TPersistent);
begin
  FComponent := Value;
  Clear;
  FillProperties(FComponent);
end;

procedure TSniperPropertyList.FillProperties(AClass: TPersistent);
var
  Item, Item1: TSniperPropertyItem;
  TypeInfo: PTypeInfo;
  PropertyCount: Integer;
  PropertyList: PPropList;
  i, j: Integer;
  FClass: TClass;

  function CreateEditor(EditorClass: TSniperPropertyEditorClass; AClass: TPersistent;
    PropInfo: PPropInfo): TSniperPropertyEditor;
  var
    Item: TSniperPropertyEditorItem;
    e: Integer;
  begin
    Result := nil;
    e := PropertyEditors.GetPropertyEditor(PropInfo.PropType^, AClass, String(PropInfo.Name));
    if e <> -1 then
    begin
      Item := PropertyEditors[e];
      if Item.EditorClass <> nil then
        Result := TSniperPropertyEditor(Item.EditorClass.NewInstance) else
        Exit;
    end
    else
      Result := TSniperPropertyEditor(EditorClass.NewInstance);

    Result.Create;
    Result.FCompList.Add(AClass);
    Result.FPropList.Add(PropInfo);
  end;

begin
  if AClass = nil then exit;

  TypeInfo := AClass.ClassInfo;
  PropertyCount := GetPropList(TypeInfo, tkProperties, nil);
  GetMem(PropertyList, PropertyCount * SizeOf(PPropInfo));
  GetPropList(TypeInfo, tkProperties, PropertyList, false); //마지막인지, 프로퍼티 영문이름으로 정렬여부

  for i := 0 to PropertyCount - 1 do
  begin
    Item := Add;
    case PropertyList[i].PropType^.Kind of
      tkInteger:
        Item.FEditor := CreateEditor(TSniperIntegerProperty, AClass, PropertyList[i]);

      tkFloat:
        Item.FEditor := CreateEditor(TSniperFloatProperty, AClass, PropertyList[i]);

      tkString, tkLString, tkWString, tkUString:
        Item.FEditor := CreateEditor(TSniperStringProperty, AClass, PropertyList[i]);

      tkEnumeration:
        Item.FEditor := CreateEditor(TSniperEnumProperty, AClass, PropertyList[i]);

      tkSet:
        begin
          Item.FSubProperty := TSniperPropertyList.Create;
          Item.FSubProperty.FParent := Self;
          Item.FEditor := CreateEditor(TSniperSetProperty, AClass, PropertyList[i]);
          with GetTypeData(GetTypeData(PropertyList[i].PropType^).CompType^)^ do
            for j := MinValue to MaxValue do
            begin
              Item1 := Item.FSubProperty.Add;
              Item1.FEditor := CreateEditor(TSniperSetElementProperty, AClass, PropertyList[i]);
              if Item1.FEditor <> nil then
                TSniperSetElementProperty(Item1.FEditor).FElement := j;
            end;
        end;

      tkClass:
        begin
          FClass := GetTypeData(PropertyList[i].PropType^)^.ClassType;
          if FClass.InheritsFrom(TComponent) then
            Item.FEditor := CreateEditor(TSniperComponentProperty, AClass, PropertyList[i])
          else if FClass.InheritsFrom(TPersistent) then
          begin
            Item.FEditor := CreateEditor(TSniperClassProperty, AClass, PropertyList[i]);
            Item.FSubProperty := TSniperPropertyList.Create;
            Item.FSubProperty.FParent := Self;
            Item.FSubProperty.Component := TPersistent(GetOrdProp(AClass, PropertyList[i]));
            if Item.FSubProperty.Count = 0 then
            begin
              Item.FSubProperty.Free;
              Item.FSubProperty := nil;
            end;
          end;
        end;

    end;
    if Item.FEditor = nil then
      Item.Free;
  end;

  FreeMem(PropertyList, PropertyCount * SizeOf(PPropInfo));
end;

procedure TSniperPropertyList.FillCommonProperties(PropertyList: TSniperPropertyList);
var
  i, j: Integer;
  p, p1: TSniperPropertyItem;
  Found: Boolean;
begin
  i := 0;
  while i < Count do
  begin
    p := Items[i];
    Found := False;
    if paMultiSelect in p.Editor.GetAttributes then
      for j := 0 to PropertyList.Count - 1 do
      begin
        p1 := PropertyList.Items[j];
        if (p1.Editor.GetPropInfo.PropType^.Kind = p.Editor.GetPropInfo.PropType^.Kind) and
           (p1.Editor.GetPropInfo.Name = p.Editor.GetPropInfo.Name) then
        begin
          Found := True;
          break;
        end;
      end;

    if not Found then
      p.Free else
      Inc(i);
  end;
end;

procedure TSniperPropertyList.AddProperties(PropertyList: TSniperPropertyList);

  procedure EnumProperties(p1, p2: TSniperPropertyList);
  var
    i: Integer;
  begin
    for i := 0 to p1.Count - 1 do
    begin
      p1[i].Editor.FCompList.Add(p2[i].Editor.FCompList[0]);
      p1[i].Editor.FPropList.Add(p2[i].Editor.FPropList[0]);
      if p1[i].SubProperty <> nil then
        EnumProperties(p1[i].SubProperty, p2[i].SubProperty);
    end;
  end;

begin
  EnumProperties(Self, PropertyList);
end;


{ TPropertyItem }

destructor TSniperPropertyItem.Destroy;
begin
  if Editor <> nil then
    Editor.Free;
  if SubProperty <> nil then
    SubProperty.Free;
  inherited;
end;


////////////////////////////////////////////////////////////////////////////////

{ TBooleanProperty }

function TSniperBooleanProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paValueList, paCustomDropDown, paSortList, paReadOnly];
end;

procedure TSniperBooleanProperty.OnDrawItem(Canvas: TCanvas; ARect: TRect);
var
  add: Integer;
begin
  inherited;
  with Canvas do
  begin
    Rectangle(ARect.Left, ARect.Top + 2, ARect.Left + (ARect.Bottom - ARect.Top - 5) - 3, ARect.Bottom - 6);
    Pen.Color := clBlack;
    if Screen.PixelsPerInch > 96 then
      add := 2
    else
      add := 0;
    if Boolean(GetOrdValue) = True then
      with ARect do
      begin
        PolyLine([Point(Left + 3 + add, Top + 6 + add), Point(Left + 5 + add, Top + 8 + add), Point(Left + 10 + add, Top + 3 + add)]);
        PolyLine([Point(Left + 3 + add, Top + 7 + add), Point(Left + 5 + add, Top + 9 + add), Point(Left + 10 + add, Top + 4 + add)]);
        PolyLine([Point(Left + 3 + add, Top + 8 + add), Point(Left + 5 + add, Top + 10 + add), Point(Left + 10 + add, Top + 5 + add)]);
      end;
  end;
end;

procedure TSniperBooleanProperty.OnDrawLBItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  inherited;
end;

{ TIntegerProperty }

function TSniperIntegerProperty.GetValue: String;
begin
  Result := IntToStr(GetOrdValue);
end;

procedure TSniperIntegerProperty.SetValue(const Value: String);
begin
  SetOrdValue(StrToInt(Value));
end;


{ TFloatProperty }

function TSniperFloatProperty.GetValue: String;
begin
  Result := FloatToStr(GetFloatValue);
end;

procedure TSniperFloatProperty.SetValue(const Value: String);
begin
  SetFloatValue(frStrToFloat(Value));
end;


{ TStringProperty }

function TSniperStringProperty.GetValue: String;
begin
  Result := GetStrValue;
end;

procedure TSniperStringProperty.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

{ TEnumProperty }

function TSniperEnumProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paReadOnly];
end;

function TSniperEnumProperty.GetValue: String;
var
  i: Integer;
  TypeInfo: PTypeInfo;
  sTypeName : String;
begin
  i := GetOrdValue;
  TypeInfo := PropInfo.PropType^;
  sTypeName := UpperCase(String(TypeInfo.Name));

  // TODO : Emun 선택값을 한글로 표기 되야할 타입 처리
  if (sTypeName = UpperCase('TPRODUCTGROUP')) or
     (sTypeName = UpperCase('TDirection')) or
     (sTypeName = UpperCase('TBlockType')) or  //차단종류
     (sTypeName = UpperCase('TBlockReasonType')) or //차단사유
     (sTypeName = UpperCase('TDetectEtcType')) or //탐지기타
     (sTypeName = UpperCase('TDATETYPE')) then
    Result := GetPropertyNameTranslation(GetEnumName(TypeInfo, i))
  else
    Result := GetEnumName(TypeInfo, i);

end;

procedure TSniperEnumProperty.GetValues;
var
  i: Integer;
  TypeInfo: PTypeInfo;
  sTypeName : String;
begin
  inherited;
  TypeInfo := PropInfo.PropType^;
  sTypeName := UpperCase(String(TypeInfo.Name));

  for i := TypeInfo.TypeData.MinValue to TypeInfo.TypeData.MaxValue do
  begin
    // TODO : Emun 선택값을 한글로 표기 되야할 타입 처리
    if (sTypeName = UpperCase('TPRODUCTGROUP')) or
       (sTypeName = UpperCase('TDirection')) or //방향
       (sTypeName = UpperCase('TBlockType')) or  //차단종류
       (sTypeName = UpperCase('TBlockReasonType')) or //차단사유
       (sTypeName = UpperCase('TDetectEtcType')) or //탐지기타
       (sTypeName = UpperCase('TDATETYPE')) then
    begin
      Values.Add(GetPropertyNameTranslation(GetEnumName(TypeInfo, i)))
    end
    else
    begin
      Values.Add(GetEnumName(TypeInfo, i));
    end;
  end;
end;

procedure TSniperEnumProperty.SetValue(const Value: String);
var
  i: Integer;
begin
  i := GetEnumValue(PropInfo.PropType^, Value);
  if i < 0 then
    raise Exception.Create('Invalid property value');
  SetOrdValue(i);
end;

{ TSetProperty }

function TSniperSetProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

function TSniperSetProperty.GetValue: String;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
  sTypeName : string;
begin
  inherited GetValue;
  Integer(S) := GetOrdValue;
  TypeInfo := GetTypeData(PropInfo.PropType^).CompType^;
  sTypeName := UpperCase(String(TypeInfo.Name));

  Result := '[';
  for i := 0 to 31 do
    if i in S then
    begin
      if Length(Result) <> 1 then
        Result := Result + ',';

      //TODO : Set 타입으로 사용시 Emun 종류중에 한글로 표기 되어야 할 타입 처리
      if (sTypeName = UpperCase('TAnnotation')) or
         (sTypeName = UpperCase('TResultState')) or
         (sTypeName = UpperCase('TBlockType')) or
         (sTypeName = UpperCase('TBlockReasonType')) or
         (sTypeName = UpperCase('TRiskLevel3Type')) or
         (sTypeName = UpperCase('TDetectEtcType')) or //탐지기타
         (sTypeName = UpperCase('TAlertLevelType')) then
        Result := Result + GetPropertyNameTranslation(GetEnumName(TypeInfo, i))
      else
        Result := Result + GetEnumName(TypeInfo, i);
    end;
  Result := Result + ']';
end;

{ TSetElementProperty }

function TSniperSetElementProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paReadOnly];
end;

function TSniperSetElementProperty.GetName: String;
begin
  Result := GetEnumName(GetTypeData(PropInfo.PropType^).CompType^, FElement);
end;

function TSniperSetElementProperty.GetValue: String;
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  if FElement in S then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TSniperSetElementProperty.GetValues;
begin
  inherited;
  Values.Add('False');
  Values.Add('True');
end;

procedure TSniperSetElementProperty.SetValue(const Value: String);
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  if CompareText(Value, 'True') = 0 then
    Include(S, FElement)
  else if CompareText(Value, 'False') = 0 then
    Exclude(S, FElement)
  else
    raise Exception.Create('Invalid property value');

  SetOrdValue(Integer(S));
end;

{ TClassProperty }

function TSniperClassProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

(*
procedure TSniperClassProperty.GetProperties(Proc: TGetPropProc);
var
  I: Integer;
  J: Integer;
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  for I := 0 to PropCount - 1 do
  begin
    J := GetOrdValueAt(I);
    if J <> 0 then
      Components.Add(TComponent(GetOrdValueAt(I)));
  end;
  if Components.Count > 0 then
    GetComponentProperties(Components, tkProperties, Designer, Proc);
end;
*)

function TSniperClassProperty.GetValue: String;
begin
  Result := '(' + String(PropInfo.PropType^.Name) + ')';
end;


{ TComponentProperty }

function TSniperComponentProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

function TSniperComponentProperty.GetValue: String;
var
  c: TComponent;
begin
  c := TComponent(GetOrdValue);
  if c <> nil then
    Result := c.Name
  else
    Result := '';
end;

{ TNameProperty }

function TSniperNameProperty.GetAttributes: TSniperPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

{ TDateProperty }

function TSniperDateProperty.GetValue: String;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then
    Result := ''
  else
    Result := DateToStr(DT);
end;

procedure TSniperDateProperty.SetValue(const Value: String);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToDate(Value);
  SetFloatValue(DT);
end;


{ TTimeProperty }

function TSniperTimeProperty.GetValue: String;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then
    Result := ''
  else
    Result := TimeToStr(DT);
end;

procedure TSniperTimeProperty.SetValue(const Value: String);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToTime(Value);
  SetFloatValue(DT);
end;

{ TPropertyEditorCollection }

constructor TSniperPropertyEditorCollection.Create;
begin
  inherited Create(TSniperPropertyEditorItem);
  FEventEditorItem := -1;
end;

function TSniperPropertyEditorCollection.GetPropertyEditorItem(
  Index: Integer): TSniperPropertyEditorItem;
begin
  Result := TSniperPropertyEditorItem(inherited Items[Index]);
end;

function TSniperPropertyEditorCollection.GetPropertyEditor(PropertyType: PTypeInfo;
  Component: TPersistent; PropertyName: String): Integer;
var
  i: Integer;
  Item: TSniperPropertyEditorItem;
begin
  Result := -1;
  for i := Count - 1 downto 0 do
  begin
    Item := Items[i];
    if (Item.ComponentClass = nil) and (Item.PropertyName = '') and
      (Item.PropertyType = PropertyType) then
      Result := i
    else if (Item.ComponentClass = nil) and (Item.PropertyType = PropertyType) and
      (CompareText(Item.PropertyName, PropertyName) = 0) then
    begin
      Result := i;
      break;
    end
    else if (Component.InheritsFrom(Item.ComponentClass)) and
      (CompareText(Item.PropertyName, PropertyName) = 0) then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TSniperPropertyEditorCollection.Register(PropertyType: PTypeInfo;
  ComponentClass: TClass; const PropertyName: String;
  EditorClass: TSniperPropertyEditorClass);
var
  Item: TSniperPropertyEditorItem;
begin
  Item := TSniperPropertyEditorItem(Add);
  Item.PropertyType := PropertyType;
  Item.ComponentClass := ComponentClass;
  Item.PropertyName := PropertyName;
  Item.EditorClass := EditorClass;
end;

procedure TSniperPropertyEditorCollection.RegisterEventEditor(
  EditorClass: TSniperPropertyEditorClass);
begin
  Register(nil, nil, '', EditorClass);
  FEventEditorItem := Count - 1;
end;

procedure TSniperPropertyEditorCollection.UnRegister(EditorClass: TSniperPropertyEditorClass);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    if Items[i].EditorClass = EditorClass then
      Items[i].Free
    else
      Inc(i);
  end;
end;

{ globals }

function PropertyEditors: TSniperPropertyEditorCollection;
begin
  if FPropertyEditors = nil then
    FPropertyEditors := TSniperPropertyEditorCollection.Create;
  Result := FPropertyEditors;
end;

function TSniperPropertyItem.GetPropertyComponent: TPersistent;
begin
  result := Editor.Component;
end;

function TSniperPropertyItem.GetSubPropertyComponent: TPersistent;
begin
  result := TPersistent(GetOrdProp(FEditor.Component, FEditor.GetPropInfo));
end;

function TSniperPropertyItem.GetSub2PropertyComponent: TPersistent;
begin
  result := FEditor.SubComponent;
end;

initialization
  //기본 타입에 대한 데이터 처리 및 속성지정 위한 프로퍼티 매핑
  PropertyEditors.Register(TypeInfo(Boolean), nil, '', TSniperBooleanProperty);
  PropertyEditors.Register(TypeInfo(TComponentName), nil, 'Name', TSniperNameProperty);
  PropertyEditors.Register(TypeInfo(TDate), nil, '', TSniperDateProperty);
  PropertyEditors.Register(TypeInfo(TTime), nil, '', TSniperTimeProperty);


finalization
  if FPropertyEditors <> nil then
    FPropertyEditors.Free;
  FPropertyEditors := nil;

end.
