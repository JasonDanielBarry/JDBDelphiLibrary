object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Graphic2D'
  ClientHeight = 673
  ClientWidth = 1556
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnShow = FormShow
  TextHeight = 15
  object JDBGraphic2D1: TJDBGraphic2D
    Left = 0
    Top = 25
    Width = 1556
    Height = 648
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    Caption = 'JDBGraphic2D1'
    ParentColor = True
    ParentShowHint = False
    ShowCaption = False
    ShowHint = True
    TabOrder = 0
    OnUpdateGeometry = JDBGraphic2D1UpdateGeometry
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 1556
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object LabelSelectGraphic: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 78
      Height = 25
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alLeft
      Caption = 'Select Graphic:'
      Layout = tlCenter
      ExplicitHeight = 15
    end
    object ComboBox1: TComboBox
      Left = 88
      Top = 0
      Width = 125
      Height = 23
      Align = alLeft
      Style = csDropDownList
      Color = clWhite
      TabOrder = 0
      OnChange = ComboBox1Change
      Items.Strings = (
        'Blue Box'
        'XY Graphs'
        'Fin Plate'
        'Soil Nail Wall'
        'Bending Beam')
    end
  end
end
