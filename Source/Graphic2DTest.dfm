object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Graphic2D'
  ClientHeight = 719
  ClientWidth = 1512
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object JDBGraphic2D1: TJDBGraphic2D
    Left = 0
    Top = 25
    Width = 1512
    Height = 694
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    Caption = 'JDBGraphic2D1'
    ParentColor = True
    ParentShowHint = False
    ShowCaption = False
    ShowHint = True
    TabOrder = 0
    OnUpdateGraphics = JDBGraphic2D1UpdateGraphics
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 1512
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
