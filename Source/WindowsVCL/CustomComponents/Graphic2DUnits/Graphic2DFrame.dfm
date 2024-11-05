object CustomGraphic2D: TCustomGraphic2D
  Left = 0
  Top = 0
  Width = 1231
  Height = 736
  TabOrder = 0
  OnResize = FrameResize
  DesignSize = (
    1231
    736)
  object SkPaintBoxGraphic: TSkPaintBox
    Left = 0
    Top = 25
    Width = 1231
    Height = 711
    Align = alClient
    OnMouseEnter = SkPaintBoxGraphicMouseEnter
    OnMouseLeave = SkPaintBoxGraphicMouseLeave
    OnDraw = SkPaintBoxGraphicDraw
    ExplicitTop = 31
  end
  object labelCoords: TLabel
    Left = 80
    Top = 576
    Width = 20
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'X, Y'
  end
  object GridPanelDirectionalPan: TGridPanel
    Left = 1153
    Top = 29
    Width = 70
    Height = 70
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333310000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = SpeedButtonShiftLeft
        Row = 1
      end
      item
        Column = 2
        Control = SpeedButtonShiftRight
        Row = 1
      end
      item
        Column = 1
        Control = SpeedButtonShiftUp
        Row = 0
      end
      item
        Column = 1
        Control = SpeedButtonShiftDown
        Row = 2
      end
      item
        Column = 1
        Control = SpeedButtonCentre
        Row = 1
      end>
    ParentColor = True
    RowCollection = <
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333310000
      end>
    TabOrder = 0
    object SpeedButtonShiftLeft: TSpeedButton
      Left = 0
      Top = 23
      Width = 23
      Height = 24
      Align = alClient
      Anchors = []
      Caption = '<'
      Flat = True
      OnClick = SpeedButtonShiftLeftClick
      ExplicitLeft = -6
      ExplicitTop = 19
      ExplicitWidth = 25
      ExplicitHeight = 25
    end
    object SpeedButtonShiftRight: TSpeedButton
      Left = 47
      Top = 23
      Width = 23
      Height = 24
      Align = alClient
      Anchors = []
      Caption = '>'
      Flat = True
      OnClick = SpeedButtonShiftRightClick
      ExplicitLeft = 608
      ExplicitTop = 8
      ExplicitHeight = 22
    end
    object SpeedButtonShiftUp: TSpeedButton
      Left = 23
      Top = 0
      Width = 24
      Height = 23
      Align = alClient
      Anchors = []
      Caption = '/\'
      Flat = True
      OnClick = SpeedButtonShiftUpClick
      ExplicitLeft = 1028
      ExplicitTop = -6
      ExplicitWidth = 25
      ExplicitHeight = 25
    end
    object SpeedButtonShiftDown: TSpeedButton
      Left = 23
      Top = 47
      Width = 24
      Height = 23
      Align = alClient
      Anchors = []
      Caption = '\/'
      Flat = True
      OnClick = SpeedButtonShiftDownClick
      ExplicitLeft = 608
      ExplicitTop = 8
      ExplicitWidth = 23
      ExplicitHeight = 22
    end
    object SpeedButtonCentre: TSpeedButton
      Left = 23
      Top = 23
      Width = 24
      Height = 24
      Align = alClient
      Anchors = []
      Caption = 'C'
      Flat = True
      OnClick = SpeedButtonCentreClick
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 25
      ExplicitHeight = 25
    end
  end
  object PanelZoom: TPanel
    Left = 0
    Top = 0
    Width = 1231
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object SpeedButtonUpdateGeometry: TSpeedButton
      Left = 1155
      Top = 0
      Width = 25
      Height = 25
      Align = alRight
      Caption = 'U'
      Flat = True
      OnClick = SpeedButtonUpdateGeometryClick
      ExplicitLeft = 0
    end
    object SpeedButtonZoomExtents: TSpeedButton
      Left = 1130
      Top = 0
      Width = 25
      Height = 25
      Align = alRight
      Caption = 'E'
      Flat = True
      OnClick = SpeedButtonZoomExtentsClick
      ExplicitLeft = 0
    end
    object SpeedButtonZoomOut: TSpeedButton
      Left = 1105
      Top = 0
      Width = 25
      Height = 25
      Align = alRight
      Caption = '-'
      Flat = True
      OnClick = SpeedButtonZoomOutClick
      ExplicitTop = -6
    end
    object SpeedButtonZoomIn: TSpeedButton
      Left = 1080
      Top = 0
      Width = 25
      Height = 25
      Align = alRight
      Caption = '+'
      Flat = True
      OnClick = SpeedButtonZoomInClick
      ExplicitTop = -6
    end
    object ComboBoxZoomPercent: TComboBox
      AlignWithMargins = True
      Left = 1180
      Top = 0
      Width = 50
      Height = 23
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 1
      Margins.Bottom = 0
      Align = alRight
      TabOrder = 0
      Text = '100'
      OnChange = ComboBoxZoomPercentChange
      Items.Strings = (
        '10'
        '20'
        '25'
        '50'
        '75'
        '100'
        '125'
        '150'
        '200'
        '250'
        '300'
        '400'
        '500')
    end
  end
end
