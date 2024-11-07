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
    Top = 23
    Width = 1231
    Height = 713
    Align = alClient
    PopupMenu = PopupMenuGraphicControls
    OnMouseEnter = SkPaintBoxGraphicMouseEnter
    OnMouseLeave = SkPaintBoxGraphicMouseLeave
    OnDraw = SkPaintBoxGraphicDraw
    ExplicitTop = 26
    ExplicitHeight = 711
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
      Action = ActionPanLeft
      Align = alClient
      Anchors = []
      Caption = '<'
      Flat = True
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
      Action = ActionPanRight
      Align = alClient
      Anchors = []
      Caption = '>'
      Flat = True
      ExplicitLeft = 608
      ExplicitTop = 8
      ExplicitHeight = 22
    end
    object SpeedButtonShiftUp: TSpeedButton
      Left = 23
      Top = 0
      Width = 24
      Height = 23
      Action = ActionPanUp
      Align = alClient
      Anchors = []
      Caption = '/\'
      Flat = True
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
      Action = ActionPanDown
      Align = alClient
      Anchors = []
      Caption = '\/'
      Flat = True
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
      Action = ActionRecentre
      Align = alClient
      Anchors = []
      Caption = 'C'
      Flat = True
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 25
      ExplicitHeight = 25
    end
  end
  object PanelGraphicControls: TPanel
    Left = 0
    Top = 0
    Width = 1231
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object SpeedButtonUpdateGeometry: TSpeedButton
      Left = 1157
      Top = 0
      Width = 23
      Height = 23
      Action = ActionUpdateGeometry
      Align = alRight
      Caption = 'U'
      Flat = True
      ExplicitLeft = 1155
    end
    object SpeedButtonZoomExtents: TSpeedButton
      Left = 1134
      Top = 0
      Width = 23
      Height = 23
      Action = ActionZoomExtents
      Align = alRight
      Caption = 'E'
      Flat = True
      ExplicitLeft = 1132
    end
    object SpeedButtonZoomOut: TSpeedButton
      Left = 1111
      Top = 0
      Width = 23
      Height = 23
      Action = ActionZoomOut
      Align = alRight
      Caption = '-'
      Flat = True
      ExplicitLeft = 1109
    end
    object SpeedButtonZoomIn: TSpeedButton
      Left = 1088
      Top = 0
      Width = 23
      Height = 23
      Action = ActionZoomIn
      Align = alRight
      Caption = '+'
      Flat = True
      ExplicitLeft = 1084
    end
    object ComboBoxZoomPercent: TComboBox
      AlignWithMargins = True
      Left = 1180
      Top = 0
      Width = 50
      Height = 23
      Hint = 'Set zoom'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 1
      Margins.Bottom = 0
      Align = alRight
      TabOrder = 0
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
        '500'
        '750'
        '1000'
        '1500')
    end
  end
  object ActionList1: TActionList
    Left = 728
    Top = 256
    object ActionZoomIn: TAction
      Category = 'Zoom'
      Caption = 'Zoom &In'
      Hint = 'Zoom in'
      OnExecute = ActionZoomInExecute
    end
    object ActionZoomOut: TAction
      Category = 'Zoom'
      Caption = 'Zoom &Out'
      Hint = 'Zoom out'
      OnExecute = ActionZoomOutExecute
    end
    object ActionZoomExtents: TAction
      Category = 'Zoom'
      Caption = 'Zoom &Extents'
      Hint = 'Reset zoom to graphic extents'
      ShortCut = 16453
      OnExecute = ActionZoomExtentsExecute
    end
    object ActionRecentre: TAction
      Category = 'Zoom'
      Caption = 'Recen&tre'
      Hint = 'Recentre graphic'
      ShortCut = 16468
      OnExecute = ActionRecentreExecute
    end
    object ActionUpdateGeometry: TAction
      Category = 'Geometry'
      Caption = '&Update Geometry'
      Hint = 'Update the graphic'#39's geometry'
      OnExecute = ActionUpdateGeometryExecute
    end
    object ActionPanLeft: TAction
      Category = 'Pan'
      Caption = 'Pan &Left'
      Hint = 'Shift graphic left'
      OnExecute = ActionPanLeftExecute
    end
    object ActionPanRight: TAction
      Category = 'Pan'
      Caption = 'Pan &Right'
      Hint = 'Shift graphic right'
      OnExecute = ActionPanRightExecute
    end
    object ActionPanUp: TAction
      Category = 'Pan'
      Caption = 'Pan U&p'
      Hint = 'Shift graphic up'
      OnExecute = ActionPanUpExecute
    end
    object ActionPanDown: TAction
      Category = 'Pan'
      Caption = 'Pan &Down'
      Hint = 'Shift graphic down'
      OnExecute = ActionPanDownExecute
    end
  end
  object PopupMenuGraphicControls: TPopupMenu
    Left = 264
    Top = 376
    object ZoomExtents1: TMenuItem
      Action = ActionZoomExtents
    end
    object ZoomIn1: TMenuItem
      Action = ActionZoomIn
    end
    object ZoomOut1: TMenuItem
      Action = ActionZoomOut
    end
    object Recentre1: TMenuItem
      Action = ActionRecentre
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object UpdateGeometry1: TMenuItem
      Action = ActionUpdateGeometry
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PanLeft1: TMenuItem
      Action = ActionPanUp
    end
    object PanRight1: TMenuItem
      Action = ActionPanLeft
    end
    object PanDown1: TMenuItem
      Action = ActionPanDown
    end
    object PanRight2: TMenuItem
      Action = ActionPanRight
    end
  end
end
