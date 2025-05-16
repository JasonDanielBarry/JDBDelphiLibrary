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
  object LabelCoords: TLabel
    Left = 80
    Top = 576
    Width = 20
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'X, Y'
  end
  object PBDrawer2D: TPaintBox
    Left = 0
    Top = 23
    Width = 1231
    Height = 713
    Align = alClient
    PopupMenu = PopupMenuGraphicControls
    ExplicitLeft = 3
    ExplicitTop = 29
  end
  object GridPanelDirectionalPan: TGridPanel
    Left = 1158
    Top = 26
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
      ExplicitTop = 17
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
      Left = 1158
      Top = 0
      Width = 23
      Height = 23
      Action = ActionUpdateGraphics
      Align = alRight
      Caption = 'U'
      Flat = True
      ExplicitLeft = 1155
    end
    object SpeedButtonZoomExtents: TSpeedButton
      Left = 1135
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
      Left = 1112
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
      Left = 1089
      Top = 0
      Width = 23
      Height = 23
      Action = ActionZoomIn
      Align = alRight
      Caption = '+'
      Flat = True
      ExplicitLeft = 1084
    end
    object SpeedButtonAxisSettings: TSpeedButton
      Left = 1066
      Top = 0
      Width = 23
      Height = 23
      Action = ActionEditAxes
      Align = alRight
      AllowAllUp = True
      Caption = 'S'
      Flat = True
      ExplicitLeft = 1059
      ExplicitTop = -6
    end
    object SpeedButtonLayerTable: TSpeedButton
      Left = 1043
      Top = 0
      Width = 23
      Height = 23
      Action = ActionEditLayerTable
      Align = alRight
      AllowAllUp = True
      Caption = 'L'
      Flat = True
      ExplicitLeft = 1059
      ExplicitTop = -6
    end
    object ComboBoxZoomPercent: TComboBox
      AlignWithMargins = True
      Left = 1181
      Top = 0
      Width = 50
      Height = 23
      Hint = 'Set zoom'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
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
  object GridPanelAxisOptions: TGridPanel
    Left = 997
    Top = 29
    Width = 150
    Height = 100
    Anchors = [akTop, akRight]
    BevelKind = bkFlat
    BevelOuter = bvNone
    BevelWidth = 2
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 35.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        ColumnSpan = 3
        Control = LabelXAxis
        Row = 0
      end
      item
        Column = 0
        Control = EditXMin
        Row = 1
      end
      item
        Column = 1
        Control = LabelXBounds
        Row = 1
      end
      item
        Column = 2
        Control = EditXMax
        Row = 1
      end
      item
        Column = 0
        ColumnSpan = 3
        Control = LabelYAxis
        Row = 2
      end
      item
        Column = 0
        Control = EditYMin
        Row = 3
      end
      item
        Column = 1
        Control = LabelYBounds
        Row = 3
      end
      item
        Column = 2
        Control = EditYMax
        Row = 3
      end>
    ParentColor = True
    RowCollection = <
      item
        Value = 25.000000000000000000
      end
      item
        Value = 25.000000000000000000
      end
      item
        Value = 25.000000000000000000
      end
      item
        Value = 25.000000000000000000
      end>
    TabOrder = 2
    Visible = False
    object LabelXAxis: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 141
      Height = 24
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption = 'X-Axis:'
      Layout = tlCenter
      ExplicitWidth = 36
      ExplicitHeight = 15
    end
    object EditXMin: TEdit
      Left = 0
      Top = 24
      Width = 56
      Height = 24
      Align = alClient
      Color = clWhite
      TabOrder = 0
      OnKeyPress = EditAxisValueKeyPress
      ExplicitHeight = 23
    end
    object LabelXBounds: TLabel
      Left = 56
      Top = 24
      Width = 35
      Height = 24
      Align = alClient
      Alignment = taCenter
      Caption = '< x <'
      Layout = tlCenter
      ExplicitWidth = 27
      ExplicitHeight = 15
    end
    object EditXMax: TEdit
      Left = 91
      Top = 24
      Width = 55
      Height = 24
      Align = alClient
      Color = clWhite
      TabOrder = 1
      OnKeyPress = EditAxisValueKeyPress
      ExplicitHeight = 23
    end
    object LabelYAxis: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 48
      Width = 141
      Height = 24
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption = 'Y-Axis:'
      Layout = tlCenter
      ExplicitWidth = 36
      ExplicitHeight = 15
    end
    object EditYMin: TEdit
      Left = 0
      Top = 72
      Width = 56
      Height = 24
      Align = alClient
      Color = clWhite
      TabOrder = 2
      OnKeyPress = EditAxisValueKeyPress
      ExplicitHeight = 23
    end
    object LabelYBounds: TLabel
      Left = 56
      Top = 72
      Width = 35
      Height = 24
      Align = alClient
      Alignment = taCenter
      Caption = '< y <'
      Layout = tlCenter
      ExplicitWidth = 28
      ExplicitHeight = 15
    end
    object EditYMax: TEdit
      Left = 91
      Top = 72
      Width = 55
      Height = 24
      Align = alClient
      Color = clWhite
      TabOrder = 3
      OnKeyPress = EditAxisValueKeyPress
      ExplicitHeight = 23
    end
  end
  object CheckListBoxLayerTable: TCheckListBox
    Left = 850
    Top = 32
    Width = 125
    Height = 63
    Anchors = [akTop, akRight]
    BevelInner = bvNone
    BevelOuter = bvNone
    ItemHeight = 17
    Items.Strings = (
      'Layer1'
      'Layer2'
      'Layer3')
    ParentColor = True
    TabOrder = 3
    Visible = False
    OnClick = CheckListBoxLayerTableClick
    OnDblClick = CheckListBoxLayerTableDblClick
  end
  object ActionListControls: TActionList
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
    object ActionUpdateGraphics: TAction
      Category = 'Graphics'
      Caption = '&Update Graphics'
      Hint = 'Update the graphic'
      OnExecute = ActionUpdateGraphicsExecute
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
    object ActionEditAxes: TAction
      Category = 'Axes'
      Caption = 'Edit Axes'
      Hint = 'Edit axis boundaries'
      OnExecute = ActionEditAxesExecute
    end
    object ActionEditLayerTable: TAction
      Category = 'Layers'
      Caption = 'Edit Layer Table'
      Hint = 'Toggle layers visible/hidden'
      OnExecute = ActionEditLayerTableExecute
    end
    object ActionShowHideControls: TAction
      Caption = 'Show Toolbar'
      OnExecute = ActionShowHideControlsExecute
    end
  end
  object PopupMenuGraphicControls: TPopupMenu
    Left = 400
    Top = 320
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
      Action = ActionUpdateGraphics
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
    object N3: TMenuItem
      Caption = '-'
    end
    object EditAxes1: TMenuItem
      Action = ActionEditAxes
    end
    object EditLayerTable1: TMenuItem
      Action = ActionEditLayerTable
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object ShowToolbar1: TMenuItem
      Action = ActionShowHideControls
    end
  end
end
