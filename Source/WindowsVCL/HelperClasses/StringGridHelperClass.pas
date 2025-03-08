unit StringGridHelperClass;

interface

    uses
        Winapi.Windows,
        System.SysUtils, system.Math, system.Types, system.UITypes,
        vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Grids;

    type
        TStringGridHelper = class helper for TStringGrid
            private
                //border panel name
                    function borderPanelName() : string;
                //get border panel
                    function getBorderPanel() : TPanel;
                //edit a border's properties
                    procedure editBorder(   const edgeWidthIn       : integer;
                                            const colourIn          : TColor;
                                            var borderPanelInOut    : TPanel    ); overload;
                //border adjustment used for sizing the grid
                    function borderAdjustment() : integer;
                //test if a row is empty
                    function rowIsEmpty(const rowIndexIn : integer) : boolean;
            public
                //check cell is empty string
                    function cellIsEmpty(const colIn, rowIn : integer) : boolean;
                //clear a grid of its contents
                    //clear a cell
                        procedure clearCell(const colIn, rowIn : integer);
                    //clear column
                        procedure clearColumn(const colIndexIn : integer);
                        procedure clearColumns(const startColIndexIn : integer = 0);
                    //clear row
                        procedure clearRow(const rowIndexIn : integer);
                        procedure clearRows(const startRowIndexIn : integer = 0);
                    procedure clearCells(const startColIndexIn : integer = 0; const startRowIndexIn : integer = 0);
                //create border
                    procedure createBorder( const edgeWidthIn   : integer;
                                            const colourIn      : TColor    );
                    procedure editBorder(   const edgeWidthIn   : integer;
                                            const colourIn      : TColor    ); overload;
                //row deletion
                    //delete a grid row
                        procedure deleteRow(const rowIndexIn : integer);
                    //delete an empty row
                        procedure deleteEmptyRow(const rowIndexIn : integer);
                    //delete all empty rows
                        procedure deleteAllEmptyRows();
                //add columns and rows
                    procedure addColumn();
                    procedure addRow();
                //get the value of a cell as a double
                    function tryCellToDouble(const colIn, rowIn : integer; out valueOut : double) : boolean;
                    function cellToDouble(const colIn, rowIn : integer) : double;
                //resize the grid to its minimum extents
                    procedure minHeight();
                    procedure minWidth();
                    procedure minSize();
        end;

implementation

    const
        BORDER_PANEL : string = 'BorderPanel';

    //private
        //border panel name
            function TStringGridHelper.borderPanelName() : string;
                begin
                    result := self.Name + 'BorderPanel';
                end;

        //free border panel
            function TStringGridHelper.getBorderPanel() : TPanel;
                var
                    i : integer;
                begin
                    for i := 0 to (self.Parent.ComponentCount - 1) do
                        if ( Components[i].Name = borderPanelName() ) then
                            begin
                                result := TPanel(Components[i]);

                                exit();
                            end;
                end;

        //border adjustment used for sizing the grid
            function TStringGridHelper.borderAdjustment() : integer;
                begin
                    if Self.BorderStyle = bsSingle then
                        result := 2
                    else
                        result := 0;
                end;

        //edit a border's properties
            procedure TStringGridHelper.editBorder( const edgeWidthIn       : integer;
                                                    const colourIn          : TColor;
                                                    var borderPanelInOut    : TPanel    );
                begin
                    //resize the grid for border
                        self.minSize();

                        self.Height := self.Height - 2;
                        self.Width  := self.Width - 2;

                    borderPanelInOut.Color := colourIn;

                    borderPanelInOut.Height  := self.Height + (2 * edgeWidthIn);
                    borderPanelInOut.Width   := self.Width + (2 * edgeWidthIn);

                    borderPanelInOut.Left    := self.Left - edgeWidthIn;
                    borderPanelInOut.Top     := self.Top - edgeWidthIn;

                    borderPanelInOut.BringToFront();
                    self.BringToFront();
                end;

        //test if a row is empty
            function TStringGridHelper.rowIsEmpty(const rowIndexIn : integer) : boolean;
                var
                    colIndex : integer;
                begin
                    for colIndex := 0 to (ColCount - 1) do
                        begin
                            result := cellIsEmpty(colIndex, rowIndexIn);

                            if (result = false) then
                                break;
                        end;
                end;

    //public
        //check cell is empty string
            function TStringGridHelper.cellIsEmpty(const colIn, rowIn : integer) : boolean;
                begin
                    result := (cells[colIn, rowIn] = '');
                end;

        //clear a grid of its contents
            //clear a cell
                procedure TStringGridHelper.clearCell(const colIn, rowIn : integer);
                    begin
                        cells[colIn, rowIn] := '';
                    end;

            //clear column
                procedure TStringGridHelper.clearColumn(const colIndexIn : integer);
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := 0 to (RowCount - 1) do
                            clearCell( colIndexIn, rowIndex );
                    end;

                procedure TStringGridHelper.clearColumns(const startColIndexIn : integer = 0);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := startColIndexIn to (ColCount - 1) do
                            clearColumn(colIndex);
                    end;

            //clear a row's content
                procedure TStringGridHelper.clearRow(const rowIndexIn : integer);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := 0 to (ColCount - 1) do
                            clearCell( colIndex, rowIndexIn );
                    end;

                procedure TStringGridHelper.clearRows(const startRowIndexIn : integer = 0);
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := startRowIndexIn to (RowCount - 1) do
                            clearRow(rowIndex);
                    end;

            procedure TStringGridHelper.clearCells(const startColIndexIn : integer = 0; const startRowIndexIn : integer = 0);
                var
                    c, r : integer;
                begin
                    for c := startColIndexIn to (ColCount - 1) do
                        for r := startRowIndexIn to (RowCount - 1) do
                            clearCell(c, r);
                end;

        //create border
            procedure TStringGridHelper.createBorder(   const edgeWidthIn   : integer;
                                                        const colourIn      : TColor    );
                var
                    borderPanel : TPanel;
                begin
                    //get rid of grid border
                        self.BevelInner := TBevelCut.bvNone;
                        self.BevelKind := TBevelKind.bkNone;
                        self.BevelOuter := TBevelCut.bvNone;
                        self.BorderStyle := bsNone;

                    //create the grid for border
                        borderPanel := TPanel.Create(self);
                        borderPanel.Parent := self.Parent;
                        borderPanel.Name := borderPanelName();
                        borderpanel.StyleElements := [seFont, {seClient, }seBorder];

                    //prime panel to be a border
                        borderPanel.ParentBackground := False;
                        borderPanel.ParentColor := False;
                        borderPanel.BevelInner := TBevelCut.bvNone;
                        borderPanel.BevelOuter := TBevelCut.bvNone;
                        borderPanel.BevelKind := TBevelKind.bkNone;
                        borderPanel.BorderStyle := bsNone;

                    editBorder( edgeWidthIn,
                                colourIn,
                                borderPanel );
                end;

            procedure TStringGridHelper.editBorder( const edgeWidthIn   : integer;
                                                    const colourIn      : TColor    );
                var
                    gridsBorderPanel : TPanel;
                begin
                    gridsBorderPanel := getBorderPanel();

                    editBorder( edgeWidthIn,
                                colourIn,
                                gridsBorderPanel);
                end;

        //row deletion
            //delete a grid row
                procedure TStringGridHelper.deleteRow(const rowIndexIn : integer);
                    var
                        row, col : integer;
                    begin
                        if (rowIndexIn < rowCount) then
                            begin
                                clearRow(rowIndexIn);

                                for row := rowIndexIn to (Self.RowCount - 2) do
                                    for col := 0 to (Self.ColCount - 1) do
                                        begin
                                            //row above accepts row below's contents
                                                Self.cells[col, row] := Self.cells[col, row + 1];
                                        end;

                                //shorten the row count by 1
                                    Self.RowCount := Self.RowCount - 1;
                            end;

                        self.minSize();
                    end;
    
            //delete an empty row
                procedure TStringGridHelper.deleteEmptyRow(const rowIndexIn : integer);
                    begin
                        if (rowIsEmpty(rowIndexIn)) then
                            deleteRow(rowIndexIn);
                    end;

            //delete all empty rows
                procedure TStringGridHelper.deleteAllEmptyRows();
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := (RowCount - 1) downto 0 do
                            if (rowIndex < rowCount) then
                                deleteEmptyRow(rowIndex);
                    end;

        //row insertion
            procedure TStringGridHelper.addColumn();
                begin
                    ColCount := ColCount + 1;
                end;

            procedure TStringGridHelper.addRow();
                begin
                    RowCount := RowCount + 1;
                end;

        //get the value of a cell as a double
            function TStringGridHelper.tryCellToDouble(const colIn, rowIn : integer; out valueOut : double) : boolean;
                begin
                    if NOT(TryStrToFloat( cells[colIn, rowIn], valueOut ) ) then
                        begin
                            valueOut := 0;
                            exit( False );
                        end;

                    result := True;
                end;

            function TStringGridHelper.cellToDouble(const colIn, rowIn : integer) : double;
                var
                    valueOut : double;
                begin
                    tryCellToDouble( colIn, rowIn, valueOut );

                    result := valueOut;
                end;

        //resize the grid to its minimum extents
            procedure TStringGridHelper.minHeight();
                var
                    row, gridHeight, sumRowHeights : integer;
                begin
                    //grid height
                        //calculate the sum of the row heights
                            sumRowHeights := 0;
                            for	row := 0 to (Self.RowCount - 1) do
                                begin
                                    sumRowHeights := sumRowHeights + Self.RowHeights[row];
                                end;

                        //add the number of rows + 1 to the row heights sum
                            gridHeight := sumRowHeights + (Self.RowCount + 1 + borderAdjustment());

                    //assign to grid
                        Self.Height := gridHeight;
                end;

            procedure TStringGridHelper.minWidth();
                var
                    col, gridWidth, sumColWidths : integer;
                begin
                    //grid width
                        //calculate the sum of the col widths
                            sumColWidths := 0;
                            for	col := 0 to (Self.ColCount - 1)	do
                                begin
                                    sumColWidths := sumColWidths + Self.ColWidths[col];
                                end;

                        //add the number of cols + 1 to the col widths sum
                            gridWidth := sumColWidths + (Self.ColCount + 1 + borderAdjustment);

                    //assign to grid
                        Self.Width 	:= gridWidth;
                end;

            procedure TStringGridHelper.minSize();
                begin
                    minHeight();
                    minWidth();
                end;

end.
