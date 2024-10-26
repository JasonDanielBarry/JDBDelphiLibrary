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
                //test if a cell's value is a double
                    function checkCellIsDouble(colIn, rowIn : integer) : boolean;
                //test if a row is empty
                    function rowIsEmpty(rowIndexIn : integer) : boolean;
            public
                //check cell is empty string
                    function cellIsEmpty(colIn, rowIn : integer) : boolean;
                //clear a grid of its contents
                    //clear a cell
                        procedure clearCell(colIn, rowIn : integer);
                    //clear column
                        procedure clearColumn(const colIndexIn : integer);
                        procedure clearColumns(const startColIndexIn : integer);
                    //clear row
                        procedure clearRow(rowIndexIn : integer);
                        procedure clearRows(const startRowIndexIn : integer);
                    procedure clearCells(const startColIndexIn, startRowIndexIn : integer);
                    procedure clearAllCells();
                //create border
                    procedure createBorder( const edgeWidthIn   : integer;
                                            const colourIn      : TColor    );

                    procedure editBorder(   const edgeWidthIn   : integer;
                                            const colourIn      : TColor    ); overload;
                //row deletion

                    //delete a grid row
                        procedure deleteRow(rowIndexIn : integer);
                    //delete an empty row
                        procedure deleteEmptyRow(rowIndexIn : integer);
                    //delete all empty rows
                        procedure deleteAllEmptyRows();
                //row insertion
                    //add row
                        procedure addRow();
                //test if a cell's value is a double and clear it if it is not
                    function isCellDouble(colIn, rowIn : integer) : boolean;
                //get the value of a cell as a double
                    function cellToDouble(colIn, rowIn : integer) : double;
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

        //test if a cell's value is a double
            function TStringGridHelper.checkCellIsDouble(colIn, rowIn : integer) : boolean;
                var
                    cellIsDoubleOut : boolean;
                    dummy           : double;
                begin
                    cellIsDoubleOut := TryStrToFloat(Cells[colIn, rowIn], dummy);

                    result := cellIsDoubleOut;
                end;

        //test if a row is empty
            function TStringGridHelper.rowIsEmpty(rowIndexIn : integer) : boolean;
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
            function TStringGridHelper.cellIsEmpty(colIn, rowIn : integer) : boolean;
                begin
                    result := (cells[colIn, rowIn] = '');
                end;

        //clear a grid of its contents
            //clear a cell
                procedure TStringGridHelper.clearCell(colIn, rowIn : integer);
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

                procedure TStringGridHelper.clearColumns(const startColIndexIn : integer);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := startColIndexIn to (ColCount - 1) do
                            clearColumn(colIndex);
                    end;

            //clear a row's content
                procedure TStringGridHelper.clearRow(rowIndexIn : integer);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := 0 to (ColCount - 1) do
                            clearCell( colIndex, rowIndexIn );
                    end;

                procedure TStringGridHelper.clearRows(const startRowIndexIn : integer);
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := startRowIndexIn to (RowCount - 1) do
                            clearRow(rowIndex);
                    end;

            procedure TStringGridHelper.clearCells(const startColIndexIn, startRowIndexIn : integer);
                var
                    c, r : integer;
                begin
                    for c := startColIndexIn to (ColCount - 1) do
                        for r := startRowIndexIn to (RowCount - 1) do
                            clearCell(c, r);
                end;

            procedure TStringGridHelper.clearAllCells();
                begin
                    clearCells(0, 0);
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
                procedure TStringGridHelper.deleteRow(rowIndexIn : integer);
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
                procedure TStringGridHelper.deleteEmptyRow(rowIndexIn : integer);
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
            //add row
                procedure TStringGridHelper.addRow();
                    begin
                        RowCount := RowCount + 1;
                    end;

        //test if a cell's value is a double and clear it if it is not
            function TStringGridHelper.isCellDouble(colIn, rowIn : integer) : boolean;
                var
                    cellIsDoubleOut : boolean;
                begin
                    cellIsDoubleOut := checkCellIsDouble(colIn, rowIn);

                    if ( (cellIsDoubleOut = False) AND (Cells[colIn, rowIn] <> '') ) then
                        begin
                            //if conversion to double fails return error message
                                Application.MessageBox('Value entered is not real number', 'Invalid Input', MB_OK);
                                cellIsDoubleOut := False;
                                cells[colIn, rowIn] := '';
                        end;

                    result := cellIsDoubleOut;
                end;

        //get the value of a cell as a double
            function TStringGridHelper.cellToDouble(colIn, rowIn : integer) : double;
                begin
                    if (checkCellIsDouble(colIn, rowIn) = True) then
                        result := cells[colIn, rowIn].ToDouble()
                    else
                        result := 0;
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
