unit CustomStringGridClass;

interface

    uses
        Winapi.Windows, Winapi.Messages,
        System.SysUtils, system.Math, system.Types, system.UITypes, System.Classes,
        vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Grids, Vcl.Themes;

    type
        TJDBStringGrid = class(TStringGrid)
            private
                var
                    onCellChangedEventAwake : boolean;
                    borderPanel             : TPanel;
                    cellContentsMatrix      : TArray< TArray< string > >;
                    onCellChangedEvent      : TNotifyEvent;
                //size cell contents
                    procedure sizeCellContents();
                    function cellContentsIdenticalToGrid() : boolean;
                //border adjustment used for sizing the grid
                    function borderAdjustment() : integer;
            protected
                //process windows messages
                    procedure wndProc(var messageInOut : TMessage); override;
            public
                //constructor
                    constructor create(AOwner: TComponent); override;
                //destructor
                    destructor destroy(); override;
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
                    procedure setBorderProperties(  const edgeWidthIn   : integer;
                                                    const colourIn      : TColor    );
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
                //get the value of a cell
                    //as an integer
                        function tryCellToInteger(const colIn, rowIn : integer; out valueOut : integer) : boolean;
                        function cellToInteger(const colIn, rowIn : integer) : integer;
                    //as a double
                        function tryCellToDouble(const colIn, rowIn : integer; out valueOut : double) : boolean;
                        function cellToDouble(const colIn, rowIn : integer) : double;
                //test if a col/row is empty
                    function colIsEmpty(const colIndexIn : integer) : boolean;
                    function rowIsEmpty(const rowIndexIn : integer) : boolean;
                //update cell contents matrix
                    procedure updateCellContentsMatrix(const resizeArray : boolean = True);
                //resize the grid to its minimum extents
                    procedure minHeight();
                    procedure minWidth();
                    procedure minSize();
            published
                property OnCellChanged : TNotifyEvent read onCellChangedEvent write onCellChangedEvent;
        end;

implementation

    const
        BORDER_PANEL : string = 'BorderPanel';

    //private
        //size cell contents
            procedure TJDBStringGrid.sizeCellContents();
                var
                    equalCols, equalRows            : boolean;
                    c, arrayColCount, arrayRowCount : integer;
                begin
                    //check the size of the cell contents matrix
                        arrayColCount := length( cellContentsMatrix );

                        if ( 0 < arrayColCount ) then
                            arrayRowCount := length( cellContentsMatrix[0] )
                        else
                            arrayRowCount := 0;

                    equalCols := arrayColCount = ColCount;
                    equalRows := arrayRowCount = RowCount;

                    if ( equalCols AND equalRows ) then
                        exit();

                    SetLength( cellContentsMatrix, ColCount );

                    for c := 0 to (ColCount - 1) do
                        SetLength( cellContentsMatrix[c], RowCount );
                end;

            function TJDBStringGrid.cellContentsIdenticalToGrid() : boolean;
                var
                    allStringsIdentical : boolean;
                    c, r                : integer;
                begin
                    allStringsIdentical := True;

                    //make sure the cell contents array is identical to the grid
                        sizeCellContents();

                    //check if all the strings are equal
                        for c := 0 to (ColCount - 1) do
                            for r := 0 to (RowCount - 1) do
                                if ( cells[c, r] <> cellContentsMatrix[c, r] ) then
                                    begin
                                        allStringsIdentical := False;
                                        break;
                                    end;

                    //update the cell contents if there are difference
                        if NOT( allStringsIdentical ) then
                            updateCellContentsMatrix( False );

                    result := allStringsIdentical;
                end;

        //border adjustment used for sizing the grid
            function TJDBStringGrid.borderAdjustment() : integer;
                begin
                    if Self.BorderStyle = bsSingle then
                        result := 2
                    else
                        result := 0;
                end;

    //protected
        //process windows messages
            procedure TJDBStringGrid.wndProc(var messageInOut : TMessage);
                begin
                    case ( messageInOut.Msg ) of
                        WM_SETFOCUS:
                            begin
                                var triggerOnCellChangedEvent : boolean;

                                triggerOnCellChangedEvent := NOT( cellContentsIdenticalToGrid() ) AND Assigned( onCellChangedEvent ) AND onCellChangedEventAwake;

                                if ( triggerOnCellChangedEvent ) then
                                    begin
                                        onCellChangedEvent( self );
                                        onCellChangedEventAwake := False;
                                    end;
                            end;

                        WM_MOUSEACTIVATE, WM_KEYDOWN:
                            onCellChangedEventAwake := True;
                    end;

                    inherited wndProc( messageInOut );
                end;

    //public
        //constructor
            constructor TJDBStringGrid.create(AOwner: TComponent);
                begin
                    inherited Create( AOwner );

                    onCellChangedEventAwake := False;

                    borderPanel := TPanel.Create( self );
                end;

        //destructor
            destructor TJDBStringGrid.destroy();
                begin
                    FreeAndNil( borderPanel );

                    inherited destroy();
                end;

        //check cell is empty string
            function TJDBStringGrid.cellIsEmpty(const colIn, rowIn : integer) : boolean;
                begin
                    result := (cells[colIn, rowIn] = '');
                end;

        //clear a grid of its contents
            //clear a cell
                procedure TJDBStringGrid.clearCell(const colIn, rowIn : integer);
                    begin
                        cells[colIn, rowIn] := '';
                    end;

            //clear column
                procedure TJDBStringGrid.clearColumn(const colIndexIn : integer);
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := 0 to (RowCount - 1) do
                            clearCell( colIndexIn, rowIndex );
                    end;

                procedure TJDBStringGrid.clearColumns(const startColIndexIn : integer = 0);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := startColIndexIn to (ColCount - 1) do
                            clearColumn(colIndex);
                    end;

            //clear a row's content
                procedure TJDBStringGrid.clearRow(const rowIndexIn : integer);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := 0 to (ColCount - 1) do
                            clearCell( colIndex, rowIndexIn );
                    end;

                procedure TJDBStringGrid.clearRows(const startRowIndexIn : integer = 0);
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := startRowIndexIn to (RowCount - 1) do
                            clearRow(rowIndex);
                    end;

            procedure TJDBStringGrid.clearCells(const startColIndexIn : integer = 0; const startRowIndexIn : integer = 0);
                var
                    c, r : integer;
                begin
                    for c := startColIndexIn to (ColCount - 1) do
                        for r := startRowIndexIn to (RowCount - 1) do
                            clearCell(c, r);
                end;

        //create border
            procedure TJDBStringGrid.setBorderProperties(   const edgeWidthIn   : integer;
                                                            const colourIn      : TColor    );
                begin
                    //prime grid for border
                        //get rid of grid border
                            self.BevelInner     := TBevelCut.bvNone;
                            self.BevelKind      := TBevelKind.bkNone;
                            self.BevelOuter     := TBevelCut.bvNone;
                            self.BorderStyle    := bsNone;

                        //size
                            self.minSize();
                            self.Height := self.Height - 2;
                            self.Width  := self.Width - 2;

                        //create the grid for border
                            borderPanel.Parent := self.Parent;
                            borderpanel.StyleElements := [seFont, {seClient, }seBorder];

                    //prime panel to be a border
                        //vcl properties
                            borderPanel.ParentBackground    := False;
                            borderPanel.ParentColor         := False;
                            borderPanel.BevelInner          := TBevelCut.bvNone;
                            borderPanel.BevelOuter          := TBevelCut.bvNone;
                            borderPanel.BevelKind           := TBevelKind.bkNone;
                            borderPanel.BorderStyle         := bsNone;

                        //colour
                            borderPanel.Color := TStyleManager.ActiveStyle.GetSystemColor( colourIn );

                        //size
                            borderPanel.Height  := self.Height + (2 * edgeWidthIn);
                            borderPanel.Width   := self.Width + (2 * edgeWidthIn);

                        //position
                            borderPanel.Left    := self.Left - edgeWidthIn;
                            borderPanel.Top     := self.Top - edgeWidthIn;

                    borderPanel.BringToFront();
                    self.BringToFront();
                end;

        //row deletion
            //delete a grid row
                procedure TJDBStringGrid.deleteRow(const rowIndexIn : integer);
                    var
                        row, col : integer;
                    begin
                        if (rowIndexIn < rowCount) then
                            begin
                                clearRow( rowIndexIn );

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
                procedure TJDBStringGrid.deleteEmptyRow(const rowIndexIn : integer);
                    begin
                        if (rowIsEmpty(rowIndexIn)) then
                            deleteRow(rowIndexIn);
                    end;

            //delete all empty rows
                procedure TJDBStringGrid.deleteAllEmptyRows();
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := (RowCount - 1) downto 0 do
                            if (rowIndex < rowCount) then
                                deleteEmptyRow(rowIndex);
                    end;

        //row insertion
            procedure TJDBStringGrid.addColumn();
                begin
                    ColCount := ColCount + 1;
                end;

            procedure TJDBStringGrid.addRow();
                begin
                    RowCount := RowCount + 1;
                end;

        //get the value of a cell
            //as an integer
                function TJDBStringGrid.tryCellToInteger(const colIn, rowIn : integer; out valueOut : integer) : boolean;
                    begin
                        if NOT( TryStrToInt( cells[colIn, rowIn], valueOut ) ) then
                            begin
                                valueOut := 0;
                                exit( False );
                            end;

                        result := True;
                    end;

                function TJDBStringGrid.cellToInteger(const colIn, rowIn : integer) : integer;
                    var
                        valueOut : integer;
                    begin
                        tryCellToInteger( colIn, rowIn, valueOut );

                        result := valueOut;
                    end;

            //as a double
                function TJDBStringGrid.tryCellToDouble(const colIn, rowIn : integer; out valueOut : double) : boolean;
                    begin
                        if NOT(TryStrToFloat( cells[colIn, rowIn], valueOut ) ) then
                            begin
                                valueOut := 0;
                                exit( False );
                            end;

                        result := True;
                    end;

                function TJDBStringGrid.cellToDouble(const colIn, rowIn : integer) : double;
                    var
                        valueOut : double;
                    begin
                        tryCellToDouble( colIn, rowIn, valueOut );

                        result := valueOut;
                    end;

        //test if a row is empty
            function TJDBStringGrid.colIsEmpty(const colIndexIn : integer) : boolean;
                var
                    rowIndex : integer;
                begin
                    result := True;

                    for rowIndex := 0 to (ColCount - 1) do
                        begin
                            result := cellIsEmpty(colIndexIn, rowIndex);

                            if (result = false) then
                                break;
                        end;
                end;

            function TJDBStringGrid.rowIsEmpty(const rowIndexIn : integer) : boolean;
                var
                    colIndex : integer;
                begin
                    result := True;

                    for colIndex := 0 to (ColCount - 1) do
                        begin
                            result := cellIsEmpty(colIndex, rowIndexIn);

                            if (result = false) then
                                break;
                        end;
                end;

        //update cell contents matrix
            procedure TJDBStringGrid.updateCellContentsMatrix(const resizeArray : boolean = True);
                var
                    c, r : integer;
                begin
                    if ( resizeArray ) then
                        sizeCellContents();

                    for c := 0 to (ColCount - 1) do
                        for r := 0 to (RowCount - 1) do
                            cellContentsMatrix[c, r] := cells[c, r];
                end;

        //resize the grid to its minimum extents
            procedure TJDBStringGrid.minHeight();
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

            procedure TJDBStringGrid.minWidth();
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

            procedure TJDBStringGrid.minSize();
                begin
                    minHeight();
                    minWidth();
                end;

end.
