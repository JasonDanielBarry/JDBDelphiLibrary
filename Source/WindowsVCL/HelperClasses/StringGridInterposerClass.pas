unit StringGridInterposerClass;

interface

    uses
        Winapi.Windows,
        System.SysUtils, system.Math, system.Types, system.UITypes, system.Generics.Collections, System.Classes,
        vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Grids;

    type
        TStringGrid = class(Vcl.grids.TStringGrid)
            private
                var
                    borderPanel : TPanel;
                //border panel name
                    function getBorderPanelName() : string;
                //edit a border's properties
                    procedure editBorder(   const edgeWidthIn       : integer;
                                            const colourIn          : TColor;
                                            var borderPanelInOut    : TPanel    );
                //border adjustment used for sizing the grid
                    function borderAdjustment() : integer;

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
            function TStringGrid.getBorderPanelName() : string;
                begin
                    result := self.Name + BORDER_PANEL;
                end;

        //border adjustment used for sizing the grid
            function TStringGrid.borderAdjustment() : integer;
                begin
                    if Self.BorderStyle = bsSingle then
                        result := 2
                    else
                        result := 0;
                end;

        //edit a border's properties
            procedure TStringGrid.editBorder(   const edgeWidthIn       : integer;
                                                const colourIn          : TColor;
                                                var borderPanelInOut    : TPanel    );
                begin
                    if NOT( Assigned( borderPanelInOut ) ) then
                        exit();


                end;

    //public
        //constructor
            constructor TStringGrid.create(AOwner: TComponent);
                begin
                    inherited Create( AOwner );

                    borderPanel := TPanel.Create( self );
                end;

        //destructor
            destructor TStringGrid.destroy();
                begin
                    FreeAndNil( borderPanel );

                    inherited destroy();
                end;

        //check cell is empty string
            function TStringGrid.cellIsEmpty(const colIn, rowIn : integer) : boolean;
                begin
                    result := (cells[colIn, rowIn] = '');
                end;

        //clear a grid of its contents
            //clear a cell
                procedure TStringGrid.clearCell(const colIn, rowIn : integer);
                    begin
                        cells[colIn, rowIn] := '';
                    end;

            //clear column
                procedure TStringGrid.clearColumn(const colIndexIn : integer);
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := 0 to (RowCount - 1) do
                            clearCell( colIndexIn, rowIndex );
                    end;

                procedure TStringGrid.clearColumns(const startColIndexIn : integer = 0);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := startColIndexIn to (ColCount - 1) do
                            clearColumn(colIndex);
                    end;

            //clear a row's content
                procedure TStringGrid.clearRow(const rowIndexIn : integer);
                    var
                        colIndex : integer;
                    begin
                        for colIndex := 0 to (ColCount - 1) do
                            clearCell( colIndex, rowIndexIn );
                    end;

                procedure TStringGrid.clearRows(const startRowIndexIn : integer = 0);
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := startRowIndexIn to (RowCount - 1) do
                            clearRow(rowIndex);
                    end;

            procedure TStringGrid.clearCells(const startColIndexIn : integer = 0; const startRowIndexIn : integer = 0);
                var
                    c, r : integer;
                begin
                    for c := startColIndexIn to (ColCount - 1) do
                        for r := startRowIndexIn to (RowCount - 1) do
                            clearCell(c, r);
                end;

        //create border
            procedure TStringGrid.setBorderProperties(  const edgeWidthIn   : integer;
                                                        const colourIn      : TColor    );
                begin
                    //prime grid for border
                        //get rid of grid border
                            self.BevelInner := TBevelCut.bvNone;
                            self.BevelKind := TBevelKind.bkNone;
                            self.BevelOuter := TBevelCut.bvNone;
                            self.BorderStyle := bsNone;

                        //size
                            self.minSize();
                            self.Height := self.Height - 2;
                            self.Width  := self.Width - 2;

                        //create the grid for border
                            borderPanel.Parent := self.Parent;
                            borderPanel.Name := getBorderPanelName();
                            borderpanel.StyleElements := [seFont, {seClient, }seBorder];

                    //prime panel to be a border
                        //vcl properties
                            borderPanel.ParentBackground := False;
                            borderPanel.ParentColor := False;
                            borderPanel.BevelInner := TBevelCut.bvNone;
                            borderPanel.BevelOuter := TBevelCut.bvNone;
                            borderPanel.BevelKind := TBevelKind.bkNone;
                            borderPanel.BorderStyle := bsNone;

                        //colour
                            borderPanel.Color := colourIn;

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
                procedure TStringGrid.deleteRow(const rowIndexIn : integer);
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
                procedure TStringGrid.deleteEmptyRow(const rowIndexIn : integer);
                    begin
                        if (rowIsEmpty(rowIndexIn)) then
                            deleteRow(rowIndexIn);
                    end;

            //delete all empty rows
                procedure TStringGrid.deleteAllEmptyRows();
                    var
                        rowIndex : integer;
                    begin
                        for rowIndex := (RowCount - 1) downto 0 do
                            if (rowIndex < rowCount) then
                                deleteEmptyRow(rowIndex);
                    end;

        //row insertion
            procedure TStringGrid.addColumn();
                begin
                    ColCount := ColCount + 1;
                end;

            procedure TStringGrid.addRow();
                begin
                    RowCount := RowCount + 1;
                end;

        //get the value of a cell
            //as an integer
                function TStringGrid.tryCellToInteger(const colIn, rowIn : integer; out valueOut : integer) : boolean;
                    begin
                        if NOT( TryStrToInt( cells[colIn, rowIn], valueOut ) ) then
                            begin
                                valueOut := 0;
                                exit( False );
                            end;

                        result := True;
                    end;

                function TStringGrid.cellToInteger(const colIn, rowIn : integer) : integer;
                    var
                        valueOut : integer;
                    begin
                        tryCellToInteger( colIn, rowIn, valueOut );

                        result := valueOut;
                    end;

            //as a double
                function TStringGrid.tryCellToDouble(const colIn, rowIn : integer; out valueOut : double) : boolean;
                    begin
                        if NOT(TryStrToFloat( cells[colIn, rowIn], valueOut ) ) then
                            begin
                                valueOut := 0;
                                exit( False );
                            end;

                        result := True;
                    end;

                function TStringGrid.cellToDouble(const colIn, rowIn : integer) : double;
                    var
                        valueOut : double;
                    begin
                        tryCellToDouble( colIn, rowIn, valueOut );

                        result := valueOut;
                    end;

        //test if a row is empty
            function TStringGrid.colIsEmpty(const colIndexIn : integer) : boolean;
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

            function TStringGrid.rowIsEmpty(const rowIndexIn : integer) : boolean;
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


        //resize the grid to its minimum extents
            procedure TStringGrid.minHeight();
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

            procedure TStringGrid.minWidth();
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

            procedure TStringGrid.minSize();
                begin
                    minHeight();
                    minWidth();
                end;

end.
