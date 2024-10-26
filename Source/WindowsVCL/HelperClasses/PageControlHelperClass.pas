unit PageControlHelperClass;

interface

    uses
        system.SysUtils, System.Math,
        Vcl.ComCtrls;

    type
        TPageControlHelper = class helper for TPageControl
            private
                procedure setAllPageTabsVisibility(const visibleIn : boolean);
            public
                procedure hideAllPageTabs();
                procedure showAllPageTabs();
        end;

implementation

    //private
        procedure TPageControlHelper.setAllPageTabsVisibility(const visibleIn: Boolean);
            var
                i : integer;
            begin
                for i := 0 to (self.PageCount - 1) do
                    self.Pages[i].TabVisible := visibleIn;
            end;

    //public
        procedure TPageControlHelper.hideAllPageTabs();
            begin
                setAllPageTabsVisibility(False);
            end;

        procedure TPageControlHelper.showAllPageTabs();
            begin
                setAllPageTabsVisibility(True);
            end;

end.
