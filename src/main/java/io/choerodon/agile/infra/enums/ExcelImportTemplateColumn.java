package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2020-08-07
 */
public class ExcelImportTemplateColumn {

    private ExcelImportTemplateColumn() {}

    public static class Issue {

        private Issue() {}

        public static final int ISSUE_TYPE_COL = 0;
        public static final int EPIC_COL = 1;
        public static final int COMPONENT_COL = 2;
        public static final int SPRINT_COL = 3;
        public static final int SUMMARY_COL = 4;
        public static final int SUB_TASK_COL = 5;
        public static final int DESCRIPTION_COL = 6;
        public static final int MANAGER_COL = 7;
        public static final int REPORTER_COL = 8;
        public static final int PRIORITY_COL = 9;
        public static final int REMAIN_TIME_COL = 10;
        public static final int FIX_VERSION_COL = 11;
        public static final int STORY_POINT_COL = 12;
        public static final int EPIC_NAME_COL = 13;

        public static final HiddenSheet PRIORITY_SHEET = new HiddenSheet(PRIORITY_COL, "hidden_priority", 2);
        public static final HiddenSheet ISSUE_TYPE_SHEET = new HiddenSheet(ISSUE_TYPE_COL, "hidden_issue_type", 3);
        public static final HiddenSheet FIX_VERSION_SHEET = new HiddenSheet(FIX_VERSION_COL, "hidden_fix_version", 4);
        public static final HiddenSheet COMPONENT_SHEET = new HiddenSheet(COMPONENT_COL, "hidden_component", 5);
        public static final HiddenSheet SPRINT_SHEET = new HiddenSheet(SPRINT_COL, "hidden_sprint", 6);
        public static final HiddenSheet MANAGER_SHEET = new HiddenSheet(MANAGER_COL, "hidden_manager", 7);
        public static final HiddenSheet REPORTER_SHEET = new HiddenSheet(REPORTER_COL, "hidden_reporter", 8);
        public static final HiddenSheet EPIC_SHEET = new HiddenSheet(EPIC_COL, "hidden_epic", 9);
    }

    public static class HiddenSheet {
        private int col;

        private String name;

        private int index;

        public HiddenSheet (int col, String name, int index) {
            this.col = col;
            this.name = name;
            this.index = index;
        }

        public int getCol() {
            return col;
        }

        public void setCol(int col) {
            this.col = col;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public int getIndex() {
            return index;
        }

        public void setIndex(int index) {
            this.index = index;
        }
    }

}
