package io.choerodon.agile.infra.enums;

import io.choerodon.core.exception.CommonException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2020-08-07
 */
public class ExcelImportTemplate {

    public static class Progress {

        private Long failCount;

        private Long successCount;

        private Integer processNum;

        public Progress () {
            this.failCount = 0L;
            this.successCount=0L;
            this.processNum = 0;
        }

        public void addSuccessCount(Long count) {
            this.successCount += count;
        }

        public void failCountIncrease(){
            this.failCount++;
        }

        public void successCountIncrease() {
            this.successCount++;
        }

        public void processNumIncrease() {
            this.processNum++;
        }

        public Long getFailCount() {
            return failCount;
        }

        public void setFailCount(Long failCount) {
            this.failCount = failCount;
        }

        public Long getSuccessCount() {
            return successCount;
        }

        public void setSuccessCount(Long successCount) {
            this.successCount = successCount;
        }

        public Integer getProcessNum() {
            return processNum;
        }

        public void setProcessNum(Integer processNum) {
            this.processNum = processNum;
        }
    }

    public static class Cursor {

        //隐藏页sheet从2开始，0为导航页，1为数据页
        private int startSheetNum;

        public Cursor () {
            this.startSheetNum = 2;
        }

        public int getAndIncreaseSheetNum() {
            return startSheetNum++;
        }

        public int getStartSheetNum() {
            return startSheetNum;
        }

        public void setStartSheetNum(int startSheetNum) {
            this.startSheetNum = startSheetNum;
        }
    }

    public static class Header {

        public static final List<Header> HEADERS;


        public static final String PARENT = "parent";
        public static final String RELATE_ISSUE = "relate_issue";
        private static final int INIT_CAPACITY = 50;
        public static final List<String> HEADER_CODES = new ArrayList<>(INIT_CAPACITY);
        private static final Map<String, String> CODE_VALUE_MAP = new HashMap(INIT_CAPACITY);
        private static final Map<String, String> VALUE_CODE_MAP = new HashMap(INIT_CAPACITY);
        private static final Map<String, Integer> VALUE_WIDTH_MAP = new HashMap<>(INIT_CAPACITY);

        static {
            HEADERS = new ArrayList<>(INIT_CAPACITY);
            HEADERS.add(new Header(FieldCode.ISSUE_TYPE, "问题类型*", true, null));
            HEADERS.add(new Header(PARENT, "父级故事/任务/缺陷", false, 12000));
            HEADERS.add(new Header(FieldCode.EPIC, "所属史诗", false, 8000));
            HEADERS.add(new Header(FieldCode.FEATURE, "所属特性", false, 8000));
            HEADERS.add(new Header(FieldCode.COMPONENT, "模块", true, null));
            HEADERS.add(new Header(FieldCode.SPRINT, "冲刺", true, null));
            HEADERS.add(new Header(FieldCode.SUMMARY, "概述*", true, null));
            HEADERS.add(new Header(FieldCode.DESCRIPTION, "描述", true, null));
            HEADERS.add(new Header(FieldCode.ASSIGNEE, "经办人", true, null));
            HEADERS.add(new Header(FieldCode.REPORTER, "报告人", true, null));
            HEADERS.add(new Header(FieldCode.PRIORITY, "优先级*", true, null));
            HEADERS.add(new Header(FieldCode.REMAINING_TIME, "预估时间(小时)", true, null));
            HEADERS.add(new Header(FieldCode.FIX_VERSION, "版本", false, null));
            HEADERS.add(new Header(FieldCode.STORY_POINTS, "故事点", true, null));
            HEADERS.add(new Header(FieldCode.EPIC_NAME, "史诗名称(仅问题类型为史诗时生效)", false, 8000));
            HEADERS.add(new Header(FieldCode.LABEL, "标签", false, null));
            HEADERS.add(new Header(FieldCode.ESTIMATED_START_TIME, "预估开始时间", false, null));
            HEADERS.add(new Header(FieldCode.ESTIMATED_END_TIME, "预估结束时间", false, null));
            HEADERS.add(new Header(RELATE_ISSUE, "关联问题", false, null));

            HEADERS.forEach(h -> {
                String code = h.getCode();
                String value = h.getValue();
                HEADER_CODES.add(code);
                CODE_VALUE_MAP.put(code, value);
                VALUE_CODE_MAP.put(value, code);
                VALUE_WIDTH_MAP.put(value, h.getDefaultWidth());
            });
        }

        private String code;

        private String value;

        private Boolean required;

        private Integer defaultWidth;

        public Header(String code, String value, Boolean required, Integer defaultWidth) {
            this.code = code;
            this.value = value;
            this.required = required;
            this.defaultWidth = defaultWidth;
        }

        public static List<Header> getByRequired(boolean required) {
            return HEADERS
                    .stream()
                    .filter(h -> h.getRequired().equals(required))
                    .collect(Collectors.toList());
        }

        public static String getValueByCode(String code) {
            return CODE_VALUE_MAP.get(code);
        }

        public static Integer getWidthByValue(String value) {
            return VALUE_WIDTH_MAP.get(value);
        }

        public static String getCodeByValue(String value) {
            return VALUE_CODE_MAP.get(value);
        }

        public static List<String> addFields(List<String> systemFields) {
            List<String> fields = new ArrayList<>();
            for (int i = 0; i < systemFields.size(); i++) {
                String code = systemFields.get(i);
                if (!HEADER_CODES.contains(code)) {
                    throw new CommonException("error.illegal.system.field." + code);
                }
                if (i == 1) {
                    fields.add(PARENT);
                }
                fields.add(code);
            }
            fields.add(RELATE_ISSUE);
            return fields;
        }

        public Integer getDefaultWidth() {
            return defaultWidth;
        }

        public void setDefaultWidth(Integer defaultWidth) {
            this.defaultWidth = defaultWidth;
        }

        public String getCode() {
            return code;
        }

        public void setCode(String code) {
            this.code = code;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public Boolean getRequired() {
            return required;
        }

        public void setRequired(Boolean required) {
            this.required = required;
        }
    }

    public static class Issue {

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
