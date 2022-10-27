package io.choerodon.agile.infra.enums;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.business.ExportIssuesVO;
import io.choerodon.core.exception.CommonException;

/**
 * @author superlee
 * @since 2020-08-07
 */
public class ExcelImportTemplate {

    public static class Progress {

        private Long failCount;

        private Long successCount;

        private Integer processNum;

        public Progress() {
            this.failCount = 0L;
            this.successCount = 0L;
            this.processNum = 0;
        }

        public void addSuccessCount(Long count) {
            this.successCount += count;
        }

        public void addProcessNum(Integer count) {
            this.processNum += count;
        }

        public void failCountIncrease() {
            this.failCount++;
        }

        public void successCountDecrease() {
            this.successCount--;
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

        public Cursor() {
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

    public static class IssueHeader {

        protected static final List<Header> HEADERS;

        public static final String PARENT = "parent";
        public static final String RELATE_ISSUE = "relate_issue";
        public static final String ISSUE_NUM = "issueNum";
        private static final int INIT_CAPACITY = 50;
        protected static final List<String> HEADER_CODES = new ArrayList<>(INIT_CAPACITY);
        private static final Map<String, String> CODE_VALUE_MAP = new HashMap(INIT_CAPACITY);
        private static final Map<String, String> VALUE_CODE_MAP = new HashMap(INIT_CAPACITY);
        private static final Map<String, Integer> VALUE_WIDTH_MAP = new HashMap<>(INIT_CAPACITY);

        static {
            HEADERS = new ArrayList<>(INIT_CAPACITY);
            HEADERS.add(new Header(ISSUE_NUM, "编号", false, null));
            HEADERS.add(new Header(FieldCode.ISSUE_TYPE, IssueConstant.ISSUE_TYPE_CN + "*", true, null));
            HEADERS.add(new Header(PARENT, "父级故事/任务/缺陷", true, 12000));
            HEADERS.add(new Header(FieldCode.EPIC, "所属史诗", false, 8000));
            HEADERS.add(new Header(FieldCode.FEATURE, "所属特性", false, 8000));
            HEADERS.add(new Header(FieldCode.COMPONENT, "模块", false, null));
            HEADERS.add(new Header(FieldCode.SPRINT, "冲刺", false, null));
            HEADERS.add(new Header(FieldCode.SUMMARY, "概要*", true, null));
            HEADERS.add(new Header(FieldCode.DESCRIPTION, "描述", false, null));
            HEADERS.add(new Header(FieldCode.ASSIGNEE, "经办人", false, null));
            HEADERS.add(new Header(FieldCode.REPORTER, "报告人", false, null));
            HEADERS.add(new Header(FieldCode.PRIORITY, "优先级*", false, null));
            HEADERS.add(new Header(FieldCode.REMAINING_TIME, "剩余预估时间(小时)", false, null));
            HEADERS.add(new Header(FieldCode.FIX_VERSION, "修复的版本", false, null));
            HEADERS.add(new Header(FieldCode.INFLUENCE_VERSION, "影响的版本", false, null));
            HEADERS.add(new Header(FieldCode.STORY_POINTS, "故事点", false, null));
            HEADERS.add(new Header(FieldCode.EPIC_NAME, "史诗名称(仅" + IssueConstant.ISSUE_TYPE_CN + "为史诗时生效)", false, 8000));
            HEADERS.add(new Header(FieldCode.LABEL, "标签", false, null));
            HEADERS.add(new Header(FieldCode.ESTIMATED_START_TIME, "预计开始时间", false, null));
            HEADERS.add(new Header(FieldCode.ESTIMATED_END_TIME, "预计结束时间", false, null));
            HEADERS.add(new Header(FieldCode.MAIN_RESPONSIBLE, "主要负责人", false, null));
            HEADERS.add(new Header(FieldCode.ENVIRONMENT, "环境", false, null));
            HEADERS.add(new Header(RELATE_ISSUE, "关联" + IssueConstant.ISSUE_CN, false, null));
            HEADERS.add(new Header(FieldCode.ISSUE_STATUS, "状态", false, null));
            HEADERS.add(new Header(FieldCode.ACTUAL_START_TIME, "实际开始时间", false, null));
            HEADERS.add(new Header(FieldCode.ACTUAL_END_TIME, "实际结束时间", false, null));
            HEADERS.add(new Header(FieldCode.PARTICIPANT, "参与人", false, null));
            HEADERS.add(new Header(FieldCode.ESTIMATE_TIME, "原始预估时间(小时)", false, null));
            HEADERS.add(new Header(FieldCode.PRODUCT, "产品", false, null));
            HEADERS.add(new Header(FieldCode.CREATION_DATE, "创建时间", false, null));
            HEADERS.add(new Header(FieldCode.LAST_UPDATE_DATE, "最后更新时间", false, null));
            HEADERS.add(new Header(FieldCode.CREATOR, "创建人", false, null));
            HEADERS.add(new Header(FieldCode.UPDATOR, "更新人", false, null));
            HEADERS.add(new Header(ExportIssuesVO.ALL_ESTIMATE_TIME, "当前预估时间", false, null));
            HEADERS.add(new Header(ExportIssuesVO.SPENT_WORK_TIME, "已耗费时间", false, null));
            HEADERS.add(new Header(ExportIssuesVO.TAGS, "Tag", false, null));
            HEADERS.forEach(h -> {
                String code = h.getCode();
                String value = h.getValue();
                HEADER_CODES.add(code);
                CODE_VALUE_MAP.put(code, value);
                VALUE_CODE_MAP.put(value, code);
                VALUE_WIDTH_MAP.put(value, h.getDefaultWidth());
            });
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
                fields.add(code);
            }
//            fields.add(RELATE_ISSUE);
            return fields;
        }
    }

}
