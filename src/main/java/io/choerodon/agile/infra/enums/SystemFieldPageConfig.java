package io.choerodon.agile.infra.enums;


/**
 * @author superlee
 * @since 2020-08-27
 */
public class SystemFieldPageConfig {

    public enum CommonField {
        ISSUE_TYPE(FieldCode.ISSUE_TYPE, true, true),
        PRIORITY(FieldCode.PRIORITY, true, true),
        EPIC_NAME(FieldCode.EPIC_NAME, true, true),
        SUMMARY(FieldCode.SUMMARY, true, true),
        DESCRIPTION(FieldCode.DESCRIPTION, true, true),
        REMAINING_TIME(FieldCode.REMAINING_TIME, true, true),
        STORY_POINTS(FieldCode.STORY_POINTS, true, true),
        ASSIGNEE(FieldCode.ASSIGNEE, true, true),
        EPIC(FieldCode.EPIC, true, true),
        SPRINT(FieldCode.SPRINT, true, true),
        FIX_VERSION(FieldCode.FIX_VERSION, true, true),
        COMPONENT(FieldCode.COMPONENT, true, true),
        LABEL(FieldCode.LABEL, true, true),
        STATUS(FieldCode.STATUS, false, true),
        REPORTER(FieldCode.REPORTER, false, true),
        INFLUENCE_VERSION(FieldCode.INFLUENCE_VERSION, false, true),
        TIME_TRACE(FieldCode.TIME_TRACE, false, true),
        CREATION_DATE(FieldCode.CREATION_DATE, false, true),
        LAST_UPDATE_DATE(FieldCode.LAST_UPDATE_DATE, false, true),
        PI(FieldCode.PI, true, true),
        BENFIT_HYPOTHESIS(FieldCode.BENFIT_HYPOTHESIS, true, true),
        ACCEPTANCE_CRITERA(FieldCode.ACCEPTANCE_CRITERA, true, true),
        ESTIMATED_START_TIME(FieldCode.ESTIMATED_START_TIME, true, true),
        ESTIMATED_END_TIME(FieldCode.ESTIMATED_END_TIME, true, true),
        SUB_PROJECT(FieldCode.SUB_PROJECT, true, true),
        PROGRAM_VERSION(FieldCode.PROGRAM_VERSION, true, true),
        MAIN_RESPONSIBLE(FieldCode.MAIN_RESPONSIBLE, true, true),
        environment(FieldCode.ENVIRONMENT, true, true),
        CREATER(FieldCode.CREATOR, false, true),
        UPDATER(FieldCode.UPDATOR, false, true),
        TAG(FieldCode.TAG, true, true),
        FEATURE_TYPE(FieldCode.FEATURE_TYPE, true, false),
        PROCESSOR(FieldCode.PROCESSOR, false, true),
        ACTUAL_START_TIME(FieldCode.ACTUAL_START_TIME, true, true),
        ACTUAL_END_TIME(FieldCode.ACTUAL_END_TIME, true, true),
        ;


        CommonField(String field, Boolean created, Boolean edited) {
            this.field = field;
            this.created = created;
            this.edited = edited;
        }

        private String field;

        private Boolean created;

        private Boolean edited;

        public String field() {
            return field;
        }

        public Boolean created() {
            return created;
        }

        public Boolean edited() {
            return edited;
        }

        public static CommonField queryByField(String field) {
            for (CommonField commonField : CommonField.values()) {
                if (commonField.field().equals(field)) {
                    return commonField;
                }
            }
            return null;
        }
    }
}
