package io.choerodon.agile.infra.enums.search;

import java.util.*;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.util.Assert;

import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldTypeCnName;
import io.choerodon.core.exception.CommonException;

import org.hzero.core.base.BaseConstants;

/**
 * @author superlee
 * @since 2022-11-02
 */
public class SearchConstant {

    public static final Logger logger = LoggerFactory.getLogger(SearchConstant.class);

    /**
     * 不需要加密的字段
     */
    public static final List<String> NO_ENCRYPT_FIELDS = Arrays.asList(FieldCode.SUB_PROJECT);

    public static final String TABLE_AGILE_ISSUE = "agile_issue";
    public static final String TABLE_BACKLOG = "backlog";
    public static final String TABLE_AGILE_ISSUE_SPRINT_REL = "agile_issue_sprint_rel";
    public static final String TABLE_AGILE_COMPONENT_ISSUE_REL = "agile_component_issue_rel";
    public static final String TABLE_AGILE_LABEL_ISSUE_REL = "agile_label_issue_rel";
    public static final String TABLE_AGILE_VERSION_ISSUE_REL = "agile_version_issue_rel";
    public static final String TABLE_AGILE_ISSUE_PARTICIPANT_REL = "agile_issue_participant_rel";
    public static final String TABLE_AGILE_ISSUE_PRODUCT_REL = "agile_issue_product_rel";
    public static final String TABLE_AGILE_TAG_ISSUE_REL = "agile_tag_issue_rel";
    public static final String TABLE_FD_STAR_BEACON = "fd_star_beacon";
    public static final String TABLE_AGILE_DATA_LOG = "agile_data_log";

    public static final Map<String, FieldTableVO> PREDEFINED_FIELD_TABLE_MAP;

    static {
        List<FieldTableVO> fieldTableList = Arrays.asList(
                new FieldTableVO(FieldCode.ISSUE_TYPE, "issue_type_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.STATUS, "status_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ASSIGNEE, "assignee_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.SPRINT, "sprint_id", TABLE_AGILE_ISSUE_SPRINT_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.PRIORITY, "priority_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.REPORTER, "reporter_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.COMPONENT, "component_id", TABLE_AGILE_COMPONENT_ISSUE_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.LABEL, "label_id", TABLE_AGILE_LABEL_ISSUE_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.FIX_VERSION, "version_id", TABLE_AGILE_VERSION_ISSUE_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.INFLUENCE_VERSION, "version_id", TABLE_AGILE_VERSION_ISSUE_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.EPIC, "epic_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.CREATION_DATE, "creation_date", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.LAST_UPDATE_DATE, "last_update_date", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ESTIMATED_START_TIME, "estimated_start_time", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ESTIMATED_END_TIME, "estimated_end_time", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ACTUAL_START_TIME, "actual_start_time", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ACTUAL_END_TIME, "actual_end_time", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.MAIN_RESPONSIBLE, "main_responsible_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ENVIRONMENT, "environment", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.CREATOR, "created_by", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.UPDATOR, "last_updated_by", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.PARTICIPANT, "participant_id", TABLE_AGILE_ISSUE_PARTICIPANT_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.TAG, null, TABLE_AGILE_TAG_ISSUE_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.STORY_POINTS, "story_points", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.REMAINING_TIME, "remaining_time", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ESTIMATE_TIME, "estimate_time", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.PRODUCT, "product_id", TABLE_AGILE_ISSUE_PRODUCT_REL, FieldTableVO.TYPE_BASE),
                new FieldTableVO(Field.CONTENT, null, TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.SUMMARY, "summary", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.ISSUE_NUM, "issue_num", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(Field.MY_STAR, "user_id", TABLE_FD_STAR_BEACON, FieldTableVO.TYPE_BASE),
                new FieldTableVO(Field.MY_PARTICIPATE, null, TABLE_AGILE_DATA_LOG, FieldTableVO.TYPE_BASE),
                new FieldTableVO(Field.TYPE_CODE, "type_code", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(FieldCode.SUB_PROJECT, "project_id", TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE),
                new FieldTableVO(Field.ISSUE_TYPE_PROJECT, null, TABLE_AGILE_ISSUE, FieldTableVO.TYPE_BASE)
        );
        PREDEFINED_FIELD_TABLE_MAP =
                fieldTableList.stream().collect(Collectors.toMap(FieldTableVO::getName, Function.identity()));
    }

    /**
     * 模拟系统字段，用于在数据库中没有的，但是界面筛选需要的字段
     */
    public static final List<ObjectSchemeFieldDTO> MOCK_FIELDS = Arrays.asList(
            new ObjectSchemeFieldDTO().setCode(Field.YQ_CLOUD_NUM).setFieldType(FieldTypeCnName.INPUT.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.CONTENT).setFieldType(FieldTypeCnName.INPUT.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.MY_STAR).setFieldType(FieldTypeCnName.SINGLE.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.MY_PARTICIPATE).setFieldType(FieldTypeCnName.SINGLE.getCode()),
            new ObjectSchemeFieldDTO().setCode(FieldCode.FEATURE).setFieldType(FieldTypeCnName.SINGLE.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.STAGE).setFieldType(FieldTypeCnName.SINGLE.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.TYPE_CODE).setFieldType(FieldTypeCnName.SINGLE.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.ISSUE_TYPE_PROJECT).setFieldType(FieldTypeCnName.SINGLE.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.TYPE_ID).setFieldType(FieldTypeCnName.SINGLE.getCode()),
            new ObjectSchemeFieldDTO().setCode(Field.SOURCE).setFieldType(FieldTypeCnName.SINGLE.getCode())
    );

    public class Field {
        /**
         * 燕千云编号
         */
        public static final String YQ_CLOUD_NUM = "yqCloudNum";
        /**
         * 输入框模糊搜索
         */
        public static final String CONTENT = "content";
        /**
         * 我关注的
         */
        public static final String MY_STAR = "myStar";
        /**
         * 我参与的
         */
        public static final String MY_PARTICIPATE = "myParticipate";

        /**
         * 瀑布阶段
         */
        public static final String STAGE = "stage";
        /**
         * 问题类型编码
         */
        public static final String TYPE_CODE = "typeCode";

        /**
         * 项目下的问题类型筛选
         */
        public static final String ISSUE_TYPE_PROJECT = "issueTypeProject";
        /**
         * 需求池需求类型
         */
        public static final String TYPE_ID = "typeId";
        /**
         * 需求池需求来源
         */
        public static final String SOURCE = "source";
        /**
         * 需求编码
         */
        public static final String BACKLOG_NUM = "backlogNum";
    }


    public enum Relationship {
        AND, OR;


        public static boolean contains(String value) {
            for (Relationship relationship : Relationship.values()) {
                if (relationship.toString().equals(value)) {
                    return true;
                }
            }
            return false;
        }
    }


    public enum Operation {
        EQUAL("="),
        LIKE("like"),
        IN("in"),
        NOT_IN("not in"),
        BETWEEN("in"),
        IS_NULL("not in"),
        IS_NOT_NULL("in"),
        /**
         * 括号
         */
        BRACKET("");

        private String opt;

        Operation(String opt) {
            this.opt = opt;
        }

        /**
         * 选择器支持的操作
         */
        public static final List<String> SELECTOR_OPERATIONS =
                Arrays.asList(
                        IN.toString(),
                        NOT_IN.toString(),
                        IS_NULL.toString(),
                        IS_NOT_NULL.toString());

        public static final List<String> DATE_OR_NUMBER_OPERATIONS =
                Arrays.asList(
                        BETWEEN.toString(),
                        IS_NULL.toString(),
                        IS_NOT_NULL.toString(),
                        EQUAL.toString());

        public static final List<String> STRING_OPERATIONS =
                Arrays.asList(
                        LIKE.toString(),
                        EQUAL.toString(),
                        IS_NULL.toString(),
                        IS_NOT_NULL.toString());


        public static boolean isEqual(String value) {
            return EQUAL.toString().equals(value);
        }

        public static boolean isLike(String value) {
            return LIKE.toString().equals(value);
        }

        public static boolean isIn(String value) {
            return IN.toString().equals(value);
        }

        public static boolean isBetween(String value) {
            return BETWEEN.toString().equals(value);
        }

        public static boolean isNull(String value) {
            return IS_NULL.toString().equals(value);
        }

        public static boolean isNotNull(String value) {
            return IS_NOT_NULL.toString().equals(value);
        }

        public static boolean isBracket(String value) {
            return BRACKET.toString().equals(value);
        }

        public static boolean contains(String value) {
            for (Operation operation : Operation.values()) {
                if (operation.toString().equals(value)) {
                    return true;
                }
            }
            return false;
        }

        public String getOpt() {
            return this.opt;
        }
    }

    public enum ValueSpecial {
        CURRENT_TIMESTAMP;

        public static boolean contains(String value) {
            for (ValueSpecial valueSpecial : ValueSpecial.values()) {
                if (valueSpecial.toString().equals(value)) {
                    return true;
                }
            }
            return false;
        }
    }

    public static class SqlTemplate {

        /**
         * ==============下拉框=================
         */

        /**
         * priority_id in (1,2,3)
         */
        public static final String SELF_TABLE_IN_OR_NOT_IN = " #{mainTableCol} #{opt} ( #{value} ) #{additionalCondition}";
        /**
         * (priority_id = 0 or priority_id is null)
         */
        public static final String SELF_TABLE_ID_IS_NULL = " (#{mainTableCol} = 0 or #{mainTableCol} is null) #{additionalCondition} ";
        /**
         * (priority_id != 0 and priority_id is not null)
         */
        public static final String SELF_TABLE_ID_IS_NOT_NULL = " (#{mainTableCol} != 0 and #{mainTableCol} is not null) #{additionalCondition} ";

        public static final String SELF_TABLE_IS_NULL = " (#{column} is null) ";

        public static final String SELF_TABLE_IS_NOT_NULL = " (#{column} is not null) ";

        public static final String SELF_TABLE_EQUAL = " (#{column} #{opt} #{value}) ";

        /**
         * 生成like的value语句
         * @param value 必须为单引号包裹的文本, 否则会出现数据问题
         * @return '%#{value}%'
         */
        public static String toLikeValueExp(String value) {
            if(StringUtils.isEmpty(value)) {
                return "%%";
            }
            // Mysql里, like会额外再将\\转义为\, 为了表达式正确, 需要再将\转义为\\
            // 下面的奇葩代码是Java中将\替换为\\的实现, 非请勿动
            return "'%" + value.substring(1, value.length() - 1).replaceAll("\\\\", "\\\\\\\\") + "%'";
        }

        /**
         * issue_id in (select issue_id from agile_component_issue_rel where component_id in (1,2,3) and additional condition )
         */
        public static final String LINKED_TABLE_IN_OR_NOT_IN = " #{mainTableCol} #{opt} ( select #{innerCol} from #{table} where #{projectCol} in (#{projectIdStr}) and #{dbColumn} in ( #{value} ) #{additionalCondition}) ";

        public static final String LINKED_TABLE_EQUAL = " #{mainTableCol} #{opt} ( select #{innerCol} from #{table} where #{projectCol} in (#{projectIdStr}) and #{dbColumn} #{innerOpt} #{value}) ";

        public static final String LINKED_TABLE_BETWEEN = " #{mainTableCol} #{opt} ( select #{innerCol} from #{table} where #{projectCol} in (#{projectIdStr}) and #{column} >= #{firstValue} and #{column} <= #{secondValue}) ";

        public static final String LINKED_TABLE_IS_NULL_OR_NOT_NULL = " #{mainTableCol} #{opt} ( select #{innerCol} from #{table} where #{projectCol} in (#{projectIdStr}) #{additionalCondition}) ";

        public static final String MY_PARTICIPATE = " #{column} in (#{value}) or #{mainTableCol} #{opt} ( select #{innerCol} from #{table} where project_id in (#{projectIdStr}) and field = 'assignee' and ( new_value in ( #{value} ) or old_value in ( #{value} ))) ";


        public static final String TAG_IN_OR_NOT_IN = " #{mainTableCol} #{opt} ( select #{innerCol} from #{table} where project_id in (#{projectIdStr}) and (#{additionalCondition}) ) ";

        public static final String YQ_CLOUD_NUM_LIKE_OR_EQUAL = " #{mainTableCol} #{opt} ( select #{innerCol} from #{table} where project_id in (#{projectIdStr}) and source = 'yqcloud' and instance_type = 'issue' and open_instance_num #{innerOpt} #{value}) ";
        /**
         * issue_id in ( select instance_id from fd_field_value where project_id in ( 1 ) and field_id = 1 and option_id in ( 1 ) and scheme_code = 'agile_issue')
         */
        public static final String CUSTOM_FIELD_IN_OR_NOT_IN = " #{mainTableCol} #{opt} ( select instance_id from fd_field_value where project_id in ( #{projectIdStr} ) and field_id = #{fieldId} and option_id in ( #{optionIds} ) and scheme_code = '#{schemeCode}') ";

        public static final String CUSTOM_FIELD_IS_NULL_OR_NOT_NULL = " #{mainTableCol} #{opt} ( select instance_id from fd_field_value where project_id in ( #{projectIdStr} ) and field_id = #{fieldId} and scheme_code = '#{schemeCode}') ";

        public static final String CUSTOM_FIELD_EQUAL_OR_LIKE = " #{mainTableCol} #{opt} ( select instance_id from fd_field_value where project_id in ( #{projectIdStr} ) and field_id = #{fieldId} and #{columnName} #{columnOpt} #{columnValue} and scheme_code = '#{schemeCode}') ";

        public static final String EPIC_IN_OR_NOT_IN = " ((#{epicIdWithAlias} #{opt} (#{value}) and #{typeCode} in ( 'story', 'task', 'bug', 'feature' )) or (#{parentIssueId} #{opt} ( select issue_id from agile_issue where issue_id = #{parentIssueId} and epic_id in (#{value})) and #{typeCode} = 'sub_task')) ";
        public static final String EPIC_IS_NULL = " (((#{epicIdWithAlias} = 0 or #{epicIdWithAlias} is null) and #{typeCode} in ( 'story', 'task', 'bug', 'feature' )) or (#{parentIssueId} in (select issue_id from agile_issue where issue_id = #{parentIssueId} and (epic_id = 0 or epic_id is null) ) and #{typeCode} = 'sub_task')) ";
        public static final String EPIC_IS_NOT_NULL = "  (((#{epicIdWithAlias} != 0 and #{epicIdWithAlias} is not null) and #{typeCode} in ( 'story', 'task', 'bug', 'feature' )) or (#{parentIssueId} in (select issue_id from agile_issue where issue_id = #{parentIssueId} and (epic_id != 0 and epic_id is not null) ) and #{typeCode} = 'sub_task')) ";


        /**
         * ================日期===============
         */

        public static final String CUSTOM_FIELD_DATE_BETWEEN = " #{mainTableCol} #{opt} ( select instance_id from fd_field_value where project_id in ( #{projectIdStr} ) and field_id = #{fieldId} and #{columnName} >= #{first} and #{columnName} <= #{second} and scheme_code = '#{schemeCode}') ";

        public static final String DATE_BETWEEN = " ( #{column} >= #{firstValue} and #{column} <= #{secondValue} ) ";

        public static final String DATE_FORMATTER = "DATE_FORMAT(%s, '%s')";

        public static final String REGEX = "#\\{(.+?)\\}";

        public static String fillInParam(EvaluationContext context,
                                         String sqlTemplate) {
            Assert.notNull(sqlTemplate, BaseConstants.ErrorCode.DATA_INVALID);
            Pattern pattern = Pattern.compile(REGEX);
            Matcher matcher = pattern.matcher(sqlTemplate);
            Set<String> propNames = new HashSet<>();
            while (matcher.find()) {
                propNames.add(matcher.group(1));
            }
            ExpressionParser parser = new SpelExpressionParser();
            String result = sqlTemplate;
            if (propNames.isEmpty()) {
                return result;
            }
            for (String propName : propNames) {
                Expression exp = parser.parseExpression(propName);
                String value = (String) exp.getValue(context);
                if (value == null) {
                    logger.error("SqlTemplateData has no value of {}, sqlTemplate: {}", propName, sqlTemplate);
                    throw new CommonException("error.propName.has.no.value");
                }
                StringBuilder builder = new StringBuilder();
                builder.append("#{").append(propName).append("}");
                result = result.replace(builder.toString(), value);
            }
            return result;
        }

    }
}
