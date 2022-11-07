package io.choerodon.agile.app.service.v2.impl;

import static org.hzero.core.base.BaseConstants.ErrorCode.DATA_INVALID;
import static io.choerodon.agile.infra.enums.search.SearchConstant.Operation;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import io.choerodon.agile.api.validator.AdvancedParamValidator;
import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.api.vo.search.Field;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.api.vo.search.Condition;
import io.choerodon.agile.api.vo.search.Value;
import io.choerodon.agile.app.service.v2.AdvancedParamParserService;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldTypeCnName;
import io.choerodon.agile.infra.enums.InstanceType;

import org.hzero.core.base.BaseConstants;


/**
 * @author superlee
 * @since 2022-11-02
 */
@Service
public class AdvancedParamParserServiceImpl implements AdvancedParamParserService {

    @Autowired
    private AdvancedParamValidator advancedParamValidator;

    private static final String TABLE_AGILE_ISSUE = "agile_issue";
    private static final String TABLE_AGILE_ISSUE_SPRINT_REL = "agile_issue_sprint_rel";
    private static final String TABLE_AGILE_COMPONENT_ISSUE_REL = "agile_component_issue_rel";
    private static final String TABLE_AGILE_LABEL_ISSUE_REL = "agile_label_issue_rel";
    private static final String TABLE_AGILE_VERSION_ISSUE_REL = "agile_version_issue_rel";
    private static final String TABLE_AGILE_ISSUE_PARTICIPANT_REL = "agile_issue_participant_rel";
    private static final String TABLE_AGILE_ISSUE_PRODUCT_REL = "agile_issue_product_rel";
    private static final String TABLE_AGILE_TAG_ISSUE_REL = "agile_tag_issue_rel";


    /**
     * priority_id in (1,2,3)
     */
    public static final String SQL_TEMPLATE_SELF_TABLE_IN_OR_NOT_IN = " %s %s ( %s ) ";
    /**
     * issue_id in (select issue_id from agile_component_issue_rel where component_id in (1,2,3) and additional condition )
     */
    public static final String SQL_TEMPLATE_LINKED_TABLE_IN_OR_NOT_IN = " %s %s ( select %s from %s where %s in ( %s ) %s) ";
    /**
     * issue_id in ( select instance_id from fd_field_value where project_id in ( 1 ) and field_id = 1 and option_id in ( 1 ) and scheme_code = 'agile_issue')
     */
    public static final String SQL_TEMPLATE_CUSTOM_FIELD_IN_OR_NOT_IN = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and option_id in ( %s ) and scheme_code = '%s') ";


    protected static final Map<String, FieldTableVO> PREDEFINED_FIELD_TABLE_MAP;

    static {

        List<FieldTableVO> fieldTableList = Arrays.asList(
                new FieldTableVO(FieldCode.ISSUE_TYPE, "issue_type_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.STATUS, "status_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ASSIGNEE, "assignee_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.SPRINT, "sprint_id", TABLE_AGILE_ISSUE_SPRINT_REL),
                new FieldTableVO(FieldCode.PRIORITY, "priority_id", TABLE_AGILE_ISSUE),
                //todo 特性&史诗
                new FieldTableVO(FieldCode.FEATURE, "feature_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.REPORTER, "reporter_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.COMPONENT, "component_id", TABLE_AGILE_COMPONENT_ISSUE_REL),
                new FieldTableVO(FieldCode.LABEL, "label_id", TABLE_AGILE_LABEL_ISSUE_REL),
                new FieldTableVO(FieldCode.FIX_VERSION, "version_id", TABLE_AGILE_VERSION_ISSUE_REL),
                new FieldTableVO(FieldCode.INFLUENCE_VERSION, "version_id", TABLE_AGILE_VERSION_ISSUE_REL),
                new FieldTableVO(FieldCode.EPIC, "epic_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.CREATION_DATE, "creation_date", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.LAST_UPDATE_DATE, "last_update_date", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ESTIMATED_START_TIME, "estimated_start_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ESTIMATED_END_TIME, "estimated_end_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ACTUAL_START_TIME, "actual_start_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ACTUAL_END_TIME, "actual_end_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.MAIN_RESPONSIBLE, "main_responsible_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ENVIRONMENT, "environment", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.CREATOR, "created_by", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.UPDATOR, "last_updated_by", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.PARTICIPANT, "participant_id", TABLE_AGILE_ISSUE_PARTICIPANT_REL),
                //todo 特殊处理
                new FieldTableVO(FieldCode.TAG, null, TABLE_AGILE_TAG_ISSUE_REL),
                new FieldTableVO(FieldCode.STORY_POINTS, "story_points", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.REMAINING_TIME, "remaining_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ESTIMATE_TIME, "estimate_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.PRODUCT, "product_id", TABLE_AGILE_ISSUE_PRODUCT_REL)
                //todo 是否关联燕千云单据
                //todo 关联燕千云单号
        );
        PREDEFINED_FIELD_TABLE_MAP =
                fieldTableList.stream().collect(Collectors.toMap(FieldTableVO::getName, Function.identity()));
    }

    @Override
    public String parse(InstanceType instanceType,
                        SearchParamVO searchParamVO,
                        Set<Long> projectIds) {
        advancedParamValidator.validate(searchParamVO);
        StringBuilder sqlBuilder = new StringBuilder();
        Map<String, FieldTableVO> predefinedFieldMap = buildPredefinedFieldMap();
        searchParamVO.getConditions()
                .forEach(condition -> {
                    Field field = condition.getField();
                    String fieldType = field.getFieldType();
                    String operation = condition.getOperation();
                    sqlBuilder.append(condition.getRelationship());
                    if (Operation.isBracket(operation)) {
                        //括号，a && (b || c)，读取 b和c然后加括号
                        List<Condition> subConditions = condition.getSubConditions();
                    } else {
                        FieldTypeCnName fieldTypeCnName = FieldTypeCnName.ofCode(fieldType).get();
                        switch (fieldTypeCnName) {
                            case MEMBER:
                            case MULTI_MEMBER:
                            case MULTIPLE:
                            case SINGLE:
                            case RADIO:
                            case CHECKBOX:
                                generateSelectorSql(sqlBuilder, predefinedFieldMap, condition, projectIds, instanceType);
                            case TIME:
                            case DATETIME:
                            case DATE:
                                //时间

                            case NUMBER:
                                //数字

                            case TEXT:
                            case INPUT:
                            default:
                                break;
                        }


                    }


                });
        return sqlBuilder.toString();
    }

    private void generateSelectorSql(StringBuilder sqlBuilder,
                                     Map<String, FieldTableVO> predefinedFieldMap,
                                     Condition condition,
                                     Set<Long> projectIds,
                                     InstanceType instanceType) {
        Field field = condition.getField();
        String operation = condition.getOperation();
        //选择器，只支持in, not in, is null, is not null
        Assert.isTrue(Operation.SELECTOR_OPERATIONS.contains(operation), DATA_INVALID);
        operation = resetSelectorOperation(operation);
        List<? extends Object> values = getOptionValues(condition);
        /////
        boolean isPredefined = field.getPredefined();
        String alias = "ai";
        if (isPredefined) {
            //预定义字段
            String fieldCode = field.getFieldCode();
            FieldTableVO fieldTable = predefinedFieldMap.get(fieldCode);
            Assert.notNull(fieldTable, DATA_INVALID);
            boolean isLinkedTable = !TABLE_AGILE_ISSUE.equals(fieldTable.getTable());
            switch (fieldCode) {
                case FieldCode.TAG:
                    break;
                case FieldCode.FIX_VERSION:
                    generateLinkedTableSql(sqlBuilder, operation, values, fieldTable, alias, "and relation_type = 'fix'");
                    break;
                case FieldCode.INFLUENCE_VERSION:
                    generateLinkedTableSql(sqlBuilder, operation, values, fieldTable, alias, "and relation_type = 'influence'");
                    break;
                default:
                    if (!isLinkedTable) {
                        String mainTableFilterColumn = buildMainTableFilterColumn(fieldTable.getField(), alias);
                        sqlBuilder.append(
                                String.format(SQL_TEMPLATE_SELF_TABLE_IN_OR_NOT_IN, mainTableFilterColumn, operation, StringUtils.join(values, BaseConstants.Symbol.COMMA)));
                    } else {
                        //关联表
                        generateLinkedTableSql(sqlBuilder, operation, values, fieldTable, alias, "");
                    }
                    break;
            }
        } else {
            String primaryKey = "issue_id";
            String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
            Long fieldId = field.getFieldId();
            String schemeCode = instanceType.getSchemeCode();
            sqlBuilder.append(
                    String.format(
                            SQL_TEMPLATE_CUSTOM_FIELD_IN_OR_NOT_IN,
                            mainTableFilterColumn,
                            operation,
                            StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                            fieldId,
                            StringUtils.join(values, BaseConstants.Symbol.COMMA),
                            schemeCode));
        }
    }

    private void generateLinkedTableSql(StringBuilder sqlBuilder,
                                        String operation,
                                        List<?> values,
                                        FieldTableVO fieldTable,
                                        String alias,
                                        String additionalCondition) {
        String dbColumn = fieldTable.getField();
        String primaryKey = "issue_id";
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        sqlBuilder.append(
                String.format(SQL_TEMPLATE_LINKED_TABLE_IN_OR_NOT_IN, mainTableFilterColumn, operation, primaryKey, table, dbColumn, StringUtils.join(values, BaseConstants.Symbol.COMMA), additionalCondition));
    }

    private String buildMainTableFilterColumn(String primaryKey,
                                              String alias) {

        String mainTableFilterColumn;
        if (alias == null) {
            mainTableFilterColumn = primaryKey;
        } else {
            mainTableFilterColumn = alias + BaseConstants.Symbol.POINT + primaryKey;
        }
        return mainTableFilterColumn;
    }

    private String resetSelectorOperation(String operation) {
        //选择器情况 is null == not in; is not null = in
        if (Operation.isNull(operation)) {
            operation = Operation.NOT_IN.toString();
        }
        if (Operation.isNotNull(operation)) {
            operation = Operation.IN.toString();
        }
        return operation;
    }

    private List<? extends Object> getOptionValues(Condition condition) {
        //todo list object tag处理
        List<? extends Object> values;
        Value value = condition.getValue();
        Field field = condition.getField();
        boolean noEncryptFlag = Boolean.TRUE.equals(field.getNoEncryptFlag());
        if (noEncryptFlag) {
            values = value.getNoEncryptIdList();
        } else {
            values = value.getValueIdList();
        }
        return values;
    }

    private Map<String, FieldTableVO> buildPredefinedFieldMap() {
        Map<String, FieldTableVO> map = new HashMap<>(PREDEFINED_FIELD_TABLE_MAP);
        //todo 瀑布，需求，项目群
        return map;
    }


}
