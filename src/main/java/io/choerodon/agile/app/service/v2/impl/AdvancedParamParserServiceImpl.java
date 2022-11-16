package io.choerodon.agile.app.service.v2.impl;

import static io.choerodon.agile.infra.enums.search.SearchConstant.SqlTemplate.*;
import static org.hzero.core.base.BaseConstants.ErrorCode.DATA_INVALID;
import static io.choerodon.agile.infra.enums.search.SearchConstant.Operation;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.validator.AdvancedParamValidator;
import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.api.vo.search.Field;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.api.vo.search.Condition;
import io.choerodon.agile.api.vo.search.Value;
import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.app.service.v2.AdvancedParamParserService;
import io.choerodon.agile.infra.dto.ProjectInfoDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldTypeCnName;
import io.choerodon.agile.infra.enums.InstanceType;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.mapper.ProjectInfoMapper;
import io.choerodon.core.exception.CommonException;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;


/**
 * @author superlee
 * @since 2022-11-02
 */
@Service
public class AdvancedParamParserServiceImpl implements AdvancedParamParserService {

    @Autowired
    private AdvancedParamValidator advancedParamValidator;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;

    public static final String SQL_YYYY_MM_DD_HH_MM = "%Y-%m-%d %H:%i";

    public static final String SINGLE_QUOT = "'";

    private static final String DEFAULT_PRIMARY_KEY = "issue_id";

    private static final String INSTANCE_ID = "instance_id";

    private static final List<String> DATETIME_MM_FIELD_LIST =
            Arrays.asList(
                    FieldCode.ESTIMATED_START_TIME,
                    FieldCode.ESTIMATED_END_TIME,
                    FieldCode.ACTUAL_START_TIME,
                    FieldCode.ACTUAL_END_TIME
            );

    @Override
    public String parse(InstanceType instanceType,
                        SearchParamVO searchParamVO,
                        Set<Long> projectIds) {
        advancedParamValidator.validate(searchParamVO);
        Map<String, FieldTableVO> predefinedFieldMap = buildPredefinedFieldMap();
        List<Condition> conditions = new ArrayList<>();
        conditions.addAll(searchParamVO.getConditions());
        conditions.addAll(searchParamVO.getAdvancedConditions());
        return generateSql(instanceType, projectIds, predefinedFieldMap, conditions, searchParamVO.getIssueIds());
    }

    private String generateSql(InstanceType instanceType,
                               Set<Long> projectIds,
                               Map<String, FieldTableVO> predefinedFieldMap,
                               List<Condition> conditions,
                               Set<Long> issueIds) {
        StringBuilder sqlBuilder = new StringBuilder();
        for (int i = 0; i < conditions.size(); i++) {
            Condition condition = conditions.get(i);
            Field field = condition.getField();
            String fieldType = field.getFieldType();
            String operation = condition.getOperation();
            if (i > 0) {
                //第一个不拼
                sqlBuilder.append(condition.getRelationship());
            }
            if (Operation.isBracket(operation)) {
                //括号，a && (b || c)，读取 b和c然后加括号
                appendBracket(instanceType, projectIds, predefinedFieldMap, sqlBuilder, condition);
            } else {
                FieldTypeCnName fieldTypeCnName = FieldTypeCnName.ofCode(fieldType).get();
                switch (fieldTypeCnName) {
                    case MEMBER:
                    case MULTI_MEMBER:
                    case MULTIPLE:
                    case SINGLE:
                    case RADIO:
                    case CHECKBOX:
                        sqlBuilder.append(generateSelectorSql(predefinedFieldMap, condition, projectIds, instanceType));
                        break;
                    case TIME:
                    case DATETIME:
                    case DATE:
                        sqlBuilder.append(generateSqlByClazz(predefinedFieldMap, condition, projectIds, instanceType, Date.class));
                        break;
                    case NUMBER:
                        sqlBuilder.append(generateSqlByClazz(predefinedFieldMap, condition, projectIds, instanceType, BigDecimal.class));
                        break;
                    case TEXT:
                    case INPUT:
                        sqlBuilder.append(generateSqlByClazz(predefinedFieldMap, condition, projectIds, instanceType, String.class));
                        break;
                    default:
                        break;
                }
            }
        }
        String issueIdSql = generateIssueIdSql(issueIds);
        if (sqlBuilder.length() > 0 && issueIdSql.length() > 0) {
            sqlBuilder.append(" and ");
        }
        sqlBuilder.append(issueIdSql);
        return sqlBuilder.toString();
    }

    private String generateIssueIdSql(Set<Long> issueIds) {
        StringBuilder sqlBuilder = new StringBuilder();
        if (ObjectUtils.isEmpty(issueIds)) {
            return sqlBuilder.toString();
        }
        String alias = "ai";
        String primaryKey = DEFAULT_PRIMARY_KEY;
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        String issueIdStr = "(" + StringUtils.join(issueIds, BaseConstants.Symbol.COMMA) + ")";
        sqlBuilder.append(String.format(SELF_TABLE_EQUAL, mainTableFilterColumn, "in", issueIdStr));
        return sqlBuilder.toString();
    }

    private void appendBracket(InstanceType instanceType,
                               Set<Long> projectIds,
                               Map<String, FieldTableVO> predefinedFieldMap,
                               StringBuilder sqlBuilder,
                               Condition condition) {
        List<Condition> subConditions = condition.getSubConditions();
        sqlBuilder
                .append(BaseConstants.Symbol.LEFT_BRACE)
                .append(" ")
                .append(generateSql(instanceType, projectIds, predefinedFieldMap, subConditions, null))
                .append(" ")
                .append(BaseConstants.Symbol.RIGHT_BRACE);
    }

    private String generateSqlByClazz(Map<String, FieldTableVO> predefinedFieldMap,
                                      Condition condition,
                                      Set<Long> projectIds,
                                      InstanceType instanceType,
                                      Class clazz) {
        StringBuilder sqlBuilder = new StringBuilder();
        Field field = condition.getField();
        String operation = condition.getOperation();
        List<String> options = new ArrayList<>();
        //判断与类型相关的操作符
        if (clazz == Date.class || clazz == BigDecimal.class) {
            options.addAll(Operation.DATE_OR_NUMBER_OPERATIONS);
        } else if (clazz == String.class) {
            options.addAll(Operation.STRING_OPERATIONS);
        }
        Assert.isTrue(options.contains(operation), DATA_INVALID);
        String fieldCode = field.getFieldCode();
        Pair<String, String> dataPair = getPairValues(condition, clazz, field);
        boolean isPredefined = field.getPredefined();
        String alias = "ai";
        if (isPredefined) {
            FieldTableVO fieldTable = predefinedFieldMap.get(fieldCode);
            Assert.notNull(fieldTable, DATA_INVALID);
            switch (fieldCode) {
                case SearchConstant.Field.YQ_CLOUD_NUM:
                    sqlBuilder.append(generateYqCloudNumSql(operation, dataPair, fieldTable, alias, projectIds));
                    break;
                case SearchConstant.Field.CONTENT:
                    sqlBuilder.append(generateContentSql(operation, dataPair, alias, projectIds, predefinedFieldMap));
                    break;
                default:
                    String column = buildColumnByCode(fieldTable.getField(), alias, fieldCode);
                    appendPredefinedSql(sqlBuilder, operation, dataPair, column);
                    break;
            }
        } else {
            String primaryKey = DEFAULT_PRIMARY_KEY;
            String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
            String schemeCode = instanceType.getSchemeCode();
            appendCustomSql(sqlBuilder, projectIds, operation, dataPair, mainTableFilterColumn, field, schemeCode);
        }
        return sqlBuilder.toString();
    }

    private String generateContentSql(String operation,
                                      Pair<String, String> dataPair,
                                      String alias,
                                      Set<Long> projectIds,
                                      Map<String, FieldTableVO> predefinedFieldMap) {
        StringBuilder sqlBuilder = new StringBuilder();
        //去除searchVO.searchArgs.issueNum或searchVO.contents的项目code前缀
        List<ProjectInfoDTO> projectInfos = projectInfoMapper.selectByProjectIds(projectIds);
        if (projectInfos.isEmpty()) {
            return sqlBuilder.toString();
        }
        List<String> projectCodes = projectInfos.stream().map(ProjectInfoDTO::getProjectCode).collect(Collectors.toList());
        String content = dataPair.getFirst();
        Pair<String, String> pair = Pair.of(null, null);
        if (!ObjectUtils.isEmpty(content)) {
            String substringContent = content;
            for (String projectCode : projectCodes) {
                String prefix = SINGLE_QUOT + projectCode + "-";
                if (content.startsWith(prefix)) {
                    substringContent = content.substring(prefix.length());
                    substringContent = SINGLE_QUOT + substringContent;
                    break;
                }
            }
            pair = Pair.of(substringContent, null);
        }

        sqlBuilder.append(BaseConstants.Symbol.LEFT_BRACE);
        //content == summary和issueNum
        FieldTableVO summary = predefinedFieldMap.get(FieldCode.SUMMARY);
        String summaryCol = buildColumnByCode(summary.getField(), alias, FieldCode.SUMMARY);
        appendPredefinedSql(sqlBuilder, operation, pair, summaryCol);
        sqlBuilder.append(" or ");

        FieldTableVO issueNum = predefinedFieldMap.get(FieldCode.ISSUE_NUM);
        String issueNumCol = buildColumnByCode(issueNum.getField(), alias, FieldCode.ISSUE_NUM);
        appendPredefinedSql(sqlBuilder, operation, pair, issueNumCol);
        sqlBuilder.append(BaseConstants.Symbol.RIGHT_BRACE);
        return sqlBuilder.toString();
    }

    private String generateYqCloudNumSql(String operation,
                                         Pair<String, String> dataPair,
                                         FieldTableVO fieldTable,
                                         String alias,
                                         Set<Long> projectIds) {
        StringBuilder sqlBuilder = new StringBuilder();
        String primaryKey = DEFAULT_PRIMARY_KEY;
        String innerColumn = INSTANCE_ID;
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        Operation opt = Operation.valueOf(operation);
        switch (Operation.valueOf(operation)) {
            case IS_NOT_NULL:
            case IS_NULL:
                sqlBuilder.append(
                        String.format(LINKED_TABLE_IS_NULL_OR_NOT_NULL, mainTableFilterColumn, opt.getOpt(), innerColumn, table, projectIdStr, " and source = 'yqcloud' and instance_type = 'issue'"));
                break;
            case EQUAL:
                String value = dataPair.getFirst();
                sqlBuilder.append(String.format(YQ_CLOUD_NUM_LIKE_OR_EQUAL, mainTableFilterColumn, Operation.IN.getOpt(), innerColumn, table, projectIdStr, opt.getOpt(), value));
                break;
            case LIKE:
                String valueStr = String.format(LIKE_VALUE, "%", dataPair.getFirst(), "%");
                sqlBuilder.append(String.format(YQ_CLOUD_NUM_LIKE_OR_EQUAL, mainTableFilterColumn, Operation.IN.getOpt(), innerColumn, table, projectIdStr, opt.getOpt(), valueStr));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private void appendCustomSql(StringBuilder sqlBuilder,
                                 Set<Long> projectIds,
                                 String operation,
                                 Pair<String, String> dataPair,
                                 String mainTableFilterColumn,
                                 Field field,
                                 String schemeCode) {
        Long fieldId = field.getFieldId();
        String fieldType = field.getFieldType();
        String columnName;
        if (FieldTypeCnName.TIME_TYPES.contains(fieldType)) {
            columnName = "date_value";
        } else if (FieldTypeCnName.NUMBER_TYPES.contains(fieldType)) {
            columnName = "number_value";
        } else if (FieldTypeCnName.INPUT.getCode().equals(fieldType)) {
            columnName = "string_value";
        } else if (FieldTypeCnName.TEXT.getCode().equals(fieldType)) {
            columnName = "text_value";
        } else {
            throw new CommonException("error.illegal.field.type");
        }
        if (FieldTypeCnName.TIME.getCode().equals(fieldType)) {
            //时间选择器格式化
            columnName = "DATE_FORMAT(" + columnName + ", '%H:%i:%s')";
        }
        switch (Operation.valueOf(operation)) {
            case BETWEEN:
                sqlBuilder.append(
                        String.format(
                                CUSTOM_FIELD_DATE_BETWEEN,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                dataPair.getFirst(),
                                columnName,
                                dataPair.getSecond(),
                                schemeCode));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(
                        String.format(
                                CUSTOM_FIELD_DATE_IS_NULL_OR_NOT_NULL,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                " is not null ",
                                schemeCode));
                break;
            case IS_NULL:
                sqlBuilder.append(
                        String.format(
                                CUSTOM_FIELD_DATE_IS_NULL_OR_NOT_NULL,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                " is null ",
                                schemeCode));
                break;
            case EQUAL:
                String value = dataPair.getFirst();
                sqlBuilder.append(
                        String.format(
                                CUSTOM_FIELD_EQUAL_OR_LIKE,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                "=",
                                value,
                                schemeCode));
                break;
            case LIKE:
                String valueStr = String.format(LIKE_VALUE, "%", dataPair.getFirst(), "%");
                sqlBuilder.append(
                        String.format(
                                CUSTOM_FIELD_EQUAL_OR_LIKE,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                "like",
                                valueStr,
                                schemeCode));
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
    }

    private void appendPredefinedSql(StringBuilder sqlBuilder,
                                     String operation,
                                     Pair<String, String> dataPair,
                                     String column) {
        switch (Operation.valueOf(operation)) {
            case BETWEEN:
                sqlBuilder.append(
                        String.format(
                                DATE_BETWEEN,
                                column,
                                dataPair.getFirst(),
                                column,
                                dataPair.getSecond()));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_IS_NOT_NULL, column));
                break;
            case IS_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_IS_NULL, column));
                break;
            case EQUAL:
                String value = dataPair.getFirst();
                sqlBuilder.append(String.format(SELF_TABLE_EQUAL, column, "=", value));
                break;
            case LIKE:
                String valueStr = String.format(LIKE_VALUE, "%", dataPair.getFirst(), "%");
                sqlBuilder.append(String.format(SELF_TABLE_EQUAL, column, "like", valueStr));
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
    }

    private String queryPatterByFieldCode(Field field) {
        String patter = BaseConstants.Pattern.DATETIME;
        if (field.getPredefined()) {
            //预定义
            String fieldCode = field.getFieldCode();
            if (DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
                patter = BaseConstants.Pattern.DATETIME_MM;
            }
        } else {
            String fieldType = field.getFieldType();
            if (FieldTypeCnName.TIME.getCode().equals(fieldType)) {
                patter = BaseConstants.Pattern.TIME_SS;
            }
        }
        return patter;
    }

    private String buildColumnByCode(String column, String alias, String fieldCode) {
        String thisColumn = buildMainTableFilterColumn(column, alias);
        if (DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
            //需要处理特殊日期格式
            thisColumn = String.format(DATE_FORMATTER, thisColumn, SQL_YYYY_MM_DD_HH_MM);
        }
        return thisColumn;
    }

    private Pair<String, String> getPairValues(Condition condition,
                                               Class clazz,
                                               Field field) {
        Pair<Value, Value> pair = condition.getBetweenValues();
        String operation = condition.getOperation();
        if (Operation.isNotNull(operation) || Operation.isNull(operation)) {
            //为空或不为空 跳过
            return Pair.of(null, null);
        }
        String first = null;
        String second = null;
        if (pair == null) {
            Value value = condition.getValue();
            Assert.notNull(value, DATA_INVALID);
            first = getValueByClazz(clazz, value, field);
        } else {
            Value firstValue = pair.getFirst();
            Value secondValue = pair.getSecond();
            Assert.notNull(firstValue, DATA_INVALID);
            Assert.notNull(secondValue, DATA_INVALID);
            first = getValueByClazz(clazz, firstValue, field);
            second = getValueByClazz(clazz, secondValue, field);
        }
        return Pair.of(first, second);
    }

    private String getValueByClazz(Class clazz, Value value, Field field) {
        String valueStr = null;
        if (clazz == Date.class) {
            String patter = queryPatterByFieldCode(field);
            DateFormat dateFormat = new SimpleDateFormat(patter);
            Date date = value.getValueDate();
            Assert.notNull(date, DATA_INVALID);
            //日期value需要拼上'
            valueStr = appendSingleQuot(dateFormat.format(date));
        } else if (clazz == BigDecimal.class) {
            BigDecimal bigDecimal = value.getValueDecimal();
            Assert.notNull(bigDecimal, DATA_INVALID);
            valueStr = bigDecimal.toString();
        } else if (clazz == String.class) {
            valueStr = value.getValueStr();
            Assert.notNull(valueStr, DATA_INVALID);
            valueStr = appendSingleQuot(valueStr);
        }
        return valueStr;
    }

    private String appendSingleQuot(String value) {
        return new StringBuilder(SINGLE_QUOT).append(value).append(SINGLE_QUOT).toString();
    }

    private String generateSelectorSql(Map<String, FieldTableVO> predefinedFieldMap,
                                       Condition condition,
                                       Set<Long> projectIds,
                                       InstanceType instanceType) {
        StringBuilder sqlBuilder = new StringBuilder();
        Field field = condition.getField();
        String operation = condition.getOperation();
        //选择器，只支持in, not in, is null, is not null
        Assert.isTrue(Operation.SELECTOR_OPERATIONS.contains(operation), DATA_INVALID);
        List<? extends Object> values = getOptionValues(condition);
        boolean isPredefined = field.getPredefined();
        String alias = "ai";
        if (isPredefined) {
            //预定义字段
            String fieldCode = field.getFieldCode();
            FieldTableVO fieldTable = predefinedFieldMap.get(fieldCode);
            Assert.notNull(fieldTable, DATA_INVALID);
            boolean isLinkedTable = !SearchConstant.TABLE_AGILE_ISSUE.equals(fieldTable.getTable());
            switch (fieldCode) {
                case FieldCode.TAG:
                    sqlBuilder.append(generateTagSql(operation, values, fieldTable, alias, projectIds));
                    break;
                case FieldCode.FIX_VERSION:
                    sqlBuilder.append(generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and relation_type = 'fix'", null));
                    break;
                case FieldCode.INFLUENCE_VERSION:
                    sqlBuilder.append(generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and relation_type = 'influence'", null));
                    break;
                case SearchConstant.Field.MY_STAR:
                    sqlBuilder.append(generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and type = 'issue'", INSTANCE_ID));
                    break;
                case SearchConstant.Field.MY_PARTICIPATE:
                    sqlBuilder.append(generateMyParticipateSql(projectIds, operation, alias, fieldTable, values));
                    break;
                default:
                    if (!isLinkedTable) {
                        //主表字段
                        sqlBuilder.append(generateSelfTableSql(operation, values, alias, fieldTable));
                    } else {
                        //关联表
                        sqlBuilder.append(generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "", null));
                    }
                    break;
            }
        } else {
            sqlBuilder.append(generateCustomFieldSelectorSql(alias, field, instanceType, operation, projectIds, values));
        }
        return sqlBuilder.toString();
    }

    private String generateMyParticipateSql(Set<Long> projectIds,
                                            String operation,
                                            String alias,
                                            FieldTableVO fieldTable,
                                            List<? extends Object> values) {
        StringBuilder sqlBuilder = new StringBuilder();
        sqlBuilder.append(BaseConstants.Symbol.LEFT_BRACE);
        String primaryKey = DEFAULT_PRIMARY_KEY;
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        String assigneeColumn = "assignee_id";
        assigneeColumn = buildMainTableFilterColumn(assigneeColumn, alias);
        String valueStr = StringUtils.join(values, BaseConstants.Symbol.COMMA);
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        Operation opt = Operation.valueOf(operation);
        sqlBuilder.append(
                String.format(MY_PARTICIPATE,
                        assigneeColumn,
                        valueStr,
                        mainTableFilterColumn,
                        opt,
                        primaryKey,
                        fieldTable.getTable(),
                        projectIdStr,
                        valueStr,
                        valueStr));
        sqlBuilder.append(BaseConstants.Symbol.RIGHT_BRACE);
        return sqlBuilder.toString();
    }

    private String generateTagSql(String operation,
                                  List<? extends Object> values,
                                  FieldTableVO fieldTable,
                                  String alias,
                                  Set<Long> projectIds) {
        StringBuilder sqlBuilder = new StringBuilder();
        List<TagVO> tags;
        try {
            String json = objectMapper.writeValueAsString(values);
            tags = objectMapper.readValue(json, new TypeReference<List<TagVO>>() {
            });
        } catch (JsonProcessingException e) {
            throw new CommonException("error.convert.object.value", e);
        }
        if (tags == null) {
            return sqlBuilder.toString();
        }
        String primaryKey = DEFAULT_PRIMARY_KEY;
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        Operation opt = Operation.valueOf(operation);
        Iterator<TagVO> tagIterator = tags.iterator();
        StringBuilder conditionBuilder = new StringBuilder();
        while (tagIterator.hasNext()) {
            TagVO tag = tagIterator.next();
            conditionBuilder.append(BaseConstants.Symbol.LEFT_BRACE)
                    .append("tag_project_id = ")
                    .append(tag.getProjectId())
                    .append(" and app_service_code = ")
                    .append(appendSingleQuot(tag.getAppServiceCode()))
                    .append(" and tag_name = ")
                    .append(appendSingleQuot(tag.getTagName()))
                    .append(BaseConstants.Symbol.RIGHT_BRACE);
            if (tagIterator.hasNext()) {
                conditionBuilder.append(" or ");
            }
        }
        String conditionSql = conditionBuilder.toString();
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        String table = fieldTable.getTable();
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(
                                TAG_IN_OR_NOT_IN,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                primaryKey,
                                table,
                                projectIdStr,
                                conditionSql));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(
                        String.format(
                                LINKED_TABLE_IS_NULL_OR_NOT_NULL,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                primaryKey,
                                table,
                                projectIdStr,
                                ""));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateCustomFieldSelectorSql(String alias,
                                                  Field field,
                                                  InstanceType instanceType,
                                                  String operation,
                                                  Set<Long> projectIds,
                                                  List<? extends Object> values) {
        StringBuilder sqlBuilder = new StringBuilder();
        String primaryKey = DEFAULT_PRIMARY_KEY;
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        Long fieldId = field.getFieldId();
        String schemeCode = instanceType.getSchemeCode();
        Operation opt = Operation.valueOf(operation);
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(
                                CUSTOM_FIELD_IN_OR_NOT_IN,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                projectIdStr,
                                fieldId,
                                StringUtils.join(values, BaseConstants.Symbol.COMMA),
                                schemeCode));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(
                        String.format(
                                CUSTOM_FIELD_IS_NULL_OR_NOT_NULL,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                projectIdStr,
                                fieldId,
                                schemeCode));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateSelfTableSql(String operation,
                                        List<?> values, String alias,
                                        FieldTableVO fieldTable) {
        StringBuilder sqlBuilder = new StringBuilder();
        Operation opt = Operation.valueOf(operation);
        String mainTableFilterColumn = buildMainTableFilterColumn(fieldTable.getField(), alias);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(
                                SELF_TABLE_IN_OR_NOT_IN,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                StringUtils.join(values, BaseConstants.Symbol.COMMA)));
                break;
            case IS_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_ID_IS_NULL, mainTableFilterColumn, mainTableFilterColumn));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_ID_IS_NOT_NULL, mainTableFilterColumn, mainTableFilterColumn));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateLinkedTableSql(String operation,
                                          List<?> values,
                                          FieldTableVO fieldTable,
                                          String alias,
                                          Set<Long> projectIds,
                                          String additionalCondition,
                                          String innerColumn) {
        StringBuilder sqlBuilder = new StringBuilder();
        String dbColumn = fieldTable.getField();
        String primaryKey = DEFAULT_PRIMARY_KEY;
        if (innerColumn == null) {
            innerColumn = primaryKey;
        }
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        Operation opt = Operation.valueOf(operation);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(LINKED_TABLE_IN_OR_NOT_IN, mainTableFilterColumn, opt.getOpt(), innerColumn, table, projectIdStr, dbColumn, StringUtils.join(values, BaseConstants.Symbol.COMMA), additionalCondition));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(
                        String.format(LINKED_TABLE_IS_NULL_OR_NOT_NULL, mainTableFilterColumn, opt.getOpt(), innerColumn, table, projectIdStr, additionalCondition));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String buildMainTableFilterColumn(String column,
                                              String alias) {

        String mainTableFilterColumn;
        if (alias == null) {
            mainTableFilterColumn = column;
        } else {
            mainTableFilterColumn = alias + BaseConstants.Symbol.POINT + column;
        }
        return mainTableFilterColumn;
    }

    private List<? extends Object> getOptionValues(Condition condition) {
        List<? extends Object> values;
        Value value = condition.getValue();
        if (value == null) {
            return null;
        }
        Field field = condition.getField();
        String fieldCode = Optional.ofNullable(field.getFieldCode()).orElse("");
        boolean noEncryptFlag = Boolean.TRUE.equals(field.getNoEncryptFlag());
        switch (fieldCode) {
            case FieldCode.ENVIRONMENT:
                List<String> valueStrList = value.getValueStrList();
                List<String> result = new ArrayList<>();
                if (!ObjectUtils.isEmpty(valueStrList)) {
                    valueStrList.forEach(v -> result.add(appendSingleQuot(v)));
                }
                values = result;
                break;
            case FieldCode.TAG:
                values = value.getObjectList();
                break;
            default:
                if (noEncryptFlag) {
                    values = value.getNoEncryptIdList();
                } else {
                    values = value.getValueIdList();
                }
                break;
        }
        return values;
    }

    private Map<String, FieldTableVO> buildPredefinedFieldMap() {
        Map<String, FieldTableVO> map = new HashMap<>(SearchConstant.PREDEFINED_FIELD_TABLE_MAP);
        if (agilePluginService != null) {
            map.putAll(agilePluginService.queryAdvanceParamFieldTableMap());
        }
        //todo 瀑布，需求，项目群
        return map;
    }


}
