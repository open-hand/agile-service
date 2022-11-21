package io.choerodon.agile.app.service.v2.impl;

import static io.choerodon.agile.infra.enums.search.SearchConstant.SqlTemplate.*;
import static org.hzero.core.base.BaseConstants.ErrorCode.DATA_INVALID;
import static io.choerodon.agile.infra.enums.search.SearchConstant.Operation;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.validator.AdvancedParamValidator;
import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.api.vo.search.Field;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.api.vo.search.Condition;
import io.choerodon.agile.api.vo.search.Value;
import io.choerodon.agile.app.service.v2.PredefinedFieldSqlGenerator;
import io.choerodon.agile.app.service.v2.AdvancedParamParserService;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldTypeCnName;
import io.choerodon.agile.infra.enums.InstanceType;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.utils.SqlUtil;
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
    private PredefinedFieldSqlGenerator predefinedFieldSqlGenerator;

    private static final String DEFAULT_PRIMARY_KEY = "issue_id";
    private static final String INSTANCE_ID = "instance_id";


    @Override
    public String parse(InstanceType instanceType,
                        SearchParamVO searchParamVO,
                        Set<Long> projectIds,
                        Map<String, FieldTableVO> predefinedFieldMap) {
        advancedParamValidator.validate(searchParamVO);
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
                        sqlBuilder.append(generateSqlByClazz(predefinedFieldMap, condition, projectIds, instanceType, List.class));
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
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
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
        Operation opt = Operation.valueOf(operation);
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        Map<String, String> dataMap = new HashMap<>();
        dataMap.put("mainTableCol", mainTableFilterColumn);
        dataMap.put("opt", opt.getOpt());
        dataMap.put("projectIdStr", projectIdStr);
        dataMap.put("fieldId", fieldId.toString());
        dataMap.put("schemeCode", schemeCode);
        dataMap.put("columnName", columnName);
        switch (Operation.valueOf(operation)) {
            case BETWEEN:
                dataMap.put("opt", Operation.IN.getOpt());
                dataMap.put("first", dataPair.getFirst());
                dataMap.put("second", dataPair.getSecond());
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, CUSTOM_FIELD_DATE_BETWEEN));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, CUSTOM_FIELD_IS_NULL_OR_NOT_NULL));
                break;
            case EQUAL:
                String value = dataPair.getFirst();
                dataMap.put("columnOpt", Operation.EQUAL.getOpt());
                dataMap.put("columnValue", value);
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, CUSTOM_FIELD_EQUAL_OR_LIKE));
                break;
            case LIKE:
                String valueStr = String.format(LIKE_VALUE, "%", dataPair.getFirst(), "%");
                dataMap.put("columnOpt", Operation.LIKE.getOpt());
                dataMap.put("columnValue", valueStr);
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, CUSTOM_FIELD_EQUAL_OR_LIKE));
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
            if (SqlUtil.DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
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
            valueStr = SqlUtil.appendSingleQuot(dateFormat.format(date));
        } else if (clazz == BigDecimal.class) {
            BigDecimal bigDecimal = value.getValueDecimal();
            Assert.notNull(bigDecimal, DATA_INVALID);
            valueStr = bigDecimal.toString();
        } else if (clazz == String.class) {
            valueStr = value.getValueStr();
            Assert.notNull(valueStr, DATA_INVALID);
            valueStr = SqlUtil.appendSingleQuot(valueStr);
        }
        return valueStr;
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
        List<? extends Object> values = null;
        Pair<String, String> dataPair = null;
        //判断与类型相关的操作符
        if (clazz == Date.class || clazz == BigDecimal.class) {
            options.addAll(Operation.DATE_OR_NUMBER_OPERATIONS);
            dataPair = getPairValues(condition, clazz, field);
        } else if (clazz == String.class) {
            options.addAll(Operation.STRING_OPERATIONS);
            dataPair = getPairValues(condition, clazz, field);
        } else if (clazz == List.class) {
            //选择器类型字段取值
            options.addAll(Operation.SELECTOR_OPERATIONS);
            values = getOptionValues(condition);
        }

        Assert.isTrue(options.contains(operation), DATA_INVALID);
        String fieldCode = field.getFieldCode();
        boolean isPredefined = field.getPredefined();
        String alias = "ai";
        boolean isSelector = (clazz == List.class);
        if (isPredefined) {
            FieldTableVO fieldTable = predefinedFieldMap.get(fieldCode);
            Assert.notNull(fieldTable, DATA_INVALID);
            sqlBuilder.append(predefinedFieldSqlGenerator.parseSql(fieldTable, condition, projectIds, values, dataPair, isSelector));
        } else {
            if (isSelector) {
                sqlBuilder.append(generateCustomFieldSelectorSql(field, instanceType, operation, projectIds, values));
            } else {
                String primaryKey = DEFAULT_PRIMARY_KEY;
                String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
                String schemeCode = instanceType.getSchemeCode();
                appendCustomSql(sqlBuilder, projectIds, operation, dataPair, mainTableFilterColumn, field, schemeCode);
            }
        }
        return sqlBuilder.toString();
    }


    private String generateCustomFieldSelectorSql(Field field,
                                                  InstanceType instanceType,
                                                  String operation,
                                                  Set<Long> projectIds,
                                                  List<? extends Object> values) {
        String alias = "ai";
        StringBuilder sqlBuilder = new StringBuilder();
        String primaryKey = DEFAULT_PRIMARY_KEY;
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
        Long fieldId = field.getFieldId();
        String schemeCode = instanceType.getSchemeCode();
        Operation opt = Operation.valueOf(operation);
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        Map<String, String> dataMap = new HashMap<>();
        dataMap.put("mainTableCol", mainTableFilterColumn);
        dataMap.put("opt", opt.getOpt());
        dataMap.put("projectIdStr", projectIdStr);
        dataMap.put("fieldId", fieldId.toString());
        dataMap.put("schemeCode", schemeCode);
        dataMap.put("optionIds", StringUtils.join(values, BaseConstants.Symbol.COMMA));
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, CUSTOM_FIELD_IN_OR_NOT_IN));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, CUSTOM_FIELD_IS_NULL_OR_NOT_NULL));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
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
            case FieldCode.FEATURE_TYPE:
                List<String> valueStrList = value.getValueStrList();
                List<String> result = new ArrayList<>();
                if (!ObjectUtils.isEmpty(valueStrList)) {
                    valueStrList.forEach(v -> result.add(SqlUtil.appendSingleQuot(v)));
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

}
