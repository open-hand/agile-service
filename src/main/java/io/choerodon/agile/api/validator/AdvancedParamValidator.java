package io.choerodon.agile.api.validator;

import static org.hzero.core.base.BaseConstants.ErrorCode.DATA_INVALID;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.search.Condition;
import io.choerodon.agile.api.vo.search.Field;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.api.vo.search.Value;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;

/**
 * @author superlee
 * @since 2022-11-04
 */
@Component
public class AdvancedParamValidator {

    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;


    public void validate(SearchParamVO searchParamVO) {
        List<Condition> conditions = Optional.ofNullable(searchParamVO.getConditions()).orElse(Collections.emptyList());
        List<Condition> advancedConditions = Optional.ofNullable(searchParamVO.getAdvancedConditions()).orElse(Collections.emptyList());
        //获取传入的预定义字段和自定义字段
        Set<String> predefinedFieldCodes = new HashSet<>();
        Set<Long> customFieldIds = new HashSet<>();
        conditions.forEach(condition -> {
            validateCondition(condition);
            findFieldIdAndCode(predefinedFieldCodes, customFieldIds, condition);
        });
        advancedConditions.forEach(condition -> {
            validateCondition(condition);
            findFieldIdAndCode(predefinedFieldCodes, customFieldIds, condition);
        });
        Map<String, ObjectSchemeFieldDTO> predefinedFieldMap = new HashMap<>();
        if (!predefinedFieldCodes.isEmpty()) {
            ObjectSchemeFieldDTO dto = new ObjectSchemeFieldDTO();
            dto.setSystem(true);
            List<ObjectSchemeFieldDTO> dbFields = objectSchemeFieldMapper.select(dto);
            //部分系统字段，只做界面筛选，在数据库里没有数据
            dbFields.addAll(SearchConstant.MOCK_FIELDS);
            dbFields.forEach(x -> {
                if (predefinedFieldCodes.contains(x.getCode())) {
                    predefinedFieldMap.put(x.getCode(), x);
                }
            });
        }
        Map<Long, ObjectSchemeFieldDTO> customFieldMap = new HashMap<>();
        if (!customFieldIds.isEmpty()) {
            //查询数据库判断自定义字段是否存在
            customFieldMap.putAll(objectSchemeFieldMapper.selectByIds(StringUtils.join(customFieldIds, BaseConstants.Symbol.COMMA))
                    .stream()
                    .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity())));
        }
        //过滤非法字段，重置fieldType
        searchParamVO.setConditions(filterIllegalAndSetFieldType(conditions, predefinedFieldMap, customFieldMap));
        searchParamVO.setAdvancedConditions(filterIllegalAndSetFieldType(advancedConditions, predefinedFieldMap, customFieldMap));
    }

    private void findFieldIdAndCode(Set<String> predefinedFieldCodes,
                                    Set<Long> customFieldIds,
                                    Condition condition) {
        Field field = condition.getField();
        if (Boolean.TRUE.equals(field.getPredefined())) {
            predefinedFieldCodes.add(field.getFieldCode());
        } else {
            customFieldIds.add(field.getFieldId());
        }
    }


    private List<Condition> filterIllegalAndSetFieldType(List<Condition> conditions,
                                                         Map<String, ObjectSchemeFieldDTO> predefinedFieldMap,
                                                         Map<Long, ObjectSchemeFieldDTO> customFieldMap) {
        if (ObjectUtils.isEmpty(conditions)) {
            return Collections.emptyList();
        }
        List<Condition> result = new ArrayList<>();
        conditions.forEach(condition -> {
            Field field = condition.getField();
            boolean predefined = Boolean.TRUE.equals(field.getPredefined());
            boolean skip = true;
            if (predefined) {
                String fieldCode = field.getFieldCode();
                //todo 处理不在数据库，但在FieldCode中定义的字段，如所属特性
                ObjectSchemeFieldDTO dto = predefinedFieldMap.get(fieldCode);
                skip = (dto == null);
                if (!skip) {
                    field.setFieldType(dto.getFieldType());
                }
                //设置是否取不加密字段
                field.setNoEncryptFlag(SearchConstant.NO_ENCRYPT_FIELDS.contains(fieldCode));
            } else {
                Long fieldId = field.getFieldId();
                ObjectSchemeFieldDTO dto = customFieldMap.get(fieldId);
                skip = (dto == null);
                if (!skip) {
                    field.setFieldType(dto.getFieldType());
                }
            }
            if (skip) {
                return;
            }
            result.add(condition);
            condition.setSubConditions(filterIllegalAndSetFieldType(condition.getSubConditions(), predefinedFieldMap, customFieldMap));
        });
        return result;
    }

    private void validateCondition(Condition condition) {
        validateField(condition);
        String operation = condition.getOperation();
        Assert.isTrue(SearchConstant.Operation.contains(operation), DATA_INVALID);
        Assert.isTrue(SearchConstant.Relationship.contains(condition.getRelationship()), DATA_INVALID);
        validateByOperation(condition, operation);
        List<Condition> subConditions = condition.getSubConditions();
        if (!ObjectUtils.isEmpty(subConditions)) {
            subConditions.forEach(sub -> validateCondition(sub));
        }
    }

    private void validateByOperation(Condition condition, String operation) {
        switch (SearchConstant.Operation.valueOf(operation)) {
            case IN:
            case EQUAL:
            case LIKE:
                Assert.notNull(condition.getValue(), DATA_INVALID);
                validateValue(condition.getValue());
                break;
            case BETWEEN:
                Pair<Value, Value> betweenValues = condition.getBetweenValues();
                Assert.isTrue(betweenValues != null, DATA_INVALID);
                validateValue(betweenValues.getFirst());
                validateValue(betweenValues.getSecond());
                break;
            case BRACKET:
                Assert.isTrue(!ObjectUtils.isEmpty(condition.getSubConditions()), DATA_INVALID);
                break;
            case IS_NULL:
            case IS_NOT_NULL:
            default:
                break;
        }
    }

    private void validateField(Condition condition) {
        //括号操作符可以没有field字段
        if (SearchConstant.Operation.isBracket(condition.getOperation())) {
            return;
        }
        Field field = condition.getField();
        Assert.notNull(field, DATA_INVALID);
        Boolean predefined = field.getPredefined();
        Assert.notNull(predefined, DATA_INVALID);
        if (Boolean.TRUE.equals(field.getPredefined())) {
            //系统字段，fieldCode不能为空
            Assert.notNull(field.getFieldCode(), DATA_INVALID);
        } else {
            //自定义字段，fieldId不能为空
            Assert.notNull(field.getFieldId(), DATA_INVALID);
        }
    }

    private void validateValue(Value value) {
        if (value == null) {
            return;
        }
        String valueSpecial = value.getValueSpecial();
        if (valueSpecial != null) {
            Assert.isTrue(SearchConstant.ValueSpecial.contains(valueSpecial), DATA_INVALID);
        }
    }
}
