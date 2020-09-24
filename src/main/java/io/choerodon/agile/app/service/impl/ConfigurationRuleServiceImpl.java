package io.choerodon.agile.app.service.impl;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.RuleExpressVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.dto.ConfigurationRuleDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.enums.ConfigurationRule;
import io.choerodon.agile.infra.enums.CustomFieldType;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.mapper.ConfigurationRuleFiledMapper;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.utils.CommonMapperUtil;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

/**
 * 页面配置服务实现类
 *
 * @author jiaxu.cui@hand-china.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ConfigurationRuleServiceImpl implements ConfigurationRuleService {

    private static final Logger LOGGER = LoggerFactory.getLogger(QuickFilterServiceImpl.class);

    @Autowired
    private ConfigurationRuleMapper configurationRuleMapper;
    @Autowired
    protected ConfigurationRuleFiledMapper configurationRuleFiledMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private ModelMapper modelMapper;
    
    public static final DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATETIME);

    private String renderPredefinedSql(String operation, String field, List<Supplier<String>> conditionList) {
        StringBuilder sb = new StringBuilder();
        String sqlOp = ConfigurationRule.OpSqlMapping.valueOf(operation).getSqlOp();
        String table = ConfigurationRule.FieldTableMapping.valueOf(field).getTable();
        if (CollectionUtils.isNotEmpty(conditionList)){
            sb.append(ConfigurationRule.TEMPLATE_SQL_WHERE)
              .append(conditionList.stream().map(Supplier::get).collect(Collectors.joining(ConfigurationRule.TEMPLATE_SQL_AND)));
        }
        return String.format(ConfigurationRule.TEMPLATE_PREDEFINED_SQL, sqlOp, table, sb.toString());
    }

    private String inSql(String field, Collection<?> value){
        String sql;
        if (CollectionUtils.isEmpty(value)) {
            return "";
        }
        String valueList = value.stream().map(this::valueToString)
                .collect(Collectors.joining(BaseConstants.Symbol.COMMA));
        sql = String.format(ConfigurationRule.TEMPLATE_IN_SQL, field, valueList);
        return sql;
    }

    protected void dealCaseVersion(RuleExpressVO quickFilterValueVO, String field, Object value,String preOp, String operation, StringBuilder sqlQuery) {
        if ("fix_version".equals(quickFilterValueVO.getFieldCode())) {
            sqlQuery.append(renderPredefinedSql(preOp, field, 
                    Arrays.asList(() -> this.conditionSql("relation_type", ConfigurationRule.OpSqlMapping.eq.name(), "fix"), () -> this.conditionSql(field, operation, value))));
        } else if ("influence_version".equals(quickFilterValueVO.getFieldCode())) {
            sqlQuery.append(renderPredefinedSql(preOp, field,
                    Arrays.asList(() -> this.conditionSql("relation_type", ConfigurationRule.OpSqlMapping.eq.name(),"influence"), () -> this.conditionSql(field, operation, value))));
        }
    }

    private String getSqlQuery(ConfigurationRuleVO configurationRuleVO, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<RuleExpressVO> ruleExpressVOList = configurationRuleVO.getExpressList();
        StringBuilder sqlQuery = new StringBuilder();
        for (RuleExpressVO ruleExpressVO : ruleExpressVOList) {
            if (StringUtils.isNotBlank(ruleExpressVO.getRelationshipWithPervious())){
                sqlQuery.append(ruleExpressVO.getRelationshipWithPervious());
            }
            Boolean predefined = ruleExpressVO.getPredefined();
            String fieldCode = ruleExpressVO.getFieldCode();
            if (ObjectUtils.isEmpty(predefined)) {
                String errorMsg = "error." + fieldCode + ".predefined.null";
                throw new CommonException(errorMsg);
            }
            if (predefined) {
                appendPredefinedFieldSql(sqlQuery, ruleExpressVO);
            } else {
                sqlQuery.append(appendCustomFieldSql(ruleExpressVO, organizationId, projectId));
            }
        }
        return sqlQuery.toString();
    }

    private String appendCustomFieldSql(RuleExpressVO ruleExpressVO, Long organizationId, Long projectId) {
        String fieldCode = ruleExpressVO.getFieldCode();
        ObjectSchemeFieldDTO objectSchemeField = objectSchemeFieldService.queryByFieldCode(organizationId, projectId, fieldCode);
        if (ObjectUtils.isEmpty(objectSchemeField)) {
            throw new CommonException("error.custom.field." + fieldCode + ".not.existed");
        }
        Long fieldId = objectSchemeField.getId();
        String operation = ruleExpressVO.getOperation();
        String preOp = ConfigurationRule.OpSqlMapping.getPreOp(operation).name();
        Object value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueIdList() : ruleExpressVO.getValueId();
        
        String customFieldType = ruleExpressVO.getFieldType();
        CustomFieldType.contains(customFieldType, true);

        if (CustomFieldType.isOption(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, 
                    Collections.singletonList(() ->  conditionSql("ffv.option_id", operation, value)));
        } else if (CustomFieldType.isDate(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() -> 
                    conditionSql(getUnixTimeExpress("ffv.date_value"), operation, getUnixTimeExpress(df.format(value)))));
        } else if (CustomFieldType.isDateHms(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() ->
                    conditionSql(getTimeFieldExpress("ffv.date_value"), operation, getTimeValueExpress(df.format(value)))));
        } else if (CustomFieldType.isNumber(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, 
                    Collections.singletonList(() -> conditionSql("ffv.number_value", operation, value)));
        } else if (CustomFieldType.isString(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId,
                    Collections.singletonList(() ->  conditionSql("ffv.string_value", operation, value)));
        } else  {
            //text
            return renderCustomSql(preOp, projectId, fieldId,
                    Collections.singletonList(() ->  conditionSql("ffv.text_value", operation, value)));
        }
    }

    private String renderCustomSql(String operation, Long projectId, Long fieldId, List<Supplier<String>> conditionList) {
        StringBuilder sb = new StringBuilder();
        String sqlOp = ConfigurationRule.OpSqlMapping.valueOf(operation).getSqlOp();
        if (CollectionUtils.isNotEmpty(conditionList)){
            sb.append(ConfigurationRule.TEMPLATE_SQL_AND)
                    .append(conditionList.stream().map(Supplier::get).collect(Collectors.joining(ConfigurationRule.TEMPLATE_SQL_AND)));
        }
        return String.format(ConfigurationRule.TEMPLATE_CUSTOM_SQL, sqlOp, projectId, fieldId,sb.toString());
    }

    protected void appendPredefinedFieldSql(StringBuilder sqlQuery, RuleExpressVO ruleExpressVO) {
        String fieldType = ruleExpressVO.getFieldType();
        String operation = ruleExpressVO.getOperation();
        Object value;
        switch (fieldType){
            case FieldType.RulePredefind.LONG_TYPE:
                value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueIdList() : ruleExpressVO.getValueId();
                break;
            case FieldType.RulePredefind.DATE_TYPE:
                value = ruleExpressVO.getValueDate();
                break;
            case FieldType.RulePredefind.TEXT_TYPE:
                value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueStrList() : ruleExpressVO.getValueStr();
                break;
            case FieldType.RulePredefind.DECIMAL_TYPE:
                value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueDecimalList() : ruleExpressVO.getValueDecimal();
                break;
            default:
                throw new CommonException("error.configuration.fieldType.error");
        }
        processPredefinedField(sqlQuery, ruleExpressVO, value, operation);
    }

    protected void processPredefinedField(StringBuilder sqlQuery, RuleExpressVO quickFilterValueVO, Object value, String operation) {
        String preOp = ConfigurationRule.OpSqlMapping.getPreOp(operation).name();
        String field = configurationRuleFiledMapper.selectByPrimaryKey(quickFilterValueVO.getFieldCode()).getField();
        switch (field) {
            case "version_id":
                dealCaseVersion(quickFilterValueVO, field, value, preOp, operation, sqlQuery);
                break;
            case "component_id":
            case "label_id":
            case "sprint_id":
                sqlQuery.append(renderPredefinedSql(preOp, field, Collections.singletonList(() -> this.conditionSql(field, operation, value))));
                break;
            case "creation_date":
            case "last_update_date":
                sqlQuery.append(conditionSql(getUnixTimeExpress(field), operation, getUnixTimeExpress(df.format(value))));
                break;
            default:
                if (ConfigurationRule.OpSqlMapping.isCollOp(operation)){
                    sqlQuery.append(renderPredefinedSql(preOp, field, Collections.singletonList(() -> this.conditionSql(field, operation, value))));
                }else {
                    sqlQuery.append(conditionSql(field, operation, valueToString(value)));
                }
                break;
        }
    }

    private String valueToString(Object value) {
        if (value instanceof String){
            return "'" + value + "'";
        }else if(value instanceof Date){
            return df.format(value);
        }else {
            return value.toString();
        }
    }

    private String getUnixTimeExpress(String field) {
        return String.format(ConfigurationRule.TEMPLATE_UNIX_TIMESTAMP_EXPRESS, field);
    }

    private String getTimeFieldExpress(String field) {
        return String.format(ConfigurationRule.TEMPLATE_TIME_FIELD_EXPRESS, field);
    }

    private String getTimeValueExpress(String field) {
        return String.format(ConfigurationRule.TEMPLATE_TIME_VALUE_EXPRESS, field);
    }

    private String conditionSql(String field, String operation, Object value) {
        if (Objects.isNull(value)) {
            return "";
        }
        if (value instanceof Collection){
            return inSql(field, (Collection<?>)value);
        }
        if (ConfigurationRule.OpSqlMapping.isLike(operation)){
            value = String.format(ConfigurationRule.TEMPLATE_LIKE_VALUE_SQL, value);
        }
        return String.format(ConfigurationRule.TEMPLATE_CONDITION_SQL, field, operation, value);
    }

    @Override
    public ConfigurationRuleVO create(Long projectId, ConfigurationRuleVO configurationRuleVO) {
        if (checkName(projectId, configurationRuleVO.getName())) {
            throw new CommonException("error.ruleName.exist");
        }
        String sqlQuery = getSqlQuery(configurationRuleVO, projectId);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(configurationRuleVO, ConfigurationRuleDTO.class);
        configurationRuleDTO.setExpressFormat(CommonMapperUtil.writeValueAsString(configurationRuleVO.getExpressFormat()));
        configurationRuleDTO.setSqlQuery(sqlQuery);
        if (configurationRuleMapper.insert(configurationRuleDTO) != 1) {
            throw new CommonException("error.rule.insert");
        }
        return modelMapper.map(configurationRuleDTO, ConfigurationRuleVO.class);
    }

    private Boolean checkNameUpdate(Long projectId, Long ruleId, String ruleName) {
        boolean flag = true;
        if (StringUtils.isNotBlank(ruleName)){
            flag = false;
        }
        ConfigurationRuleDTO check = new ConfigurationRuleDTO();
        check.setProjectId(projectId);
        check.setName(ruleName);
        ConfigurationRuleDTO ruleDTO = configurationRuleMapper.selectOne(check);
        if (ruleId.equals(ruleDTO.getId())) {
            flag = false;
        }
        return flag;
    }

    @Override
    public ConfigurationRuleVO update(Long projectId, Long ruleId, ConfigurationRuleVO ConfigurationRuleVO) {
        if (checkNameUpdate(projectId, ruleId, ConfigurationRuleVO.getName())) {
            throw new CommonException("error.ruleName.exist");
        }
        String sqlQuery = getSqlQuery(ConfigurationRuleVO, projectId);
        ConfigurationRuleVO.setId(ruleId);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(ConfigurationRuleVO, ConfigurationRuleDTO.class);
        configurationRuleDTO.setSqlQuery(sqlQuery);
        configurationRuleDTO.setExpressFormat(ConfigurationRuleVO.getExpressFormat());
        return updateBySelective(configurationRuleDTO);
    }

    @Override
    public void deleteById(Long projectId, Long filterId) {
        ConfigurationRuleDTO quickFilterDTO = configurationRuleMapper.selectByPrimaryKey(filterId);
        if (quickFilterDTO == null) {
            throw new CommonException("error.rule.get");
        }
        if (configurationRuleMapper.deleteByPrimaryKey(filterId) != 1) {
            throw new CommonException("error.rule.delete");
        }
    }

    @Override
    public ConfigurationRuleVO queryById(Long projectId, Long filterId) {
        ConfigurationRuleDTO quickFilterDTO = configurationRuleMapper.selectByPrimaryKey(filterId);
        if (quickFilterDTO == null) {
            throw new CommonException("error.rule.get");
        }
        return modelMapper.map(quickFilterDTO, ConfigurationRuleVO.class);
    }

    @Override
    public List<ConfigurationRuleVO> listByProjectId(Long projectId) {
        List<ConfigurationRuleVO> configurationRuleVOList = configurationRuleMapper.queryFiltersByProjectId(projectId);
        configurationRuleVOList.forEach(v -> v.setExpressFormat(v.getExpressFormat()));
        return configurationRuleVOList;
    }

    @Override
    public Boolean checkName(Long projectId, String quickFilterName) {
        ConfigurationRuleDTO configurationRuleDTO = new ConfigurationRuleDTO();
        configurationRuleDTO.setProjectId(projectId);
        configurationRuleDTO.setName(quickFilterName);
        List<ConfigurationRuleDTO> quickFilterDTOList = configurationRuleMapper.select(configurationRuleDTO);
        return quickFilterDTOList != null && !quickFilterDTOList.isEmpty();
    }

    public ConfigurationRuleVO updateBySelective(ConfigurationRuleDTO configurationRuleDTO) {
        if (configurationRuleMapper.updateByPrimaryKeySelective(configurationRuleDTO) != 1) {
            throw new CommonException("error.rule.update");
        }
        return modelMapper.map(configurationRuleDTO, ConfigurationRuleVO.class);
    }
}
