package io.choerodon.agile.app.service.impl;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.RuleExpressVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.dto.ConfigurationRuleDTO;
import io.choerodon.agile.infra.dto.ConfigurationRuleReceiverDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.ConfigurationRule;
import io.choerodon.agile.infra.enums.CustomFieldType;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.ConfigurationRuleFiledMapper;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.mapper.ConfigurationRuleReceiverMapper;
import io.choerodon.agile.infra.utils.CommonMapperUtil;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.modelmapper.ModelMapper;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * 页面配置服务实现类
 *
 * @author jiaxu.cui@hand-china.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ConfigurationRuleServiceImpl implements ConfigurationRuleService {

    @Autowired
    private ConfigurationRuleMapper configurationRuleMapper;
    @Autowired
    protected ConfigurationRuleFiledMapper configurationRuleFiledMapper;
    @Autowired
    private ConfigurationRuleReceiverMapper configurationRuleReceiverMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ModelMapper modelMapper;
    
    public static final DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATETIME);

    @Override
    public ConfigurationRuleVO create(Long projectId, ConfigurationRuleVO configurationRuleVO) {
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getReceiverList()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getExpressList()), BaseConstants.ErrorCode.DATA_INVALID);
        checkUniqueName(projectId, configurationRuleVO.getName());
        String sqlQuery = getSqlQuery(configurationRuleVO, projectId);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(configurationRuleVO, ConfigurationRuleDTO.class);
        configurationRuleDTO.setExpressFormat(CommonMapperUtil.writeValueAsString(configurationRuleVO.getExpressList()));
        configurationRuleDTO.setSqlQuery(sqlQuery);
        configurationRuleDTO.setSource(ConfigurationRuleDTO.SOURCE_CUSTOM);
        if (configurationRuleMapper.insert(configurationRuleDTO) != 1) {
            throw new CommonException("error.rule.insert");
        }
        createProjectReportReceiver(projectId,configurationRuleVO, configurationRuleDTO);
        return configurationRuleVO;
    }

    @Override
    public void checkUniqueName(Long projectId, String name) {
        ConfigurationRuleDTO configurationRuleDTO = new ConfigurationRuleDTO();
        configurationRuleDTO.setProjectId(projectId);
        configurationRuleDTO.setName(name);
        List<ConfigurationRuleDTO> exist = configurationRuleMapper.select(configurationRuleDTO);
        if (CollectionUtils.isNotEmpty(exist)){
            throw new CommonException(BaseConstants.ErrorCode.DATA_INVALID);
        }
    }

    @Override
    public ConfigurationRuleVO update(Long projectId, Long ruleId, ConfigurationRuleVO configurationRuleVO) {
        // 检查是否是预定义规则
        checkPredefinedRule(projectId, ruleId);
        // 检查是否名称唯一
        checkUniqueName(projectId, configurationRuleVO.getName());
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getReceiverList()), BaseConstants.ErrorCode.DATA_INVALID);
        configurationRuleVO.setId(ruleId);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(configurationRuleVO, ConfigurationRuleDTO.class);
        configurationRuleDTO.setSqlQuery(getSqlQuery(configurationRuleVO, projectId));
        configurationRuleDTO.setExpressFormat(CommonMapperUtil.writeValueAsString(configurationRuleVO.getExpressList()));
        if (configurationRuleMapper.updateOptional(configurationRuleDTO, ConfigurationRuleDTO.FIELD_SQL_QUERY,
                ConfigurationRuleDTO.FIELD_EXPRESS_QUERY, ConfigurationRuleDTO.FIELD_EXPRESS_FORMAT) != 1) {
            throw new CommonException("error.rule.update");
        }
        // 更新通知对象
        configurationRuleReceiverMapper.delete(new ConfigurationRuleReceiverDTO(configurationRuleVO.getId(), projectId));
        createProjectReportReceiver(projectId, configurationRuleVO, configurationRuleDTO);
        return configurationRuleVO;
    }

    private void checkPredefinedRule(Long projectId, Long ruleId) {
        ConfigurationRuleDTO configurationRuleDTO = configurationRuleMapper.selectOne(new ConfigurationRuleDTO(ruleId
                , projectId));
        Assert.notNull(configurationRuleDTO, BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        if (StringUtils.equals(ConfigurationRuleDTO.SOURCE_PREDEFINED, configurationRuleDTO.getSource())){
            throw new CommonException(BaseConstants.ErrorCode.DATA_INVALID);
        }
    }

    @Override
    public void deleteById(Long projectId, Long ruleId) {
        Assert.notNull(projectId, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(ruleId, BaseConstants.ErrorCode.DATA_INVALID);
        // 检查是否是预定义规则
        checkPredefinedRule(projectId, ruleId);
        List<ConfigurationRuleReceiverDTO> receiverDTOList =
                configurationRuleReceiverMapper.select(new ConfigurationRuleReceiverDTO(ruleId, projectId));
        if (CollectionUtils.isNotEmpty(receiverDTOList)){
            for (ConfigurationRuleReceiverDTO ccDTO : receiverDTOList) {
                configurationRuleReceiverMapper.deleteByPrimaryKey(ccDTO.getId());
            }
        }
        ConfigurationRuleDTO projectReportDTO = new ConfigurationRuleDTO(ruleId, projectId);
        if (configurationRuleMapper.delete(projectReportDTO) != 1) {
            throw new CommonException("error.rule.delete");
        }
    }

    @Override
    public ConfigurationRuleVO queryById(Long projectId, Long ruleId) {
        ConfigurationRuleDTO configurationRuleDTO = configurationRuleMapper.selectByPrimaryKey(ruleId);
        ConfigurationRuleVO configurationRuleVO = new ConfigurationRuleVO();
        // 配置基本信息
        BeanUtils.copyProperties(configurationRuleDTO, configurationRuleVO);
        // 翻译表达式list
        if (StringUtils.isNotBlank(configurationRuleDTO.getExpressFormat())){
            configurationRuleVO.setExpressList(CommonMapperUtil.readValue(configurationRuleDTO.getExpressFormat(),
                    CommonMapperUtil.getTypeFactory().constructParametricType(List.class, RuleExpressVO.class)));
        }
        List<ConfigurationRuleReceiverDTO> receiverList = configurationRuleReceiverMapper.select(new ConfigurationRuleReceiverDTO(ruleId, projectId));
        if (CollectionUtils.isEmpty(receiverList)){
            return configurationRuleVO;
        }
        // 设置收件人列表
        Map<String, List<ConfigurationRuleReceiverDTO>> group =
                receiverList.stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getUserType));
        Long[] receiverIds = group.getOrDefault(ConfigurationRuleReceiverDTO.TYPE_RECEIVER, Collections.emptyList())
                .stream().map(ConfigurationRuleReceiverDTO::getUserId).toArray(Long[]::new);
        if (ArrayUtils.isNotEmpty(receiverIds)){
            List<UserDTO> userList = baseFeignClient.listUsersByIds(receiverIds, false).getBody();
            configurationRuleVO.setReceiverList(userList);
        }
        // 设置抄送人列表
        Long[] ccIds = group.getOrDefault(ConfigurationRuleReceiverDTO.TYPE_CC, Collections.emptyList())
                .stream().map(ConfigurationRuleReceiverDTO::getUserId).toArray(Long[]::new);
        if (ArrayUtils.isNotEmpty(ccIds)){
            List<UserDTO> userList = baseFeignClient.listUsersByIds(ccIds, false).getBody();
            configurationRuleVO.setCcList(userList);
        }
        return configurationRuleVO;
    }

    @Override
    public Page<ConfigurationRuleVO> listByProjectId(Long projectId, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () -> {
            List<ConfigurationRuleVO> page = configurationRuleMapper.selectByProjectId(projectId);
            if (CollectionUtils.isEmpty(page)){
                return page;
            }
            List<Long> ruleIdList = page.stream().map(ConfigurationRuleVO::getId).collect(Collectors.toList());
            Map<Long, ConfigurationRuleVO> map = selectRuleReceiverWithCc(ruleIdList);
            for (ConfigurationRuleVO reportVO : page) {
                reportVO.setReceiverList(map.get(reportVO.getId()).getReceiverList());
                reportVO.setCcList(map.get(reportVO.getId()).getCcList());
            }
            return page;
        });
    }
    
    @Override
    public Map<Long, ConfigurationRuleVO> selectRuleReceiverWithCc(List<Long> ruleIdList){
        if (CollectionUtils.isEmpty(ruleIdList)){
            return new HashMap<>();
        }
        List<ConfigurationRuleReceiverDTO> receiverDTOList = configurationRuleReceiverMapper.selectReceiver(ruleIdList, null);
        Map<String, List<ConfigurationRuleReceiverDTO>> group =
                receiverDTOList.stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getUserType));
        Map<Long, List<ConfigurationRuleReceiverDTO>> receiverGroup = new HashMap<>();
        Map<Long, List<ConfigurationRuleReceiverDTO>> ccGroup = new HashMap<>();
        List<UserDTO> userList = baseFeignClient.listUsersByIds(receiverDTOList.stream()
                .map(ConfigurationRuleReceiverDTO::getUserId).toArray(Long[]::new), false).getBody();
        Map<Long, UserDTO> userDTOMap = userList.stream().collect(Collectors.toMap(UserDTO::getId,
                Function.identity()));
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_RECEIVER))){
            receiverGroup.putAll(group.get(ConfigurationRuleReceiverDTO.TYPE_RECEIVER).stream()
                    .collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getRuleId)));
        }
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_CC))){
            ccGroup.putAll(group.get(ConfigurationRuleReceiverDTO.TYPE_CC).stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getRuleId)));
        }
        return ruleIdList.stream().map(ruleId -> {
            ConfigurationRuleVO ruleVO = new ConfigurationRuleVO();
            ruleVO.setId(ruleId);
            ruleVO.setReceiverList(receiverGroup
                    .getOrDefault(ruleId, Collections.emptyList())
                    .stream().map(receiver -> userDTOMap.get(receiver.getUserId()))
                    .collect(Collectors.toList()));
            ruleVO.setCcList(ccGroup
                    .getOrDefault(ruleId, Collections.emptyList())
                    .stream().map(receiver -> userDTOMap.get(receiver.getUserId()))
                    .collect(Collectors.toList()));
            return ruleVO;
        }).collect(Collectors.toMap(ConfigurationRuleVO::getId, Function.identity()));
    }

    @Override
    public void changeRuleEnabled(Long projectId, Long ruleId, boolean enabled) {
        Assert.notNull(projectId, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(ruleId, BaseConstants.ErrorCode.DATA_INVALID);
        ConfigurationRuleDTO configurationRuleDTO = new ConfigurationRuleDTO(ruleId, projectId);
        configurationRuleDTO.setEnabled(enabled);
        if (configurationRuleMapper.updateOptional(configurationRuleDTO, ConfigurationRuleDTO.FIELD_ENABLED) != 1) {
            throw new CommonException("error.rule.update");
        }
    }

    private String renderLinkTableSql(String sourceOp, String field, List<Supplier<String>> conditionList) {
        String operation = ConfigurationRule.OpSqlMapping.valueOf(sourceOp).withField(field).name();
        StringBuilder sb = new StringBuilder();
        String sqlOp = ConfigurationRule.OpSqlMapping.valueOf(operation).getSqlOp();
        String table = ConfigurationRule.FieldTableMapping.matches(field).getTable();
        if (ConfigurationRule.OpSqlMapping.is.name().equals(sourceOp) || ConfigurationRule.OpSqlMapping.is_not.name().equals(sourceOp)){
            return String.format(ConfigurationRule.TEMPLATE_LINK_TABLE_SQL, sqlOp, table, "");
        }
        if (CollectionUtils.isNotEmpty(conditionList)){
            sb.append(ConfigurationRule.TEMPLATE_SQL_WHERE)
              .append(conditionList.stream().map(Supplier::get).collect(Collectors.joining(ConfigurationRule.TEMPLATE_SQL_AND)));
        }
        return String.format(ConfigurationRule.TEMPLATE_LINK_TABLE_SQL, sqlOp, table, sb.toString());
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

    protected void dealCaseVersion(RuleExpressVO quickFilterValueVO, String field, Object value,String operation, StringBuilder sqlQuery) {
        if ("fix_version".equals(quickFilterValueVO.getFieldCode())) {
            sqlQuery.append(renderLinkTableSql(operation, field, 
                    Arrays.asList(() -> this.conditionSql("relation_type", ConfigurationRule.OpSqlMapping.eq.name(), valueToString("fix")), () -> this.conditionSql(field, operation, value))));
        } else if ("influence_version".equals(quickFilterValueVO.getFieldCode())) {
            sqlQuery.append(renderLinkTableSql(operation, field,
                    Arrays.asList(() -> this.conditionSql("relation_type", ConfigurationRule.OpSqlMapping.eq.name(),valueToString("influence")), () -> this.conditionSql(field, operation, value))));
        }
    }

    private String getSqlQuery(ConfigurationRuleVO configurationRuleVO, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<RuleExpressVO> ruleExpressVOList = configurationRuleVO.getExpressList();
        StringBuilder sqlQuery = new StringBuilder();
        for (RuleExpressVO ruleExpressVO : ruleExpressVOList) {
            if (StringUtils.isNotBlank(ruleExpressVO.getRelationshipWithPervious())){
                sqlQuery.append(ConfigurationRule.OpSqlMapping.valueOf(ruleExpressVO.getRelationshipWithPervious()).getSqlOp());
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
        Object value;
        
        String customFieldType = ruleExpressVO.getFieldType();
        CustomFieldType.contains(customFieldType, true);

        if (CustomFieldType.isOption(customFieldType)) {
            value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueIdList() : ruleExpressVO.getValueId();
            return renderCustomSql(preOp, projectId, fieldId, 
                    Collections.singletonList(() ->  conditionSql("ffv.option_id", operation, value)));
        } else if (CustomFieldType.isDate(customFieldType)) {
            value = ruleExpressVO.getValueDate();
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() -> 
                    conditionSql(getUnixTimeExpress("ffv.date_value"), operation, getUnixTimeExpress(valueToString(value)))));
        } else if (CustomFieldType.isDateHms(customFieldType)) {
            value = ruleExpressVO.getValueDateHms();
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() ->
                    conditionSql(getTimeFieldExpress("ffv.date_value"), operation, getTimeValueExpress(valueToString(value)))));
        } else if (CustomFieldType.isNumber(customFieldType)) {
            value = getNumber(operation, ruleExpressVO);
            return renderCustomSql(preOp, projectId, fieldId, 
                    Collections.singletonList(() -> conditionSql("ffv.number_value", operation, value)));
        } else if (CustomFieldType.isString(customFieldType)) {
            value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueStrList() : ruleExpressVO.getValueStr();
            return renderCustomSql(preOp, projectId, fieldId,
                    Collections.singletonList(() ->  conditionSql("ffv.string_value", operation, value)));
        } else  {
            //text
            value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueStrList() : ruleExpressVO.getValueStr();
            return renderCustomSql(preOp, projectId, fieldId,
                    Collections.singletonList(() ->  conditionSql("ffv.text_value", operation, value)));
        }
    }
    
    private Object getNumber(String operation, RuleExpressVO ruleExpressVO){
        if (ConfigurationRule.OpSqlMapping.isCollOp(operation)){
            if (BooleanUtils.isTrue(ruleExpressVO.getAllowDecimals())){
                return ruleExpressVO.getValueDecimalList();
            }else {
                return ruleExpressVO.getValueNumList();
            }
        }else {
            if (BooleanUtils.isTrue(ruleExpressVO.getAllowDecimals())){
                return ruleExpressVO.getValueDecimal();
            }else {
                return ruleExpressVO.getValueNum();
            }
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

    protected void processPredefinedField(StringBuilder sqlQuery, RuleExpressVO ruleExpressVO, Object value, String operation) {
        String field = configurationRuleFiledMapper.selectByPrimaryKey(ruleExpressVO.getFieldCode()).getField();
        switch (field) {
            case "version_id":
                dealCaseVersion(ruleExpressVO, field, value, operation, sqlQuery);
                break;
            case "component_id":
            case "label_id":
            case "sprint_id":
                sqlQuery.append(renderLinkTableSql(operation, field, 
                        Collections.singletonList(() -> this.conditionSql(field, operation, value))));
                break;
            case "creation_date":
            case "last_update_date":
                sqlQuery.append(this.conditionSql(getUnixTimeExpress(field), operation, getUnixTimeExpress(valueToString(value))));
                break;
            default:
                if (ConfigurationRule.OpSqlMapping.isCollOp(operation)){
                    sqlQuery.append(this.conditionSql(field, operation, value));
                }else {
                    sqlQuery.append(this.conditionSql(field, operation, valueToString(value)));
                }
                break;
        }
    }

    private String valueToString(Object value) {
        if (value instanceof String){
            return "'" + value + "'";
        }else if(value instanceof Date){
            return "'" + df.format(value) + "'";
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
        if (Objects.isNull(value) && !ConfigurationRule.OpSqlMapping.isNullKey(operation)) {
            return "";
        }
        if (value instanceof Collection){
            return inSql(field, (Collection<?>)value);
        }
        if (Objects.isNull(value) && ConfigurationRule.OpSqlMapping.isNullKey(operation)){
            value = "NULL";
        }
        if (ConfigurationRule.OpSqlMapping.isLike(operation)){
            value = String.format(ConfigurationRule.TEMPLATE_LIKE_VALUE_SQL, value);
        }
        return String.format(ConfigurationRule.TEMPLATE_CONDITION_SQL, field, ConfigurationRule.OpSqlMapping.valueOf(operation).getSqlOp(), value);
    }

    private void createProjectReportReceiver(Long projectId, ConfigurationRuleVO configurationRuleVO,
                                             ConfigurationRuleDTO configurationRuleDTO) {
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getReceiverList()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getAssigneeList()), BaseConstants.ErrorCode.DATA_INVALID);
        for (UserDTO userDTO : configurationRuleVO.getReceiverList()) {
            ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
            configurationRuleReceiverDTO.setProjectId(projectId);
            configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
            configurationRuleReceiverDTO.setUserType(ConfigurationRuleReceiverDTO.TYPE_RECEIVER);
            configurationRuleReceiverDTO.setUserId(userDTO.getId());
            configurationRuleReceiverMapper.insertSelective(configurationRuleReceiverDTO);
        }
        for (UserDTO userDTO : configurationRuleVO.getAssigneeList()) {
            ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
            configurationRuleReceiverDTO.setProjectId(projectId);
            configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
            configurationRuleReceiverDTO.setUserType(ConfigurationRuleReceiverDTO.TYPE_ASSIGNEE);
            configurationRuleReceiverDTO.setUserId(userDTO.getId());
            configurationRuleReceiverMapper.insertSelective(configurationRuleReceiverDTO);
        }
        if (CollectionUtils.isNotEmpty(configurationRuleVO.getCcList())) {
            for (UserDTO userDTO : configurationRuleVO.getCcList()) {
                ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
                configurationRuleReceiverDTO.setProjectId(projectId);
                configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
                configurationRuleReceiverDTO.setUserType(ConfigurationRuleReceiverDTO.TYPE_CC);
                configurationRuleReceiverDTO.setUserId(userDTO.getId());
                configurationRuleReceiverMapper.insertSelective(configurationRuleReceiverDTO);
            }
        }
    }
}
