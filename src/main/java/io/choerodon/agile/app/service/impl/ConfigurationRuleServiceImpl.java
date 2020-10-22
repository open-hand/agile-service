package io.choerodon.agile.app.service.impl;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.JavaType;
import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.RuleExpressVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.dto.ConfigurationRuleDTO;
import io.choerodon.agile.infra.dto.ConfigurationRuleReceiverDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.ConfigurationRule;
import io.choerodon.agile.infra.enums.CustomFieldType;
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
    
    private static final String[] RECEIVER_LIST = new String[]{ConfigurationRuleReceiverDTO.TYPE_RECEIVER, 
            ConfigurationRuleReceiverDTO.TYPE_CC, ConfigurationRuleReceiverDTO.TYPE_ASSINGEE, 
            ConfigurationRuleReceiverDTO.TYPE_REPORTER, ConfigurationRuleReceiverDTO.TYPE_PROJECT_OWNER, 
            ConfigurationRuleReceiverDTO.TYPE_PROCESSER};
    public static final DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATETIME);

    @Override
    public ConfigurationRuleVO create(Long projectId, ConfigurationRuleVO configurationRuleVO) {
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getReceiverList()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getExpressList()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getIssueTypes()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(checkUniqueName(projectId, configurationRuleVO.getName()), BaseConstants.ErrorCode.DATA_INVALID);
        String sqlQuery = generateSqlQuery(configurationRuleVO, projectId);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(configurationRuleVO, ConfigurationRuleDTO.class);
        configurationRuleDTO.setExpressFormat(CommonMapperUtil.writeValueAsString(configurationRuleVO.getExpressList()));
        configurationRuleDTO.setTypeCode(CommonMapperUtil.writeValueAsString(configurationRuleVO.getIssueTypes()));
        configurationRuleDTO.setSqlQuery(sqlQuery);
        configurationRuleDTO.setSource(ConfigurationRuleDTO.SOURCE_CUSTOM);
        if (configurationRuleMapper.insertSelective(configurationRuleDTO) != 1) {
            throw new CommonException("error.rule.insert");
        }
        createProjectReportReceiver(projectId,configurationRuleVO, configurationRuleDTO);
        return configurationRuleVO;
    }

    @Override
    public boolean checkUniqueName(Long projectId, String name) {
        boolean flag = true;
        ConfigurationRuleDTO configurationRuleDTO = new ConfigurationRuleDTO();
        configurationRuleDTO.setProjectId(projectId);
        configurationRuleDTO.setName(name);
        List<ConfigurationRuleDTO> exist = configurationRuleMapper.select(configurationRuleDTO);
        if (CollectionUtils.isNotEmpty(exist)){
            flag = false;
        }
        return flag;
    }

    @Override
    public ConfigurationRuleVO update(Long projectId, Long ruleId, ConfigurationRuleVO configurationRuleVO) {
        // 检查是否是预定义规则
        checkPredefinedRule(projectId, ruleId);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getReceiverList()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getIssueTypes()), BaseConstants.ErrorCode.DATA_INVALID);
        // 检查是否名称唯一
        Assert.isTrue(checkUniqueName(projectId, configurationRuleVO.getName()), BaseConstants.ErrorCode.DATA_INVALID);
        configurationRuleVO.setId(ruleId);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(configurationRuleVO, ConfigurationRuleDTO.class);
        configurationRuleDTO.setSqlQuery(generateSqlQuery(configurationRuleVO, projectId));
        configurationRuleDTO.setExpressFormat(CommonMapperUtil.writeValueAsString(configurationRuleVO.getExpressList()));
        configurationRuleDTO.setTypeCode(CommonMapperUtil.writeValueAsString(configurationRuleVO.getIssueTypes()));
        if (configurationRuleMapper.updateOptional(configurationRuleDTO, ConfigurationRuleDTO.FIELD_SQL_QUERY,
                ConfigurationRuleDTO.FIELD_EXPRESS_QUERY, ConfigurationRuleDTO.FIELD_EXPRESS_FORMAT,
                ConfigurationRuleDTO.FIELD_TYPE_CODE) != 1) {
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
        Map<Long, ConfigurationRuleVO> map = selectRuleALLReceiver(Collections.singletonList(ruleId));
        JavaType javaType = CommonMapperUtil.getTypeFactory().constructParametricType(List.class, String.class);
        configurationRuleVO.setReceiverList(map.get(ruleId).getReceiverList());
        configurationRuleVO.setCcList(map.get(ruleId).getCcList());
        configurationRuleVO.setUserTypes(map.get(ruleId).getUserTypes());
        configurationRuleVO.setIssueTypes(CommonMapperUtil.readValue(configurationRuleDTO.getTypeCode(),javaType));
        configurationRuleVO.setProcesserList(map.get(ruleId).getProcesserList());
        return configurationRuleVO;
    }

    @Override
    public Page<ConfigurationRuleVO> listByProjectId(Long projectId, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () -> {
            List<ConfigurationRuleVO> page = configurationRuleMapper.selectByProjectId(projectId);
            if (CollectionUtils.isEmpty(page)){
                return page;
            }
            JavaType javaType = CommonMapperUtil.getTypeFactory().constructParametricType(List.class, String.class);
            List<Long> ruleIdList = page.stream().map(ConfigurationRuleVO::getId).collect(Collectors.toList());
            Map<Long, ConfigurationRuleVO> map = selectRuleALLReceiver(ruleIdList);
            for (ConfigurationRuleVO reportVO : page) {
                reportVO.setReceiverList(map.get(reportVO.getId()).getReceiverList());
                reportVO.setCcList(map.get(reportVO.getId()).getCcList());
                reportVO.setIssueTypes(CommonMapperUtil.readValue(reportVO.getTypeCode(), javaType));
                reportVO.setUserTypes(map.get(reportVO.getId()).getUserTypes());
            }
            return page;
        });
    }
    
    @Override
    public Map<Long, ConfigurationRuleVO> selectRuleALLReceiver(List<Long> ruleIdList){
        if (CollectionUtils.isEmpty(ruleIdList)){
            return new HashMap<>();
        }
        List<ConfigurationRuleReceiverDTO> receiverDTOList = configurationRuleReceiverMapper.selectReceiver(ruleIdList, Arrays.asList(RECEIVER_LIST));
        Map<String, List<ConfigurationRuleReceiverDTO>> group =
                receiverDTOList.stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getUserType));
        Map<Long, List<ConfigurationRuleReceiverDTO>> receiverGroup = new HashMap<>();
        Map<Long, List<ConfigurationRuleReceiverDTO>> ccGroup = new HashMap<>();
        Map<Long, List<ConfigurationRuleReceiverDTO>> userTypeGroup = new HashMap<>();
        Map<Long, List<ConfigurationRuleReceiverDTO>> processerGroup = new HashMap<>();
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
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_PROJECT_OWNER))){
            userTypeGroup.putAll(group.get(ConfigurationRuleReceiverDTO.TYPE_PROJECT_OWNER).stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getRuleId)));
        }
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_ASSINGEE))){
            userTypeGroup.putAll(group.get(ConfigurationRuleReceiverDTO.TYPE_ASSINGEE).stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getRuleId)));
        }
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_REPORTER))){
            userTypeGroup.putAll(group.get(ConfigurationRuleReceiverDTO.TYPE_REPORTER).stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getRuleId)));
        }
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_PROCESSER))){
            processerGroup.putAll(group.get(ConfigurationRuleReceiverDTO.TYPE_PROCESSER).stream().collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getRuleId)));
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
            ruleVO.setUserTypes(userTypeGroup
                    .getOrDefault(ruleId, Collections.emptyList())
                    .stream().map(ConfigurationRuleReceiverDTO::getUserType)
                    .collect(Collectors.toList()));
            ruleVO.setProcesserList(processerGroup
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
        configurationRuleDTO = configurationRuleMapper.selectOne(configurationRuleDTO);
        if (Objects.isNull(configurationRuleDTO)){
            throw new CommonException("error.rule.update");
        }
        configurationRuleDTO.setEnabled(enabled);
        if (configurationRuleMapper.updateOptional(configurationRuleDTO, ConfigurationRuleDTO.FIELD_ENABLED) != 1) {
            throw new CommonException("error.rule.update");
        }
    }

    private String renderLinkTableSql(String sourceOp, String field, List<Supplier<String>> conditionList) {
        String operation = ConfigurationRule.OpSqlMapping.valueOf(sourceOp).withField(field).name();
        StringBuilder sb = new StringBuilder();
        String sqlOp = ConfigurationRule.OpSqlMapping.valueOf(operation).getSqlOp();
        String table = ConfigurationRule.FieldTableMapping.matchesField(field).getTable();
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
            return StringUtils.EMPTY;
        }
        String valueList = value.stream().map(this::valueToString)
                .collect(Collectors.joining(BaseConstants.Symbol.COMMA));
        sql = String.format(ConfigurationRule.TEMPLATE_IN_SQL, field, valueList);
        return sql;
    }

    protected String dealCaseVersion(RuleExpressVO quickFilterValueVO, String field, Object value,String operation) {
        if ("fix_version".equals(quickFilterValueVO.getFieldCode())) {
            return renderLinkTableSql(operation, field,
                    Arrays.asList(() -> this.conditionSql("relation_type", ConfigurationRule.OpSqlMapping.eq.name(), valueToString("fix")), () -> this.conditionSql(field, operation, value)));
        } else if ("influence_version".equals(quickFilterValueVO.getFieldCode())) {
            return renderLinkTableSql(operation, field,
                    Arrays.asList(() -> this.conditionSql("relation_type", ConfigurationRule.OpSqlMapping.eq.name(),valueToString("influence")), () -> this.conditionSql(field, operation, value)));
        }
        return StringUtils.EMPTY;
    }

    private String generateSqlQuery(ConfigurationRuleVO configurationRuleVO, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<RuleExpressVO> ruleExpressVOList = configurationRuleVO.getExpressList();
        Map<String, ObjectSchemeFieldVO> fieldMap = objectSchemeFieldService
                .listPageFieldWithOption(organizationId, projectId, "agile_issue", configurationRuleVO.getIssueTypes())
                .stream().collect(Collectors.toMap(ObjectSchemeFieldVO::getCode, Function.identity()));
        StringBuilder sqlQuery = new StringBuilder();
        String typeLimit;
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
            typeLimit = inSql("type_code", Optional.ofNullable(fieldMap.get(ruleExpressVO.getFieldCode()))
                    .map(ObjectSchemeFieldVO::getContexts).orElse(null));
            if (predefined) {
                sqlQuery.append(addTypeLimit(typeLimit, processPredefinedField(ruleExpressVO, getValue(ruleExpressVO))));
            } else {
                sqlQuery.append(addTypeLimit(typeLimit, processCustomField(ruleExpressVO, organizationId, projectId, getValue(ruleExpressVO))));
            }
        }
        return sqlQuery.toString();
    }
    
    private String addTypeLimit(String typeLimit, String sql){
        if (StringUtils.isBlank(typeLimit)){
            typeLimit = ConfigurationRule.SQL_VAR_NOT_EQUALS;
        }
        return String.format(ConfigurationRule.TEMPLATE_TYPE_LIMIT, typeLimit, ConfigurationRule.OpSqlMapping.and.getSqlOp(), sql);
    }

    private String processCustomField(RuleExpressVO ruleExpressVO, Long organizationId, Long projectId, Object value) {
        String fieldCode = ruleExpressVO.getFieldCode();
        ObjectSchemeFieldDTO objectSchemeField = objectSchemeFieldService.queryByFieldCode(organizationId, projectId, fieldCode);
        if (ObjectUtils.isEmpty(objectSchemeField)) {
            throw new CommonException("error.custom.field." + fieldCode + ".not.existed");
        }
        Long fieldId = objectSchemeField.getId();
        String operation = ruleExpressVO.getOperation();
        String preOp = ConfigurationRule.OpSqlMapping.getPreOp(operation).name();
        
        String customFieldType = ruleExpressVO.getFieldType();
        CustomFieldType.contains(customFieldType, true);

        if (CustomFieldType.isOption(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, 
                    Collections.singletonList(() ->  conditionSql("ffv.option_id", operation, value)));
        } else if (CustomFieldType.isDate(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() -> 
                    conditionSql(getUnixTimeExpress("ffv.date_value"), operation, getUnixTimeExpress(valueToString(value)))));
        } else if (CustomFieldType.isDateHms(customFieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() ->
                    conditionSql(getTimeFieldExpress("ffv.date_value"), operation, getTimeValueExpress(valueToString(value)))));
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


    private Object getValue(RuleExpressVO ruleExpressVO){
        Object value;
        String fieldType = ruleExpressVO.getFieldType();
        String operation = ruleExpressVO.getOperation();
        if (CustomFieldType.isOption(fieldType)) {
            value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueIdList() : ruleExpressVO.getValueId();
        } else if (CustomFieldType.isDate(fieldType)) {
            value = BooleanUtils.isTrue(ruleExpressVO.getNowFlag()) ? ruleExpressVO.getValueStr() : ruleExpressVO.getValueDate();
        } else if (CustomFieldType.isDateHms(fieldType)) {
            value = ruleExpressVO.getValueDateHms();
        } else if (CustomFieldType.isNumber(fieldType)) {
            value = getNumber(operation, ruleExpressVO);
        } else if (CustomFieldType.isString(fieldType)) {
            value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueStrList() : ruleExpressVO.getValueStr();
        } else  {
            //text
            value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueStrList() : ruleExpressVO.getValueStr();
        }
        return value;
    }
    
    private Object getNumber(String operation, RuleExpressVO ruleExpressVO){
        if (StringUtils.equalsAny(ruleExpressVO.getFieldCode(), "storyPoints", "remainingTime")){
             return ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueDecimalList() : ruleExpressVO.getValueDecimal();
        }
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


    protected String processPredefinedField(RuleExpressVO ruleExpressVO, Object value) {
        String operation = ruleExpressVO.getOperation();
        String field = ConfigurationRule.FieldTableMapping.matches(ruleExpressVO.getFieldCode()).getField();
        String sql;
        switch (field) {
            case "version_id":
                sql = dealCaseVersion(ruleExpressVO, field, value, operation);
                break;
            case "component_id":
            case "label_id":
            case "sprint_id":
                sql = renderLinkTableSql(operation, field, Collections.singletonList(() -> this.conditionSql(field, operation, value)));
                break;
            case "creation_date":
            case "last_update_date":
                sql = this.conditionSql(getUnixTimeExpress(field), operation, getUnixTimeExpress(valueToString(value)));
                break;
            default:
                if (ConfigurationRule.OpSqlMapping.isCollOp(operation)){
                    sql = this.conditionSql(field, operation, value);
                }else {
                    sql = this.conditionSql(field, operation, valueToString(value));
                }
                break;
        }
        return sql;
    }

    private String valueToString(Object value) {
        if (value instanceof String){
            return ConfigurationRule.isSqlVar((String)value) ? (String)value : "'" + value + "'";
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
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getProcesserList()), BaseConstants.ErrorCode.DATA_INVALID);
        for (UserDTO userDTO : configurationRuleVO.getReceiverList()) {
            ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
            configurationRuleReceiverDTO.setProjectId(projectId);
            configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
            configurationRuleReceiverDTO.setUserType(ConfigurationRuleReceiverDTO.TYPE_RECEIVER);
            configurationRuleReceiverDTO.setUserId(userDTO.getId());
            configurationRuleReceiverMapper.insertSelective(configurationRuleReceiverDTO);
        }
        for (UserDTO userDTO : configurationRuleVO.getProcesserList()) {
            ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
            configurationRuleReceiverDTO.setProjectId(projectId);
            configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
            configurationRuleReceiverDTO.setUserType(ConfigurationRuleReceiverDTO.TYPE_PROCESSER);
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
        if (CollectionUtils.isNotEmpty(configurationRuleVO.getUserTypes())){
            for (String userType : configurationRuleVO.getUserTypes()) {
                if (StringUtils.equalsAny(userType, 
                        ConfigurationRuleReceiverDTO.TYPE_ASSINGEE,
                        ConfigurationRuleReceiverDTO.TYPE_REPORTER,
                        ConfigurationRuleReceiverDTO.TYPE_PROJECT_OWNER)){
                    ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
                    configurationRuleReceiverDTO.setProjectId(projectId);
                    configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
                    configurationRuleReceiverDTO.setUserType(userType);
                    configurationRuleReceiverMapper.insertSelective(configurationRuleReceiverDTO);
                }
            }
        }
    }
}
