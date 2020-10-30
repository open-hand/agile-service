package io.choerodon.agile.app.service.impl;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.databind.JavaType;
import com.google.common.base.CaseFormat;
import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.RuleExpressVO;
import io.choerodon.agile.app.service.BacklogExpandService;
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
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    
    private static final String[] RECEIVER_LIST = new String[]{ConfigurationRuleReceiverDTO.TYPE_RECEIVER, 
            ConfigurationRuleReceiverDTO.TYPE_CC, ConfigurationRuleReceiverDTO.TYPE_ASSINGEE, 
            ConfigurationRuleReceiverDTO.TYPE_REPORTER, ConfigurationRuleReceiverDTO.TYPE_PROJECT_OWNER, 
            ConfigurationRuleReceiverDTO.TYPE_PROCESSER};
    public static final DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATETIME);

    @Override
    public ConfigurationRuleVO create(Long projectId, ConfigurationRuleVO configurationRuleVO) {
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getExpressList()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getIssueTypes()), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(checkUniqueName(projectId, null, configurationRuleVO.getName()), BaseConstants.ErrorCode.DATA_INVALID);
        generateSqlQuery(configurationRuleVO, true);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(configurationRuleVO, ConfigurationRuleDTO.class);
        configurationRuleDTO.setExpressFormat(CommonMapperUtil.writeValueAsString(configurationRuleVO.getExpressList()));
        configurationRuleDTO.setTypeCode(CommonMapperUtil.writeValueAsString(configurationRuleVO.getIssueTypes()));
        configurationRuleDTO.setSource(ConfigurationRuleDTO.SOURCE_CUSTOM);
        if (configurationRuleMapper.insertSelective(configurationRuleDTO) != 1) {
            throw new CommonException("error.rule.insert");
        }
        createProjectReportReceiver(projectId,configurationRuleVO, configurationRuleDTO);
        return configurationRuleVO;
    }

    @Override
    public boolean checkUniqueName(Long projectId, Long ruleId, String name) {
        ConfigurationRuleDTO configurationRuleDTO = new ConfigurationRuleDTO();
        configurationRuleDTO.setProjectId(projectId);
        configurationRuleDTO.setName(name);
        List<ConfigurationRuleDTO> exist = configurationRuleMapper.select(configurationRuleDTO);
        return exist.stream().allMatch(rule -> Objects.equals(rule.getId(), ruleId));
    }

    @Override
    public ConfigurationRuleVO update(Long projectId, Long ruleId, ConfigurationRuleVO configurationRuleVO) {
        // 检查是否是预定义规则
        checkPredefinedRule(projectId, ruleId);
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getIssueTypes()), BaseConstants.ErrorCode.DATA_INVALID);
        // 检查是否名称唯一
        Assert.isTrue(checkUniqueName(projectId, ruleId, configurationRuleVO.getName()), BaseConstants.ErrorCode.DATA_INVALID);
        configurationRuleVO.setId(ruleId);
        ConfigurationRuleDTO configurationRuleDTO = modelMapper.map(configurationRuleVO, ConfigurationRuleDTO.class);
        generateSqlQuery(configurationRuleVO, true);
        configurationRuleDTO.setExpressFormat(CommonMapperUtil.writeValueAsString(configurationRuleVO.getExpressList()));
        configurationRuleDTO.setTypeCode(CommonMapperUtil.writeValueAsString(configurationRuleVO.getIssueTypes()));
        if (configurationRuleMapper.updateOptional(configurationRuleDTO,
                ConfigurationRuleDTO.FIELD_EXPRESS_QUERY, ConfigurationRuleDTO.FIELD_EXPRESS_FORMAT,
                ConfigurationRuleDTO.FIELD_TYPE_CODE, ConfigurationRuleDTO.FIELD_NAME) != 1) {
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
    public Page<ConfigurationRuleVO> listByProjectId(ConfigurationRuleVO configurationRuleVO, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () -> {
            List<ConfigurationRuleVO> page = configurationRuleMapper.selectByProjectId(configurationRuleVO);
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
                reportVO.setProcesserList(map.get(reportVO.getId()).getProcesserList());
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
            userTypeGroup.putAll(getMap(group, userTypeGroup, ConfigurationRuleReceiverDTO.TYPE_PROJECT_OWNER));
        }
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_ASSINGEE))){
            userTypeGroup.putAll(getMap(group, userTypeGroup, ConfigurationRuleReceiverDTO.TYPE_ASSINGEE));
        }
        if (CollectionUtils.isNotEmpty(group.get(ConfigurationRuleReceiverDTO.TYPE_REPORTER))){
            userTypeGroup.putAll(getMap(group, userTypeGroup, ConfigurationRuleReceiverDTO.TYPE_REPORTER));
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

    private Map<Long, List<ConfigurationRuleReceiverDTO>> getMap(Map<String, List<ConfigurationRuleReceiverDTO>> group, 
                                                                 Map<Long, List<ConfigurationRuleReceiverDTO>> userTypeGroup,
                                                                 String userType) {
        return Stream.concat(userTypeGroup.entrySet().stream(),
                group.get(userType).stream()
                        .collect(Collectors.groupingBy(ConfigurationRuleReceiverDTO::getRuleId)).entrySet().stream())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (v1, v2) -> {
                    List<ConfigurationRuleReceiverDTO> t = new ArrayList<>();
                    t.addAll(v1);
                    t.addAll(v2);
                    return t;
                }));
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

    @Override
    public List<ConfigurationRuleVO> processRule(List<ConfigurationRuleVO> sourceList, Set<String> fieldList, 
                                                 boolean allFieldCheck, boolean checkMode) {
        List<ConfigurationRuleVO> ruleList = new ArrayList<>(sourceList);
        JavaType javaType = CommonMapperUtil.getTypeFactory().constructParametricType(List.class, RuleExpressVO.class);
        for (ConfigurationRuleVO ruleVO : ruleList) {
            ruleVO.setExpressList(CommonMapperUtil.readValue(ruleVO.getExpressFormat(), javaType));
            ruleVO.setSqlQuery(this.generateSqlQuery(ruleVO,checkMode));
        }
        if (allFieldCheck){
            return ruleList;
        }
        if (CollectionUtils.isEmpty(fieldList)){
            return Collections.emptyList();
        }
        return ruleList.stream()
                .filter(rule -> rule.getExpressList().stream()
                        .anyMatch(express -> fieldList.contains(fieldCode2Field(express.getFieldCode()))))
                .collect(Collectors.toList());
    }

    private String fieldCode2Field(String fieldCode){
        String field = getFieldByCode(fieldCode);
        if (Objects.nonNull(field)){
            return CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, field);
        }
        return fieldCode;
    }
    
    private String getFieldByCode(String fieldCode){
        return Stream.of(ConfigurationRule.fieldTableList, 
                Optional.ofNullable(backlogExpandService).map(BacklogExpandService::getBacklogField)
                        .orElse(Collections.emptyList()))
                .flatMap(Collection::stream)
                .filter(item -> StringUtils.equals(item.getName(), fieldCode))
                .findFirst().map(FieldTableVO::getField)
                .orElse(null);
    }

    private String renderLinkTableSql(String sourceOp, String field, List<Supplier<String>> conditionList) {
        String operation = ConfigurationRule.OpSqlMapping.valueOf(sourceOp).withField(field).name();
        StringBuilder sb = new StringBuilder();
        String sqlOp = ConfigurationRule.OpSqlMapping.valueOf(operation).getSqlOp();
        String table = Stream.of(ConfigurationRule.fieldTableList,
                Optional.ofNullable(backlogExpandService).map(BacklogExpandService::getBacklogField)
                        .orElse(Collections.emptyList()))
                .flatMap(Collection::stream)
                .filter(item -> StringUtils.equals(item.getField(), field))
                .findFirst().map(FieldTableVO::getTable)
                .orElse(null);
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

    @Override
    public String generateSqlQuery(ConfigurationRuleVO configurationRuleVO, boolean checkMode) {
        Long projectId = configurationRuleVO.getProjectId();
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
            if (Objects.isNull(predefined)) {
                String errorMsg = "error." + fieldCode + ".predefined.null";
                throw new CommonException(errorMsg);
            }
            typeLimit = inSql("type_code", Optional.ofNullable(fieldMap.get(ruleExpressVO.getFieldCode()))
                    .map(ObjectSchemeFieldVO::getContexts).orElse(null));
            if (predefined) {
                sqlQuery.append(addTypeLimit(typeLimit, processPredefinedField(ruleExpressVO, getValue(ruleExpressVO)), checkMode));
            } else {
                sqlQuery.append(addTypeLimit(typeLimit, processCustomField(ruleExpressVO, organizationId, projectId, getValue(ruleExpressVO), configurationRuleVO.getIssueTypes()), checkMode));
            }
        }
        return sqlQuery.toString();
    }
    
    private String addTypeLimit(String typeLimit, String sql, boolean checkMode){
        if (checkMode){
            return sql;
        }
        if (StringUtils.isBlank(typeLimit)){
            typeLimit = ConfigurationRule.SQL_VAR_NOT_EQUALS;
        }
        return String.format(ConfigurationRule.TEMPLATE_TYPE_LIMIT, typeLimit, ConfigurationRule.OpSqlMapping.and.getSqlOp(), sql);
    }

    private String processCustomField(RuleExpressVO ruleExpressVO, Long organizationId, Long projectId, Object value, List<String> issueTypeList) {
        String fieldCode = ruleExpressVO.getFieldCode();
        ObjectSchemeFieldDTO objectSchemeField = objectSchemeFieldService.queryByFieldCode(organizationId, projectId, fieldCode);
        if (Objects.isNull(objectSchemeField)) {
            return ConfigurationRule.SQL_VAR_NOT_EQUALS;
        }
        Long fieldId = objectSchemeField.getId();
        String operation = ruleExpressVO.getOperation();
        String preOp = ConfigurationRule.OpSqlMapping.getPreOp(operation).name();
        
        String fieldType = ruleExpressVO.getFieldType();
        CustomFieldType.contains(fieldType, true);
        String primaryKey = issueTypeList.contains("backlog")? ConfigurationRule.FIELD_BACKLOG_ID : ConfigurationRule.FIELD_ISSUE_ID;
        if (CustomFieldType.isOption(fieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, 
                    Collections.singletonList(() ->  conditionSql("ffv.option_id", operation, value)), primaryKey);
        } else if (CustomFieldType.isDate(fieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() -> 
                    conditionSql(getUnixTimeExpress("ffv.date_value"), operation, getUnixTimeExpress(valueToString(value)))), primaryKey);
        } else if (CustomFieldType.isDateHms(fieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, Collections.singletonList(() ->
                    conditionSql(getTimeFieldExpress("ffv.date_value"), operation, getHmsTime(value))), primaryKey);
        } else if (CustomFieldType.isNumber(fieldType)) {
            return renderCustomSql(preOp, projectId, fieldId, 
                    Collections.singletonList(() -> conditionSql("ffv.number_value", operation, value)), primaryKey);
        } else if (CustomFieldType.isString(fieldType)) {
            return renderCustomSql(preOp, projectId, fieldId,
                    Collections.singletonList(() ->  conditionSql("ffv.string_value", operation, value)), primaryKey);
        } else  {
            //text
            return renderCustomSql(preOp, projectId, fieldId,
                    Collections.singletonList(() ->  conditionSql("ffv.text_value", operation, value)), primaryKey);
        }
    }


    private Object getValue(RuleExpressVO ruleExpressVO){
        Object value;
        String fieldType = ruleExpressVO.getFieldType();
        String operation = ruleExpressVO.getOperation();
        if (CustomFieldType.isOption(fieldType)) {
            value = ConfigurationRule.OpSqlMapping.isCollOp(operation) ? ruleExpressVO.getValueIdList() : ruleExpressVO.getValueId();
        } else if (CustomFieldType.isDate(fieldType)) {
            value = BooleanUtils.isTrue(ruleExpressVO.getNowFlag()) ? ConfigurationRule.SQL_VAR_NOW_EXPRESS : ruleExpressVO.getValueDate();
        } else if (CustomFieldType.isDateHms(fieldType)) {
            value = BooleanUtils.isTrue(ruleExpressVO.getNowFlag()) ? ConfigurationRule.SQL_VAR_NOW_EXPRESS : ruleExpressVO.getValueDateHms();
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

    private String renderCustomSql(String operation, Long projectId, Long fieldId, List<Supplier<String>> conditionList, String primartKey) {
        StringBuilder sb = new StringBuilder();
        String sqlOp = ConfigurationRule.OpSqlMapping.valueOf(operation).getSqlOp();
        if (CollectionUtils.isNotEmpty(conditionList)){
            sb.append(ConfigurationRule.TEMPLATE_SQL_AND)
                    .append(conditionList.stream().map(Supplier::get).collect(Collectors.joining(ConfigurationRule.TEMPLATE_SQL_AND)));
        }

        return String.format(ConfigurationRule.TEMPLATE_CUSTOM_SQL, primartKey, sqlOp, projectId, fieldId,sb.toString());
    }


    protected String processPredefinedField(RuleExpressVO ruleExpressVO, Object value) {
        String operation = ruleExpressVO.getOperation();
        String field = Objects.requireNonNull(getFieldByCode(ruleExpressVO.getFieldCode()));
        String fieldType = ruleExpressVO.getFieldType();
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
                if (CustomFieldType.isDateHms(fieldType)){
                    sql = this.conditionSql(getTimeFieldExpress(field), operation, getHmsTime(value));
                }else {
                    sql = this.conditionSql(getUnixTimeExpress(field), operation, getUnixTimeExpress(valueToString(value)));
                }
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

    private String getHmsTime(Object value) {
        return ConfigurationRule.isSqlVar((String) value) ? getTimeFieldExpress(valueToString(value)) :
                getTimeValueExpress(valueToString(value));
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
        
        Assert.isTrue(CollectionUtils.isNotEmpty(configurationRuleVO.getReceiverList()) || 
                CollectionUtils.isNotEmpty(configurationRuleVO.getProcesserList()) ||
                CollectionUtils.isNotEmpty(configurationRuleVO.getUserTypes()), BaseConstants.ErrorCode.DATA_INVALID);
        if (CollectionUtils.isNotEmpty(configurationRuleVO.getReceiverList())){
            for (UserDTO userDTO : configurationRuleVO.getReceiverList()) {
                ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
                configurationRuleReceiverDTO.setProjectId(projectId);
                configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
                configurationRuleReceiverDTO.setUserType(ConfigurationRuleReceiverDTO.TYPE_RECEIVER);
                configurationRuleReceiverDTO.setUserId(userDTO.getId());
                configurationRuleReceiverMapper.insertSelective(configurationRuleReceiverDTO);
            }
        }
        if (CollectionUtils.isNotEmpty(configurationRuleVO.getProcesserList())){
            for (UserDTO userDTO : configurationRuleVO.getProcesserList()) {
                ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = new ConfigurationRuleReceiverDTO();
                configurationRuleReceiverDTO.setProjectId(projectId);
                configurationRuleReceiverDTO.setRuleId(configurationRuleDTO.getId());
                configurationRuleReceiverDTO.setUserType(ConfigurationRuleReceiverDTO.TYPE_PROCESSER);
                configurationRuleReceiverDTO.setUserId(userDTO.getId());
                configurationRuleReceiverMapper.insertSelective(configurationRuleReceiverDTO);
            }
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
