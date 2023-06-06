package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.DataLogCreateVO;
import io.choerodon.agile.api.vo.DataLogFixVO;
import io.choerodon.agile.api.vo.FieldDataLogVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.api.vo.business.DataLogVO;
import io.choerodon.agile.api.vo.business.InstanceOpenRelVO;
import io.choerodon.agile.api.vo.business.RuleLogRelVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.DataLogDTO;
import io.choerodon.agile.infra.dto.DataLogStatusChangeDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.DataLogType;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.mapper.DataLogMapper;
import io.choerodon.agile.infra.mapper.FieldDataLogMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/6/14.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class DataLogServiceImpl implements DataLogService {

    @Autowired
    private DataLogMapper dataLogMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private FieldDataLogService fieldDataLogService;
    @Autowired
    private FieldDataLogMapper fieldDataLogMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired(required = false)
    private AgileTriggerService agileTriggerService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    private static final String ISSUE = "issue";
    private static final String CUSTOM_FIELD = "custom_field";

    @Override
    public DataLogVO createDataLog(Long projectId, DataLogCreateVO createVO) {
        DataLogDTO dataLogDTO = modelMapper.map(createVO, DataLogDTO.class);
        dataLogDTO.setProjectId(projectId);
        if (dataLogMapper.insert(dataLogDTO) != 1) {
            throw new CommonException("error.dataLog.insert");
        }
        return modelMapper.map(dataLogMapper.selectByPrimaryKey(dataLogDTO.getLogId()), DataLogVO.class);
    }

    @Override
    public List<DataLogVO> listByIssueId(Long projectId, Long issueId) {
        List<DataLogVO> dataLogVOS = modelMapper.map(dataLogMapper.selectByIssueId(projectId, issueId), new TypeToken<List<DataLogVO>>() {
        }.getType());
        Map<Long, RuleLogRelVO> ruleLogRelMap = queryRuleLogRelMap(projectId, issueId);
        Set<Long> logIds = dataLogVOS.stream().map(DataLogVO::getLogId).collect(Collectors.toSet());
        Map<Long, InstanceOpenRelVO> issueLogOpenRelMap = queryDataLogOpenRelMap(projectId, logIds, DataLogType.ISSUE.value());
        appendSummary(dataLogVOS, ruleLogRelMap, issueLogOpenRelMap);
        List<FieldDataLogVO> fieldDataLogVOS = fieldDataLogService.queryByInstanceId(projectId, issueId, ObjectSchemeCode.AGILE_ISSUE);
        Map<Long, RuleLogRelVO> customFieldRuleLogMap = queryCustomFieldRuleLogMap(fieldDataLogVOS, projectId);
        Set<Long> customFieldLogIds = fieldDataLogVOS.stream().map(FieldDataLogVO::getId).collect(Collectors.toSet());
        Map<Long, InstanceOpenRelVO> customFieldLogOpenRelMap = queryDataLogOpenRelMap(projectId, customFieldLogIds, DataLogType.CUSTOM_FIELD.value());
        for (FieldDataLogVO fieldDataLogVO : fieldDataLogVOS) {
            Long logId = fieldDataLogVO.getId();
            DataLogVO dataLogVO = modelMapper.map(fieldDataLogVO, DataLogVO.class);
            dataLogVO.setField(fieldDataLogVO.getFieldCode());
            dataLogVO.setIssueId(fieldDataLogVO.getInstanceId());
            dataLogVO.setIsCusLog(true);
            dataLogVOS.add(dataLogVO);
            RuleLogRelVO ruleLogRel = customFieldRuleLogMap.get(logId);
            if (!ObjectUtils.isEmpty(ruleLogRel)) {
                dataLogVO.setRuleName(ruleLogRel.getRuleName());
            }
            InstanceOpenRelVO instanceOpenRelVO = customFieldLogOpenRelMap.get(logId);
            if (instanceOpenRelVO != null) {
                dataLogVO.setInstanceOpenRelVO(instanceOpenRelVO);
            }
        }
        fillUserAndStatus(projectId, dataLogVOS);
        return dataLogVOS.stream().sorted(Comparator.comparing(DataLogVO::getCreationDate).reversed()).collect(Collectors.toList());
    }

    @Override
    public Map<Long, InstanceOpenRelVO> queryDataLogOpenRelMap(Long projectId,
                                                               Set<Long> logIds,
                                                               String businessType) {
        if (CollectionUtils.isEmpty(logIds) || agilePluginService == null) {
            return MapUtils.EMPTY_MAP;
        }
        return agilePluginService.listDataLogOpenInstanceRel(logIds, businessType, projectId).stream().collect(Collectors.toMap(InstanceOpenRelVO::getInstanceId, Function.identity()));
    }

    private Map<Long, RuleLogRelVO> queryRuleLogRelMap(Long projectId, Long issueId) {
        Map<Long, RuleLogRelVO> ruleLogRelMap = new HashMap<>();
        if (agileTriggerService != null) {
            RuleLogRelVO ruleLogRelVO = new RuleLogRelVO();
            ruleLogRelVO.setProjectId(projectId);
            ruleLogRelVO.setInstanceId(issueId);
            ruleLogRelVO.setBusinessType(ISSUE);
            List<RuleLogRelVO> ruleLogRelList = agileTriggerService.queryRuleLogRelList(ruleLogRelVO);
            ruleLogRelMap = ruleLogRelList.stream().collect(Collectors.toMap(RuleLogRelVO::getLogId, Function.identity()));
        }
        return ruleLogRelMap;
    }

    private Map<Long, RuleLogRelVO> queryCustomFieldRuleLogMap(List<FieldDataLogVO> fieldDataLogs,
                                                               Long projectId) {
        if (ObjectUtils.isEmpty(fieldDataLogs) || ObjectUtils.isEmpty(agileTriggerService)) {
            return new HashMap<>();
        }
        Set<Long> customFieldLogIds = fieldDataLogs.stream().map(FieldDataLogVO::getId).collect(Collectors.toSet());
        RuleLogRelVO ruleLogRelVO = new RuleLogRelVO();
        ruleLogRelVO.setProjectId(projectId);
        ruleLogRelVO.setSearchLogIds(customFieldLogIds);
        ruleLogRelVO.setBusinessType(CUSTOM_FIELD);
        return agileTriggerService.queryRuleLogRelList(ruleLogRelVO)
                .stream()
                .collect(Collectors.toMap(RuleLogRelVO::getLogId, Function.identity()));
    }

    private void appendSummary(List<DataLogVO> dataLogs,
                               Map<Long, RuleLogRelVO> ruleLogRelMap,
                               Map<Long, InstanceOpenRelVO> issueLogOpenRelMap) {
        List<String> specialFields = Arrays.asList("Feature Link", "Epic Link", "Epic Child", "Feature Child");
        Set<Long> issueIds = new HashSet<>();
        List<DataLogVO> dataLogList = new ArrayList<>();
        dataLogs.forEach(dataLog -> {
            if (specialFields.contains(dataLog.getField())) {
                dataLogList.add(dataLog);
                String oldValue = dataLog.getOldValue();
                String newValue = dataLog.getNewValue();
                if (oldValue != null) {
                    issueIds.add(Long.valueOf(oldValue));
                }
                if (newValue != null) {
                    issueIds.add(Long.valueOf(newValue));
                }
            }
            Long logId = dataLog.getLogId();
            if (ruleLogRelMap.get(logId) != null) {
                dataLog.setRuleName(ruleLogRelMap.get(logId).getRuleName());
            }
            InstanceOpenRelVO instanceOpenRelVO = issueLogOpenRelMap.get(logId);
            if (instanceOpenRelVO != null) {
                dataLog.setInstanceOpenRelVO(instanceOpenRelVO);
            }
        });
        Map<Long, String> summaryMap = new HashMap<>();
        if (!issueIds.isEmpty()) {
            summaryMap.putAll(
                    issueMapper.selectByIds(StringUtils.join(issueIds, ","))
                            .stream()
                            .collect(Collectors.toMap(IssueDTO::getIssueId, IssueDTO::getSummary)));
        }
        dataLogList.forEach(x -> {
            String oldValue = x.getOldValue();
            String newValue = x.getNewValue();
            if (oldValue != null) {
                Long issueId = Long.valueOf(oldValue);
                String summary = summaryMap.get(issueId);
                if (summary != null) {
                    String oldString = x.getOldString();
                    oldString = oldString + ":" + summary;
                    x.setOldString(oldString);
                }
            }
            if (newValue != null) {
                Long issueId = Long.valueOf(newValue);
                String summary = summaryMap.get(issueId);
                if (summary != null) {
                    String newString = x.getNewString();
                    newString = newString + ":" + summary;
                    x.setNewString(newString);
                }
            }
        });
    }

    /**
     * 填充用户信息、状态信息、触发器日志信息、第三方应用触发操作信息等
     *
     * @param projectId projectId
     * @param dataLogVOS dataLogVOS
     */
    private void fillUserAndStatus(Long projectId, List<DataLogVO> dataLogVOS) {
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        List<Long> createByIds = dataLogVOS.stream().filter(dataLogDTO -> dataLogDTO.getCreatedBy() != null && !Objects.equals(dataLogDTO.getCreatedBy(), 0L)).map(DataLogVO::getCreatedBy).distinct().collect(Collectors.toList());
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(createByIds, true);
        for (DataLogVO dto : dataLogVOS) {
            UserMessageDTO userMessageDTO = usersMap.get(dto.getCreatedBy());
            if (!ObjectUtils.isEmpty(userMessageDTO)) {
                dto.setName(userMessageDTO.getName());
                dto.setLoginName(userMessageDTO.getLoginName());
                dto.setRealName(userMessageDTO.getRealName());
                dto.setImageUrl(userMessageDTO.getImageUrl());
                dto.setEmail(userMessageDTO.getEmail());
            }
            if ("status".equals(dto.getField())) {
                StatusVO statusMapVO = statusMapDTOMap.get(Long.parseLong(dto.getNewValue()));
                dto.setCategoryCode(statusMapVO != null ? statusMapVO.getType() : null);
            }
            if (dto.getIsCusLog() == null) {
                dto.setIsCusLog(false);
            }
        }
    }

    @Override
    public DataLogDTO create(DataLogDTO dataLogDTO) {
        if (dataLogMapper.insert(dataLogDTO) != 1) {
            throw new CommonException("error.dataLog.insert");
        }
        return dataLogMapper.selectByPrimaryKey(dataLogDTO.getLogId());
    }

    @Override
    public Set<Long> delete(DataLogDTO dataLogDTO) {
        Set<Long> logIds = dataLogMapper.select(dataLogDTO).stream().map(DataLogDTO::getLogId).collect(Collectors.toSet());
        dataLogMapper.delete(dataLogDTO);
        return logIds;
    }

    @Override
    public void batchDeleteErrorDataLog(Set<Long> dataLogIds) {
        dataLogMapper.batchDeleteErrorDataLog(dataLogIds);
    }

    @Override
    public void batchUpdateErrorDataLog(Set<DataLogStatusChangeDTO> dataLogStatusChangeDTOS) {
        dataLogMapper.batchUpdateErrorDataLog(dataLogStatusChangeDTOS);
    }

    @Override
    public List<DataLogFixVO> queryListByProjectId(Long projectId) {
        List<DataLogFixVO> dataLogDTOS = dataLogMapper.queryListByProjectId(projectId);
        if (dataLogDTOS != null && !dataLogDTOS.isEmpty()) {
            return dataLogDTOS;
        } else {
            return new ArrayList<>();
        }
    }
}
