package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.DataLogDTO;
import io.choerodon.agile.infra.dto.DataLogStatusChangeDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.mapper.DataLogMapper;
import io.choerodon.agile.infra.mapper.FieldDataLogMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

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
        Map<Long, RuleLogRelVO> ruleLogRelMap = new HashMap<>();
        if (agileTriggerService != null) {
            RuleLogRelVO ruleLogRelVO = new RuleLogRelVO();
            ruleLogRelVO.setProjectId(projectId);
            ruleLogRelVO.setInstanceId(issueId);
            ruleLogRelVO.setBusinessType("issue");
            List<RuleLogRelVO> ruleLogRelList = agileTriggerService.queryRuleLogRelList(ruleLogRelVO);
            ruleLogRelMap = ruleLogRelList.stream().collect(Collectors.toMap(RuleLogRelVO::getLogId, Function.identity()));
        }
        List<FieldDataLogVO> fieldDataLogVOS = fieldDataLogService.queryByInstanceId(projectId, issueId, ObjectSchemeCode.AGILE_ISSUE);
        for (FieldDataLogVO fieldDataLogVO : fieldDataLogVOS) {
            DataLogVO dataLogVO = modelMapper.map(fieldDataLogVO, DataLogVO.class);
            dataLogVO.setField(fieldDataLogVO.getFieldCode());
            dataLogVO.setIssueId(fieldDataLogVO.getInstanceId());
            dataLogVO.setIsCusLog(true);
            dataLogVOS.add(dataLogVO);
        }
        fillUserAndStatus(projectId, dataLogVOS, ruleLogRelMap);
        return dataLogVOS.stream().sorted(Comparator.comparing(DataLogVO::getCreationDate).reversed()).collect(Collectors.toList());
    }

    /**
     * 填充用户信息
     *
     * @param projectId
     * @param dataLogVOS
     */
    private void fillUserAndStatus(Long projectId, List<DataLogVO> dataLogVOS,
                                   Map<Long, RuleLogRelVO> ruleLogRelMap) {
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        List<Long> createByIds = dataLogVOS.stream().filter(dataLogDTO -> dataLogDTO.getCreatedBy() != null && !Objects.equals(dataLogDTO.getCreatedBy(), 0L)).map(DataLogVO::getCreatedBy).distinct().collect(Collectors.toList());
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(createByIds, true);
        for (DataLogVO dto : dataLogVOS) {
            UserMessageDTO userMessageDTO = usersMap.get(dto.getCreatedBy());
            String name = userMessageDTO != null ? userMessageDTO.getName() : null;
            String loginName = userMessageDTO != null ? userMessageDTO.getLoginName() : null;
            String realName = userMessageDTO != null ? userMessageDTO.getRealName() : null;
            String imageUrl = userMessageDTO != null ? userMessageDTO.getImageUrl() : null;
            String email = userMessageDTO != null ? userMessageDTO.getEmail() : null;
            dto.setName(name);
            dto.setLoginName(loginName);
            dto.setRealName(realName);
            dto.setImageUrl(imageUrl);
            dto.setEmail(email);
            if (ruleLogRelMap.get(dto.getLogId()) != null) {
                dto.setRuleName(ruleLogRelMap.get(dto.getLogId()).getRuleName());
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
    public void delete(DataLogDTO dataLogDTO) {
        dataLogMapper.delete(dataLogDTO);
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

    @Override
    public Page<AllDataLogVO> listAllDataLogByProjectId(Long projectId, DataLogQueryVO dataLogQueryVO, PageRequest pageRequest) {
        List<AllDataLogVO> issueDataLogList = dataLogMapper.listIssueDataLogByProjectId(projectId, dataLogQueryVO);
        List<AllDataLogVO> allDataLogList = new ArrayList<>(issueDataLogList);
        boolean containBacklog = ((CollectionUtils.isEmpty(dataLogQueryVO.getTypeIds()) || dataLogQueryVO.getTypeIds().contains(0L)) && backlogExpandService != null);
        List<AllDataLogVO> fdDataLogList = fieldDataLogMapper.listFdDataLogByProjectId(projectId, dataLogQueryVO, containBacklog);
        allDataLogList.addAll(fdDataLogList);

        if (containBacklog) {
            List<AllDataLogVO> backlogDataLogList = backlogExpandService.listBacklogDataLogByProjectId(projectId, dataLogQueryVO);
            allDataLogList.addAll(backlogDataLogList);
        }

        Page<AllDataLogVO> result = PageUtils.createPageFromList(
                allDataLogList.stream().sorted(Comparator.comparing(AllDataLogVO::getCreationDate).reversed()).collect(Collectors.toList()),
                pageRequest
        );
        setDataLogIssueInfo(result, projectId);
        if (containBacklog) {
            backlogExpandService.setDataLogBacklogInfo(result);
        }
        setDataLogUserInfo(result);
        return result;
    }

    private void setDataLogUserInfo(List<AllDataLogVO> dataLogList) {
        List<Long> createdByIds = dataLogList.stream().map(AllDataLogVO::getCreatedBy).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(createdByIds)) {
            return;
        }
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(createdByIds, true);
        dataLogList.forEach(dataLog -> dataLog.setCreatedByUser(userMap.get(dataLog.getCreatedBy())));
    }

    private void setDataLogIssueInfo(List<AllDataLogVO> issueDataLogList, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<Long> issueIds = issueDataLogList
                .stream()
                .filter(dataLog -> "agile_issue".equals(dataLog.getLogType()))
                .map(AllDataLogVO::getInstanceId)
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(issueIds)) {
            return;
        }
        Map<Long, IssueTypeVO> issueTypeMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
        List<IssueSearchDTO> issueList = issueMapper.queryIssueByIssueIds(projectId, issueIds);
        Map<Long, IssueSearchDTO> issueMap = issueList.stream().collect(Collectors.toMap(IssueSearchDTO::getIssueId, Function.identity()));
        issueDataLogList.forEach(dataLog -> {
            IssueSearchDTO issue = issueMap.get(dataLog.getInstanceId());
            if(issue == null){
                return;
            }
            dataLog.setNum(issue.getIssueNum());
            dataLog.setSummary(issue.getSummary());
            dataLog.setIssueTypeVO(issueTypeMap.get(issue.getIssueTypeId()));
        });
    }
}
