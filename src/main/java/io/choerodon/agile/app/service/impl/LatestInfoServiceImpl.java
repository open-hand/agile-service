package io.choerodon.agile.app.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.agile.app.service.BacklogExpandService;
import io.choerodon.agile.app.service.IssueTypeService;
import io.choerodon.agile.app.service.LatestInfoService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;
import io.choerodon.agile.infra.mapper.DataLogMapper;
import io.choerodon.agile.infra.mapper.FieldDataLogMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author chihao.ran@hand-china.com
 * @since 2021-03-23
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class LatestInfoServiceImpl implements LatestInfoService {

    private static final int MAX_SIZE = 100;

    @Autowired
    private DataLogMapper dataLogMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private FieldDataLogMapper fieldDataLogMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;

    @Override
    public Page<AllDataLogVO> listLatestOperationInfoByProjectId(Long projectId, DataLogQueryVO dataLogQueryVO, PageRequest pageRequest) {
        if (pageRequest.getSize() > MAX_SIZE) {
            pageRequest.setSize(MAX_SIZE);
        }
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
