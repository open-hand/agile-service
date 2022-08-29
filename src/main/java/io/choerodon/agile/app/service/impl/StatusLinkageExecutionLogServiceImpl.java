package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.ExecutionLogQueryVO;
import io.choerodon.agile.api.vo.IssueLinkVO;
import io.choerodon.agile.api.vo.StatusLinkageExecutionLogVO;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.StatusLinkageExecutionLogService;
import io.choerodon.agile.infra.dto.StatusLinkageExecutionLogDTO;
import io.choerodon.agile.infra.mapper.StatusLinkageExecutionLogMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-08-03 9:41
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusLinkageExecutionLogServiceImpl implements StatusLinkageExecutionLogService {

    @Autowired
    private StatusLinkageExecutionLogMapper statusLinkageExecutionLogMapper;
    @Autowired
    private IssueService issueService;

    @Override
    public StatusLinkageExecutionLogDTO create(Long projectId, Long organizationId, StatusLinkageExecutionLogDTO statusLinkageExecutionLogDTO) {
        statusLinkageExecutionLogDTO.setProjectId(projectId);
        statusLinkageExecutionLogDTO.setOrganizationId(organizationId);
        if (ObjectUtils.isEmpty(statusLinkageExecutionLogDTO.getContent())) {
            throw new CommonException("error.link.setting.id.is.empty");
        }
        if (statusLinkageExecutionLogMapper.insertSelective(statusLinkageExecutionLogDTO) != 1) {
            throw new CommonException("error.link.issue.execution.log.insert");
        }
        return statusLinkageExecutionLogDTO;
    }

    @Override
    public void deleteByIssueId(Long projectId, Long organizationId, Long issueId) {
        statusLinkageExecutionLogMapper.deleteByIssueId(projectId, organizationId, issueId);
    }

    @Override
    public Page<StatusLinkageExecutionLogVO> pageExecutionLogS(Long projectId, PageRequest pageRequest, ExecutionLogQueryVO executionLogQueryVO) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Page<StatusLinkageExecutionLogVO> page = PageHelper.doPageAndSort(pageRequest, () -> statusLinkageExecutionLogMapper.listExecutionLog(projectId, organizationId, executionLogQueryVO));
        List<StatusLinkageExecutionLogVO> content = page.getContent();
        if (CollectionUtils.isEmpty(content)) {
            return new Page<>();
        }
        // 获取所有的issueId
        List<Long> issueIds = new ArrayList<>();
        content.forEach(v -> {
            issueIds.add(v.getPreIssueId());
            issueIds.add(v.getCurIssueId());
        });
        List<IssueLinkVO> linkVOS = issueService.queryIssueByIssueIds(projectId, issueIds);
        Map<Long, IssueLinkVO> issueMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(linkVOS)){
            issueMap.putAll(linkVOS.stream().collect(Collectors.toMap(IssueLinkVO::getIssueId, Function.identity())));
        }
        for (StatusLinkageExecutionLogVO statusLinkageExecutionLog : content) {
            statusLinkageExecutionLog.setPreIssueInfo(issueMap.getOrDefault(statusLinkageExecutionLog.getPreIssueId(), null));
            statusLinkageExecutionLog.setCurIssueInfo(issueMap.getOrDefault(statusLinkageExecutionLog.getCurIssueId(), null));
        }
        page.setContent(content);
        return page;
    }
}
