package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.StatusBranchMergeSettingVO;
import io.choerodon.agile.api.vo.TransformVO;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StatusBranchMergeSettingService;
import io.choerodon.agile.infra.dto.StatusBranchMergeSettingDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.feign.RemoteIamFeignClient;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.StatusBranchMergeSettingMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.List;

/**
 * @author superlee
 * @since 2021-04-19
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusBranchMergeSettingServiceImpl implements StatusBranchMergeSettingService {
    @Autowired
    private StatusBranchMergeSettingMapper statusBranchMergeSettingMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private RemoteIamFeignClient remoteIamFeignClient;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private IssueService issueService;

    private static final Logger LOGGER = LoggerFactory.getLogger(StatusBranchMergeSettingServiceImpl.class);

    @Override
    public StatusBranchMergeSettingVO query(Long projectId, Long organizationId, Long issueTypeId, Long statusId) {
        StatusBranchMergeSettingDTO result = queryByIssueTypeIdAndStatusId(projectId, organizationId, issueTypeId, statusId);
        if (result != null) {
            return modelMapper.map(result, StatusBranchMergeSettingVO.class);
        } else {
            return null;
        }
    }

    private StatusBranchMergeSettingDTO queryByIssueTypeIdAndStatusId(Long projectId, Long organizationId, Long issueTypeId, Long statusId) {
        StatusBranchMergeSettingDTO dto = new StatusBranchMergeSettingDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setIssueTypeId(issueTypeId);
        dto.setStatusId(statusId);
        return statusBranchMergeSettingMapper.selectOne(dto);
    }

    @Override
    public void updateAutoTransform(Long projectId,
                                    Long organizationId,
                                    Long issueTypeId,
                                    Long statusId,
                                    Boolean autoTransform) {
        StatusBranchMergeSettingDTO result = queryByIssueTypeIdAndStatusId(projectId, organizationId, issueTypeId, statusId);
        Long id;
        if (result == null) {
            StatusBranchMergeSettingDTO dto = new StatusBranchMergeSettingDTO();
            dto.setProjectId(projectId);
            dto.setOrganizationId(organizationId);
            dto.setAutoTransform(autoTransform);
            dto.setStatusId(statusId);
            dto.setIssueTypeId(issueTypeId);
            statusBranchMergeSettingMapper.insertSelective(dto);
            id = dto.getId();
        } else {
            result.setAutoTransform(autoTransform);
            if (statusBranchMergeSettingMapper.updateByPrimaryKeySelective(result) != 1) {
                throw new CommonException("error.update.status.branch.merge.autoTransform");
            }
            id = result.getId();
        }
        //更新其他问题类型的autoTransform为false
        updateAutoTransformExceptSelf(id);
    }

    @Override
    public void handleBranchMergeEvent(Long projectId, Long issueId) {
        IssueDTO issue = new IssueDTO();
        issue.setProjectId(projectId);
        issue.setIssueId(issueId);
        IssueDTO result = issueMapper.selectOne(issue);
        if (ObjectUtils.isEmpty(result)) {
            LOGGER.error("update issue status error when branch merge because of issue not existed, issueId: {}", issueId);
            return;
        }
        Long issueTypeId = result.getIssueTypeId();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        StatusBranchMergeSettingDTO dto = new StatusBranchMergeSettingDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setIssueTypeId(issueTypeId);
        dto.setAutoTransform(true);
        List<StatusBranchMergeSettingDTO> list = statusBranchMergeSettingMapper.select(dto);
        if (!list.isEmpty()) {
            StatusBranchMergeSettingDTO statusBranchMergeSettingDTO = list.get(0);
            if(!statusBranchMergeSettingDTO.getStatusId().equals(result.getStatusId())) {
                //更新状态
                updateStatus(result, statusBranchMergeSettingDTO.getStatusId(), projectId);
            }
        }
    }

    @Override
    public List<StatusBranchMergeSettingVO> listByOptions(Long projectId,
                                                          Long organizationId,
                                                          Long issueTypeId,
                                                          List<Long> statusIds) {
        List<StatusBranchMergeSettingDTO> result =
                statusBranchMergeSettingMapper.listByOptions(projectId, organizationId, issueTypeId, statusIds);
        return modelMapper.map(result, new TypeToken<List<StatusBranchMergeSettingVO>>(){}.getType());
    }

    private void updateStatus(IssueDTO issue, Long targetStatusId, Long projectId) {
        Long issueTypeId = issue.getIssueTypeId();
        String applyType = projectConfigService.getApplyType(projectId, issueTypeId);
        List<TransformVO> transforms =
                projectConfigService.queryTransformsByProjectId(projectId, issue.getStatusId(), issue.getIssueId(), issue.getIssueTypeId(), applyType);
        Long transformId = null;
        for (TransformVO transform : transforms) {
            if (targetStatusId.equals(transform.getStatusVO().getId())) {
                transformId = transform.getId();
            }
        }
        if (transformId != null) {
            issueService.updateIssueStatus(projectId, issue.getIssueId(), transformId, issue.getObjectVersionNumber(), applyType);
        }
    }

    private void updateAutoTransformExceptSelf(Long id) {
        StatusBranchMergeSettingDTO self = statusBranchMergeSettingMapper.selectByPrimaryKey(id);
        if (self.getAutoTransform()) {
            StatusBranchMergeSettingDTO dto = new StatusBranchMergeSettingDTO();
            dto.setProjectId(self.getProjectId());
            dto.setOrganizationId(self.getOrganizationId());
            dto.setIssueTypeId(self.getIssueTypeId());
            statusBranchMergeSettingMapper.select(dto).forEach(x -> {
                if (!self.getId().equals(x.getId()) && x.getAutoTransform()) {
                    x.setAutoTransform(false);
                    statusBranchMergeSettingMapper.updateByPrimaryKeySelective(x);
                }
            });
        }
    }
}
