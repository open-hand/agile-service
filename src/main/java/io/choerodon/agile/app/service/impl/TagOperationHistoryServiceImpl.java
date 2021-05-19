package io.choerodon.agile.app.service.impl;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;

import io.choerodon.agile.api.vo.TagOperationHistoryVO;
import io.choerodon.agile.app.service.TagOperationHistoryService;
import io.choerodon.agile.infra.dto.TagOperationHistoryDTO;
import io.choerodon.agile.infra.mapper.TagOperationHistoryMapper;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 10:59
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class TagOperationHistoryServiceImpl implements TagOperationHistoryService {

    private static final String DOING = "doing";

    @Resource
    private TagOperationHistoryMapper tagOperationHistoryMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public TagOperationHistoryDTO createDefaultHistory(Long projectId, Long organizationId, Long publishVersionId, String action) {
        TagOperationHistoryDTO tagOperationHistoryDTO = new TagOperationHistoryDTO();
        tagOperationHistoryDTO.setProjectId(projectId);
        tagOperationHistoryDTO.setSuccessCount(0L);
        tagOperationHistoryDTO.setFailCount(0L);
        tagOperationHistoryDTO.setOrganizationId(organizationId);
        tagOperationHistoryDTO.setPublishVersionId(publishVersionId);
        tagOperationHistoryDTO.setAction(action);
        tagOperationHistoryDTO.setUserId(DetailsHelper.getUserDetails().getUserId());
        tagOperationHistoryDTO.setStatus(DOING);
        tagOperationHistoryMapper.insert(tagOperationHistoryDTO);
        return tagOperationHistoryMapper.selectByPrimaryKey(tagOperationHistoryDTO.getId());
    }

    @Override
    public void updateStatus(TagOperationHistoryDTO tagOperationHistoryDTO, String status, String message) {
        TagOperationHistoryDTO tagOperationHistoryUpdate = new TagOperationHistoryDTO();
        tagOperationHistoryUpdate.setId(tagOperationHistoryDTO.getId());
        tagOperationHistoryUpdate.setStatus(status);
        tagOperationHistoryUpdate.setMsg(message);
        tagOperationHistoryUpdate.setFailCount(tagOperationHistoryDTO.getFailCount());
        tagOperationHistoryUpdate.setSuccessCount(tagOperationHistoryDTO.getSuccessCount());
        tagOperationHistoryUpdate.setObjectVersionNumber(tagOperationHistoryDTO.getObjectVersionNumber());
        tagOperationHistoryMapper.updateByPrimaryKeySelective(tagOperationHistoryUpdate);
    }

    @Override
    public TagOperationHistoryVO queryLatestHistory(Long projectId, Long publishVersionId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        TagOperationHistoryDTO tagOperationHistoryDTO = tagOperationHistoryMapper.queryLatestHistory(projectId, publishVersionId, userId);
        return tagOperationHistoryDTO == null ? new TagOperationHistoryVO() : modelMapper.map(tagOperationHistoryDTO, TagOperationHistoryVO.class);
    }
}
