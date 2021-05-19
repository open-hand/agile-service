package io.choerodon.agile.app.service.impl;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;

import io.choerodon.agile.api.vo.PublishVersionTagHistoryVO;
import io.choerodon.agile.app.service.PublishVersionTagHistoryService;
import io.choerodon.agile.infra.dto.PublishVersionTagHistoryDTO;
import io.choerodon.agile.infra.mapper.PublishVersionTagHistoryMapper;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 10:59
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PublishVersionTagHistoryServiceImpl implements PublishVersionTagHistoryService {

    private static final String DOING = "doing";

    @Resource
    private PublishVersionTagHistoryMapper publishVersionTagHistoryMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public PublishVersionTagHistoryDTO createDefaultHistory(Long projectId, Long organizationId, Long publishVersionId, String action) {
        PublishVersionTagHistoryDTO publishVersionTagHistoryDTO = new PublishVersionTagHistoryDTO();
        publishVersionTagHistoryDTO.setProjectId(projectId);
        publishVersionTagHistoryDTO.setOrganizationId(organizationId);
        publishVersionTagHistoryDTO.setPublishVersionId(publishVersionId);
        publishVersionTagHistoryDTO.setAction(action);
        publishVersionTagHistoryDTO.setUserId(DetailsHelper.getUserDetails().getUserId());
        publishVersionTagHistoryDTO.setStatus(DOING);
        publishVersionTagHistoryMapper.insert(publishVersionTagHistoryDTO);
        return publishVersionTagHistoryMapper.selectByPrimaryKey(publishVersionTagHistoryDTO.getId());
    }

    @Override
    public void updateStatus(PublishVersionTagHistoryDTO publishVersionTagHistoryDTO, String status) {
        PublishVersionTagHistoryDTO tagOperationHistoryUpdate = new PublishVersionTagHistoryDTO();
        tagOperationHistoryUpdate.setId(publishVersionTagHistoryDTO.getId());
        tagOperationHistoryUpdate.setStatus(status);
        tagOperationHistoryUpdate.setObjectVersionNumber(publishVersionTagHistoryDTO.getObjectVersionNumber());
        publishVersionTagHistoryMapper.updateByPrimaryKeySelective(tagOperationHistoryUpdate);
    }

    @Override
    public PublishVersionTagHistoryVO queryLatestHistory(Long projectId, Long publishVersionId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        PublishVersionTagHistoryDTO publishVersionTagHistoryDTO = publishVersionTagHistoryMapper.queryLatestHistory(projectId, publishVersionId, userId);
        return publishVersionTagHistoryDTO == null ? new PublishVersionTagHistoryVO() : modelMapper.map(publishVersionTagHistoryDTO, PublishVersionTagHistoryVO.class);
    }
}
