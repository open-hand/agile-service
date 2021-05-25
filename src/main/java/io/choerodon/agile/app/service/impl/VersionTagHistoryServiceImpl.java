package io.choerodon.agile.app.service.impl;

import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;

import io.choerodon.agile.api.vo.VersionTagHistoryVO;
import io.choerodon.agile.app.service.VersionTagHistoryService;
import io.choerodon.agile.infra.dto.VersionTagHistoryDTO;
import io.choerodon.agile.infra.mapper.VersionTagHistoryMapper;
import io.choerodon.core.oauth.DetailsHelper;

import java.util.Arrays;
import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 10:59
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class VersionTagHistoryServiceImpl implements VersionTagHistoryService {

    private static final String DOING = "doing";

    private static final List<String> VERSION_TYPES = Arrays.asList("program", "publish");

    @Resource
    private VersionTagHistoryMapper versionTagHistoryMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    @Transactional(rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
    public VersionTagHistoryDTO createDefaultHistory(Long projectId,
                                                     Long organizationId,
                                                     Long versionId,
                                                     String action,
                                                     String versionType) {
        VersionTagHistoryDTO versionTagHistoryDTO = new VersionTagHistoryDTO();
        versionTagHistoryDTO.setProjectId(projectId);
        versionTagHistoryDTO.setOrganizationId(organizationId);
        versionTagHistoryDTO.setVersionId(versionId);
        versionTagHistoryDTO.setAction(action);
        versionTagHistoryDTO.setUserId(DetailsHelper.getUserDetails().getUserId());
        versionTagHistoryDTO.setStatus(DOING);
        if (!VERSION_TYPES.contains(versionType)) {
            throw new CommonException("error.illegal.version.type");
        }
        versionTagHistoryDTO.setVersionType(versionType);
        versionTagHistoryMapper.insert(versionTagHistoryDTO);
        return versionTagHistoryMapper.selectByPrimaryKey(versionTagHistoryDTO.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
    public void updateStatus(VersionTagHistoryDTO versionTagHistoryDTO, String status) {
        VersionTagHistoryDTO tagOperationHistoryUpdate = new VersionTagHistoryDTO();
        tagOperationHistoryUpdate.setId(versionTagHistoryDTO.getId());
        tagOperationHistoryUpdate.setStatus(status);
        tagOperationHistoryUpdate.setObjectVersionNumber(versionTagHistoryDTO.getObjectVersionNumber());
        versionTagHistoryMapper.updateByPrimaryKeySelective(tagOperationHistoryUpdate);
    }

    @Override
    public VersionTagHistoryVO queryLatestHistory(Long projectId, Long versionId, String versionType) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        VersionTagHistoryDTO versionTagHistoryDTO = versionTagHistoryMapper.queryLatestHistory(projectId, versionId, userId, versionType);
        return versionTagHistoryDTO == null ? new VersionTagHistoryVO() : modelMapper.map(versionTagHistoryDTO, VersionTagHistoryVO.class);
    }
}
