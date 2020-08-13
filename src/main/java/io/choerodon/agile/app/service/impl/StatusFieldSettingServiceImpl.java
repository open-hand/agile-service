package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.StatusFieldSettingVO;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StatusFieldSettingService;
import io.choerodon.agile.infra.dto.StatusFieldSettingDTO;
import io.choerodon.agile.infra.dto.StatusFieldValueSettingDTO;
import io.choerodon.agile.infra.mapper.StatusFieldSettingMapper;
import io.choerodon.agile.infra.mapper.StatusFieldValueSettingMapper;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-13 14:51
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusFieldSettingServiceImpl implements StatusFieldSettingService {
    @Autowired
    private StatusFieldSettingMapper statusFieldSettingMapper;
    @Autowired
    private StatusFieldValueSettingMapper statusFieldValueSettingMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ProjectConfigService projectConfigService;

    @Override
    public List<StatusFieldSettingVO> createOrUpdate(Long project, Long issueType, Long statusId, Long objectVersionNumber, String applyType, List<StatusFieldSettingVO> list) {
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(project, issueType, statusId);
        if (!CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            deleteStatusFieldSetting(statusFieldSettingDTOS);
        }
        // 遍历
        for (StatusFieldSettingVO statusFieldSettingVO : list) {
            StatusFieldSettingDTO map = modelMapper.map(statusFieldSettingVO, StatusFieldSettingDTO.class);
            map.setProjectId(project);
            map.setStatusId(statusId);
            map.setIssueTypeId(issueType);
            baseInsert(map);
            // 插入field值
            List<StatusFieldValueSettingDTO> fieldValueList = statusFieldSettingVO.getFieldValueList();
            if (!CollectionUtils.isEmpty(fieldValueList)) {
                insertStatusFieldValue(project, map.getId(), fieldValueList);
            }
        }
        // 更新node
        projectConfigService.updateNodeObjectVersionNumber(project, issueType, statusId, objectVersionNumber, applyType);
        return list(project, issueType, statusId);
    }

    @Override
    public List<StatusFieldSettingDTO> listFieldSetting(Long project, Long issueType, Long statusId) {
        StatusFieldSettingDTO statusFieldSettingDTO = new StatusFieldSettingDTO();
        statusFieldSettingDTO.setIssueTypeId(issueType);
        statusFieldSettingDTO.setStatusId(statusId);
        statusFieldSettingDTO.setProjectId(project);
        return statusFieldSettingMapper.select(statusFieldSettingDTO);
    }

    @Override
    public List<StatusFieldSettingVO> list(Long projectId, Long issueType, Long statusId) {
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(projectId, issueType, statusId);
        if (CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            return new ArrayList<>();
        }
        List<StatusFieldSettingVO> list = new ArrayList<>();
        for (StatusFieldSettingDTO statusFieldSettingDTO : statusFieldSettingDTOS) {
            StatusFieldSettingVO map = modelMapper.map(statusFieldSettingDTO, StatusFieldSettingVO.class);
            StatusFieldValueSettingDTO statusFieldValueSettingDTO = new StatusFieldValueSettingDTO();
            statusFieldValueSettingDTO.setProjectId(statusFieldSettingDTO.getProjectId());
            statusFieldValueSettingDTO.setStatusFieldSettingId(statusFieldSettingDTO.getId());
            map.setFieldValueList(statusFieldValueSettingMapper.select(statusFieldValueSettingDTO));
            list.add(map);
        }
        return list;
    }

    private void deleteStatusFieldSetting(List<StatusFieldSettingDTO> statusFieldSettingDTOS) {
        for (StatusFieldSettingDTO statusFieldSettingDTO : statusFieldSettingDTOS) {
            StatusFieldValueSettingDTO statusFieldValueSettingDTO = new StatusFieldValueSettingDTO();
            statusFieldValueSettingDTO.setProjectId(statusFieldSettingDTO.getProjectId());
            statusFieldValueSettingDTO.setStatusFieldSettingId(statusFieldSettingDTO.getId());
            statusFieldValueSettingMapper.delete(statusFieldValueSettingDTO);
            statusFieldSettingMapper.deleteByPrimaryKey(statusFieldSettingDTO.getId());
        }
    }

    private void insertStatusFieldValue(Long projectId, Long fieldSettingId, List<StatusFieldValueSettingDTO> fieldValueList) {
        for (StatusFieldValueSettingDTO statusFieldValueSettingDTO : fieldValueList) {
            statusFieldValueSettingDTO.setStatusFieldSettingId(fieldSettingId);
            statusFieldValueSettingDTO.setProjectId(projectId);
            baseInertFieldValue(statusFieldValueSettingDTO);
        }
    }

    private void baseInertFieldValue(StatusFieldValueSettingDTO statusFieldValueSettingDTO) {
        if (statusFieldValueSettingMapper.insert(statusFieldValueSettingDTO) != 1) {
            throw new CommonException("error.insert.status.field.value.setting");
        }
    }

    private void baseInsert(StatusFieldSettingDTO map) {
        if (statusFieldSettingMapper.insert(map) != 1) {
            throw new CommonException("error.insert.status.field.setting");
        }
    }
}
