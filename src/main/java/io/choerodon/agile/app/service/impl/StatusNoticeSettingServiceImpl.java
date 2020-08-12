package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.agile.infra.dto.StatusNoticeSettingDTO;
import io.choerodon.agile.infra.mapper.StatusMapper;
import io.choerodon.agile.infra.mapper.StatusNoticeSettingMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.collections4.CollectionUtils;
import org.hzero.core.base.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 邮件通知应用服务默认实现
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusNoticeSettingServiceImpl implements StatusNoticeSettingService {

    @Autowired
    private StatusNoticeSettingMapper statusNoticeSettingMapper;
    @Autowired
    private StatusMapper statusMapper;

    @Override
    public StatusNoticeSettingVO detail(Long projectId, Long issueTypeId, Long statusId) {
        StatusNoticeSettingVO statusNoticeSettingVO = new StatusNoticeSettingVO(projectId, issueTypeId, statusId);
        StatusNoticeSettingDTO notice = new StatusNoticeSettingDTO(projectId, issueTypeId, statusId);
        List<StatusNoticeSettingDTO> list = statusNoticeSettingMapper.select(notice);
        if (CollectionUtils.isEmpty(list)){
            return statusNoticeSettingVO;
        }
        list.forEach(item -> statusNoticeSettingVO.addUserWithNotice(item.getUserType(), item.getUserId()));
        statusNoticeSettingVO.setNoticeType(list.stream().findFirst().orElse(new StatusNoticeSettingDTO()).getNoticeType());
        return statusNoticeSettingVO;
    }

    @Override
    public void save(Long projectId, StatusNoticeSettingVO statusNoticeSettingVO) {
        StatusNoticeSettingDTO noticeDTO = new StatusNoticeSettingDTO(projectId, statusNoticeSettingVO.getIssueTypeId(),
                statusNoticeSettingVO.getStatusId());
        // 删除
        List<StatusNoticeSettingDTO> deleteList = statusNoticeSettingMapper.select(noticeDTO);
        if (CollectionUtils.isNotEmpty(deleteList)){
            deleteList.forEach(item -> statusNoticeSettingMapper.delete(item));
        }
        // 插入
        List<StatusNoticeSettingDTO> saveList = statusNoticeSettingVO.getUserTypeList()
                .stream()
                .map(useType -> new StatusNoticeSettingDTO(statusNoticeSettingVO, useType))
                .collect(Collectors.toList());
        saveList.addAll(statusNoticeSettingVO.getUserIdList()
                .stream()
                .map(userId -> new StatusNoticeSettingDTO(statusNoticeSettingVO, userId))
                .collect(Collectors.toList()));
        saveList.forEach(statusNoticeSettingMapper::insertSelective);
        StatusDTO statusDTO = new StatusDTO();
        statusDTO.setId(statusNoticeSettingVO.getStatusId());
        statusDTO.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        int i = statusMapper.updateOptional(statusDTO);
        if (i != 1){
            throw new CommonException(BaseConstants.ErrorCode.OPTIMISTIC_LOCK);
        }
    }
}
