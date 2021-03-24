package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.StatusFieldSettingVO;
import io.choerodon.agile.infra.dto.StatusFieldSettingDTO;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-13 14:46
 */
public interface StatusFieldSettingService {
   List<StatusFieldSettingVO> createOrUpdate(Long project, Long issueType, Long statusId, Long objectVersionNumber,String  applyType, List<StatusFieldSettingVO> list);

   List<StatusFieldSettingDTO> listFieldSetting(Long organizationId, Long project, Long issueType, Long statusId);

   List<StatusFieldSettingVO> list(Long projectId, Long issueType, Long statusId);

   List<StatusFieldSettingVO> listByStatusIds(Long projectId, Long issueType, List<Long> statusIds);

   void handlerSettingToUpdateIssue(Long projectId,Long issueId);

   List<StatusFieldSettingVO> saveStatusFieldSettings(Long organizationId, Long issueType, Long statusId, Long objectVersionNumber, List<StatusFieldSettingVO> list);

   List<StatusFieldSettingVO> listByOptions(Long organizationId, Long issueType, Long statusId);

   List<StatusFieldSettingVO> listStatusFieldSetting(Long organizationId, Long issueType, List<Long> statusIds);
}
