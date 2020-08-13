package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.StatusTransferSettingCreateVO;
import io.choerodon.agile.api.vo.StatusTransferSettingVO;
import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-12 9:59
 */
public interface StatusTransferSettingService {

   void createOrUpdate(Long projectId, Long issueTypeId, Long statusId,Long objectVersionNumber,String applyType,List<StatusTransferSettingCreateVO> list);

   List<StatusTransferSettingDTO> query(Long projectId, Long issueTypeId, Long statusId);

   void delete(Long projectId, Long issueTypeId, Long statusId);

   List<StatusTransferSettingVO> listByStatusIds(Long projectId, Long issueTypeId, List<Long> statusIds);

    void checkStatusTransferSetting(Long projectId, Long issueTypeId, Long endStatusId);
}
