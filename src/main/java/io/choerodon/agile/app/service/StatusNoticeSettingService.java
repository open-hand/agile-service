package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;

/**
 * 邮件通知应用服务
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
public interface StatusNoticeSettingService {

    StatusNoticeSettingVO detail(Long projectId, Long issueTypeId, Long statusId, String schemeCode);

    void save(Long projectId, StatusNoticeSettingVO StatusNoticeSettingVO);

    void noticeByChangeStatus(Long projectId, Long issueId);

    List<StatusNoticeSettingVO> list(Long projectId, Long issueTypeId, List<Long> statusIdList, String applyType);
}
