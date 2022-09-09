package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.infra.dto.business.IssueDTO;

/**
 * 邮件通知应用服务
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
public interface StatusNoticeSettingService {

    StatusNoticeSettingVO detail(Long projectId, Long issueTypeId, Long statusId, String schemeCode);

    void save(Long projectId, StatusNoticeSettingVO statusNoticeSettingVO, String applyType);

    void noticeByChangeStatus(Long projectId, Long issueId);

    List<StatusNoticeSettingVO> list(Long projectId, Long issueTypeId, List<Long> statusIdList, String schemeCode);

    /**
     * 获取通知方式和通知对象
     * @param projectId projectId
     * @param issue issue
     * @return 请注意: StatusNoticeSettingVO里仅userIdList和userTypeList有值
     */
    StatusNoticeSettingVO selectNoticeUserAndType(Long projectId, IssueDTO issue);

    /**
     * 组织层状态机模板查询通知设置
     * @param organizationId organizationId
     * @param issueTypeId issueTypeId
     * @param statusId statusId
     * @param schemeCode schemeCode
     * @return result
     */
    StatusNoticeSettingVO statusNoticeDetail(Long organizationId, Long issueTypeId, Long statusId, String schemeCode);

    /**
     * 组织层状态机模板设置通知设置
     * @param organizationId organizationId
     * @param statusNoticeSettingVO statusNoticeSettingVO
     */
    void saveStatusNotice(Long organizationId, StatusNoticeSettingVO statusNoticeSettingVO);

    /**
     * 查询组织状态流转显示通知设置列表
     * @param organizationId organizationId
     * @param issueTypeId issueTypeId
     * @param statusIdList statusIdList
     * @param schemeCode schemeCode
     * @return result
     */
    List<StatusNoticeSettingVO> listStatusNoticeSetting(Long organizationId, Long issueTypeId, List<Long> statusIdList, String schemeCode);
}
