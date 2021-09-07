package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PageFieldViewUpdateVO;
import io.choerodon.agile.api.vo.StatusFieldSettingVO;
import io.choerodon.agile.api.vo.VersionIssueRelVO;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.infra.dto.StatusFieldSettingDTO;
import io.choerodon.agile.infra.dto.StatusFieldValueSettingDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;

import java.util.List;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2020-08-13 14:46
 */
public interface StatusFieldSettingService {
   List<StatusFieldSettingVO> createOrUpdate(Long project, Long issueType, Long statusId, Long objectVersionNumber,String  applyType, List<StatusFieldSettingVO> list);

   List<StatusFieldSettingDTO> listFieldSetting(Long organizationId, Long project, Long issueType, Long statusId);

   List<StatusFieldSettingVO> list(Long projectId, Long issueType, Long statusId);

   List<StatusFieldSettingVO> listByStatusIds(Long projectId, Long issueType, List<Long> statusIds);

   void handlerSettingToUpdateIssue(Long projectId, Long issueId, TriggerCarrierVO triggerCarrierVO);

   List<StatusFieldSettingVO> saveStatusFieldSettings(Long organizationId, Long issueType, Long statusId, Long objectVersionNumber, List<StatusFieldSettingVO> list);

   List<StatusFieldSettingVO> listByOptions(Long organizationId, Long issueType, Long statusId);

   List<StatusFieldSettingVO> listStatusFieldSetting(Long organizationId, Long issueType, List<Long> statusIds);

   void processSystemFieldValues(IssueDTO issueDTO,
                                 IssueUpdateVO issueUpdateVO,
                                 List<String> field,
                                 Map<String, List<VersionIssueRelVO>> versionMap,
                                 Map<String, Object> specifyMap,
                                 String fieldCode,
                                 List<StatusFieldValueSettingDTO> statusFieldValueSettings);


   void processCustomFieldValues(IssueDTO issueDTO,
                                 List<PageFieldViewUpdateVO> customField,
                                 Long fieldId, String fieldType,
                                 List<StatusFieldValueSettingDTO> statusFieldValueSettings,
                                 String fieldCode);

   void updateIssue(IssueDTO issueDTO,
                    List<String> field,
                    IssueUpdateVO issueUpdateVO,
                    List<PageFieldViewUpdateVO> customField,
                    Map<String, List<VersionIssueRelVO>> versionMap,
                    Map<String, Object> specifyMap,
                    boolean doRuleNotice,
                    TriggerCarrierVO triggerCarrierVO);
}
