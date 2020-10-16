package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.QuickFilterValueVO;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.infra.dto.*;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-10-12 10:35
 */
public interface AgilePluginService {
    String getSystemFieldContext(String code);

    void deleteIssueForBusiness(IssueConvertDTO issueConvertDTO);

    void appendProgramFieldSql(StringBuilder sqlQuery, QuickFilterValueVO quickFilterValueVO, String value, String operation,Long projectId);

    void handlerFeatureRank(Long projectId, String type);

    void getProgramEpicIds(List<Long> epicIds, Long projectId);

    List<PageFieldDTO> handlerProgramPageField(Long projectId, String issueType, List<PageFieldDTO> pageFields);

    void handleInitIssue(List<LookupValueDTO> colorList, IssueConvertDTO issueConvertDTO);

    void updateIssueSprintChanged(IssueConvertDTO oldIssue, Long projectId, Long sprintId, String issueType);

    void updateIssueSprintChanged(String issueType, List<String> fieldList, Long projectId, IssueUpdateVO issueUpdateVO, IssueDTO originIssue);

    void checkFeatureBeforeUpdateIssue(IssueUpdateVO issueUpdateVO, Long projectId);

    void handlerAssociateSprintsWithFeature(Long projectId, Long sprintId, List<Long> frontIncomingIssues, List<IssueSearchDTO> issueSearchDTOList);

    void handlerCloneFeature(Long issueId, IssueCreateVO issueCreateVO, String applyType, Long projectId);

    void setBusinessAttributes(IssueDetailDTO issue);

    void programIssueDetailDTOToVO(IssueVO issueVO,IssueDetailDTO issue);

    void checkBeforeCreateIssue(IssueCreateVO issueCreateVO,String applyType);

    void handlerBusinessAfterCreateIssue(IssueConvertDTO issueConvertDTO, Long projectId, Long issueId, IssueCreateVO issueCreateVO);
}
