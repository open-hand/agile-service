package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.InfluenceIssueVO;
import io.choerodon.agile.api.vo.IssueLinkChangeVO;
import io.choerodon.agile.api.vo.LinkIssueStatusLinkageVO;
import io.choerodon.agile.api.vo.StatusLinkageVO;
import io.choerodon.agile.infra.dto.StatusLinkageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2020-08-17 19:12
 */
public interface StatusLinkageService {
    List<StatusLinkageVO> createOrUpdate(Long projectId, Long issueTypeId, Long statusId, Long objectVersionNumber, String applyType, List<StatusLinkageVO> linkageVOS);

    List<StatusLinkageVO> listByIssueTypeAndStatusId(Long projectId, Long issueTypeId, Long statusId);

    List<StatusLinkageVO> listByStatusIds(Long projectId, Long issueTypeId, List<Long> statusIds,String applyType);

    boolean updateParentStatus(Long projectId, Long issueId, String applyType, Set<Long> influenceIssueIds);

    void getUpdateParentStatusIssue(Long projectId, IssueDTO issueDTO, Long statusId, String applyType, InfluenceIssueVO influenceIssueVO, Map<Long, List<Long>> allInfluenceMap, Map<Long, List<IssueLinkChangeVO>> issueLinkChangeGroup);

    List<StatusLinkageVO> listStatusLinkageByProjectId(Long projectId);

    List<StatusLinkageVO> saveStatusLinkage(Long organizationId, Long issueTypeId, Long statusId, Long objectVersionNumber, List<StatusLinkageVO> linkageVOS);

    List<StatusLinkageVO> listByOptions(Long organizationId, Long issueTypeId, Long statusId);

    List<StatusLinkageVO> listStatusLinkage(Long projectId, Long issueTypeId, List<Long> statusIds);

    LinkIssueStatusLinkageVO queryById(Long projectId, Long id);

    List<StatusLinkageDTO> queryByStatusIdAndIssueTypeId(Long projectId, Long issueTypeId, Long statusId);

    boolean updateParentStatusByStatusLinkage(String applyType,
                                              Set<Long> influenceIssueIds,
                                              IssueDTO sourceIssue,
                                              IssueDTO parentIssue,
                                              StatusLinkageDTO statusLinkageDTO,
                                              List<IssueDTO> childrenIssueOfParentIssue);
}
