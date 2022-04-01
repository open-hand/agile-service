package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssueStatusLinkageVO;
import io.choerodon.agile.api.vo.LinkIssueStatusLinkageVO;
import io.choerodon.agile.api.vo.StatusVO;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2021-06-09 11:03
 */
public interface LinkIssueStatusLinkageService {

    List<LinkIssueStatusLinkageVO> createOrUpdate(Long projectId, Long organizationId, Long issueTyId, Long statusId, List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS);

    List<LinkIssueStatusLinkageVO> listByIssueTypeAndStatusId(Long projectId, Long organizationId, Long issueTypeId, Long statusId);

    Map<Long,List<LinkIssueStatusLinkageVO>> listByIssueTypeAndStatusIds(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds);

    void updateLinkIssueStatus(Long projectId, Long issueId, String applyType, Set<Long> influenceIssueIds);

    void deleteByStatusId(Long projectId, Long organizationId, Long statusId, Long issueTypeId);

    List<StatusVO> queryStatus(Long projectId, Long organizationId, LinkIssueStatusLinkageVO linkageVO);

    Map<Long, IssueStatusLinkageVO> queryMapByProject(Long projectId, Long organizationId);
}
