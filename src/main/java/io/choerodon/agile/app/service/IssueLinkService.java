package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.infra.dto.IssueLinkDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/14
 */
public interface IssueLinkService {

    /**
     * 创建issueLink
     *
     * @param issueLinkCreateVOList issueLinkCreateVOList
     * @param issueId                issueId
     * @param projectId              projectId
     * @return IssueLinkVO
     */
    IssueLinkResponseVO createIssueLinkList(List<IssueLinkCreateVO> issueLinkCreateVOList, Long issueId, Long projectId);

    /**
     * 根据issueLinkId删除issueLink
     *
     * @param issueLinkId issueLinkId
     */
    void deleteIssueLink(Long issueLinkId);

    /**
     * 根据issueId查询issueLink
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @param noIssueTest noIssueTest
     * @return IssueLinkVO
     */
    List<IssueLinkVO> listIssueLinkByIssueId(Long issueId, Long projectId, Boolean noIssueTest);

    List<IssueLinkVO> listIssueLinkByBatch(Long projectId, List<Long> issueIds);


    List<IssueLinkDTO> create(IssueLinkDTO issueLinkDTO);

    int deleteByIssueId(Long issueId);

    int delete(Long issueLinkId);

    List<IssueLinkFixVO> listIssueLinkByIssuedIds(Long projectId);

    void deleteIssueLinkByIssueId(IssueConvertDTO issueConvertDTO, List<IssueLinkDTO> issueLinkDTOS);

    /**
     * 查询未关联当前问题的问题
     *
     * @param issueId 测试用例id
     * @param projectId 项目id
     * @param searchVO 查询参数
     * @param pageRequest 分页参数
     * @param organizationId 组织id
     * @return 测试用例未关联的问题
     */
    Page<IssueListFieldKVVO> listUnLinkIssue(Long issueId, Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId);

    /**
     * 校验关联问题时状态联动是否会出现死循环
     * @param projectId projectId
     * @param issueId issueId
     * @param linkTypeId linkTypeId
     * @param linkIssueIds linkIssueIds
     * @return result
     */
    List<Long> checkLinkIssueCycle(Long projectId, Long issueId,  Long linkTypeId, List<Long> linkIssueIds);
}
