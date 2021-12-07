package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssueTypeRankVO;
import io.choerodon.agile.api.vo.ProjectIssueTypeVO;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.infra.dto.IssueTypeDTO;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.agile.api.vo.IssueTypeSearchVO;
import io.choerodon.agile.api.vo.IssueTypeVO;

import java.util.List;
import java.util.Map;

/**
 * @author shinan.chen
 * @Date 2018/8/8
 */
public interface IssueTypeService {

    IssueTypeVO queryById(Long issueTypeId, Long projectId);

    IssueTypeVO create(Long organizationId,
                       Long projectId,
                       IssueTypeVO issueTypeVO);

    IssueTypeVO update(IssueTypeVO issueTypeVO, List<String> fieldList);

    void delete(Long organizationId, Long projectId, Long issueTypeId);

    Page<IssueTypeVO> pagedQuery(PageRequest pageRequest,
                                 Long organizationId,
                                 Long projectId,
                                 IssueTypeSearchVO issueTypeSearchVO);

    Boolean checkName(Long organizationId, Long projectId, String name, Long id);

    List<IssueTypeVO> queryByOrgId(Long organizationId, Long projectId);

    /**
     * 消费组织创建事件生成组织初始化的五种issue类型
     *
     * @param organizationId organizationId
     */
    void initIssueTypeByConsumeCreateOrganization(Long organizationId);

    Map<Long, IssueTypeVO> listIssueTypeMap(Long organizationId, Long projectId);

    Map<Long, Map<String, Long>> initIssueTypeData(Long organizationId, List<Long> orgIds);

    IssueTypeDTO createIssueType(IssueTypeDTO issueType);

    /**
     * @param organizationId
     * @param projectId
     * @param issueTypeId
     * @return
     */
    IssueTypeVO query(Long organizationId, Long projectId, Long issueTypeId);

    Boolean canDisable(Long organizationId, Long projectId, Long issueTypeId);

    /**
     * 更新启停用字段
     *
     * @param organizationId
     * @param projectId
     * @param issueTypeId
     * @param enabled
     */
    void updateEnabled(Long organizationId, Long projectId, Long issueTypeId, Boolean enabled);

    /**
     * 组织问题类型更新是否可以被引用
     *
     * @param organizationId
     * @param issueTypeId
     * @param referenced
     */
    void updateReferenced(Long organizationId, Long issueTypeId, Boolean referenced);

    /**
     * 组织问题类型查询使用详情
     *
     * @param organizationId
     * @param issueTypeId
     * @param pageRequest
     * @return
     */
    Page<ProjectIssueTypeVO> usageDetail(Long organizationId, Long issueTypeId,
                                         PageRequest pageRequest);

    /**
     * 查询组织可以引用的问题类型
     *
     * @param pageRequest
     * @param organizationId
     * @param projectId
     * @return
     */
    Page<IssueTypeVO> pageQueryReference(PageRequest pageRequest, Long organizationId, Long projectId);

    /**
     * 将组织层问题类型引用到项目层
     *
     * @param projectId
     * @param organizationId
     * @param referenceId
     * @param issueTypeVO
     */
    void reference(Long projectId, Long organizationId, Long referenceId, IssueTypeVO issueTypeVO);

    String getIssueTypeById(Long issueTypeId);

    /**
     * 项目层更新系统问题类型
     *
     * @param organizationId
     * @param projectId
     * @param issueTypeId
     * @param issueTypeVO
     */
    void updateSystemIssueType(Long organizationId,
                               Long projectId,
                               Long issueTypeId,
                               IssueTypeVO issueTypeVO);

    /**
     * 判断icon是否重复
     *
     * @param organizationId
     * @param projectId
     * @param icon
     * @param id
     * @return
     */
    Boolean checkIcon(Long organizationId, Long projectId, String icon, Long id);

    /**
     * 根据projectIds查询问题类型
     *
     * @param organizationId
     * @param projectIds
     * @return
     */
    Map<Long, List<IssueTypeVO>> listIssueTypeMapByProjectIds(Long organizationId, List<Long> projectIds);

    /**
     * 更新rank值
     *
     * @param projectId
     * @param organizationId
     * @param issueTypeId
     * @param issueTypeRankVO
     */
    void updateRank(Long projectId, Long organizationId, Long issueTypeId, IssueTypeRankVO issueTypeRankVO);

    Page<IssueTypeVO> pagingProjectIssueTypes(PageRequest pageRequest, Long organizationId, IssueTypeSearchVO issueTypeSearchVO);

    void initIssueTypeIfNotExisted(Long organizationId);
}
