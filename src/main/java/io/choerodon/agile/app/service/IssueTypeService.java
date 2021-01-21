package io.choerodon.agile.app.service;

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

    IssueTypeVO queryById(Long organizationId, Long issueTypeId);

    IssueTypeVO create(Long organizationId,
                       Long projectId,
                       IssueTypeVO issueTypeVO);

    IssueTypeVO update(IssueTypeVO issueTypeVO);

    void delete(Long organizationId, Long projectId, Long issueTypeId);

    Page<IssueTypeVO> pagedQuery(PageRequest pageRequest,
                                 Long organizationId,
                                 Long projectId,
                                 IssueTypeSearchVO issueTypeSearchVO);

    Boolean checkName(Long organizationId, Long projectId, String name, Long id);

    List<IssueTypeVO> queryByOrgId(Long organizationId);

    /**
     * 通过状态机方案id查询当前组织下的问题类型（包含对应的状态机）
     *
     * @param organizationId 组织id
     * @return 问题类型列表
     */
    List<IssueTypeVO> queryIssueTypeByStateMachineSchemeId(Long organizationId, Long schemeId);

    /**
     * 消费组织创建事件生成组织初始化的五种issue类型
     *
     * @param organizationId organizationId
     */
    void initIssueTypeByConsumeCreateOrganization(Long organizationId);

    Map<Long, IssueTypeVO> listIssueTypeMap(Long organizationId);

    Map<Long, Map<String, Long>> initIssueTypeData(Long organizationId, List<Long> orgIds);

    IssueTypeDTO createIssueType(IssueTypeDTO issueType);

    /**
     * 查询issueType map, key为typeCode, value为id
     *
     * @param organizationId
     * @return
     */
    Map<String, Long> queryIssueTypeMap(Long organizationId);

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
     */
    void reference(Long projectId, Long organizationId, Long referenceId);
}
