package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.IssueTypeDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.IssueTypeSchemeSearchVO;
import io.choerodon.agile.api.vo.IssueTypeSchemeVO;
import io.choerodon.agile.api.vo.IssueTypeSchemeWithInfoVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.infra.dto.IssueTypeSchemeDTO;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author shinan.chen
 * @Date 2018/8/10
 */
public interface IssueTypeSchemeService {

    IssueTypeSchemeDTO baseCreate(IssueTypeSchemeDTO scheme);

    /**
     * 查询方案
     *
     * @param organizationId
     * @param issueTypeSchemeId
     * @param projectId
     * @return
     */
    IssueTypeSchemeVO queryById(Long organizationId,
                                Long projectId,
                                Long issueTypeSchemeId);

    /**
     * 创建方案
     *
     * @param organizationId
     * @param issueTypeSchemeVO
     * @return
     */
    IssueTypeSchemeVO create(Long organizationId, IssueTypeSchemeVO issueTypeSchemeVO);

    /**
     * 更新方案
     *
     * @param organizationId
     * @param issueTypeSchemeVO
     * @return
     */
    IssueTypeSchemeVO update(Long organizationId, IssueTypeSchemeVO issueTypeSchemeVO);

    /**
     * 校验是否可以删除
     *
     * @param organizationId
     * @param issueTypeSchemeId
     * @return
     */
    Map<String, Object> checkDelete(Long organizationId, Long issueTypeSchemeId);

    /**
     * 删除
     *
     * @param organizationId
     * @param issueTypeSchemeId
     * @return
     */
    Boolean delete(Long organizationId, Long issueTypeSchemeId);

    /**
     * 校验方案名是否可用
     *
     * @param organizationId
     * @param name
     * @return
     */
    Boolean checkName(Long organizationId, String name, Long id);

    /**
     * 创建方案配置
     *
     * @param organizationId
     * @param issueTypeSchemeId
     * @param issueTypeVOS
     */
    void createConfig(Long organizationId, Long issueTypeSchemeId, List<IssueTypeVO> issueTypeVOS);

    /**
     * 创建项目初始化问题类型方案
     *
     * @param projectId
     * @param projectCode
     * @return
     */
    void initByConsumeCreateProject(Long projectId, String projectCode);

    void initByConsumeCreateProjectByCodes(Long projectId, String projectCode, Set<String> codes);


    Page<IssueTypeSchemeWithInfoVO> queryIssueTypeSchemeList(PageRequest pageRequest, Long organizationId, IssueTypeSchemeSearchVO issueTypeSchemeVO);

    void initScheme(Long projectId, Long organizationId, String name, Long defaultIssueTypeId, String schemeApplyType, Map<String, IssueTypeDTO> issueTypeMap);

    void initSchemeByCodes(Long projectId, Long organizationId, String name, Long defaultIssueTypeId, String schemeApplyType, Map<String, IssueTypeDTO> issueTypeMap, Set<String> codes);
}
