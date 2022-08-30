package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.FieldPermissionVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;
import io.choerodon.agile.api.vo.PageTemplateFieldVO;
import io.choerodon.agile.api.vo.PermissionVO;

/**
 * @author superlee
 * @since 2021-07-20
 */
public interface FieldPermissionService {

    /**
     * 创建或者更新字段的权限
     *
     * @param projectId projectId
     * @param organizationId organizationId
     * @param fieldPermissionVO fieldPermissionVO
     */
    void create(Long projectId,
                Long organizationId,
                FieldPermissionVO fieldPermissionVO);

    /**
     * 批量创建或者更新字段的权限
     *
     * @param projectId projectId
     * @param organizationId organizationId
     * @param fieldPermissionVO fieldPermissionVO
     */
    void batchCreate(Long projectId,
                     Long organizationId,
                     FieldPermissionVO fieldPermissionVO);

    /**
     * 查询单个字段的权限
     *
     * @param projectId projectId
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @param issueTypeId issueTypeId
     * @return result
     */
    List<PermissionVO> queryByFieldId(Long projectId,
                                      Long organizationId,
                                      Long fieldId,
                                      Long issueTypeId);

    /**
     * 查询权限配置忽略的字段code集合
     *
     * @return result
     */
    List<String> ignoredFields();

    /**
     * @param issueTypeId issueTypeId
     * @param projectId projectId
     * @param organizationId organizationId
     * @param pageTemplateFieldList pageTemplateFieldList
     * @see PageTemplateFieldVO#setPermissionList(List)  设置权限集合
     * @see PageTemplateFieldVO#setAllowedEditPermission(Boolean) 设置是否可以编辑权限
     */
    void setFieldPermissionList(Long issueTypeId,
                                Long projectId,
                                Long organizationId,
                                List<PageTemplateFieldVO> pageTemplateFieldList);

    List<PageFieldViewVO> filterPageFieldViewVO(Long projectId,
                                                Long organizationId,
                                                Long issueTypeId,
                                                List<PageFieldViewVO> pageFieldViews);

    /**
     * 筛选没有权限的字段
     *
     * @param result result
     * @return result
     */
    List<PageFieldViewVO> filterNoPermissionFields(Long projectId,
                                                   Long organizationId,
                                                   Long issueTypeId,
                                                   List<PageFieldViewVO> result);
}
