package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.FieldPermissionVO;
import io.choerodon.agile.api.vo.PageTemplateFieldVO;
import io.choerodon.agile.api.vo.PermissionVO;

import java.util.List;

/**
 * @author superlee
 * @since 2021-07-20
 */
public interface FieldPermissionService {

    /**
     * 创建或者更新字段的权限
     *
     * @param projectId
     * @param organizationId
     * @param fieldPermissionVO
     */
    void create(Long projectId,
                Long organizationId,
                FieldPermissionVO fieldPermissionVO);

    /**
     * 批量创建或者更新字段的权限
     *
     * @param projectId
     * @param organizationId
     * @param fieldPermissionVO
     */
    void batchCreate(Long projectId,
                     Long organizationId,
                     FieldPermissionVO fieldPermissionVO);

    /**
     * 查询单个字段的权限
     *
     * @param projectId
     * @param organizationId
     * @param fieldId
     * @param issueTypeId
     * @return
     */
    List<PermissionVO> queryByFieldId(Long projectId,
                                      Long organizationId,
                                      Long fieldId,
                                      Long issueTypeId);

    /**
     * 查询权限配置忽略的字段code集合
     *
     * @return
     */
    List<String> ignoredFields();

    /**
     * @param issueTypeId
     * @param projectId
     * @param organizationId
     * @param pageTemplateFieldList
     * @see PageTemplateFieldVO#setPermissionList(List)  设置权限集合
     * @see PageTemplateFieldVO#setAllowedEditPermission(Boolean) 设置是否可以编辑权限
     */
    void setFieldPermissionList(Long issueTypeId,
                                Long projectId,
                                Long organizationId,
                                List<PageTemplateFieldVO> pageTemplateFieldList);
}
