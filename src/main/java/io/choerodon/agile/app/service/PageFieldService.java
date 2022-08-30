package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.PageFieldDTO;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface PageFieldService {

    PageFieldDTO baseCreate(PageFieldDTO pageField);

    void baseDelete(Long pageFieldId);

    void baseUpdate(PageFieldDTO pageField);

    PageFieldDTO baseQueryById(Long organizationId, Long projectId, Long pageFieldId);

    /**
     * 根据pageCode和context获取pageField，不存在则创建
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageCode pageCode
     * @param issueTypeId issueTypeId
     * @return result
     */
    List<PageFieldDTO> queryPageField(Long organizationId, Long projectId, String pageCode, Long issueTypeId);

    /**
     * 组织层/项目层 根据页面编码获取字段列表
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageCode pageCode
     * @param context context
     * @return result
     */
    Map<String, Object> listQuery(Long organizationId, Long projectId, String pageCode, String context, Long issueTypeId);

    /**
     * 组织层/项目层 调整字段顺序
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param adjustOrder adjustOrder
     */
    PageFieldVO adjustFieldOrder(Long organizationId, Long projectId, String pageCode, AdjustOrderVO adjustOrder);

    /**
     * 组织层/项目层 更新页面字段
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param fieldId fieldId
     * @param updateDTO updateDTO
     * @return result
     */
    PageFieldVO update(Long organizationId, Long projectId, String pageCode, Long fieldId, PageFieldUpdateVO updateDTO);

    /**
     * 组织层初始化页面字段
     *
     * @param organizationId organizationId
     */
    void initPageFieldByOrg(Long organizationId);

    /**
     * 组织层 创建页面字段
     *
     * @param organizationId organizationId
     * @param field field
     */
    void createByFieldWithOrg(Long organizationId, ObjectSchemeFieldDTO field);

    /**
     * 项目层 创建页面字段
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param field field
     */
    void createByFieldWithPro(Long organizationId, Long projectId, ObjectSchemeFieldDTO field);

    /**
     * 删除字段
     *
     * @param fieldId fieldId
     */
    void deleteByFieldId(Long fieldId);

    /**
     * 界面上获取字段列表，带有字段选项
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param paramDTO paramDTO
     * @return result
     */
    List<PageFieldViewVO> queryPageFieldViewList(Long organizationId, Long projectId, PageFieldViewParamVO paramDTO);

    List<PageFieldViewVO> queryPageFieldViewsNoPermissionFilter(Long organizationId,
                                                                Long projectId,
                                                                PageFieldViewParamVO paramDTO);

    /**
     * 根据实例id从界面上获取字段列表，带有字段值、字段选项
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param instanceId instanceId
     * @param paramDTO paramDTO
     * @return result
     */
    List<PageFieldViewVO> queryPageFieldViewListWithInstanceId(Long organizationId, Long projectId, Long instanceId, PageFieldViewParamVO paramDTO);

    /**
     * 根据实例ids获取全部自定义字段的CodeValue键值对
     *
     * @param organizationId organizationId
     * @param projectIds projectIds
     * @param instanceIds instanceIds
     * @return result
     */
    Map<Long, Map<String, Object>> queryFieldValueWithIssueIdsForAgileExport(Long organizationId, List<Long> projectIds, List<Long> instanceIds, Boolean isJustStr);

    /**
     * 根据传入的fieldIds,过滤出必填的字段
     * @param projectId projectId
     * @param issueTypeId issueTypeId
     * @param fieldIds fieldIds
     * @return result
     */
    List<PageFieldViewVO> filterRequireFieldByFieldCodes(Long projectId, Long issueTypeId, List<String> fieldIds);
}
