package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.FieldOptionUpdateVO;
import io.choerodon.agile.api.vo.FieldOptionVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;
import io.choerodon.agile.infra.dto.FieldOptionDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface FieldOptionService {

    FieldOptionDTO baseCreate(FieldOptionDTO option);

    void baseDelete(Long optionId);

    void baseUpdate(FieldOptionDTO option);

    FieldOptionDTO baseQueryById(Long organizationId, Long optionId);

    /**
     * 处理字段选项
     *
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @param newOptions newOptions
     */
    String handleFieldOption(Long organizationId, Long fieldId, List<FieldOptionUpdateVO> newOptions);

    /**
     * 组织层/项目层 根据字段id获取字段选项列表
     *
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @return result
     */
    List<FieldOptionVO> queryByFieldId(Long organizationId, Long fieldId);

    /**
     * 组织层/项目层 创建字段选项
     *
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @param updateDTO updateDTO
     * @return result
     */
    void create(Long organizationId, Long fieldId, FieldOptionUpdateVO updateDTO);

    /**
     * 组织层/项目层 根据字段id删除所有字段选项
     *
     * @param organizationId organizationId
     * @param fieldId fieldId
     */
    void deleteByFieldId(Long organizationId, Long fieldId);

    /**
     * 填充字段选项
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageFieldViews pageFieldViews
     */
    void fillOptions(Long organizationId, Long projectId, List<PageFieldViewVO> pageFieldViews);

    /**
     * 分页查询自定义字段下的选项
     * @param organizationId 组织id
     * @param filedId 自定义字段id
     * @param searchValue 搜索参数值
     * @param selected 已选择选项
     * @param enabled 是否启用
     * @param pageRequest 分页参数
     * @return 自定义字段下的选项
     */
    Page<FieldOptionVO> getOptionsPageByFieldId(Long organizationId, Long filedId, String searchValue, List<Long> selected, Boolean enabled, PageRequest pageRequest);

    /**
     * 创建自定义字段下的选项
     * @param fieldOptionUpdateVO 要创建的自定义字段
     * @param fieldId 字段id
     * @param organizationId 组织id
     * @return 创建的自定义字段
     */
    FieldOptionVO insertOption(FieldOptionUpdateVO fieldOptionUpdateVO, Long fieldId, Long organizationId);

    /**
     * 更新自定义字段下的选项
     * @param fieldOptionUpdateVO 要更新的自定义字段
     * @param fieldId 字段id
     * @param organizationId 组织id
     * @return 更新的自定义字段
     */
    FieldOptionVO updateOption(FieldOptionUpdateVO fieldOptionUpdateVO, Long fieldId, Long organizationId);

    /**
     * 删除自定义字段
     * @param optionId 选项id
     * @param fieldId 字段id
     * @param organizationId 组织id
     */
    void deleteOption(Long optionId, Long fieldId, Long organizationId);
}
