package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

import io.choerodon.agile.api.vo.ComponentForListVO;
import io.choerodon.agile.api.vo.FieldOptionVO;
import io.choerodon.agile.api.vo.PriorityVO;
import io.choerodon.agile.api.vo.ProductVersionNameVO;
import io.choerodon.agile.infra.dto.FieldCascadeRuleOptionDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 16:07
 */
public interface FieldCascadeRuleOptionMapper extends BaseMapper<FieldCascadeRuleOptionDTO> {
    /**
     * 批量删除级联规则选项
     *
     * @param ids 要删除的id
     */
    void batchDeleteFieldCascadeRuleOptionByIds(@Param("ids") List<Long> ids);

    /**
     * 通过级联规则id批量删除级联规则选项
     *
     * @param ruleIds   要删除的id
     * @param projectId 项目id
     */
    void batchDeleteByFieldCascadeRuleIds(@Param("ruleIds") List<Long> ruleIds, @Param("projectId") Long projectId);

    /**
     * 查询级联字段可见模块
     *
     * @param projectId           项目id
     * @param fieldCascadeRuleIds 级联规则id
     * @param content             搜索参数
     * @return 级联字段可见模块
     */
    List<ComponentForListVO> selectCascadeFieldComponent(@Param("projectId") Long projectId, @Param("fieldCascadeRuleIds") List<Long> fieldCascadeRuleIds, @Param("content") String content);

    /**
     * 查询级联字段可见优先级
     *
     * @param projectId           项目id
     * @param organizationId      组织id
     * @param fieldCascadeRuleIds 级联规则id
     * @param priorityName        搜索参数
     * @return 级联字段可见优先级
     */
    List<PriorityVO> selectCascadeFieldPriority(@Param("projectId") Long projectId, @Param("organizationId") Long organizationId, @Param("fieldCascadeRuleIds") List<Long> fieldCascadeRuleIds, @Param("priorityName") String priorityName);

    /**
     * 查询多个级联规则下可见的选项Id
     *
     * @param projectId           项目id
     * @param fieldCascadeRuleIds 级联规则id
     * @return 可见的选项
     */
    Set<Long> selectVisibleOptionIds(
            @Param("projectId") Long projectId,
            @Param("fieldCascadeRuleIds") List<Long> fieldCascadeRuleIds);

    /**
     * 查询级联规则可见自定义字段可选项值
     *
     * @param organizationId      组织id
     * @param fieldId             字段id
     * @param projectId           项目id
     * @param searchParam         查询参数
     * @param selected            已选择选项
     * @param fieldCascadeRuleIds 级联规则id
     * @return 可见自定义可选项值
     */
    List<FieldOptionVO> selectCascadeFieldCustom(
            @Param("organizationId") Long organizationId,
            @Param("projectId") Long projectId,
            @Param("fieldId") Long fieldId,
            @Param("searchParam") String searchParam,
            @Param("selected") Set<Long> selected,
            @Param("fieldCascadeRuleIds") List<Long> fieldCascadeRuleIds);

    /**
     * 按id查询级联规则可见自定义字段可选项值
     *
     * @param organizationId      组织id
     * @param projectId           项目id
     * @param optionIds           选项id
     * @param fieldCascadeRuleIds 级联规则id
     * @return 可见自定义字段可选项值
     */
    List<FieldOptionVO> selectCascadeFieldCustomByOptionIds(
            @Param("organizationId") Long organizationId,
            @Param("projectId") Long projectId,
            @Param("optionIds") Set<Long> optionIds,
            @Param("fieldCascadeRuleIds") List<Long> fieldCascadeRuleIds);

    /**
     * 查询级联规则可见版本选项
     *
     * @param projectId 组织id
     * @param organizationId 项目id
     * @param fieldCascadeRuleIds 级联规则id
     * @param statusCodes 状态code
     * @return 可见版本选项
     */
    List<ProductVersionNameVO> selectCascadeFieldVersion(
            @Param("projectId") Long projectId,
            @Param("organizationId") Long organizationId,
            @Param("fieldCascadeRuleIds") List<Long> fieldCascadeRuleIds,
            @Param("statusCodes") List<String> statusCodes);

    List<Long> selectFieldCascadeRuleByFieldIdAndOptionId(@Param("organizationId") Long organizationId, @Param("fieldId") Long fieldId, @Param("optionId") Long optionId);

    void deleteByCascadeRuleIds(@Param("cascadeRuleIds") List<Long> cascadeRuleIds);
}
