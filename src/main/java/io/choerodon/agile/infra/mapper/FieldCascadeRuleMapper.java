package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;

import io.choerodon.agile.api.vo.FieldCascadeRuleVO;
import io.choerodon.agile.infra.dto.FieldCascadeRuleDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 16:07
 */
public interface FieldCascadeRuleMapper extends BaseMapper<FieldCascadeRuleDTO> {

    /**
     * 查询项目下指定问题类型的所有级联规则
     *
     * @param projectId   项目id
     * @param issueTypeId 问题类型id
     * @param fieldId     字段id
     * @return 指定问题类型的所有级联规则
     */
    List<FieldCascadeRuleVO> listFieldCascadeRuleByIssueType(@Param("projectId") Long projectId, @Param("issueTypeId") Long issueTypeId, @Param("fieldId") Long fieldId);

    /**
     * 查询级联规则的详情
     *
     * @param projectId          项目id
     * @param fieldCascadeRuleId 级联规则id
     * @return 级联规则的详情
     */
    FieldCascadeRuleVO selectFieldCascadeRuleDetail(@Param("projectId") Long projectId, @Param("fieldCascadeRuleId") Long fieldCascadeRuleId);

    /**
     * 按ruleId批量删除规则
     *
     * @param ids       ruleId
     * @param projectId 项目id
     */
    void batchDeleteByIds(@Param("ids") List<Long> ids, @Param("projectId") Long projectId);

    /**
     * 查询问题类型下所有更改必输或更改隐藏的规则
     *
     * @param projectId   项目id
     * @param issueTypeId 问题类型id
     * @return 问题类型下所有更改必输或更改隐藏的规则
     */
    List<FieldCascadeRuleVO> selectFieldCascadeRequiredOrHiddenRule(@Param("projectId") Long projectId, @Param("issueTypeId") Long issueTypeId);
}
