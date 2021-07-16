package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;

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
}
