package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.FieldCascadeCreateVO;
import io.choerodon.agile.api.vo.FieldCascadeRuleVO;
import io.choerodon.agile.api.vo.FieldCascadeUpdateVO;
import io.choerodon.agile.api.vo.PageConfigFieldVO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 16:06
 */
public interface FieldCascadeRuleService {

    /**
     * 创建级联规则
     * @param projectId 项目id
     * @param fieldCascadeCreate 创建参数
     * @return 创建的级联规则
     */
    FieldCascadeRuleVO createFieldCascadeRule(Long projectId, FieldCascadeCreateVO fieldCascadeCreate);

    /**
     * 查询项目下指定问题类型的所有级联规则
     * @param projectId 项目id
     * @param issueTypeId 问题类型id
     * @param fieldId 字段id
     * @return 指定问题类型的所有级联规则
     */
    List<FieldCascadeRuleVO> listFieldCascadeRuleByIssueType(Long projectId, Long issueTypeId, Long fieldId);

    /**
     * 更新级联规则
     * @param projectId 项目id
     * @param fieldCascadeRuleId 要更新的规则id
     * @param fieldCascadeUpdate 要更新的内容
     * @return 更新的级联规则
     */
    FieldCascadeRuleVO updateFieldCascadeRule(Long projectId, Long fieldCascadeRuleId, FieldCascadeUpdateVO fieldCascadeUpdate);

    /**
     * 查询级联规则详情
     * @param projectId 项目id
     * @param fieldCascadeRuleId 级联规则id
     * @return 级联规则详情
     */
    FieldCascadeRuleVO fieldCascadeRuleDetail(Long projectId, Long fieldCascadeRuleId);

    /**
     * 查询可级联字段
     * @param projectId 项目id
     * @param issueTypeId 问题类型id
     * @param fieldId 字段id
     * @return 可级联字段
     */
    List<PageConfigFieldVO> listCascadePageFieldView(Long projectId, Long issueTypeId, Long fieldId);

    /**
     * 批量创建/更新/修改级联规则
     * @param projectId 项目id
     * @param fieldCascadeRuleList 要变动的级联规则
     * @return 变动的级联规则
     */
    List<FieldCascadeRuleVO> batchMutationFieldCascadeRule(Long projectId, List<FieldCascadeRuleVO> fieldCascadeRuleList);
}
