package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.StateMachineSchemeVO;
import io.choerodon.agile.api.vo.StatusMachineSchemeConfigVO;
import io.choerodon.agile.api.vo.event.StateMachineSchemeChangeItem;

/**
 * @author shinan.chen
 * @Date 2018/8/2
 */
public interface StateMachineSchemeConfigService {

    /**
     * 根据状态机id删除配置
     *
     * @param organizationId 组织id
     * @param stateMachineId 状态机id
     * @return result
     */
    StateMachineSchemeVO delete(Long organizationId, Long schemeId, Long stateMachineId);

    /**
     * 删除状态机方案及配置
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     */
    void deleteBySchemeId(Long organizationId, Long schemeId);

    /**
     * 创建方案
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @param schemeVOS schemeVOS
     * @return result
     */
    StateMachineSchemeVO create(Long organizationId, Long schemeId, Long stateMachineId, List<StatusMachineSchemeConfigVO> schemeVOS);

    /**
     * 创建默认配置
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @param stateMachineId stateMachineId
     */
    void createDefaultConfig(Long organizationId, Long schemeId, Long stateMachineId);

    /**
     * 更新默认配置
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @param stateMachineId stateMachineId
     */
    void updateDefaultConfig(Long organizationId, Long schemeId, Long stateMachineId);

    /**
     * 获取默认配置
     *
     * @return result
     */
    StatusMachineSchemeConfigVO selectDefault(Boolean isDraft, Long organizationId, Long schemeId);

    /**
     * 通过状态机方案id和问题类型id查询出状态机id
     *
     * @param schemeId schemeId
     * @param issueTypeId issueTypeId
     * @return result
     */
    Long queryStateMachineIdBySchemeIdAndIssueTypeId(Boolean isDraft, Long organizationId, Long schemeId, Long issueTypeId);

    /**
     * 通过状态机方案id和状态机id查询出问题类型id
     *
     * @param isDraft isDraft
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @param stateMachineId stateMachineId
     * @return result
     */
    List<Long> queryIssueTypeIdBySchemeIdAndStateMachineId(Boolean isDraft, Long organizationId, Long schemeId, Long stateMachineId);

    /**
     * 根据方案查询配置
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @return result
     */
    List<StatusMachineSchemeConfigVO> queryBySchemeId(Boolean isDraft, Long organizationId, Long schemeId);

    List<StatusMachineSchemeConfigVO> queryBySchemeIds(Boolean isDraft, Long organizationId, Set<Long> schemeIds);


    /**
     * 查询状态机关联的方案
     *
     * @return result
     */
    List<Long> querySchemeIdsByStateMachineId(Boolean isDraft, Long organizationId, Long stateMachineId);

    /**
     * 发布状态机方案
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @return result
     */
    Boolean deploy(Long organizationId, Long schemeId, List<StateMachineSchemeChangeItem> changeItems, Long objectVersionNumber);

    /**
     * 发布状态机方案校验
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @return result
     */
    List<StateMachineSchemeChangeItem> checkDeploy(Long organizationId, Long schemeId);

    /**
     * 删除状态机方案草稿配置
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @return result
     */
    StateMachineSchemeVO deleteDraft(Long organizationId, Long schemeId);


    /**
     * 把活跃的配置写到到草稿中，id一致
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     */
    void copyDeployToDraft(Boolean isDeleteOldDeploy, Long organizationId, Long schemeId);

    /**
     * 清除发布配置，复制草稿配置给发布配置
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     */
    void copyDraftToDeploy(Boolean isDeleteOldDeploy, Long organizationId, Long schemeId);

    Long queryStatusMachineBySchemeIdAndIssueType(Long organizationId, Long stateMachineSchemeId, Long issueTypeId);

    Long initStatusMachineAndSchemeConfig(Long organizationId, String name, Long schemeId, Long issueTypeId, ProjectVO projectVO, String applyType, Long statusMachineId);

    Map<String, Map<Long, Long>> queryStatusMachineMapByAppleTypes(Long organizationId, Long projectId, List<String> applyTypes);
}
