package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import io.choerodon.agile.api.vo.NoticeEventVO;
import io.choerodon.agile.api.vo.business.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.domain.AuditDomain;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * 服务接口
 *
 * @author jiaxu.cui@hand-china.com
 * @date 2020-09-23 09:29:15
 */
public interface ConfigurationRuleService {
    ConfigurationRuleVO create(Long projectId, ConfigurationRuleVO configurationRuleVO);

    ConfigurationRuleVO update(Long projectId, Long ruleId, ConfigurationRuleVO configurationRuleVO);

    void deleteById(Long projectId, Long filterId);

    ConfigurationRuleVO queryById(Long projectId, Long filterId);

    /**
     * 分页搜索过滤条件
     *
     * @param pageRequest pageRequest
     * @return ConfigurationRuleVO
     */
    Page<ConfigurationRuleVO> listByProjectId(ConfigurationRuleVO configurationRuleVO, PageRequest pageRequest);

    /**
     * 获取ruleList对应的接收人和抄送人
     * @param ruleIds ruleIds
     * @return map
     */
    Map<Long, ConfigurationRuleVO> selectRuleSettingsById(List<Long> ruleIds, Long projectId);
    
    void changeRuleEnabled(Long projectId, Long ruleId, boolean enabled);

    boolean checkUniqueName(Long projectId, Long ruleId, String name);

    /**
     * 生成sql查询条件
     * @param configurationRuleVO configurationRuleVO
     * @param checkMode 在检查模式下，仅会根据语句条件生成，不会去拼接字段类型条件，仅作检查
     * @return sql
     */
    String generateSqlQuery(ConfigurationRuleVO configurationRuleVO,
                            Map<String, ObjectSchemeFieldDTO> customFieldMap,
                            boolean checkMode,
                            boolean isBacklogTrigger);

    /**
     * 筛选出检测更新字段受页面规则限制的规则
     * @param sourceList 页面规则
     * @param fieldList 更新字段
     * @param allFieldCheck 是否是全字段检测
     * @param checkMode 在检查模式下，仅会根据语句条件生成，不会去拼接字段类型条件，仅作检查
     * @return 仅对更新字段检测的规则集合
     */
    List<ConfigurationRuleVO> processRule(Long projectId,
                                          List<ConfigurationRuleVO> sourceList,
                                          Set<String> fieldList,
                                          boolean allFieldCheck,
                                          boolean checkMode,
                                          boolean isBacklogTrigger);

    List<ConfigurationRuleVO> selectByProjectId(ConfigurationRuleVO configurationRuleVO);

    List<Long> selectReceiverByRuleIds(List<Long> ruleIdList, List<String> types);

    void doTrigger(boolean isCreatedEvent,
                   AuditDomain auditDomain,
                   ConfigurationRuleVO rule,
                   Long projectId,
                   boolean createCustomFieldMember,
                   Long noticeInstanceId);

    ConfigurationRuleVO filterRuleByCondition(ConfigurationRuleVO rule,
                                              Long instanceId,
                                              NoticeEventVO noticeEvent,
                                              Set<Long> ruleIdsFindByMemberFields,
                                              boolean checkMode);

    Map<Long, Map<String, String>> selectDataMapByIds(Set<Long> backlogIds);

    void batchUpdateInvokeTrigger(List<TriggerCarrierVO> triggerCarriers);

    void batchInsertInvokeTrigger(List<TriggerCarrierVO> triggerCarriers);
}

