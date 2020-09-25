package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * 服务接口
 *
 * @author jiaxu.cui@hand-china.com
 * @date 2020-09-23 09:29:15
 */
public interface ConfigurationRuleService {
    ConfigurationRuleVO create(Long projectId, ConfigurationRuleVO ConfigurationRuleVO);

    ConfigurationRuleVO update(Long projectId, Long ruleId, ConfigurationRuleVO ConfigurationRuleVO);

    void deleteById(Long projectId, Long filterId);

    ConfigurationRuleVO queryById(Long projectId, Long filterId);

    /**
     * 分页搜索过滤条件
     *
     * @param projectId            projectId
     * @return ConfigurationRuleVO
     */
    List<ConfigurationRuleVO> listByProjectId(Long projectId, PageRequest pageRequest);
}

