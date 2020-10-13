package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.infra.dto.ConfigurationRuleFiledDTO;
/**
 * 服务接口
 *
 * @author jiaxu.cui@hand-china.com
 * @date 2020-09-23 09:29:15
 */
public interface ConfigurationRuleFiledService {

    List<ConfigurationRuleFiledDTO> list(Long projectId);
}

