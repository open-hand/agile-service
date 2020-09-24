package io.choerodon.agile.app.service.impl;

import java.util.List;

import io.choerodon.agile.infra.mapper.ConfigurationRuleFiledMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import io.choerodon.agile.infra.dto.ConfigurationRuleFiledDTO;
import io.choerodon.agile.app.service.ConfigurationRuleFiledService;

/**
 * 服务实现类
 *
 * @author jiaxu.cui@hand-china.com
 * @date 2020-09-23 09:29:15
 */
@Service
public class ConfigurationRuleFiledServiceImpl implements ConfigurationRuleFiledService {
    
    @Autowired
    private ConfigurationRuleFiledMapper configurationRuleFiledMapper;

    @Override
    public List<ConfigurationRuleFiledDTO> list(Long projectId) {
        return configurationRuleFiledMapper.selectAll();
    }
}
