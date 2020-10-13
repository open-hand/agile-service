package io.choerodon.agile.infra.mapper;

import java.util.List;

import io.choerodon.agile.infra.dto.ConfigurationRuleReceiverDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * Mapper
 *
 * @author jiaxu.cui@hand-china.com 2020-09-23 09:29:15
 */
public interface ConfigurationRuleReceiverMapper extends BaseMapper<ConfigurationRuleReceiverDTO> {

    List<ConfigurationRuleReceiverDTO> selectReceiver(@Param("ruleIdList") List<Long> ruleIdList,
                                                  @Param("type") String type);
}

