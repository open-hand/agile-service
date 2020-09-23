package io.choerodon.agile.infra.mapper;

import java.util.List;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.infra.dto.ConfigurationRuleDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * Mapper
 *
 * @author jiaxu.cui@hand-china.com 2020-09-23 09:29:15
 */
public interface ConfigurationRuleMapper extends BaseMapper<ConfigurationRuleDTO> {

    List<ConfigurationRuleVO> queryFiltersByProjectId(@Param("projectId") Long projectId);
}

