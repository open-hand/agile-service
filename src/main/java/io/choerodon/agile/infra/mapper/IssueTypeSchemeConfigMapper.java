package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssueTypeSchemeConfigDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * @author shinan.chen
 * @date 2018/8/10
 */
@Component
public interface IssueTypeSchemeConfigMapper extends BaseMapper<IssueTypeSchemeConfigDTO> {
    void deleteBySchemeId(@Param("organizationId") Long organizationId, @Param("schemeId") Long schemeId);

    BigDecimal selectMaxSequence(@Param("issueTypeSchemeId") Long issueTypeSchemeId,
                                 @Param("organizationId") Long organizationId);
}
