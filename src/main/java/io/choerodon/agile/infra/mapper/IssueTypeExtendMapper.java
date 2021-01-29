package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssueTypeExtendDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-01-20
 */
public interface IssueTypeExtendMapper extends BaseMapper<IssueTypeExtendDTO> {

    List<IssueTypeExtendDTO> selectByIssueTypeIds(@Param("issueTypeIds") Set<Long> issueTypeIds,
                                                  @Param("organizationId") Long organizationId);
}
