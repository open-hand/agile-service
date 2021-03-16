package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.AppVersionWithIssueVO;
import io.choerodon.agile.infra.dto.AppVersionIssueRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-09
 */
public interface AppVersionIssueRelMapper extends BaseMapper<AppVersionIssueRelDTO> {

    List<AppVersionWithIssueVO> selectCompletedBugByAppVersionIds(@Param("projectIds") Set<Long> projectIds,
                                                                  @Param("organizationId") Long organizationId,
                                                                  @Param("appVersionIds") Set<Long> appVersionIds);
}
